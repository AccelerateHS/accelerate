{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Array.Remote.LRU
-- Copyright   : [2015..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module extends the memory tables provided by
-- 'Data.Array.Accelerate.Array.Remote.Table' with an LRU caching policy that
-- evicts old arrays from the remote memory space once it runs out of memory.
-- Consequently, use of this module requires the backend client to keep track of
-- which remote arrays are currently being used, so that they will not be
-- evicted. See: 'withRemote' for more details on this requirement.
--
module Data.Array.Accelerate.Array.Remote.LRU (

  -- Tables for host/device memory associations
  MemoryTable, new, withRemote, malloc, free, insertUnmanaged, reclaim,

  -- Asynchronous tasks
  Task(..)

) where

import Data.Array.Accelerate.Analysis.Match                         ( matchSingleType, (:~:)(..) )
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Remote.Class
import Data.Array.Accelerate.Array.Remote.Table                     ( StableArray, makeWeakArrayData, formatStableArray )
import Data.Array.Accelerate.Array.Unique                           ( touchUniqueArray )
import Data.Array.Accelerate.Error                                  ( internalError )
import Data.Array.Accelerate.Type
import qualified Data.Array.Accelerate.Array.Remote.Table           as Basic
import qualified Data.Array.Accelerate.Debug.Internal.Flags         as Debug
import qualified Data.Array.Accelerate.Debug.Internal.Trace         as Debug

import Control.Concurrent.MVar                                      ( MVar, newMVar, withMVar, takeMVar, putMVar, mkWeakMVar )
import Control.Monad                                                ( filterM )
import Control.Monad.Catch
import Control.Monad.IO.Class                                       ( MonadIO, liftIO )
import Data.Functor
import Data.Maybe                                                   ( isNothing )
import Data.Text.Lazy.Builder                                       ( Builder )
import Formatting
import System.CPUTime
import System.Mem.Weak                                              ( Weak, deRefWeak, finalize )
import qualified Data.HashTable.IO                                  as HT
import Prelude                                                      hiding ( lookup )

import GHC.Stack


-- We build cached memory tables on top of a basic memory table.
--
-- A key invariant is that the arrays in the MemoryTable are a subset of the
-- arrays in the UseTable. The UseTable reflects all arrays that have ever been
-- in the cache.
--
data MemoryTable p task = MemoryTable {-# UNPACK #-} !(Basic.MemoryTable p)
                                      {-# UNPACK #-} !(UseTable task)
                                      {-# UNPACK #-} !(Weak (UseTable task))

type UT task            = HT.CuckooHashTable StableArray (Used task)
type UseTable task      = MVar (UT task)

data Status = Clean     -- Array in remote memory matches array in host memory.
            | Dirty     -- Array in remote memory has been modified.
            | Unmanaged -- Array in remote memory was injected by FFI, so we
                        -- cannot remove it under any circumstance.
            | Evicted   -- Array has been evicted from remote memory
            deriving Eq

type Timestamp = Integer

data Used task where
  Used :: ArrayData e ~ ScalarArrayData e
       => !Timestamp
       -> !Status
       -> {-# UNPACK #-} !Int                   -- Use count
       -> ![task]                               -- Asynchronous tasks using the array
       -> {-# UNPACK #-} !Int                   -- Number of elements
       -> !(SingleType e)
       -> {-# UNPACK #-} !(Weak (ScalarArrayData e))
       -> Used task

-- | A Task represents a process executing asynchronously that can be polled for
-- its status. This is necessary for backends that work asynchronously (i.e.
-- the CUDA backend). If a backend is synchronous, the () instance can be used.
--
class Task task where
  -- |Returns true when the task has finished.
  completed :: task -> IO Bool

instance Task () where
  completed () = return True

-- | Create a new memory cache from host to remote arrays.
--
-- The function supplied should be the `free` for the remote pointers being
-- stored. This function will be called by the GC, which typically runs on a
-- different thread. Unlike the `free` in `RemoteMemory`, this function cannot
-- depend on any state.
--
new :: (forall a. ptr a -> IO ()) -> IO (MemoryTable ptr task)
new release = do
  mt        <- Basic.new release
  utbl      <- HT.new
  ref       <- newMVar utbl
  weak_utbl <- mkWeakMVar ref (cache_finalizer utbl)
  return    $! MemoryTable mt ref weak_utbl

-- | Perform some action that requires the remote pointer corresponding to
-- the given array. Returns `Nothing` if the array have NEVER been in the
-- cache. If the array was previously in the cache, but was evicted due to its
-- age, then the array will be copied back from host memory.
--
-- The continuation passed as the third argument needs to obey some precise
-- properties. As with all bracketed functions, the supplied remote pointer must
-- not leak out of the function, as it is only guaranteed to be valid within it.
-- If it is required that it does leak (e.g. the backend uses concurrency to
-- interleave execution of different parts of the program), then `completed` on
-- the returned task should not return true until it is guaranteed there are no
-- more accesses of the remote pointer.
--
withRemote
    :: forall task m a c. (HasCallStack, Task task, RemoteMemory m, MonadIO m, Functor m)
    => MemoryTable (RemotePtr m) task
    -> SingleType a
    -> ArrayData a
    -> (RemotePtr m (ScalarArrayDataR a) -> m (task, c))
    -> m (Maybe c)
withRemote (MemoryTable !mt !ref _) !tp !arr run | SingleArrayDict <- singleArrayDict tp = do
  key <- Basic.makeStableArray tp arr
  mp  <- withMVar' ref $ \utbl -> do
    mu  <- liftIO . HT.mutate utbl key $ \case
      Nothing -> (Nothing,           Nothing)
      Just u  -> (Just (incCount u), Just u)
    --
    case mu of
      Nothing -> do
        message ("withRemote/array has never been malloc'd: " % formatStableArray) key
        return Nothing -- The array was never in the table

      Just u  -> do
        mp  <- liftIO $ Basic.lookup @m mt tp arr
        ptr <- case mp of
                Just p          -> return p
                Nothing
                  | isEvicted u -> copyBack utbl (incCount u)
                  | otherwise   -> do message ("lost array " % formatStableArray) key
                                      internalError "non-evicted array has been lost"
        return (Just ptr)
  --
  case mp of
    Nothing  -> return Nothing
    Just ptr -> Just <$> go key ptr
  where
    updateTask :: Used task -> task -> IO (Used task)
    updateTask (Used _ status count tasks n tp' weak_arr) task = do
      ts      <- getCPUTime
      tasks'  <- cleanUses tasks
      return (Used ts status (count - 1) (task : tasks') n tp' weak_arr)

    copyBack :: HasCallStack => UT task -> Used task -> m (RemotePtr m (ScalarArrayDataR a))
    copyBack utbl (Used ts _ count tasks n tp' weak_arr)
      | Just Refl <- matchSingleType tp tp' = do
        message "withRemote/reuploading-evicted-array"
        p <- mallocWithUsage mt utbl tp arr (Used ts Clean count tasks n tp weak_arr)
        pokeRemote tp n p arr
        return p
      | otherwise = internalError "Type mismatch"

    -- We can't combine the use of `withMVar ref` above with the one here
    -- because the `permute` operation from the PTX backend requires nested
    -- calls to `withRemote` in order to copy the defaults array.
    --
    go :: (HasCallStack, ArrayData a ~ ScalarArrayData a)
       => StableArray
       -> RemotePtr m (ScalarArrayDataR a)
       -> m c
    go key ptr = do
      message ("withRemote/using: " % formatStableArray) key
      (task, c) <- run ptr
      liftIO . withMVar ref  $ \utbl -> do
        HT.mutateIO utbl key $ \case
          Nothing -> internalError "invariant violated"
          Just u  -> do
            u' <- updateTask u task
            return (Just u', ())
        --
        touchUniqueArray arr
      return c


-- | Allocate a new device array to be associated with the given host-side array.
-- This has similar behaviour to malloc in Data.Array.Accelerate.Array.Memory.Table
-- but also will copy remote arrays back to main memory in order to make space.
--
-- The third argument indicates that the array should be considered frozen. That
-- is to say that the array contents will never change. In the event that the
-- array has to be evicted from the remote memory, the copy already residing in
-- host memory should be considered valid.
--
-- If this function is called on an array that is already contained within the
-- cache, this is a no-op.
--
-- On return, 'True' indicates that we allocated some remote memory, and 'False'
-- indicates that we did not need to.
--
malloc :: forall e m task. (HasCallStack, RemoteMemory m, MonadIO m, Task task)
       => MemoryTable (RemotePtr m) task
       -> SingleType e
       -> ArrayData e
       -> Bool            -- ^ True if host array is frozen.
       -> Int             -- ^ Number of elements
       -> m Bool          -- ^ Was the array allocated successfully?
malloc (MemoryTable mt ref weak_utbl) !tp !ad !frozen !n | SingleArrayDict <- singleArrayDict tp = do -- Required for ArrayData e ~ ScalarArrayData e
  ts  <- liftIO $ getCPUTime
  key <- Basic.makeStableArray tp ad
  --
  let status = if frozen
                 then Clean
                 else Dirty
  --
  withMVar' ref $ \utbl -> do
    mu <- liftIO $ HT.lookup utbl key
    if isNothing mu
      then do
        weak_arr <- liftIO $ makeWeakArrayData tp ad ad (Just $ finalizer key weak_utbl)
        _        <- mallocWithUsage mt utbl tp ad (Used ts status 0 [] n tp weak_arr)
        return True
      else
        return False

mallocWithUsage
    :: forall e m task. (HasCallStack, RemoteMemory m, MonadIO m, Task task, ArrayData e ~ ScalarArrayData e)
    => Basic.MemoryTable (RemotePtr m)
    -> UT task
    -> SingleType e
    -> ArrayData e
    -> Used task
    -> m (RemotePtr m (ScalarArrayDataR e))
mallocWithUsage !mt !utbl !tp !ad !usage@(Used _ _ _ _ n _ _) = malloc'
  where
    malloc' :: HasCallStack => m (RemotePtr m (ScalarArrayDataR e))
    malloc' = do
      mp <- Basic.malloc @e @m mt tp ad n :: m (Maybe (RemotePtr m (ScalarArrayDataR e)))
      case mp of
        Nothing -> do
          success <- evictLRU utbl mt
          if success then malloc'
                     else internalError "Remote memory exhausted"
        Just p -> liftIO $ do
          key <- Basic.makeStableArray tp ad
          HT.insert utbl key usage
          return p

evictLRU
    :: forall m task. (HasCallStack, RemoteMemory m, MonadIO m, Task task)
    => UT task
    -> Basic.MemoryTable (RemotePtr m)
    -> m Bool
evictLRU !utbl !mt = trace "evictLRU/evicting-eldest-array" $ do
  mused <- liftIO $ HT.foldM eldest Nothing utbl
  case mused of
    Just (sa, Used ts status count tasks n tp weak_arr) -> do
      mad <- liftIO $ deRefWeak weak_arr
      case mad of
        Nothing -> liftIO $ do
          -- This can only happen if our eviction process was interrupted by
          -- garbage collection. In which case, even though we didn't actually
          -- evict anything, we should return true, as we know some remote
          -- memory is now free.
          --
          -- Small caveat: Due to finalisers being delayed, it's a good idea
          -- to free the array here.
          Basic.freeStable @m mt sa
          delete utbl sa
          message "evictLRU/Accelerate GC interrupted by GHC GC"

        Just arr -> do
          message ("evictLRU/evicting " % formatStableArray) sa
          copyIfNecessary status n tp arr
          -- liftIO $ Debug.remote_memory_evict sa (remoteBytes tp n)
          liftIO $ Basic.freeStable @m mt sa
          liftIO $ HT.insert utbl sa (Used ts Evicted count tasks n tp weak_arr)
      return True
    _ -> trace "evictLRU/All arrays in use, unable to evict" $ return False
  where
    -- Find the eldest, not currently in use, array.
    eldest :: (Maybe (StableArray, Used task)) -> (StableArray, Used task) -> IO (Maybe (StableArray, Used task))
    eldest prev (sa, used@(Used ts status count tasks n tp weak_arr))
      | count == 0
      , evictable status
      = do
          tasks' <- cleanUses tasks
          HT.insert utbl sa (Used ts status count tasks' n tp weak_arr)
          case tasks' of
            [] | Just (_, Used ts' _ _ _ _ _ _) <- prev
               , ts < ts'        -> return (Just (sa, used))
               | Nothing <- prev -> return (Just (sa, used))
            _  -> return prev
    eldest prev _ = return prev

    -- remoteBytes :: SingleType e -> Int -> Int
    -- remoteBytes tp n = bytesElt (TupRsingle (SingleScalarType tp)) * n

    evictable :: Status -> Bool
    evictable Clean     = True
    evictable Dirty     = True
    evictable Unmanaged = False
    evictable Evicted   = False

    copyIfNecessary :: Status -> Int -> SingleType e -> ArrayData e -> m ()
    copyIfNecessary Clean     _ _  _  = return ()
    copyIfNecessary Unmanaged _ _  _  = return ()
    copyIfNecessary Evicted   _ _  _  = internalError "Attempting to evict already evicted array"
    copyIfNecessary Dirty     n tp ad = do
      mp <- liftIO $ Basic.lookup @m mt tp ad
      case mp of
        Nothing -> return () -- RCE: I think this branch is actually impossible.
        Just p  -> peekRemote tp n p ad

-- | Deallocate the device array associated with the given host-side array.
-- Typically this should only be called in very specific circumstances. This
-- operation is not thread-safe.
--
free :: forall m a task. (HasCallStack, RemoteMemory m)
     => MemoryTable (RemotePtr m) task
     -> SingleType a
     -> ArrayData a
     -> IO ()
free (MemoryTable !mt !ref _) !tp !arr
  = withMVar' ref
  $ \utbl -> do
      key <- Basic.makeStableArray tp arr
      delete utbl key
      Basic.freeStable @m mt key

-- | Record an association between a host-side array and a remote memory area
-- that was not allocated by accelerate. The remote memory will NOT be re-used
-- once the host-side array is garbage collected.
--
-- This typically only has use for backends that provide an FFI.
--
insertUnmanaged
    :: (HasCallStack, MonadIO m, RemoteMemory m)
    => MemoryTable (RemotePtr m) task
    -> SingleType e
    -> ArrayData e
    -> RemotePtr m (ScalarArrayDataR e)
    -> m ()
insertUnmanaged (MemoryTable mt ref weak_utbl) !tp !arr !ptr | SingleArrayDict <- singleArrayDict tp = do -- Gives evidence that ArrayData e ~ ScalarArrayData e
  key <- Basic.makeStableArray tp arr
  ()  <- Basic.insertUnmanaged mt tp arr ptr
  liftIO
    $ withMVar ref
    $ \utbl -> do
      ts        <- getCPUTime
      weak_arr  <- makeWeakArrayData tp arr arr (Just $ finalizer key weak_utbl)
      HT.insert utbl key (Used ts Unmanaged 0 [] 0 tp weak_arr)


-- Removing entries
-- ----------------

finalizer :: StableArray -> Weak (UseTable task) -> IO ()
finalizer !key !weak_utbl = do
  mref <- deRefWeak weak_utbl
  case mref of
    Nothing  -> message "finalize cache/dead table"
    Just ref -> trace (bformat ("finalize cache: " % formatStableArray) key) $ withMVar' ref (`delete` key)

delete :: UT task -> StableArray -> IO ()
delete = HT.delete


-- | Initiate garbage collection and `free` any remote arrays that no longer
-- have matching host-side equivalents.
--
reclaim
    :: forall m task. (HasCallStack, RemoteMemory m, MonadIO m)
    => MemoryTable (RemotePtr m) task
    -> m ()
reclaim (MemoryTable !mt _ _) = Basic.reclaim mt

cache_finalizer :: UT task -> IO ()
cache_finalizer !tbl
  = trace "cache finaliser"
  $ HT.mapM_ (\(_,u) -> f u) tbl
  where
    f :: Used task -> IO ()
    f (Used _ _ _ _ _ _ w) = finalize w

-- Miscellaneous
-- -------------

cleanUses :: Task task => [task] -> IO [task]
cleanUses = filterM (fmap not . completed)

incCount :: Used task -> Used task
incCount (Used ts status count uses n tp weak_arr) = Used ts status (count + 1) uses n tp weak_arr

isEvicted :: Used task -> Bool
isEvicted (Used _ status _ _ _ _ _) = status == Evicted

{-# INLINE withMVar' #-}
withMVar' :: (MonadIO m, MonadMask m) => MVar a -> (a -> m b) -> m b
withMVar' m f =
  mask $ \restore -> do
    a <- takeMVar' m
    b <- restore (f a) `onException` putMVar' m a
    putMVar' m a
    return b

{-# INLINE putMVar' #-}
putMVar' :: (MonadIO m, MonadMask m) => MVar a -> a -> m ()
putMVar' m a = liftIO (putMVar m a)

{-# INLINE takeMVar' #-}
takeMVar' :: (MonadIO m, MonadMask m) => MVar a -> m a
takeMVar' m = liftIO (takeMVar m)


-- Debug
-- -----

{-# INLINE trace #-}
trace :: MonadIO m => Builder -> m a -> m a
trace msg next = message builder msg >> next

{-# INLINE message #-}
message :: MonadIO m => Format (m ()) a -> a
message fmt = Debug.traceM Debug.dump_gc ("gc: " % fmt)

