{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Array.Memory.Cache
-- Copyright   : [2015..2015] Manuel M T Chakravarty, Gabriele Keller,
--                            Robert Clifton-Everest
-- License     : BSD3
--
-- Maintainer  : Robert Clifton-Everest <robertce@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module contains a memory manager accelerate backends can use, similar
-- to 'Data.Array.Accelerate.Array.Memory.Table'. The difference, however, is
-- that a `MemoryCache` will move old arrays from the remote memory into host
-- memory when the remote memory is exhausted. This requires keeping of track
-- of what remote pointers are being "used". See `withRemote` below.
--
module Data.Array.Accelerate.Array.Memory.Cache (

  -- Tables for host/device memory associations
  MemoryCache, new, withRemote, malloc, free, insertUnmanaged, reclaim,

  -- Asynchronous tasks
  Task(..)

) where

import Data.Functor
import Data.Maybe                                               ( isNothing )
import Data.Proxy
import Control.Monad                                            ( filterM )
import Control.Monad.Catch
import Control.Monad.IO.Class                                   ( MonadIO, liftIO )
import Control.Concurrent.MVar                                  ( MVar, newMVar, takeMVar, putMVar, mkWeakMVar )
import Control.Concurrent                                       ( yield )
import System.CPUTime
import System.Mem.Weak                                          ( Weak, deRefWeak, finalize )
import Prelude                                                  hiding ( lookup )

import qualified Data.HashTable.IO                              as HT

import qualified Data.Array.Accelerate.Debug                    as D
import Data.Array.Accelerate.Error                              ( internalError )
import Data.Array.Accelerate.Array.Data                         ( ArrayData, touchArrayData )
import Data.Array.Accelerate.Array.Memory                       ( RemoteMemory, RemotePointer, PrimElt )
import qualified Data.Array.Accelerate.Array.Memory             as M
import Data.Array.Accelerate.Array.Memory.Table                 ( MemoryTable, StableArray, makeWeakArrayData )
import qualified Data.Array.Accelerate.Array.Memory.Table       as MT

-- We build the cache on top of a memory table.
-- A key invariant is that the arrays in the MemoryTable are a subset of the
-- arrays in the UseTable. The UseTable reflects all arrays that have ever been
-- in the cache.
data MemoryCache p task = MemoryCache (MemoryTable p) (UseTable task) (Weak (UseTable task))

type UseTable task = MVar (UT task)

type UT task = HT.BasicHashTable StableArray (Used task)

data Status = Clean     -- Array in remote memory matches array in host memory.
            | Dirty     -- Array in remote memory has been modified.
            | Unmanaged -- Array in remote memory was injected by FFI, so we
                        -- cannot remove it under any circumstance.
            | Evicted   -- Array has been evicted from remote memory
            deriving Eq

type Timestamp = Integer

data Used task where
  Used :: (PrimElt e a)
       => Timestamp
       -> Status
       -> Int      -- Use count
       -> [task]   -- Asynchronous tasks using the array
       -> Int      -- Array size
       -> Weak (ArrayData e)
       -> Used task

-- |A Task represents a process executing asynchronously that can be polled for
-- its status. This is necessary for backends that work asynchronously (i.e.
-- the CUDA backend). If a backend is synchronous, the () instance can be used.
class Task task where
  -- |Returns true when the task has finished.
  isDone :: task -> IO Bool

instance Task () where
  isDone () = return True

-- |Create a new memory cache from host to remote arrays.
--
-- The function supplied should be the `free` for the remote pointers being
-- stored. This function will be called by the GC, which typically runs on a
-- different thread. Unlike the `free` in `RemoteMemory`, this function cannot
-- depend on any state.
--
new :: (RemoteMemory m, MonadIO m) => (forall a. RemotePointer m a -> IO ()) -> m (MemoryCache (RemotePointer m) task)
new free = do
  mt   <- MT.new free
  utbl <- liftIO $ HT.new
  ref  <- liftIO $ newMVar utbl
  weak_utbl <- liftIO $ mkWeakMVar ref (cache_finalizer utbl)
  return $ MemoryCache mt ref weak_utbl

-- |Perform some IO action that requires the remote pointer corresponding to
-- the given array. Returns `Nothing` if the array have NEVER been in the
-- cache. If the array was previously in the cache, but was evicted due to its
-- age, then the array will be copied back from host memory.
--
-- The continuation passed as the third argument needs to obey some precise
-- properties. Firstly, the supplied remote pointer should not leak out of the
-- function, as it is only guaranteed to be valid within it. If it is required
-- that it does leak (e.g. the backend is uses concurrency to interleave
-- execution of different parts of the program), then `isDone` on the returned
-- task should not return true until it is guaranteed there are no more acesses
-- of the remote pointer.
--
withRemote :: forall task m a b c. (PrimElt a b, Task task, RemoteMemory m, MonadIO m, Functor m)
           => MemoryCache (RemotePointer m) task
           -> ArrayData a
           -> (RemotePointer m b -> IO (task, c))
           -> m (Maybe c)
withRemote (MemoryCache !mt !ref _) !arr run = do
  key  <- MT.makeStableArray arr
  mp <- withMVar' ref $ \utbl -> do
   mu <- liftIO $ HT.lookup utbl key
   case mu of
     Nothing -> trace ("withRemote/array has never been malloc'd: " ++ show key)
              $ return Nothing
     Just u  -> Just <$> do
       liftIO $ HT.insert utbl key (incCount u)
       mp <- liftIO $ MT.lookup mt arr
       case mp of
         Nothing | isEvicted u -> copy utbl (incCount u)
         Just p                -> return p
         _                     -> trace ("lost array " ++ show key) $ $internalError "withRemote" "non-evicted array has been lost"

  case mp of
    Just p -> Just <$> run' p
    Nothing -> return Nothing -- The array was never in the table.
  where
    updateTask :: Maybe (Used task) -> task -> IO (Used task)
    updateTask mu task = do
      ts  <- getCPUTime
      case mu of
        Nothing -> $internalError "withRemote" "Invariant violated"
        Just (Used _ status count tasks n weak_arr) -> do
          tasks'  <- cleanUses tasks
          return (Used ts status (count - 1) (task : tasks') n weak_arr)

    copy :: UT task -> Used task -> m (RemotePointer m b)
    copy utbl (Used ts _ count tasks n weak_arr) = do
      message "withRemote/reuploading-evicted-array"
      p <- mallocWithUsage mt utbl arr
                 (Used ts Clean count tasks n weak_arr)
      M.poke n p arr
      return p


    run' :: RemotePointer m b -> m c
    run' p = liftIO $ do
      key <- MT.makeStableArray arr
      message ("withRemote/using: " ++ show key)
      (task, c) <- run p
      withMVar' ref $ \utbl -> do
        mu       <- HT.lookup utbl key
        u        <- updateTask mu task
        HT.insert utbl key u
      touchArrayData arr
      return c


-- | Allocate a new device array to be associated with the given host-side array.
-- This has similar behaviour to malloc in Data.Array.Accelerate.Array.Memory.Table
-- but also will copy remote arrays back to main memory in order to make space.
--
-- The third argument indicates that the array should be considered frozen.
-- That is to say the array arrays contents will never change. In the event that
-- the array has to be evicted from the remote memory, the copy already residing
-- in host memory should be considered valid.
--
-- If malloc is called on an array that is already contained within the cache,
-- it becomes a no-op.
--
malloc :: forall a e m task. (PrimElt e a, RemoteMemory m, MonadIO m, Task task)
       => MemoryCache (RemotePointer m) task
       -> ArrayData e
       -> Bool                               -- ^True if host array is frozen.
       -> Int
       -> m Bool
malloc (MemoryCache mt ref weak_utbl) !ad !frozen !n = do
  ts <- liftIO $ getCPUTime
  key <- MT.makeStableArray ad
  let status = if frozen then Clean else Dirty

  withMVar' ref $ \utbl -> do
    mu <- liftIO $ HT.lookup utbl key
    if isNothing mu then do
      weak_arr <- liftIO $ makeWeakArrayData ad ad (Just $ finalizer key weak_utbl)
      _ <- mallocWithUsage mt utbl ad (Used ts status 0 [] n weak_arr)
      return True
    else return False

mallocWithUsage
    :: forall a e m task. (PrimElt e a, RemoteMemory m, MonadIO m, Task task)
    => MemoryTable (RemotePointer m)
    -> UT task
    -> ArrayData e
    -> Used task
    -> m (RemotePointer m a)
mallocWithUsage !mt utbl !ad !usage@(Used _ _ _ _ n _) = malloc'
  where
    malloc' = do
      mp <- MT.malloc mt ad n :: m (Maybe (RemotePointer m a))
      case mp of
        Nothing -> do
          success <- evictLRU utbl mt
          if success then malloc' else $internalError "malloc" "Remote memory exhausted"
        Just p -> liftIO $ do
          key <- MT.makeStableArray ad
          HT.insert utbl key usage
          return p

evictLRU :: forall m task. (RemoteMemory m, MonadIO m, Task task)
         => UT task
         -> MemoryTable (RemotePointer m)
         -> m Bool
evictLRU utbl mt = trace "evictLRU/evicting-eldest-array" $  do
  mused <- liftIO $ HT.foldM eldest Nothing utbl
  case mused of
    Just (sa, Used ts status count tasks n weak_arr) -> do
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
          MT.freeStable (Proxy :: Proxy m) mt sa
          delete utbl sa
          message "evictLRU/Accelerate GC interrupted by GHC GC"
        Just arr -> do
          message ("evictLRU/evicting " ++ show sa)
          copyIfNecessary status n arr
          liftIO $ MT.freeStable (Proxy :: Proxy m) mt sa
          liftIO $ HT.insert utbl sa (Used ts Evicted count tasks n weak_arr)
      return True
    _ -> trace "evictLRU/All arrays in use, unable to evict" $ return False
  where
    -- Find the eldest, not currently in use, array.
    eldest :: (Maybe (StableArray, Used task)) -> (StableArray, Used task) -> IO (Maybe (StableArray, Used task))
    eldest prev (sa, used@(Used ts status count tasks n weak_arr)) | count == 0
                                                                   , evictable status = do
      tasks' <- cleanUses tasks
      HT.insert utbl sa (Used ts status count tasks' n weak_arr)
      case tasks' of
        [] | Just (_, Used ts' _ _ _ _ _) <- prev
           , ts < ts'        -> return (Just (sa, used))
           | Nothing <- prev -> return (Just (sa, used))
        _  -> return prev
    eldest prev _ = return prev

    evictable :: Status -> Bool
    evictable Clean     = True
    evictable Dirty     = True
    evictable Unmanaged = False
    evictable Evicted   = False

    copyIfNecessary :: PrimElt e a => Status -> Int -> ArrayData e -> m ()
    copyIfNecessary Clean     _ _  = return ()
    copyIfNecessary Unmanaged _ _  = return ()
    copyIfNecessary Evicted   _ _  = $internalError "evictLRU" "Attempting to evict already evicted array"
    copyIfNecessary Dirty     n ad = do
      mp <- liftIO $ MT.lookup mt ad
      case mp of
        Nothing -> return () -- RCE: I think this branch is actually impossible.
        Just p  -> M.peek n p ad

-- | Deallocate the device array associated with the given host-side array.
-- Typically this should only be called in very specific circumstances. This
-- operation is not thread-safe.
--
free :: (RemoteMemory m, PrimElt a b) => proxy m -> MemoryCache (RemotePointer m) task -> ArrayData a -> IO ()
free proxy (MemoryCache !mt !ref _) !arr = withMVar' ref $ \utbl -> do
  key <- MT.makeStableArray arr
  mu <- HT.lookup utbl key
  case mu of
    Nothing -> return ()
    Just (Used _ _ _ _ _ weak_arr) -> finalize weak_arr
  MT.freeStable proxy mt key
  yield

-- |Record an association between a host-side array and a remote memory area
-- that was not allocated by accelerate. The remote memory will NOT be re-used
-- once the host-side array is garbage collected.
--
-- This typically only has use for backends that provide an FFI.
--
insertUnmanaged :: (PrimElt e a, MonadIO m, Task task) => MemoryCache p task -> ArrayData e -> p a -> m ()
insertUnmanaged (MemoryCache mt ref weak_utbl) !arr !ptr = liftIO . withMVar' ref $ \utbl -> do
  key <- MT.makeStableArray arr
  MT.insertUnmanaged mt arr ptr
  ts <- getCPUTime
  weak_arr <- makeWeakArrayData arr arr (Just $ finalizer key weak_utbl)
  HT.insert utbl key (Used ts Unmanaged 0 [] 0 weak_arr)


-- Removing entries
-- ----------------

finalizer :: Task task => StableArray -> Weak (UseTable task) -> IO ()
finalizer !key !weak_utbl = do
  mref <- deRefWeak weak_utbl
  case mref of
    Nothing -> trace "finalize cache/dead table" $ return ()
    Just ref -> trace ("finalize cache: " ++ show key) $ withMVar' ref (`delete` key)

delete :: Task task => UT task -> StableArray -> IO ()
delete utbl key = do
  mu <- HT.lookup utbl key
  case mu of
    Nothing -> return ()
    Just _  -> HT.delete utbl key

-- |Initiate garbage collection and `free` any remote arrays that no longer
-- have matching host-side equivalents.
--
reclaim :: forall m task. (RemoteMemory m, MonadIO m) => MemoryCache (RemotePointer m) task -> m ()
reclaim (MemoryCache !mt _ _) = MT.reclaim mt

cache_finalizer :: UT task -> IO ()
cache_finalizer !tbl
  = trace "cache finaliser"
  $ HT.mapM_ (\(_,u) -> f u)
             tbl
  where
    f :: Used task -> IO ()
    f (Used _ _ _ _ _ w) = finalize w

-- Miscellaneous
-- -------------

cleanUses :: Task task => [task] -> IO [task]
cleanUses = filterM (fmap not . isDone)

incCount :: Used task -> Used task
incCount (Used ts status count uses n weak_arr) = Used ts status (count + 1) uses n weak_arr

isEvicted :: Used task -> Bool
isEvicted (Used _ status _ _ _ _) = status == Evicted

withMVar' :: (MonadIO m, MonadCatch m, MonadMask m) => MVar a -> (a -> m b) -> m b
withMVar' m f = mask $ \restore -> do
  a <- liftIO $ takeMVar m
  b <- restore (f a) `onException` (liftIO $ putMVar m a)
  liftIO $ putMVar m a
  return b

-- Debug
-- -----

{-# INLINE trace #-}
trace :: MonadIO m => String -> m a -> m a
trace msg next = message msg >> next

{-# INLINE message #-}
message :: MonadIO m => String -> m ()
message msg = liftIO $ D.traceIO D.dump_gc ("gc: " ++ msg)

