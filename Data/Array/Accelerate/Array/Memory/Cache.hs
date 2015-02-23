{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
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
  MemoryCache, new, withRemote, contains, malloc, free, insertUnmanaged, reclaim,

  -- Asynchronous tasks
  Task(..)

) where

import Prelude                                                  hiding ( lookup )
import Data.Functor                                             ( (<$>) )
import Data.Maybe                                               ( isJust )
import Data.Typeable                                            ( Typeable )
import Control.Monad                                            ( unless, filterM )
import Control.Monad.IO.Class                                   ( MonadIO, liftIO )
import Control.Concurrent.MVar                                  ( MVar, newMVar, withMVar, takeMVar, putMVar )
import System.CPUTime
import System.Mem.Weak                                          ( Weak, deRefWeak, mkWeakPtr )

import qualified Data.HashTable.IO                              as HT

import qualified Data.Array.Accelerate.Debug                    as D
import Data.Array.Accelerate.Error                              ( internalError )
import Data.Array.Accelerate.Array.Data                         ( ArrayData )
import Data.Array.Accelerate.Array.Memory                       ( RemoteMemory, RemotePointer, PrimElt )
import qualified Data.Array.Accelerate.Array.Memory             as M
import Data.Array.Accelerate.Array.Memory.Table                 ( MemoryTable, StableArray )
import qualified Data.Array.Accelerate.Array.Memory.Table       as MT

-- We build the cache on top of a memory table.
-- A key invariant is that the arrays in the MemoryTable are a subset of the
-- arrays in the UseTable. The UseTable reflects all arrays that have ever been
-- in the cache.
data MemoryCache p task = MemoryCache (MemoryTable p) (UseTable task)

type UseTable task = MVar (HT.BasicHashTable StableArray (Used task))

data Status = Clean     -- Array in remote memory matches array in host memory.
            | Dirty     -- Array in remote memory has been modified.
            | Unmanaged -- Array in remote memory was injected by FFI, so we
                        -- cannot remove it under any circumstance.
            deriving Eq

type Timestamp = Integer

data Used task where
  Used :: (PrimElt e a)
       => Timestamp
       -> Status
       -> [task]
       -> Int
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

-- Referencing arrays
-- ------------------

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
  return $ MemoryCache mt ref

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
withRemote mc@(MemoryCache !tbl !ref) !arr run = do
  mp  <- liftIO $ withMVar ref $ const $ MT.lookup tbl arr
  case mp of
    Just p  -> Just <$> run' p -- The array already exists, use it.
    Nothing -> trace "withRemote/array not found" $ do
      mp' <- copyIfKnown -- If we have the array backed up in host memory,
                         -- move it back into the remote memory and use it.
      case mp' of
        Just p -> Just <$> run' p -- The move was possible.
        Nothing -> return Nothing -- The array was never in the table.
  where
    updateTask :: Maybe (Used task) -> task -> IO (Used task)
    updateTask mu task = do
      ts  <- getCPUTime
      case mu of
        Nothing -> $internalError "withRemote" "Invariant violated"
        Just (Used _ status tasks n weak_arr) -> do
          tasks'  <- cleanUses tasks
          return (Used ts status (task : tasks') n weak_arr)

    copyIfKnown :: m (Maybe (RemotePointer m b))
    copyIfKnown = do
      key <- MT.makeStableArray arr
      mu <- liftIO $ withMVar ref $ flip HT.lookup key
      case mu of
        Nothing -> return Nothing
        Just usage -> Just <$> mallocWithUsage mc arr usage

    run' :: RemotePointer m b -> m c
    run' p = liftIO $ do
      (task, c) <- run p
      key <- MT.makeStableArray arr
      withMVar ref $ \utbl -> do
        mu       <- HT.lookup utbl key
        u        <- updateTask mu task
        HT.insert utbl key u
      return c


-- | Check if a given array has ever been in the cache.
--
contains :: forall e a p task. PrimElt e a => MemoryCache p task -> ArrayData e -> IO Bool
contains (MemoryCache _ ref) !ad = withMVar ref $ \utbl -> do
  key <- MT.makeStableArray ad
  isJust <$> HT.lookup utbl key


-- | Allocate a new device array to be associated with the given host-side array.
-- This has similar behaviour to malloc in Data.Array.Accelerate.Array.Memory.Table
-- but also will copy remote arrays back to main memory in order to make space.
--
-- The third argument indicates that the array should be considered frozen.
-- That is to say the array arrays contents will never change. In the event that
-- the array has to be evicted from the remote memory, the copy already residing
-- in host memory should be considered valid.
malloc :: forall a e m task. (PrimElt e a, RemoteMemory m, MonadIO m, Task task)
       => MemoryCache (RemotePointer m) task
       -> ArrayData e
       -> Bool                               -- ^True if host array is frozen.
       -> Int
       -> m ()
malloc mc !ad !frozen !n = do
  ts <- liftIO $ getCPUTime
  let status = if frozen then Clean else Dirty
  weak_arr <- liftIO $ mkWeakPtr ad Nothing
  _ <- mallocWithUsage mc ad (Used ts status [] n weak_arr)
  return ()

mallocWithUsage
    :: forall a e m task. (PrimElt e a, RemoteMemory m, MonadIO m, Task task)
    => MemoryCache (RemotePointer m) task
    -> ArrayData e
    -> Used task
    -> m (RemotePointer m a)
mallocWithUsage (MemoryCache !mt !ref) !ad !usage@(Used _ _ _ n _) = do
  utbl <- liftIO $ takeMVar ref
  p <- malloc' utbl
  liftIO $ putMVar ref utbl
  return p
  where
    malloc' :: HT.BasicHashTable StableArray (Used task) -> m (RemotePointer m a)
    malloc' utbl = do
      mp <- MT.malloc mt ad n :: m (Maybe (RemotePointer m a))
      p <- case mp of
        Nothing -> trace "malloc/evicting-eldest-array" $ do
          success <- evictLRU utbl mt
          if success then malloc' utbl else error "Remote memory exhausted"
        Just p -> return p
      liftIO $ do
        key <- MT.makeStableArray ad
        HT.insert utbl key usage
      return p

evictLRU :: forall m task. (RemoteMemory m, MonadIO m, Task task)
         => HT.BasicHashTable StableArray (Used task)
         -> MemoryTable (RemotePointer m)
         -> m Bool
evictLRU utbl mt = do
  mused <- liftIO $ HT.foldM eldest Nothing utbl
  case mused of
    Just (sa, Used _ status _ n weak_arr) -> do
      mad <- liftIO $ deRefWeak weak_arr
      case mad of
        Nothing ->
          -- The array is no longer reachable. We can free it without concern.
          liftIO $ MT.freeStable mt sa
        Just arr -> do
          copyIfNecessary status n arr
          unless (status == Unmanaged) (liftIO $ MT.free mt arr)
      return True
    _ -> trace "evictLRU/All arrays in use, unable to evict" $ return False
  where
    -- Find the eldest, not currently in use, array.
    eldest :: (Maybe (StableArray, Used task)) -> (StableArray, Used task) -> IO (Maybe (StableArray, Used task))
    eldest prev (sa, used@(Used ts status tasks n weak_arr)) = do
        tasks' <- cleanUses tasks
        HT.insert utbl sa (Used ts status tasks' n weak_arr)
        case tasks' of
          [] | Just (_, Used ts' _ _ _ _) <-prev
             , ts' < ts -> return (Just (sa, used))
          _  -> return prev

    copyIfNecessary :: PrimElt e a => Status -> Int -> ArrayData e -> m ()
    copyIfNecessary Clean     _ _ = return ()
    copyIfNecessary Unmanaged _ _ = return ()
    copyIfNecessary Dirty n ad = do
      mp <- liftIO $ MT.lookup mt ad
      case mp of
        Nothing -> return () -- RCE: I think this branch is actually impossible.
        Just p  -> M.peek n p ad

-- | Deallocate the device array associated with the given host-side array.
-- Typically this should only be called in very specific circumstances.
--
free :: Typeable a => MemoryCache p task -> ArrayData a -> IO ()
free (MemoryCache !mt !ref) !arr = withMVar ref $ \utbl -> do
  key <- MT.makeStableArray arr
  HT.delete utbl key
  MT.freeStable mt key


-- |Record an association between a host-side array and a remote memory area
-- that was not allocated by accelerate. The remote memory will NOT be re-used
-- once the host-side array is garbage collected.
--
-- This typically only has use for backends that provide an FFI.
--
insertUnmanaged :: (PrimElt e a, MonadIO m) => MemoryCache p task -> ArrayData e -> p a -> m ()
insertUnmanaged (MemoryCache mt ref) !arr !ptr = liftIO . withMVar ref $ \utbl -> do
  key <- MT.makeStableArray arr
  MT.insertUnmanaged mt arr ptr
  ts <- getCPUTime
  weak_arr <- mkWeakPtr arr Nothing
  HT.insert utbl key (Used ts Unmanaged [] 0 weak_arr)


-- Removing entries
-- ----------------

-- |Initiate garbage collection and `free` any remote arrays that no longer
-- have matching host-side equivalents.
--
reclaim :: forall m task. (RemoteMemory m, MonadIO m) => MemoryCache (RemotePointer m) task -> m ()
reclaim (MemoryCache !mt _) = MT.reclaim mt


-- Miscellaneous
-- -------------

cleanUses :: Task task => [task] -> IO [task]
cleanUses = filterM (fmap not . isDone)

-- Debug
-- -----

{-# INLINE trace #-}
trace :: MonadIO m => String -> m a -> m a
trace msg next = message msg >> next

{-# INLINE message #-}
message :: MonadIO m => String -> m ()
message msg = liftIO $ D.traceIO D.dump_gc ("gc: " ++ msg)

