{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
-- |
-- Module      : Data.Array.Accelerate.Array.Memory.Table
-- Copyright   : [2008..2014] Manuel M T Chakravarty, Gabriele Keller
--               [2009..2014] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Accelerate backends often need to copy arrays to a remote memory before they
-- can be used in computation. This module provides an automated method for
-- doing so. Keeping track of arrays in a `MemoryTable` ensures that any memory
-- allocated for them will be freed when GHC's garbage collector collects the
-- host array.
--
module Data.Array.Accelerate.Array.Memory.Table (

  -- Tables for host/device memory associations
  MemoryTable, new, lookup, malloc, free, freeStable, insertUnmanaged, reclaim,

  StableArray, makeStableArray

) where

import Prelude                                                  hiding ( lookup )
import Data.Maybe                                               ( isJust )
import Data.Hashable                                            ( Hashable(..) )
import Data.Proxy
import Data.Typeable                                            ( Typeable, gcast )
import Control.Monad                                            ( unless, when )
import Control.Monad.IO.Class                                   ( MonadIO, liftIO )
import Control.Concurrent                                       ( yield )
import Control.Concurrent.MVar                                  ( MVar, newMVar, withMVar, modifyMVar_, mkWeakMVar )
import System.Mem                                               ( performGC )
import System.Mem.Weak                                          ( Weak, mkWeak, deRefWeak, finalize )
import System.Mem.StableName                                    ( StableName, makeStableName, hashStableName )
import Foreign.Storable                                         ( Storable, sizeOf )

import qualified Data.HashTable.IO                              as HT

import Data.Array.Accelerate.Error                              ( internalError )
import Data.Array.Accelerate.Array.Data                         ( ArrayData )
import Data.Array.Accelerate.Array.Memory                       ( RemoteMemory, RemotePointer )
import Data.Array.Accelerate.Array.Memory.Nursery               ( Nursery(..), NRS )
import qualified Data.Array.Accelerate.Array.Memory             as M
import qualified Data.Array.Accelerate.Array.Memory.Nursery     as N
import qualified Data.Array.Accelerate.Debug                    as D


-- We use an MVar to the hash table, so that several threads may safely access
-- it concurrently. This includes the finalisation threads that remove entries
-- from the table.
--
-- It is important that we can garbage collect old entries from the table when
-- the key is no longer reachable in the heap. Hence the value part of each
-- table entry is a (Weak val), where the stable name 'key' is the key for the
-- memo table, and the 'val' is the value of this table entry. When the key
-- becomes unreachable, a finaliser will fire and remove this entry from the
-- hash buckets, and further attempts to dereference the weak pointer will
-- return Nothing. References from 'val' to the key are ignored (see the
-- semantics of weak pointers in the documentation).
--
type HashTable key val  = HT.BasicHashTable key val
type MT p               = MVar ( HashTable StableArray (RemoteArray p) )
data MemoryTable p      = MemoryTable {-# UNPACK #-} !(MT p)
                                      {-# UNPACK #-} !(Weak (MT p))
                                      {-# UNPACK #-} !(Nursery p)
                                      (forall a. p a -> IO ())

-- | An untyped reference to an array, similar to a stablename.
--
data StableArray where
  StableArray :: Typeable e
              => {-# UNPACK #-} !(StableName (ArrayData e))
              -> StableArray

data RemoteArray p where
  RemoteArray :: Typeable e
              => {-# UNPACK #-} !(Weak (p e))
              -> RemoteArray p

instance Eq StableArray where
  StableArray a1 == StableArray a2
    = maybe False (== a2) (gcast a1)

instance Hashable StableArray where
  {-# INLINE hashWithSalt #-}
  hashWithSalt salt (StableArray sn)
    = salt `hashWithSalt` sn

instance Show StableArray where
  show (StableArray sn) = "Array #" ++ show (hashStableName sn)


-- At what percentage of available memory should we consider it empty.
--
-- RCE: This is highly necessary with the CUDA backend as OSX's (and presumably
-- other OSes) UI doesn't handle GPU memory exhaustion very well (it crashes).
--
-- RCE: Experimentation should be done to come up with a better value.
threshold :: Float
threshold = 5.0

-- |Create a new memory table from host to remote arrays.
--
-- The function supplied should be the `free` for the remote pointers being
-- stored. This function will be called by the GC, which typically runs on a
-- different thread. Unlike the `free` in `RemoteMemory`, this function cannot
-- depend on any state.
--
new :: (RemoteMemory m, MonadIO m) => (forall a. RemotePointer m a -> IO ()) -> m (MemoryTable (RemotePointer m))
new free = do
  message "initialise memory table"
  tbl  <- liftIO $ HT.new
  ref  <- liftIO $ newMVar tbl
  nrs  <- N.new free
  weak <- liftIO $ mkWeakMVar ref (table_finalizer tbl)
  return $! MemoryTable ref weak nrs free


-- | Look for the remote pointer corresponding to a given host-side array.
--
lookup :: (Typeable a, Typeable b) => MemoryTable p -> ArrayData a -> IO (Maybe (p b))
lookup (MemoryTable !ref _ _ _) !arr = do
  sa <- makeStableArray  arr
  mw <- withMVar ref (`HT.lookup` sa)
  case mw of
    Nothing              -> trace ("lookup/not found: " ++ show sa) $ return Nothing
    Just (RemoteArray w) -> do
      mv <- deRefWeak w
      case mv of
        Just v | Just p <- gcast v -> trace ("lookup/found: " ++ show sa) $ return (Just p)
               | otherwise         -> $internalError "lookup" $ "type mismatch"

        -- Note: [Weak pointer weirdness]
        --
        -- After the lookup is successful, there might conceivably be no further
        -- references to 'arr'. If that is so, and a garbage collection
        -- intervenes, the weak pointer might get tombstoned before 'deRefWeak'
        -- gets to it. In that case we throw an error (below). However, because
        -- we have used 'arr' in the continuation, this ensures that 'arr' is
        -- reachable in the continuation of 'deRefWeak' and thus 'deRefWeak'
        -- always succeeds. This sort of weirdness, typical of the world of weak
        -- pointers, is why we can not reuse the stable name 'sa' computed
        -- above in the error message.
        --
        Nothing                    ->
          makeStableArray  arr >>= \x -> $internalError "lookup" $ "dead weak pair: " ++ show x


-- | Allocate a new device array to be associated with the given host-side array.
-- This may not always use the `malloc` provided by the `RemoteMemory` instance.
-- In order to reduce the number of raw allocations, previously allocated remote
-- arrays will be re-used. In the event that the remote memory is exhausted,
-- 'Nothing' is returned.
--
malloc :: forall a b m. (Typeable a, Typeable b, Storable b, RemoteMemory m, MonadIO m)
       => MemoryTable (RemotePointer m)
       -> ArrayData a
       -> Int
       -> m (Maybe (RemotePointer m b))
malloc mt@(MemoryTable _ _ !nursery _) !ad !n = do
  -- Note: [Allocation sizes]
  --
  -- Instead of allocating the exact number of elements requested, we round up to
  -- a fixed chunk size; currently set at 128 elements. This means there is a
  -- greater chance the nursery will get a hit, and moreover that we can search
  -- the nursery for an exact size.
  --
  -- TLM: I believe the CUDA API allocates in chunks, of size 4MB.
  --
  chunk <- M.chunkSize
  let -- next highest multiple of f from x
      multiple x f      = (x + (f-1)) `div` f

      !n'               = chunk * multiple n chunk
      !bytes            = n' * sizeOf (undefined :: b)
  --
  mp  <- liftIO $ N.malloc bytes nursery
  case mp of
    Just p       -> trace "malloc/nursery" $ do
      let p' = M.castPtr (Proxy :: Proxy m) p
      insert mt ad p' bytes
      return (Just p')

    Nothing      -> trace "malloc/new"     $ do
      left <- percentAvailable
      when (left < threshold) $ do
        management "malloc/threshold-reached (reclaiming)"
        reclaim mt
      mp' <- M.malloc bytes
      case mp' of
        Nothing -> management "malloc/remote-malloc-failed (reclaiming)" >> do
          reclaim mt
          mp'' <- M.malloc bytes
          case mp'' of
            Nothing -> management "malloc/remote-malloc-failed (non-recoverable)" >>
              return Nothing
            Just p'' -> do
              insert mt ad p'' bytes
              return (Just p'')
        Just p' -> do
          insert mt ad p' bytes
          return (Just p')



-- | Deallocate the device array associated with the given host-side array.
-- Typically this should only be called in very specific circumstances.
--
free :: Typeable a => MemoryTable p -> ArrayData a -> IO ()
free mt !arr = do
  sa <- makeStableArray  arr
  freeStable mt sa

-- | Deallocate the device array associated with the given StableArray. This
-- is useful for other memory managers built on top of the memory table.
--
freeStable :: MemoryTable p -> StableArray -> IO ()
freeStable (MemoryTable !ref _ _ _) !sa = do
  mw <- withMVar ref (`HT.lookup` sa)
  case mw of
    Nothing              -> message ("free/not found: " ++ show sa)
    Just (RemoteArray w) -> trace   ("free/evict: " ++ show sa) $ finalize w


-- Record an association between a host-side array and a new device memory area.
-- The device memory will be freed when the host array is garbage collected.
--
insert :: forall m a b. (Typeable a, Typeable b, RemoteMemory m, MonadIO m)
       => MemoryTable (RemotePointer m)
       -> ArrayData a
       -> RemotePointer m b
       -> Int
       -> m ()
insert (MemoryTable !ref !weak_ref (Nursery _ !weak_nrs) free) !arr !ptr !bytes = do
  key  <- makeStableArray  arr
  dev  <- liftIO $ RemoteArray `fmap` mkWeak arr ptr (Just $ finalizer (Proxy :: Proxy m) free weak_ref weak_nrs key ptr bytes)
  message      $ "insert: " ++ show key
  liftIO $ withMVar ref $ \tbl -> HT.insert tbl key dev


-- |Record an association between a host-side array and a remote memory area
-- that was not allocated by accelerate. The remote memory will NOT be re-used
-- once the host-side array is garbage collected.
--
-- This typically only has use for backends that provide an FFI.
--
insertUnmanaged :: (Typeable a, Typeable b, MonadIO m) => MemoryTable p -> ArrayData a -> p b -> m ()
insertUnmanaged (MemoryTable !ref !weak_ref _ _) !arr !ptr = do
  key  <- makeStableArray  arr
  dev  <- liftIO $ RemoteArray `fmap` mkWeak arr ptr (Just $ remoteFinalizer weak_ref key)
  message $ "insertUnmanaged: " ++ show key
  liftIO $ withMVar ref $ \tbl -> HT.insert tbl key dev

percentAvailable :: (RemoteMemory m, MonadIO m) => m Float
percentAvailable = do
  left  <- M.availableMem
  total <- M.totalMem
  return (100.0 * fromIntegral left / fromIntegral total)


-- Removing entries
-- ----------------

-- |Initiate garbage collection and `free` any remote arrays that no longer
-- have matching host-side equivalents.
--
reclaim :: forall m. (RemoteMemory m, MonadIO m) => MemoryTable (RemotePointer m) -> m ()
reclaim (MemoryTable _ weak_ref (Nursery nrs _) free) = do
  before <- M.availableMem
  total <- M.totalMem
  liftIO $ do
    performGC
    yield
    modifyMVar_ nrs (\(tbl,_) -> N.flush free tbl >> return (tbl, 0))
    mr <- deRefWeak weak_ref
    case mr of
      Nothing  -> return ()
      Just ref -> withMVar ref $ \tbl ->
        flip HT.mapM_ tbl $ \(_,RemoteArray w) -> do
          alive <- isJust `fmap` deRefWeak w
          unless alive $ finalize w
  --
  D.when D.dump_gc $ do
    after <- M.availableMem
    message $ "reclaim: freed "   ++ showBytes (before - after)
                        ++ ", "   ++ showBytes after
                        ++ " of " ++ showBytes total ++ " remaining"

-- Because a finaliser might run at any time, we must reinstate the context in
-- which the array was allocated before attempting to release it.
--
-- Note also that finaliser threads will silently terminate if an exception is
-- raised. If the context, and thereby all allocated memory, was destroyed
-- externally before the thread had a chance to run, all we need do is update
-- the hash tables --- but we must do this first before failing to use a dead
-- context.
--
finalizer :: (RemoteMemory m, MonadIO m)
          => proxy m
          -> (forall a. RemotePointer m a -> IO ())
          -> Weak (MT (RemotePointer m))
          -> Weak (NRS (RemotePointer m))
          -> StableArray
          -> RemotePointer m b
          -> Int
          -> IO ()
finalizer proxy free !weak_ref !weak_nrs !key !ptr !bytes = do
  mr <- deRefWeak weak_ref
  case mr of
    Nothing  -> message ("finalise/dead table: " ++ show key)
    Just ref -> withMVar ref (`HT.delete` key)
  --
  mn <- deRefWeak weak_nrs
  case mn of
    Nothing  -> trace ("finalise/free: "     ++ show key) $ free ptr
    Just nrs -> trace ("finalise/nursery: "  ++ show key) $ N.stash proxy bytes  nrs ptr

remoteFinalizer :: Weak (MT p) -> StableArray -> IO ()
remoteFinalizer !weak_ref !key = do
  mr <- deRefWeak weak_ref
  case mr of
    Nothing  -> message ("finalise/dead table: " ++ show key)
    Just ref -> trace   ("finalise: "            ++ show key) $ withMVar ref (`HT.delete` key)

table_finalizer :: HashTable StableArray (RemoteArray p) -> IO ()
table_finalizer !tbl
  = trace "table finaliser"
  $ HT.mapM_ (\(_,RemoteArray w) -> finalize w) tbl

-- Miscellaneous
-- -------------

-- | Make a new StableArray.
{-# INLINE makeStableArray #-}
makeStableArray :: (MonadIO m, Typeable a) => ArrayData a -> m StableArray
makeStableArray !arr = do
  sn <- liftIO (makeStableName arr)
  return (StableArray sn)

-- Debug
-- -----

{-# INLINE showBytes #-}
showBytes :: Integral n => n -> String
showBytes x = D.showFFloatSIBase (Just 0) 1024 (fromIntegral x :: Double) "B"

{-# INLINE trace #-}
trace :: MonadIO m => String -> m a -> m a
trace msg next = message msg >> next

{-# INLINE message #-}
message :: MonadIO m => String -> m ()
message msg = liftIO $ D.traceIO D.dump_gc ("gc: " ++ msg)

{-# INLINE management #-}
management :: (RemoteMemory m, MonadIO m) => String -> m ()
management msg = D.when D.dump_gc $ do
  left <- M.availableMem
  total <- M.totalMem
  message (msg ++ " (" ++ showBytes left ++ "/" ++ showBytes total ++ " available)")
