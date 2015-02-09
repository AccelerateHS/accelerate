{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
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
module Data.Array.Accelerate.Array.Memory.Table (

  -- Tables for host/device memory associations
  MemoryTable, new, lookup, malloc, free, insertUnmanaged, reclaim

) where

import Prelude                                                  hiding ( lookup )
import Data.Maybe                                               ( isJust )
import Data.Hashable                                            ( Hashable(..) )
import Data.Proxy
import Data.Typeable                                            ( Typeable, gcast )
import Control.Monad                                            ( unless )
import Control.Concurrent                                       ( yield )
import Control.Concurrent.MVar                                  ( MVar, newMVar, withMVar, mkWeakMVar )
import Control.Applicative                                      ( (<$>) )
import System.Mem                                               ( performGC )
import System.Mem.Weak                                          ( Weak, mkWeak, deRefWeak, finalize )
import System.Mem.StableName                                    ( StableName, makeStableName, hashStableName )
import Foreign.Storable                                         ( Storable, sizeOf )

import qualified Data.HashTable.IO                              as HT

import qualified Data.Array.Accelerate.Debug                    as D
import Data.Array.Accelerate.Error                              ( internalError )
import Data.Array.Accelerate.Array.Data                         ( ArrayData )
import Data.Array.Accelerate.Array.Memory                       ( RemoteMemory )
import qualified Data.Array.Accelerate.Array.Memory             as M
import Data.Array.Accelerate.Array.Memory.Nursery               ( Nursery(..), NRS )
import qualified Data.Array.Accelerate.Array.Memory.Nursery     as N


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
type MT p               = MVar ( HashTable HostArray (RemoteArray p) )
data MemoryTable p      = MemoryTable {-# UNPACK #-} !(MT p)
                                      {-# UNPACK #-} !(Weak (MT p))
                                      {-# UNPACK #-} !(Nursery p)

-- Arrays on the host and in the remote memory.
--
data HostArray where
  HostArray :: Typeable e
            => {-# UNPACK #-} !(StableName (ArrayData e))
            -> HostArray

data RemoteArray p where
  RemoteArray :: Typeable e
              => {-# UNPACK #-} !(Weak (p e))
              -> RemoteArray p

instance Eq HostArray where
  HostArray a1 == HostArray a2
    = maybe False (== a2) (gcast a1)

instance Hashable HostArray where
  {-# INLINE hashWithSalt #-}
  hashWithSalt salt (HostArray sn)
    = salt `hashWithSalt` sn

instance Show HostArray where
  show (HostArray sn) = "Array #" ++ show (hashStableName sn)


-- Referencing arrays
-- ------------------

-- |Create a new memory table from host to remote arrays.
--
new :: RemoteMemory p => IO (MemoryTable p)
new = do
  message "initialise memory table"
  tbl  <- HT.new
  ref  <- newMVar tbl
  nrs  <- N.new
  weak <- mkWeakMVar ref (table_finalizer tbl)
  return $! MemoryTable ref weak nrs


-- | Look for the remote pointer corresponding to a given host-side array.
--
lookup :: (Typeable a, Typeable b) => MemoryTable p -> ArrayData a -> IO (Maybe (p b))
lookup (MemoryTable !ref _ _) !arr = do
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
-- arrays will be re-used.
--
malloc :: forall a b p. (Typeable a, Typeable b, Storable b, RemoteMemory p) => MemoryTable p -> ArrayData a -> Int -> IO (p b)
malloc mt@(MemoryTable _ _ !nursery) !ad !n = do
  -- Note: [Allocation sizes]
  -- Instead of allocating the exact number of elements requested, we round up to
  -- a fixed chunk size; currently set at 128 elements. This means there is a
  -- greater chance the nursery will get a hit, and moreover that we can search
  -- the nursery for an exact size. TLM: I believe the CUDA API allocates in
  -- chunks, of size 4MB.
  let -- next highest multiple of f from x
      multiple x f      = (x + (f-1)) `div` f
      chunk             = M.chunkSize (Proxy :: Proxy p)

      !n'               = chunk * multiple n chunk
      !bytes            = n' * sizeOf (undefined :: b)
  --
  mp  <- N.malloc bytes nursery
  ptr <- case mp of
           Just p       -> trace "malloc/nursery" $ return (M.castPtr p)
           Nothing      -> trace "malloc/new"     $ do
             mp' <- M.malloc n'
             case mp' of
               Nothing -> trace "malloc/failed-reclaiming" $ reclaim mt >>
                 (maybe (error "Remote memory exhausted") return =<< M.malloc n')
               Just p -> return p

  insert  mt ad ptr bytes
  return ptr


-- | Deallocate the device array associated with the given host-side array.
-- Typically this should only be called in very specific circumstances.
--
free :: Typeable a => MemoryTable p -> ArrayData a -> IO ()
free (MemoryTable !ref _ _) !arr = do
  sa <- makeStableArray  arr
  mw <- withMVar ref (`HT.lookup` sa)
  case mw of
    Nothing              -> message ("free/not found: " ++ show sa)
    Just (RemoteArray w) -> trace   ("free/evict: " ++ show sa) $ finalize w


-- Record an association between a host-side array and a new device memory area.
-- The device memory will be freed when the host array is garbage collected.
--
insert :: (Typeable a, Typeable b, RemoteMemory p) => MemoryTable p -> ArrayData a -> p b -> Int -> IO ()
insert (MemoryTable !ref !weak_ref (Nursery _ !weak_nrs)) !arr !ptr !bytes = do
  key  <- makeStableArray  arr
  dev  <- RemoteArray `fmap` mkWeak arr ptr (Just $ finalizer weak_ref weak_nrs key ptr bytes)
  message      $ "insert: " ++ show key
  withMVar ref $ \tbl -> HT.insert tbl key dev


-- |Record an association between a host-side array and a remote memory area
-- that was not allocated by accelerate. The remote memory will NOT be re-used
-- once the host-side array is garbage collected.
--
-- This typically only has use for backends that provide an FFI.
--
insertUnmanaged :: (Typeable a, Typeable b) => MemoryTable p -> ArrayData a -> p b -> IO ()
insertUnmanaged (MemoryTable !ref !weak_ref _) !arr !ptr = do
  key  <- makeStableArray  arr
  dev  <- RemoteArray `fmap` mkWeak arr ptr (Just $ remoteFinalizer weak_ref key)
  message      $ "insertUnmanaged: " ++ show key
  withMVar ref $ \tbl -> HT.insert tbl key dev


-- Removing entries
-- ----------------

-- |Initiate garbage collection and `free` any remote arrays that no longer
-- have matching host-side equivalents.
--
reclaim :: forall p. RemoteMemory p => MemoryTable p -> IO ()
reclaim (MemoryTable _ weak_ref (Nursery nrs _)) = do
  before <- M.availableMem (Proxy :: Proxy p)
  total <- M.totalMem (Proxy :: Proxy p)
  performGC
  yield
  withMVar nrs N.flush
  mr <- deRefWeak weak_ref
  case mr of
    Nothing  -> return ()
    Just ref -> withMVar ref $ \tbl ->
      flip HT.mapM_ tbl $ \(_,RemoteArray w) -> do
        alive <- isJust `fmap` deRefWeak w
        unless alive $ finalize w
  --
  D.when D.dump_gc $ do
    after <- M.availableMem (Proxy :: Proxy p)
    message $ "reclaim: freed "   ++ showBytes (fromIntegral (before - after))
                        ++ ", "   ++ showBytes (fromIntegral after)
                        ++ " of " ++ showBytes (fromIntegral total) ++ " remaining"

-- Because a finaliser might run at any time, we must reinstate the context in
-- which the array was allocated before attempting to release it.
--
-- Note also that finaliser threads will silently terminate if an exception is
-- raised. If the context, and thereby all allocated memory, was destroyed
-- externally before the thread had a chance to run, all we need do is update
-- the hash tables --- but we must do this first before failing to use a dead
-- context.
--
finalizer :: RemoteMemory p => Weak (MT p) -> Weak (NRS p) -> HostArray -> p b -> Int -> IO ()
finalizer !weak_ref !weak_nrs !key !ptr !bytes = do
  mr <- deRefWeak weak_ref
  case mr of
    Nothing  -> message ("finalise/dead table: " ++ show key)
    Just ref -> withMVar ref (`HT.delete` key)
  --
  mn <- deRefWeak weak_nrs
  case mn of
    Nothing  -> trace ("finalise/free: "     ++ show key) $ M.free ptr
    Just nrs -> trace ("finalise/nursery: "  ++ show key) $ N.stash bytes  nrs ptr

remoteFinalizer :: Weak (MT p) -> HostArray -> IO ()
remoteFinalizer !weak_ref !key = do
  mr <- deRefWeak weak_ref
  case mr of
    Nothing  -> message ("finalise/dead table: " ++ show key)
    Just ref -> trace   ("finalise: "            ++ show key) $ withMVar ref (`HT.delete` key)

table_finalizer :: HashTable HostArray (RemoteArray p) -> IO ()
table_finalizer !tbl
  = trace "table finaliser"
  $ HT.mapM_ (\(_,RemoteArray w) -> finalize w) tbl


-- Miscellaneous
-- -------------

{-# INLINE makeStableArray #-}
makeStableArray :: Typeable a => ArrayData a -> IO HostArray
makeStableArray !arr = HostArray <$> makeStableName arr


-- Debug
-- -----

{-# INLINE showBytes #-}
showBytes :: Int -> String
showBytes x = D.showFFloatSIBase (Just 0) 1024 (fromIntegral x :: Double) "B"

{-# INLINE trace #-}
trace :: String -> IO a -> IO a
trace msg next = message msg >> next

{-# INLINE message #-}
message :: String -> IO ()
message msg = D.traceIO D.dump_gc ("gc: " ++ msg)

