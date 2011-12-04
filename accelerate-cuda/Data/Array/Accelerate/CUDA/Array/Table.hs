{-# LANGUAGE BangPatterns, CPP, GADTs #-}
-- |
-- Module      : Data.Array.Accelerate.CUDA.Array.Table
-- Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.CUDA.Array.Table (

  -- Tables for host/device memory associations
  MemoryTable, new, lookup, insert, reclaim

) where

import Prelude                                          hiding ( lookup )
import Data.IORef                                       ( IORef, newIORef, readIORef, mkWeakIORef )
import Data.Maybe                                       ( isJust )
import Data.Hashable                                    ( Hashable, hash )
import Data.Typeable                                    ( Typeable, gcast )
import Control.Monad                                    ( unless )
import System.Mem                                       ( performGC )
import System.Mem.Weak                                  ( Weak, mkWeak, deRefWeak, finalize )
import System.Mem.StableName                            ( StableName, makeStableName, hashStableName )
import Foreign.CUDA.Ptr                                 ( DevicePtr )

import qualified Foreign.CUDA.Driver                    as CUDA
import qualified Data.HashTable.IO                      as Hash

import Data.Array.Accelerate.Array.Data                 ( ArrayData )
import qualified Data.Array.Accelerate.CUDA.Debug       as D ( debug, dump_gc )

#include "accelerate.h"


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
type HashTable key val = Hash.BasicHashTable key val
type MT                = IORef ( HashTable HostArray DeviceArray )
data MemoryTable       = MemoryTable !MT !(Weak MT)


data HostArray where
  HostArray :: Typeable e
            => StableName (ArrayData e)
            -> HostArray

data DeviceArray where
  DeviceArray :: Typeable e
              => Weak (DevicePtr e)
              -> DeviceArray

instance Eq HostArray where
  HostArray a1 == HostArray a2
    = maybe False (== a2) (gcast a1)

instance Hashable HostArray where
  hash (HostArray sn) = hash sn

instance Show HostArray where
  show (HostArray sn) = "Array #" ++ show (hashStableName sn)


-- Referencing arrays
-- ------------------

-- Create a new hash table from host to device arrays. When the structure is
-- collected it will finalise all entries in the table.
--
new :: IO MemoryTable
new = do
  tbl  <- Hash.new
  ref  <- newIORef tbl
  weak <- mkWeakIORef ref (table_finalizer tbl)
  return $! MemoryTable ref weak


-- Look for the device memory corresponding to a given host-side array.
--
lookup :: (Typeable a, Typeable b) => MemoryTable -> ArrayData a -> IO (Maybe (DevicePtr b))
lookup (MemoryTable ref _) !arr = do
  sa <- makeStableArray arr
  mw <- withIORef ref (`Hash.lookup` sa)
  case mw of
    Nothing              -> trace ("lookup/not found: " ++ show sa) $ return Nothing
    Just (DeviceArray w) -> do
      mv <- deRefWeak w
      case mv of
        Just v | Just p <- gcast v   -> trace ("lookup/found: " ++ show sa) $ return (Just p)
               | otherwise           -> INTERNAL_ERROR(error) "lookup" $ "type mismatch"
        Nothing                      ->
          makeStableArray arr >>= \x -> INTERNAL_ERROR(error) "lookup" $ "dead weak pair: " ++ show x


-- Record an association between a host-side array and a new device memory area.
-- The device memory will be freed when the host array is garbage collected.
--
insert :: (Typeable a, Typeable b) => MemoryTable -> ArrayData a -> DevicePtr b -> IO ()
insert (MemoryTable ref weak_ref) !arr !ptr = do
  key  <- makeStableArray arr
  dev  <- DeviceArray `fmap` mkWeak arr ptr (Just $ finalizer weak_ref key ptr)
  tbl  <- readIORef ref
  debug $ "insert: " ++ show key
  Hash.insert tbl key dev


-- Removing entries
-- ----------------

-- Initiate garbage collection and finalise any arrays that have been marked as
-- unreachable.
--
reclaim :: MemoryTable -> IO ()
reclaim (MemoryTable _ weak_ref) = do
  trace "reclaim" performGC
  mr <- deRefWeak weak_ref
  case mr of
    Nothing  -> return ()
    Just ref -> withIORef ref $ \tbl ->
      flip Hash.mapM_ tbl $ \(_,DeviceArray w) -> do
        alive <- isJust `fmap` deRefWeak w
        unless alive $ finalize w


finalizer :: Weak MT -> HostArray -> DevicePtr b -> IO ()
finalizer !weak_ref !key !ptr = do
  CUDA.free ptr
  mr <- deRefWeak weak_ref
  case mr of
    Nothing  -> trace nom $ return ()
    Just ref -> trace del $ withIORef ref (`Hash.delete` key)
  --
  where del = "finalise: " ++ show key
        nom = "finalise/dead table: " ++ show key


table_finalizer :: HashTable HostArray DeviceArray -> IO ()
table_finalizer !tbl
  = trace "table finaliser"
  $ Hash.mapM_ (\(_,DeviceArray w) -> finalize w) tbl


-- Miscellaneous
-- -------------

{-# INLINE makeStableArray #-}
makeStableArray :: Typeable a => ArrayData a -> IO HostArray
makeStableArray !arr = HostArray `fmap` makeStableName arr

{-# INLINE withIORef #-}
withIORef :: IORef a -> (a -> IO b) -> IO b
withIORef ref f = readIORef ref >>= f


-- Debug
-- -----

{-# INLINE trace #-}
trace :: String -> IO a -> IO a
trace msg next = D.debug D.dump_gc ("gc: " ++ msg) >> next

{-# INLINE debug #-}
debug :: String -> IO ()
debug s = s `trace` return ()

