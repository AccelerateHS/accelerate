{-# LANGUAGE CPP, GADTs #-}
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

import Prelude                          hiding ( lookup )
import Data.Hashable                    ( Hashable, hash )
import Data.Typeable                    ( Typeable, gcast )
import Data.Array.Unboxed               ( UArray )
import System.Mem                       ( performGC )
import System.Mem.Weak                  ( Weak, mkWeak, mkWeakPtr, deRefWeak, finalize )
import System.Mem.StableName            ( StableName, makeStableName, hashStableName )
import Control.Concurrent.MVar          ( MVar, newMVar, withMVar, addMVarFinalizer )
import Foreign.CUDA.Ptr                 ( DevicePtr )

import qualified Foreign.CUDA.Driver    as CUDA
import qualified Data.HashTable.IO      as Hash

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
type MT                = MVar ( HashTable HostArray DeviceArray )
data MemoryTable       = MemoryTable !MT !(Weak MT)


data HostArray where
  HostArray :: Typeable e
            => StableName (UArray Int e)
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
  show (HostArray sn) = "HostArray #" ++ show (hashStableName sn)


-- Referencing arrays
-- ------------------

-- Create a new hash table from host to device arrays. When the structure is
-- collected it will finalise all entries in the table.
--
new :: IO MemoryTable
new = do
  tbl  <- Hash.newSized 1001
  mvar <- newMVar tbl
  weak <- mkWeakPtr mvar Nothing
  addMVarFinalizer mvar (table_finalizer tbl)
  return $! MemoryTable mvar weak


-- NOTE: we require the INLINE pragma on both 'lookup' and 'insert'
--
{-# INLINE lookup #-}
lookup :: (Typeable a, Typeable b) => MemoryTable -> UArray Int a -> IO (Maybe (DevicePtr b))
lookup (MemoryTable ref _) key
  = key `seq` withMVar ref
  $ \tbl -> do
      sn <- makeStableName key
      mw <- Hash.lookup tbl (HostArray sn)
      case mw of
        Nothing              -> return Nothing
        Just (DeviceArray w) -> do
          mv <- deRefWeak w
          case mv of
            Just v | Just p <- gcast v -> return (Just p)
                   | otherwise         -> INTERNAL_ERROR(error) "lookup" $ "type mismatch"
            Nothing                    -> INTERNAL_ERROR(error) "lookup" $ "dead weak pair: " ++ show (HostArray sn)


{-# INLINE insert #-}
insert :: (Typeable a, Typeable b) => MemoryTable -> UArray Int a -> DevicePtr b -> IO ()
insert (MemoryTable ref weak_ref) arr ptr
  = arr `seq` withMVar ref
  $ \tbl -> do
      sn   <- makeStableName arr
      weak <- mkWeak arr ptr (Just $ finalizer weak_ref sn ptr)
      Hash.insert tbl (HostArray sn) (DeviceArray weak)


-- Removing entries
-- ----------------

-- Initiate garbage collection and finalise any arrays that have been marked as
-- unreachable.
--
reclaim :: MemoryTable -> IO ()
reclaim (MemoryTable _ weak_ref) = do
  performGC
  mr <- deRefWeak weak_ref
  case mr of
    Nothing  -> return ()
    Just ref -> withMVar ref $ Hash.mapM_ (\(_,DeviceArray w) -> finalize w)


finalizer :: Typeable a => Weak MT -> StableName (UArray Int a) -> DevicePtr b -> IO ()
finalizer weak_ref sn ptr
  = TRACE ("finalise: " ++ show (HostArray sn))
  $ do
    CUDA.free ptr
    mr <- deRefWeak weak_ref
    case mr of
      Nothing  -> TRACE "dead memory table" $ return ()
      Just ref -> withMVar ref (`Hash.delete` HostArray sn)

table_finalizer :: HashTable HostArray DeviceArray -> IO ()
table_finalizer tbl
  = TRACE "table finaliser"
  $ Hash.mapM_ (\(_,DeviceArray w) -> finalize w) tbl

