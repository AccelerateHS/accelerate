{-# LANGUAGE CPP, GADTs, MagicHash, UnboxedTuples #-}
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
import Data.Maybe                                       ( isJust )
import Data.Hashable                                    ( Hashable, hash )
import Data.Typeable                                    ( Typeable, gcast )
import Data.Array.Unboxed                               ( UArray )
import Control.Monad                                    ( unless )
import System.Mem                                       ( performGC )
import System.Mem.Weak                                  ( Weak, mkWeak, deRefWeak, finalize )
import System.Mem.StableName                            ( StableName, makeStableName, hashStableName )
import Control.Concurrent.MVar                          ( MVar, newMVar, withMVar )
import Foreign.CUDA.Ptr                                 ( DevicePtr )

import GHC.Base                                         ( IO(IO), mkWeak# )
import GHC.MVar                                         ( MVar(MVar) )
import GHC.Weak                                         ( Weak(Weak) )

import qualified Foreign.CUDA.Driver                    as CUDA
import qualified Data.HashTable.IO                      as Hash

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
  show (HostArray sn) = "Array #" ++ show (hashStableName sn)


-- Referencing arrays
-- ------------------

-- Create a new hash table from host to device arrays. When the structure is
-- collected it will finalise all entries in the table.
--
new :: IO MemoryTable
new = do
  tbl           <- Hash.new
  mvar@(MVar m) <- newMVar tbl
  weak          <- IO $ \s -> case mkWeak# m mvar (table_finalizer tbl) s of
                                (# s', w #) -> (# s', Weak w #)
  return $! MemoryTable mvar weak


-- NOTE: we require the INLINE pragma on both 'lookup' and 'insert', else we get
-- inconsistent StableName generation.
--
{-# INLINE lookup #-}
lookup :: (Typeable a, Typeable b) => MemoryTable -> UArray Int a -> IO (Maybe (DevicePtr b))
lookup (MemoryTable ref _) arr = seq arr $ do
  sa <- makeStableArray arr
  mw <- withMVar ref (`Hash.lookup` sa)
  case mw of
    Nothing              -> trace ("lookup/not found: " ++ show sa) $ return Nothing
    Just (DeviceArray w) -> do
      mv <- deRefWeak w
      case mv of
        Just v | Just p <- gcast v   -> trace ("lookup/found: " ++ show sa) $ return (Just p)
               | otherwise           -> INTERNAL_ERROR(error) "lookup" $ "type mismatch"
        Nothing                      ->
          makeStableArray arr >>= \x -> INTERNAL_ERROR(error) "lookup" $ "dead weak pair: " ++ show x


{-# INLINE insert #-}
insert :: (Typeable a, Typeable b) => MemoryTable -> UArray Int a -> DevicePtr b -> IO ()
insert (MemoryTable ref weak_ref) arr ptr = seq arr $ do
  sn   <- makeStableName arr
  weak <- mkWeak arr ptr (Just $ finalizer weak_ref sn ptr)
  debug $ "insert: " ++ show (HostArray sn)
  withMVar ref $ \tbl -> Hash.insert tbl (HostArray sn) (DeviceArray weak)


{-# INLINE makeStableArray #-}
makeStableArray :: Typeable a => UArray Int a -> IO HostArray
makeStableArray arr = HostArray `fmap` makeStableName arr


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
    Just ref -> withMVar ref $ \tbl ->
      flip Hash.mapM_ tbl    $ \(_,DeviceArray w) -> do
        alive <- isJust `fmap` deRefWeak w
        unless alive $ finalize w


finalizer :: Typeable a => Weak MT -> StableName (UArray Int a) -> DevicePtr b -> IO ()
finalizer weak_ref sn ptr = do
  let s = "finalise: " ++ show (HostArray sn)
      d = s ++ " // dead memory table"
  CUDA.free ptr
  mr <- deRefWeak weak_ref
  case mr of
    Nothing  -> trace d $ return ()
    Just ref -> trace s $ withMVar ref (`Hash.delete` HostArray sn)

table_finalizer :: HashTable HostArray DeviceArray -> IO ()
table_finalizer tbl
  = trace "table finaliser"
  $ Hash.mapM_ (\(_,DeviceArray w) -> finalize w) tbl


-- Debug
-- -----

{-# INLINE trace #-}
trace :: String -> IO a -> IO a
trace msg next = D.debug D.dump_gc ("gc: " ++ msg) >> next

{-# INLINE debug #-}
debug :: String -> IO ()
debug s = s `trace` return ()

