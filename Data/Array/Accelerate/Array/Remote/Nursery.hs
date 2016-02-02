{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Array.Remote.Nursery
-- Copyright   : [2008..2014] Manuel M T Chakravarty, Gabriele Keller
--               [2009..2016] Trevor L. McDonell
--               [2015..2015] Robert Clifton-Everest
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Array.Remote.Nursery (

  Nursery(..), NRS, new, malloc, stash, flush, size

) where

-- friends
import Data.Array.Accelerate.FullList                           ( FullList(..) )
import Data.Array.Accelerate.Array.Remote.Class
import qualified Data.Array.Accelerate.FullList                 as FL
import qualified Data.Array.Accelerate.Debug                    as D

-- libraries
import Prelude
import Control.Concurrent.MVar                                  ( MVar, newMVar, withMVar, modifyMVar, modifyMVar_, mkWeakMVar )
import Control.Monad.IO.Class
import Data.Int
import Data.Proxy
import System.Mem.Weak                                          ( Weak )

import qualified Data.HashTable.IO                              as HT


-- The nursery is a place to store remote memory arrays that are no longer
-- needed. If a new array is requested of a similar size, we might return an
-- array from the nursery instead of calling into the backends underlying API
-- to allocate fresh memory.
--
-- Note that since there might be many arrays for the same size, each entry in
-- the map keeps a (non-empty) list of remote arrays.
--
type HashTable key val = HT.BasicHashTable key val

type NRS p             = MVar ( HashTable Int (FullList () (p ())), Int64 )
data Nursery p         = Nursery {-# UNPACK #-} !(NRS p)
                                 {-# UNPACK #-} !(Weak (NRS p))


-- Generate a fresh nursery
--
new :: forall m. (RemoteMemory m, MonadIO m)
    => (RemotePtr m () -> IO ())
    -> m (Nursery (RemotePtr m))
new free = liftIO $ do
  tbl    <- HT.new
  ref    <- newMVar (tbl, 0)
  weak   <- mkWeakMVar ref (flush free tbl)
  return $! Nursery ref weak


-- Look for a chunk of memory in the nursery of a given size (or a little bit
-- larger). If found, it is removed from the nursery and a pointer to it
-- returned.
--
{-# INLINE malloc #-}
malloc :: Int
       -> Nursery p
       -> IO (Maybe (p ()))
malloc !n (Nursery !ref _) = modifyMVar ref $ \(tbl,sz) -> do
  mp  <- HT.lookup tbl n
  case mp of
    Nothing               -> return ((tbl,sz),Nothing)
    Just (FL () ptr rest) ->
      case rest of
        FL.Nil          -> HT.delete tbl n              >> return ((tbl,sz - fromIntegral n), Just ptr)
        FL.Cons () v xs -> HT.insert tbl n (FL () v xs) >> return ((tbl,sz - fromIntegral n), Just ptr)


-- Add a device pointer to the nursery.
--
{-# INLINE stash #-}
stash :: forall m e proxy. RemoteMemory m
      => proxy m
      -> Int
      -> NRS (RemotePtr m)
      -> RemotePtr m e
      -> IO ()
stash _ !n !ref (castRemotePtr (Proxy :: Proxy m) -> ptr) = modifyMVar_ ref $ \(tbl,sz) -> do
  mp  <- HT.lookup tbl n
  case mp of
    Nothing     -> HT.insert tbl n (FL.singleton () ptr)
    Just xs     -> HT.insert tbl n (FL.cons () ptr xs)
  return (tbl, sz + fromIntegral n)


-- Delete all entries from the nursery and free all associated device memory.
--
flush :: (p () -> IO ())
      -> HashTable Int (FullList () (p ()))
      -> IO ()
flush free !tbl =
  let clean (!key,!val) = do
        FL.mapM_ (const free) val
        HT.delete tbl key
  in
  message "flush nursery" >> HT.mapM_ clean tbl

-- The total size of all arrays stashed in the nursery.
--
size :: Nursery p -> IO Int64
size (Nursery ref _) = withMVar ref (return . snd)


-- Debug
-- -----

{-# INLINE message #-}
message :: String -> IO ()
message msg = D.traceIO D.dump_gc ("gc: " ++ msg)

