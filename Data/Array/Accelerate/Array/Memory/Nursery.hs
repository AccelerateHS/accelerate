{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Array.Memory.Nursery
-- Copyright   : [2008..2014] Manuel M T Chakravarty, Gabriele Keller
--               [2009..2014] Trevor L. McDonell
--               [2015..2015] Robert Clifton-Everest
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Array.Memory.Nursery (

  Nursery(..), NRS, new, malloc, stash, flush,

) where

-- friends
import Data.Array.Accelerate.FullList                      ( FullList(..) )
import qualified Data.Array.Accelerate.FullList                 as FL
import qualified Data.Array.Accelerate.Debug                    as D
import Data.Array.Accelerate.Array.Memory                       ( RemoteMemory, RemotePointer )
import qualified Data.Array.Accelerate.Array.Memory             as M

-- libraries
import Prelude
import Control.Concurrent.MVar                                  ( MVar, newMVar, withMVar, mkWeakMVar )
import Control.Monad.IO.Class
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
type HashTable key val  = HT.BasicHashTable key val

type NRS p            = MVar ( HashTable Int (FullList () (p ())) )
data Nursery p        = Nursery {-# UNPACK #-} !(NRS p)
                                {-# UNPACK #-} !(Weak (NRS p))


-- Generate a fresh nursery
--
new :: forall m. (RemoteMemory m, MonadIO m) => (RemotePointer m () -> IO ()) -> m (Nursery (RemotePointer m))
new free = liftIO $ do
  tbl    <- HT.new
  ref    <- newMVar tbl
  weak   <- mkWeakMVar ref (flush free tbl)
  return $! Nursery ref weak


-- Look for a chunk of memory in the nursery of a given size (or a little bit
-- larger). If found, it is removed from the nursery and a pointer to it
-- returned.
--
{-# INLINE malloc #-}
malloc :: Int -> Nursery p -> IO (Maybe (p ()))
malloc !n (Nursery !ref _) = withMVar ref $ \tbl -> do
  mp  <- HT.lookup tbl n
  case mp of
    Nothing               -> return Nothing
    Just (FL () ptr rest) ->
      case rest of
        FL.Nil          -> HT.delete tbl n              >> return (Just ptr)
        FL.Cons () v xs -> HT.insert tbl n (FL () v xs) >> return (Just ptr)


-- Add a device pointer to the nursery.
--
{-# INLINE stash #-}
stash :: forall m e proxy. RemoteMemory m => proxy m -> Int -> NRS (RemotePointer m) -> RemotePointer m e -> IO ()
stash _ !n !ref (M.castPtr (Proxy :: Proxy m) -> ptr) = withMVar ref $ \tbl -> do
  mp  <- HT.lookup tbl n
  case mp of
    Nothing     -> HT.insert tbl n (FL.singleton () ptr)
    Just xs     -> HT.insert tbl n (FL.cons () ptr xs)


-- Delete all entries from the nursery and free all associated device memory.
--
flush :: (p () -> IO ()) -> HashTable Int (FullList () (p ())) -> IO ()
flush free !tbl =
  let clean (!key,!val) = do
        FL.mapM_ (const free) val
        HT.delete tbl key
  in
  message "flush nursery" >> HT.mapM_ clean tbl


-- Debug
-- -----

{-# INLINE message #-}
message :: String -> IO ()
message msg = D.traceIO D.dump_gc ("gc: " ++ msg)

