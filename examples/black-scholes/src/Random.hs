{-# LANGUAGE BangPatterns, FlexibleContexts #-}

module Random where

import Control.Exception
import System.Random.MWC

import Data.Array.IArray
import Data.Array.Unboxed                       (UArray)
import Data.Array.IO                            (MArray, IOUArray)
import Data.Vector.Storable                     (Storable)
import qualified Data.Array.MArray              as M
import qualified Data.Vector.Storable           as V
import qualified Data.Vector.Storable.Mutable   as MV
import qualified Data.Array.Accelerate          as Acc


-- Convert a storable vector to an Accelerate array
--
convertVector :: (IArray UArray a, MArray IOUArray a IO, Storable a, Acc.Elem a) => V.Vector a -> IO (Acc.Vector a)
convertVector vec = do
  arr <- Acc.fromIArray `fmap` toIArray vec
  evaluate (arr `Acc.indexArray` 0) >> return arr
  where
    toIArray :: (MArray IOUArray a IO, IArray UArray a, Storable a) => V.Vector a -> IO (UArray Int a)
    toIArray v = do
      let n = V.length v
      mu <- M.newArray_ (0,n-1) :: MArray IOUArray a IO => IO (IOUArray Int a)
      let go !i | i < n     = M.writeArray mu i (V.unsafeIndex v i) >> go (i+1)
                | otherwise = M.unsafeFreeze mu
      go 0

-- Generate a uniformly distributed (storable) vector of given size and range
--
randomVectorR :: (Variate a, Storable a) => GenIO -> (a,a) -> Int -> IO (V.Vector a)
randomVectorR gen lim n = do
  mu <- MV.new n
  let go !i | i < n     = uniformR lim gen >>= MV.unsafeWrite mu i >> go (i+1)
            | otherwise = V.unsafeFreeze mu
  go 0

