{-# LANGUAGE BangPatterns, FlexibleContexts #-}

module Random where

import System.Random.MWC
import Data.Array.IArray
import Data.Array.Unboxed			(UArray)
import Data.Array.IO                    	(MArray, IOUArray)
import Control.Exception                	(evaluate)
import Data.Array.Accelerate            	(Z(..),(:.)(..))
import qualified Data.Vector.Generic		as G
import qualified Data.Vector.Generic.Mutable	as GM
import qualified Data.Array.MArray		as M
import qualified Data.Array.Accelerate  	as Acc


-- Convert an Unboxed Data.Array to an Accelerate Array
--
convertUArray :: (IArray UArray e, Acc.Elt e) => UArray Int e -> IO (Acc.Vector e)
convertUArray v =
  let arr = Acc.fromIArray v
  in  evaluate (arr `Acc.indexArray` (Z:.0)) >> return arr


-- Convert a Data.Vector to an Accelerate Array
--
convertVector
  :: (IArray UArray a, MArray IOUArray a IO, G.Vector v a, Acc.Elt a)
  => v a
  -> IO (Acc.Vector a)

convertVector vec = do
  arr <- Acc.fromIArray `fmap` toIArray vec
  evaluate (arr `Acc.indexArray` (Z:.0)) >> return arr
  where
    toIArray :: (MArray IOUArray a IO, IArray UArray a, G.Vector v a) => v a -> IO (UArray Int a)
    toIArray v = do
      let n = G.length v
      mu <- M.newArray_ (0,n-1) :: MArray IOUArray a IO => IO (IOUArray Int a)
      let go !i | i < n     = M.writeArray mu i (G.unsafeIndex v i) >> go (i+1)
                | otherwise = M.unsafeFreeze mu
      go 0


-- Generate a random, uniformly distributed vector of specified size over the
-- range. For integral types the range is inclusive, for floating point numbers
-- the range (a,b] is used, if one ignores rounding errors.
--
randomUArrayR
  :: (Variate a, MArray IOUArray a IO, IArray UArray a)
  => (a,a)
  -> GenIO
  -> Int
  -> IO (UArray Int a)

randomUArrayR lim gen n = do
  mu  <- M.newArray_ (0,n-1) :: MArray IOUArray e IO => IO (IOUArray Int e)
  let go !i | i < n     = uniformR lim gen >>= M.writeArray mu i >> go (i+1)
            | otherwise = M.unsafeFreeze mu
  go 0


-- Generate a uniformly distributed Data.Vector of specified range and size
--
randomVectorR :: (G.Vector v a, Variate a) => (a,a) -> GenIO -> Int -> IO (v a)
randomVectorR lim gen n = do
  mu <- GM.unsafeNew n
  let go !i | i < n     = uniformR lim gen >>= GM.unsafeWrite mu i >> go (i+1)
            | otherwise = G.unsafeFreeze mu
  go 0


