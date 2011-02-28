{-# LANGUAGE BangPatterns, FlexibleContexts #-}

module Random where

import System.Random.MWC
import Data.Array.IArray
import Data.Array.Unboxed               (UArray)
import Data.Array.IO                    (MArray, IOUArray)
import Control.Exception                (evaluate)
import Data.Array.Accelerate            (Z(..),(:.)(..))
import qualified Data.Array.MArray      as M
import qualified Data.Array.Accelerate  as Acc


-- Convert an unboxed array to an Accelerate array
--
convertVector :: (IArray UArray e, Acc.Elt e) => UArray Int e -> IO (Acc.Vector e)
convertVector v =
  let arr = Acc.fromIArray v
  in  evaluate (arr `Acc.indexArray` (Z:.0)) >> return arr


-- Generate a random, uniformly distributed vector of specified size over the
-- range. For integral types the range is inclusive, for floating point numbers
-- the range (a,b] is used, if one ignores rounding errors.
--
randomVectorR
  :: (Variate a, MArray IOUArray a IO, IArray UArray a)
  => (a,a)
  -> GenIO
  -> Int
  -> IO (UArray Int a)

randomVectorR lim gen n = do
  mu  <- M.newArray_ (0,n-1) :: MArray IOUArray e IO => IO (IOUArray Int e)
  let go !i | i < n     = uniformR lim gen >>= M.writeArray mu i >> go (i+1)
            | otherwise = M.unsafeFreeze mu
  go 0

