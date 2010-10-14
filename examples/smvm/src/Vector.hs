{-# LANGUAGE BangPatterns, FlexibleContexts #-}

module Vector where

import Control.Exception
import Data.Array.IArray
import Data.Array.Unboxed                       (UArray)
import Data.Array.IO                            (MArray, IOUArray)
import qualified Data.Vector.Generic            as V
import qualified Data.Array.MArray              as M
import qualified Data.Array.Accelerate          as Acc

-- Convert a Data.Vector to an Accelerate Array
--
convertVector
  :: (IArray UArray a, MArray IOUArray a IO, V.Vector v a, Acc.Elem a)
  => v a
  -> IO (Acc.Vector a)
{-# INLINE convertVector #-}

convertVector vec = do
  arr <- Acc.fromIArray `fmap` toIArray vec
  evaluate (arr `Acc.indexArray` 0) >> return arr
  where
    toIArray :: (MArray IOUArray a IO, IArray UArray a, V.Vector v a) => v a -> IO (UArray Int a)
    toIArray v = do
      let n = V.length v
      mu <- M.newArray_ (0,n-1) :: MArray IOUArray a IO => IO (IOUArray Int a)
      let go !i | i < n     = M.writeArray mu i (V.unsafeIndex v i) >> go (i+1)
                | otherwise = M.unsafeFreeze mu
      go 0

