{-# LANGUAGE ParallelListComp #-}

module Sum (sum, sum_ref) where

import Prelude   hiding (replicate, zip, map, filter, max, min, not, zipWith, sum)
import qualified Prelude

import Data.Array.Unboxed
import Data.Array.IArray

import Data.Array.Accelerate

sum :: Vector Float -> Acc (Scalar Float)
sum xs
  = let
      xs' = use xs
    in 
    fold (\x y -> x + y) (constant 0.0) xs'

sum_ref :: UArray Int Float -> UArray () Float
sum_ref xs
  = listArray ((), ()) $ [Prelude.sum $ elems xs]
  
