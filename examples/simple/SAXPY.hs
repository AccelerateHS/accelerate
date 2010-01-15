{-# LANGUAGE ParallelListComp #-}

module SAXPY (saxpy, saxpy_ref) where

import Prelude   hiding (replicate, zip, map, filter, max, min, not, zipWith)
import qualified Prelude

import Data.Array.Unboxed
import Data.Array.IArray

import Data.Array.Accelerate

saxpy :: Float -> Vector Float -> Vector Float -> Acc (Vector Float)
saxpy alpha xs ys
  = let
      xs' = use xs
      ys' = use ys
    in 
    zipWith (\x y -> constant alpha * x + y) xs' ys'

saxpy_ref :: Float -> UArray Int Float -> UArray Int Float -> UArray Int Float
saxpy_ref alpha xs ys
  = listArray (bounds xs) [alpha * x + y | x <- elems xs | y <- elems ys]
  