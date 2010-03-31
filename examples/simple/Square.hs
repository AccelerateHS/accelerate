{-# LANGUAGE ParallelListComp #-}

module Square (square, square_ref) where

import Prelude   hiding (replicate, zip, map, filter, max, min, not, zipWith)
import qualified Prelude

import Data.Array.Unboxed
import Data.Array.IArray

import Data.Array.Accelerate

square :: Vector Float -> Acc (Vector Float)
square xs
  = let
      xs' = use xs
    in 
    map (\x -> x * x) xs'

square_ref :: UArray Int Float -> UArray Int Float
square_ref xs
  = listArray (bounds xs) [x * x| x <- elems xs]
  
