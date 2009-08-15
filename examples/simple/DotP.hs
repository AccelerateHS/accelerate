{-# LANGUAGE ParallelListComp #-}

module DotP (dotp, dotp_ref) where

import Prelude   hiding (replicate, zip, map, filter, max, min, not, zipWith)
import qualified Prelude

import Data.Array.Unboxed
import Data.Array.IArray

import Data.Array.Accelerate

dotp :: Vector Float -> Vector Float -> Acc (Scalar Float)
dotp xs ys 
  = let
      xs' = use xs
      ys' = use ys
    in
    fold (+) 0 (zipWith (*) xs' ys')

dotp_ref :: UArray Int Float 
         -> UArray Int Float 
         -> UArray ()  Float
dotp_ref xs ys 
  = listArray ((), ()) $ [sum [x * y | x <- elems xs | y <- elems ys]]
