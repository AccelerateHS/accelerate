{-# LANGUAGE ParallelListComp #-}

module SMVM (smvm, smvm_ref) where

import Prelude   hiding (replicate, zip, unzip, map, filter, max, min, not,
                         zipWith)
import qualified Prelude

import Data.Array.Unboxed hiding ((!))
import Data.Array.IArray  hiding ((!))

import Data.Array.Accelerate


type SparseVector a = Vector (Int, a)
type SparseMatrix a = (Segments, SparseVector a)

smvm :: SparseMatrix Float -> Vector Float -> Acc (Vector Float)
smvm (segd', smat') vec'
  = let
      segd         = use segd'
      (inds, vals) = unzip (use smat')
      vec          = use vec'
      ---
      vecVals  = backpermute (shape inds) (\i -> inds!i) vec
      products = zipWith (*) vecVals vals
    in
    foldSeg (+) 0 products segd


type USparseMatrix a = (UArray Int Int, (UArray Int Int, UArray Int a))

smvm_ref :: USparseMatrix Float 
         -> UArray Int Float 
         -> UArray Int Float
smvm_ref (segd, (inds, values)) vec
  = undefined --listArray ((), ()) $ [sum [x * y | x <- elems xs | y <- elems ys]]

