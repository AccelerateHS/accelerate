{-# LANGUAGE ParallelListComp #-}

module SMVM (smvm, smvm_ref) where

import Data.Array.Unboxed

import Data.Array.Accelerate           (Vector, Segments, Acc)
import qualified Data.Array.Accelerate as Acc


type SparseVector a = Vector (Int, a)
type SparseMatrix a = (Segments, SparseVector a)

smvm :: SparseMatrix Float -> Vector Float -> Acc (Vector Float)
smvm (segd', smat') vec'
  = let
      segd         = Acc.use segd'
      (inds, vals) = Acc.unzip (Acc.use smat')
      vec          = Acc.use vec'
      ---
      vecVals  = Acc.backpermute (Acc.shape inds) (\i -> inds Acc.! i) vec
      products = Acc.zipWith (*) vecVals vals
    in
    Acc.foldSeg (+) 0 products segd


type USparseMatrix a = (UArray Int Int, (UArray Int Int, UArray Int a))

smvm_ref :: USparseMatrix Float 
         -> UArray Int Float 
         -> UArray Int Float
smvm_ref (segd, (inds, values)) vec
  = listArray (0, rangeSize (bounds segd) - 1)
  $ [sum [ values!i * vec!(inds!i) | i <- range seg] | seg <- segd' ]
  where
    segbegin = scanl  (+) 0 $ elems segd
    segend   = scanl1 (+)   $ elems segd
    segd'    = zipWith (\x y -> (x,y-1)) segbegin segend

