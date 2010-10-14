
module SMVM (smvm) where

import Data.Array.Accelerate           (Vector, Segments, Acc)
import qualified Data.Array.Accelerate as Acc


type SparseVector a = (Vector Int, Vector a)
type SparseMatrix a = (Segments, SparseVector a)

smvm :: SparseMatrix Float -> Vector Float -> Acc (Vector Float)
smvm (segd', (inds', vals')) vec'
  = let
      segd         = Acc.use segd'
      inds         = Acc.use inds'
      vals         = Acc.use vals'
      vec          = Acc.use vec'
      ---
      vecVals  = Acc.backpermute (Acc.shape inds) (\i -> inds Acc.! i) vec
      products = Acc.zipWith (*) vecVals vals
    in
    Acc.foldSeg (+) 0 products segd

