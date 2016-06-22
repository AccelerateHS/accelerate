{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SMVM where

import Data.Array.Accelerate            as A


-- Sparse-matrix vector multiplication
-- -----------------------------------

type SparseVector e = Vector (Int32, e)
type SparseMatrix e = (Segments Int32, SparseVector e)


smvm :: A.Num a => Acc (SparseMatrix a) -> Acc (Vector a) -> Acc (Vector a)
smvm smat vec
  = let (segd, svec)    = unlift smat
        (inds, vals)    = A.unzip svec

        vecVals         = gather (A.map A.fromIntegral inds) vec
        products        = A.zipWith (*) vecVals vals
    in
    foldSeg (+) 0 products segd

