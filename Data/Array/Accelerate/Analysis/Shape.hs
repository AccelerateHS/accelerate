{-# LANGUAGE ScopedTypeVariables, GADTs #-}
-- |
-- Module      : Data.Array.Accelerate.Analysis.Shape
-- Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Analysis.Shape (accDim, accDim2)
  where

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Sugar


-- |Reify the dimensionality of the result type of an array computation
--
accDim :: forall aenv dim e. OpenAcc aenv (Array dim e) -> Int
accDim (Let _ acc)            = accDim acc
accDim (Let2 _ acc)           = accDim acc
accDim (Avar _)               = ndim (elemType (undefined::dim))
accDim (Use (Array _ _))      = ndim (elemType (undefined::dim))
accDim (Unit _)               = 0
accDim (Reshape _ _)          = ndim (elemType (undefined::dim))
accDim (Replicate _ _ _)      = ndim (elemType (undefined::dim))
accDim (Index _ _ _)          = ndim (elemType (undefined::dim))
accDim (Map _ acc)            = accDim acc
accDim (ZipWith _ _ acc)      = accDim acc
accDim (Fold _ _ _)           = 0
accDim (FoldSeg _ _ _ _)      = 1
accDim (Permute _ acc _ _)    = accDim acc
accDim (Backpermute _ _ _)    = ndim (elemType (undefined::dim))
accDim (Stencil _ _ acc)      = accDim acc
accDim (Stencil2 _ _ acc _ _) = accDim acc

-- |Reify the dimensionality of the results of a computation that yields two
-- arrays
--
accDim2 :: OpenAcc aenv (Array dim1 e1, Array dim2 e2) -> (Int,Int)
accDim2 (Scanl _ _ acc) = (accDim acc, 0)
accDim2 (Scanr _ _ acc) = (accDim acc, 0)

-- Count the number of components to a tuple type
--
ndim :: TupleType a -> Int
ndim UnitTuple       = 0
ndim (SingleTuple _) = 1
ndim (PairTuple a b) = ndim a + ndim b

