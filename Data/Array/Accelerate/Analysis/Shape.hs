{-# LANGUAGE ScopedTypeVariables, GADTs #-}
-- |
-- Module      : Data.Array.Accelerate.Analysis.Shape
-- Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Analysis.Shape (
  accDim, accDim2
) where

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Sugar


-- |Reify the dimensionality of the result type of an array computation
--
accDim :: forall aenv sh e. OpenAcc aenv (Array sh e) -> Int
accDim (OpenAcc acc) = accDim' acc

accDim' :: forall aenv sh e. PreOpenAcc OpenAcc aenv (Array sh e) -> Int
accDim' (Let _ acc)            = accDim acc
accDim' (Let2 _ acc)           = accDim acc
accDim' (Avar _)               = -- ndim (eltType (undefined::sh))   -- should work - GHC 6.12 bug?
                                 case arrays :: ArraysR (Array sh e) of 
                                   ArraysRarray -> ndim (eltType (undefined::sh))
accDim' (Apply _ _)            = -- ndim (eltType (undefined::sh))   -- should work - GHC 6.12 bug?
                                 case arrays :: ArraysR (Array sh e) of 
                                   ArraysRarray -> ndim (eltType (undefined::sh))
accDim' (Acond _ acc _)        = accDim acc
accDim' (Use (Array _ _))      = ndim (eltType (undefined::sh))
accDim' (Unit _)               = 0
accDim' (Generate _ _)         = ndim (eltType (undefined::sh))
accDim' (Reshape _ _)          = ndim (eltType (undefined::sh))
accDim' (Replicate _ _ _)      = ndim (eltType (undefined::sh))
accDim' (Index _ _ _)          = ndim (eltType (undefined::sh))
accDim' (Map _ acc)            = accDim acc
accDim' (ZipWith _ _ acc)      = accDim acc
accDim' (Fold _ _ acc)         = accDim acc - 1
accDim' (FoldSeg _ _ _ acc)    = accDim acc
accDim' (Fold1 _ acc)          = accDim acc - 1
accDim' (Fold1Seg _ _ acc)     = accDim acc
accDim' (Scanl _ _ acc)        = accDim acc
accDim' (Scanl1 _ acc)         = accDim acc
accDim' (Scanr _ _ acc)        = accDim acc
accDim' (Scanr1 _ acc)         = accDim acc
accDim' (Permute _ acc _ _)    = accDim acc
accDim' (Backpermute _ _ _)    = ndim (eltType (undefined::sh))
accDim' (Stencil _ _ acc)      = accDim acc
accDim' (Stencil2 _ _ acc _ _) = accDim acc

-- |Reify the dimensionality of the results of a computation that yields two
-- arrays
--
accDim2 :: forall aenv sh1 e1 sh2 e2. OpenAcc aenv (Array sh1 e1, Array sh2 e2) -> (Int, Int)
accDim2 (OpenAcc acc) = accDim2' acc

accDim2' :: forall aenv sh1 e1 sh2 e2. 
            PreOpenAcc OpenAcc aenv (Array sh1 e1, Array sh2 e2) -> (Int, Int)
accDim2' (Let _ acc)      = accDim2 acc
accDim2' (Let2 _ acc)     = accDim2 acc
accDim2' (Avar _)         = -- (ndim (eltType (undefined::dim1)), ndim (eltType (undefined::dim2)))
                            -- should work - GHC 6.12 bug?
                             case arrays :: ArraysR (Array sh1 e1, Array sh2 e2) of 
                               ArraysRpair ArraysRarray ArraysRarray 
                                 -> (ndim (eltType (undefined::sh1)), 
                                     ndim (eltType (undefined::sh2)))
                               _ -> error "GHC is too dumb to realise that this is dead code"
accDim2' (Apply _ _)      = -- (ndim (eltType (undefined::dim1)), ndim (eltType (undefined::dim2)))
                            -- should work - GHC 6.12 bug?
                             case arrays :: ArraysR (Array sh1 e1, Array sh2 e2) of 
                               ArraysRpair ArraysRarray ArraysRarray 
                                 -> (ndim (eltType (undefined::sh1)), 
                                     ndim (eltType (undefined::sh2)))
                               _ -> error "GHC is too dumb to realise that this is dead code"
accDim2' (Acond _ acc _)  = accDim2 acc
accDim2' (Scanl' _ _ acc) = (accDim acc, 0)
accDim2' (Scanr' _ _ acc) = (accDim acc, 0)

-- Count the number of components to a tuple type
--
ndim :: TupleType a -> Int
ndim UnitTuple       = 0
ndim (SingleTuple _) = 1
ndim (PairTuple a b) = ndim a + ndim b

