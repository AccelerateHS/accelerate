{-# LANGUAGE ScopedTypeVariables, GADTs, RankNTypes #-}
{-# OPTIONS_HADDOCK hide #-}
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

  -- * query AST dimensionality
  AccDim, AccDim2,
  accDim, accDim2,
  preAccDim, preAccDim2

) where

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Sugar


type AccDim acc  = forall aenv sh e. acc aenv (Array sh e) -> Int
type AccDim2 acc = forall aenv sh1 e1 sh2 e2. acc aenv (Array sh1 e1, Array sh2 e2) -> (Int,Int)

-- |Reify the dimensionality of the result type of an array computation
--
accDim :: AccDim OpenAcc
accDim (OpenAcc acc) = preAccDim accDim acc

-- |Reify dimensionality of a computation parameterised over a recursive closure
--
preAccDim :: forall acc aenv sh e. AccDim acc -> PreOpenAcc acc aenv (Array sh e) -> Int
preAccDim k pacc =
  case pacc of
    Alet  _ acc          -> k acc
    Alet2 _ acc          -> k acc
    Avar _               -> -- ndim (eltType (undefined::sh))   -- should work - GHC 6.12 bug?
                            case arrays :: ArraysR (Array sh e) of
                              ArraysRarray -> ndim (eltType (undefined::sh))
    Apply _ _            -> -- ndim (eltType (undefined::sh))   -- should work - GHC 6.12 bug?
                            case arrays :: ArraysR (Array sh e) of
                              ArraysRarray -> ndim (eltType (undefined::sh))
    Acond _ acc _        -> k acc
    Use (Array _ _)      -> ndim (eltType (undefined::sh))
    Unit _               -> 0
    Generate _ _         -> ndim (eltType (undefined::sh))
    Reshape _ _          -> ndim (eltType (undefined::sh))
    Replicate _ _ _      -> ndim (eltType (undefined::sh))
    Index _ _ _          -> ndim (eltType (undefined::sh))
    Map _ acc            -> k acc
    ZipWith _ _ acc      -> k acc
    Fold _ _ acc         -> k acc - 1
    Fold1 _ acc          -> k acc - 1
    FoldSeg _ _ acc _    -> k acc
    Fold1Seg _ acc _     -> k acc
    Scanl _ _ acc        -> k acc
    Scanl1 _ acc         -> k acc
    Scanr _ _ acc        -> k acc
    Scanr1 _ acc         -> k acc
    Permute _ acc _ _    -> k acc
    Backpermute _ _ _    -> ndim (eltType (undefined::sh))
    Stencil _ _ acc      -> k acc
    Stencil2 _ _ acc _ _ -> k acc


-- |Reify the dimensionality of the results of a computation that yields two
-- arrays
--
accDim2 :: AccDim2 OpenAcc
accDim2 (OpenAcc acc) = preAccDim2 accDim accDim2 acc

preAccDim2 :: forall acc aenv sh1 e1 sh2 e2.
              AccDim  acc
           -> AccDim2 acc
           -> PreOpenAcc acc aenv (Array sh1 e1, Array sh2 e2)
           -> (Int, Int)
preAccDim2 k1 k2 pacc =
  case pacc of
    Alet  _ acc          -> k2 acc
    Alet2 _ acc          -> k2 acc
    PairArrays acc1 acc2 -> (k1 acc1, k1 acc2)
    Avar _ ->
      -- (ndim (eltType (undefined::dim1)), ndim (eltType (undefined::dim2)))
      -- should work - GHC 6.12 bug?
      case arrays :: ArraysR (Array sh1 e1, Array sh2 e2) of
        ArraysRpair ArraysRarray ArraysRarray
          -> (ndim (eltType (undefined::sh1))
             ,ndim (eltType (undefined::sh2)))
        _ -> error "GHC is too dumb to realise that this is dead code"
    Apply _ _ ->
      -- (ndim (eltType (undefined::dim1)), ndim (eltType (undefined::dim2)))
      -- should work - GHC 6.12 bug?
      case arrays :: ArraysR (Array sh1 e1, Array sh2 e2) of
        ArraysRpair ArraysRarray ArraysRarray
          -> (ndim (eltType (undefined::sh1))
             ,ndim (eltType (undefined::sh2)))
        _ -> error "GHC is too dumb to realise that this is dead code"
    Acond _ acc _  -> k2 acc
    Scanl' _ _ acc -> (k1 acc, 0)
    Scanr' _ _ acc -> (k1 acc, 0)

-- Count the number of components to a tuple type
--
ndim :: TupleType a -> Int
ndim UnitTuple       = 0
ndim (SingleTuple _) = 1
ndim (PairTuple a b) = ndim a + ndim b

