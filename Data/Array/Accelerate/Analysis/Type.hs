{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE GADTs, TypeFamilies, PatternGuards #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Analysis.Type
-- Copyright   : [2009..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The Accelerate AST does not explicitly store much type information.  Most of
-- it is only indirectly through type class constraints -especially, 'Elt'
-- constraints- available.  This module provides functions that reify that
-- type information in the form of a 'TupleType' value.  This is, for example,
-- needed to emit type information in a backend.
--

module Data.Array.Accelerate.Analysis.Type (

  -- * Query AST types
  AccType, AccType2,
  arrayType, accType, accType2, expType, sizeOf,
  preAccType, preAccType2, preExpType

) where

-- standard library
import qualified Foreign.Storable as F

-- friends
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.AST



-- |Determine an array type
-- ------------------------

-- |Reify the element type of an array.
--
arrayType :: forall sh e. Array sh e -> TupleType (EltRepr e)
arrayType (Array _ _) = eltType (undefined::e)


-- |Determine the type of an expressions
-- -------------------------------------

type AccType  acc = forall aenv sh e. acc aenv (Array sh e) -> TupleType (EltRepr e)
type AccType2 acc = forall aenv sh1 e1 sh2 e2.
                      acc aenv (Array sh1 e1, Array sh2 e2) -> (TupleType (EltRepr e1), 
                                                                TupleType (EltRepr e2))

-- |Reify the element type of the result of an array computation.
--
accType :: AccType OpenAcc
accType (OpenAcc acc) = preAccType accType acc

-- |Reify the element type of the result of an array computation using the array computation AST
-- before tying the knot.
--
preAccType :: forall acc aenv sh e.
              AccType acc
           -> PreOpenAcc acc aenv (Array sh e)
           -> TupleType (EltRepr e)
preAccType k pacc =
  case pacc of
    Alet  _ acc         -> k acc
    Alet2 _ acc         -> k acc
    Avar _              -> -- eltType (undefined::e)   -- should work - GHC 6.12 bug?
                           case arrays :: ArraysR (Array sh e) of
                             ArraysRarray -> eltType (undefined::e)
    Apply _ _           -> -- eltType (undefined::e)   -- should work - GHC 6.12 bug?
                           case arrays :: ArraysR (Array sh e) of
                             ArraysRarray -> eltType (undefined::e)
    Acond _ acc _       -> k acc
    Use arr             -> arrayType arr
    Unit _              -> eltType (undefined::e)
    Generate _ _        -> eltType (undefined::e)
    Reshape _ acc       -> k acc
    Replicate _ _ acc   -> k acc
    Index _ acc _       -> k acc
    Map _ _             -> eltType (undefined::e)
    ZipWith _ _ _       -> eltType (undefined::e)
    Fold _ _ acc        -> k acc
    FoldSeg _ _ acc _   -> k acc
    Fold1 _ acc         -> k acc
    Fold1Seg _ acc _    -> k acc
    Scanl _ _ acc       -> k acc
    Scanl1 _ acc        -> k acc
    Scanr _ _ acc       -> k acc
    Scanr1 _ acc        -> k acc
    Permute _ _ _ acc   -> k acc
    Backpermute _ _ acc -> k acc
    Stencil _ _ _       -> eltType (undefined::e)
    Stencil2 _ _ _ _ _  -> eltType (undefined::e)

-- |Reify the element types of the results of an array computation that yields
-- two arrays.
--
accType2 :: AccType2 OpenAcc
accType2 (OpenAcc acc) = preAccType2 accType accType2 acc

-- |Reify the element types of the results of an array computation that yields
-- two arrays using the array computation AST before tying the knot.
--
preAccType2 :: forall acc aenv sh1 e1 sh2 e2.
               AccType  acc
            -> AccType2 acc
            -> PreOpenAcc acc aenv (Array sh1 e1, Array sh2 e2)
            -> (TupleType (EltRepr e1), TupleType (EltRepr e2))
preAccType2 k1 k2 pacc =
  case pacc of
    Alet  _ acc          -> k2 acc
    Alet2 _ acc          -> k2 acc
    PairArrays acc1 acc2 -> (k1 acc1, k1 acc2)
    Avar _    ->
      -- (eltType (undefined::e1), eltType (undefined::e2))
      -- should work - GHC 6.12 bug?
      case arrays :: ArraysR (Array sh1 e1, Array sh2 e2) of
        ArraysRpair ArraysRarray ArraysRarray
          -> (eltType (undefined::e1), eltType (undefined::e2))
        _ -> error "GHC is too dumb to realise that this is dead code"
    Apply _ _ ->
      -- (eltType (undefined::e1), eltType (undefined::e2))
      -- should work - GHC 6.12 bug?
      case arrays :: ArraysR (Array sh1 e1, Array sh2 e2) of
        ArraysRpair ArraysRarray ArraysRarray
          -> (eltType (undefined::e1), eltType (undefined::e2))
        _ -> error "GHC is too dumb to realise that this is dead code"
    Acond _ acc _  -> k2 acc
    Scanl' _ e acc -> (k1 acc, preExpType k1 e)
    Scanr' _ e acc -> (k1 acc, preExpType k1 e)

-- |Reify the result type of a scalar expression.
--
expType :: OpenExp aenv env t -> TupleType (EltRepr t)
expType = preExpType accType

-- |Reify the result types of of a scalar expression using the expression AST before tying the
-- knot.
--
preExpType :: forall acc aenv env t.
              AccType acc
           -> PreOpenExp acc aenv env t
           -> TupleType (EltRepr t)
preExpType k e =
  case e of
    Let _ _           -> eltType (undefined::t)
    Var _             -> eltType (undefined::t)
    Const _           -> eltType (undefined::t)
    Tuple _           -> eltType (undefined::t)
    Prj idx _         -> tupleIdxType idx
    IndexNil          -> eltType (undefined::t)
    IndexCons _ _     -> eltType (undefined::t)
    IndexHead _       -> eltType (undefined::t)
    IndexTail _       -> eltType (undefined::t)
    IndexAny          -> eltType (undefined::t)
    Cond _ t _        -> preExpType k t
    PrimConst _       -> eltType (undefined::t)
    PrimApp _ _       -> eltType (undefined::t)
    IndexScalar acc _ -> k acc
    Shape _           -> eltType (undefined::t)
    ShapeSize _       -> eltType (undefined::t)

-- |Reify the result type of a tuple projection.
--
tupleIdxType :: forall t e. TupleIdx t e -> TupleType (EltRepr e)
tupleIdxType ZeroTupIdx       = eltType (undefined::e)
tupleIdxType (SuccTupIdx idx) = tupleIdxType idx


-- |Size of a tuple type, in bytes
--
sizeOf :: TupleType a -> Int
sizeOf UnitTuple       = 0
sizeOf (PairTuple a b) = sizeOf a + sizeOf b

sizeOf (SingleTuple (NumScalarType (IntegralNumType t)))
  | IntegralDict <- integralDict t = F.sizeOf $ (undefined :: IntegralType a -> a) t
sizeOf (SingleTuple (NumScalarType (FloatingNumType t)))
  | FloatingDict <- floatingDict t = F.sizeOf $ (undefined :: FloatingType a -> a) t
sizeOf (SingleTuple (NonNumScalarType t))
  | NonNumDict   <- nonNumDict t   = F.sizeOf $ (undefined :: NonNumType a   -> a) t

