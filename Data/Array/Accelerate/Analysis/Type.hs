{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Analysis.Type
-- Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
--               [2009..2012] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
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
  AccType, arrayType, sizeOf,
  accType, expType, delayedAccType, delayedExpType,
  preAccType, preExpType

) where

-- standard library
import qualified Foreign.Storable as F

-- friends
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Trafo



-- |Determine an array type
-- ------------------------

-- |Reify the element type of an array.
--
arrayType :: forall sh e. Array sh e -> TupleType (EltRepr e)
arrayType (Array _ _) = eltType (undefined::e)


-- |Determine the type of an expressions
-- -------------------------------------

type AccType  acc = forall aenv sh e. acc aenv (Array sh e) -> TupleType (EltRepr e)

-- |Reify the element type of the result of an array computation.
--
accType :: AccType OpenAcc
accType (OpenAcc acc) = preAccType accType acc

delayedAccType :: AccType DelayedOpenAcc
delayedAccType (Manifest acc) = preAccType delayedAccType acc
delayedAccType (Delayed _ f _)
  | Lam (Body e) <- f   = delayedExpType e
  | otherwise           = error "my favourite place in the world is wherever you happen to be"


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

    -- The following all contain impossible pattern matches, but GHC's type
    -- checker does no grok that
    --
    Avar _              -> case arrays' (undefined :: (Array sh e)) of
                             ArraysRarray -> eltType (undefined::e)
                             _            -> error "When I get sad, I stop being sad and be AWESOME instead."

    Apply _ _           -> case arrays' (undefined :: Array sh e) of
                             ArraysRarray -> eltType (undefined::e)
                             _            -> error "TRUE STORY."

    Atuple _            -> case arrays' (undefined :: Array sh e) of
                             ArraysRarray -> eltType (undefined::e)
                             _            -> error "I made you a cookie, but I eated it."

    Aprj _ _            -> case arrays' (undefined :: Array sh e) of
                             ArraysRarray -> eltType (undefined::e)
                             _            -> error "Hey look! even the leaves are falling for you."

    Aforeign _ _ _      -> case arrays' (undefined :: Array sh e) of
                             ArraysRarray -> eltType (undefined::e)
                             _            -> error "Who on earth wrote all these weird error messages?"

    Acond _ acc _       -> k acc
    Awhile _ _ acc      -> k acc
    Use ((),a)          -> arrayType a
    Unit _              -> eltType (undefined::e)
    Generate _ _        -> eltType (undefined::e)
    Transform _ _ _ _   -> eltType (undefined::e)
    Reshape _ acc       -> k acc
    Replicate _ _ acc   -> k acc
    Slice _ acc _       -> k acc
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


-- |Reify the result type of a scalar expression.
--
expType :: OpenExp env aenv t -> TupleType (EltRepr t)
expType = preExpType accType

delayedExpType :: DelayedOpenExp env aenv t -> TupleType (EltRepr t)
delayedExpType = preExpType delayedAccType

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
    Prj _ _           -> eltType (undefined::t)
    IndexNil          -> eltType (undefined::t)
    IndexCons _ _     -> eltType (undefined::t)
    IndexHead _       -> eltType (undefined::t)
    IndexTail _       -> eltType (undefined::t)
    IndexAny          -> eltType (undefined::t)
    IndexSlice _ _ _  -> eltType (undefined::t)
    IndexFull _ _ _   -> eltType (undefined::t)
    ToIndex _ _       -> eltType (undefined::t)
    FromIndex _ _     -> eltType (undefined::t)
    Cond _ t _        -> preExpType k t
    While _ _ _       -> eltType (undefined::t)
    PrimConst _       -> eltType (undefined::t)
    PrimApp _ _       -> eltType (undefined::t)
    Index acc _       -> k acc
    LinearIndex acc _ -> k acc
    Shape _           -> eltType (undefined::t)
    ShapeSize _       -> eltType (undefined::t)
    Intersect _ _     -> eltType (undefined::t)
    Foreign _ _ _     -> eltType (undefined::t)


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

