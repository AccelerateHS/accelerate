{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Analysis.Type
-- Copyright   : [2008..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The Accelerate AST does not explicitly store much type information.  Most of
-- it is only indirectly through type class constraints -especially, 'Elt'
-- constraints- available. This module provides functions that reify that type
-- information in the form of a 'TupleType value. This is, for example, needed
-- to emit type information in a backend.
--

module Data.Array.Accelerate.Analysis.Type (

  AccType, arrayType,
  accType, expType, delayedAccType, delayedExpType,
  preAccType, preExpType,

  sizeOf,
  sizeOfScalarType,
  sizeOfSingleType,
  sizeOfVectorType,
  sizeOfNumType,
  sizeOfNonNumType,

) where

-- friends
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Trafo.Base
import Data.Array.Accelerate.Type

-- standard library
import qualified Foreign.Storable as F


-- |Determine an array type
-- ------------------------

-- |Reify the element type of an array.
--
arrayType :: forall sh e. Elt e => Array sh e -> TupleType (EltRepr e)
arrayType _ = eltType @e


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
    Alet _ acc          -> k acc

    -- The following all contain impossible pattern matches, but GHC's type
    -- checker does no grok that
    --
    Avar{}              -> case arrays @(Array sh e) of
                             ArraysRarray -> eltType @e
#if __GLASGOW_HASKELL__ < 800
                             _            -> error "When I get sad, I stop being sad and be AWESOME instead."
#endif

    Apply{}             -> case arrays @(Array sh e) of
                             ArraysRarray -> eltType @e
#if __GLASGOW_HASKELL__ < 800
                             _            -> error "TRUE STORY."
#endif

    Atuple{}            -> case arrays @(Array sh e) of
                             ArraysRarray -> eltType @e
#if __GLASGOW_HASKELL__ < 800
                             _            -> error "I made you a cookie, but I eated it."
#endif

    Aprj{}              -> case arrays @(Array sh e) of
                             ArraysRarray -> eltType @e
#if __GLASGOW_HASKELL__ < 800
                             _            -> error "Hey look! even the leaves are falling for you."
#endif

    Aforeign{}          -> case arrays @(Array sh e) of
                             ArraysRarray -> eltType @e
#if __GLASGOW_HASKELL__ < 800
                             _            -> error "Who on earth wrote all these weird error messages?"
#endif

    Use{}               -> case arrays @(Array sh e) of
                             ArraysRarray -> eltType @e
#if __GLASGOW_HASKELL__ < 800
                             _            -> error "rob you are terrible at this game"
#endif

    Acond _ acc _       -> k acc
    Awhile _ _ acc      -> k acc
    Unit _              -> eltType @e
    Generate _ _        -> eltType @e
    Transform _ _ _ _   -> eltType @e
    Reshape _ acc       -> k acc
    Replicate _ _ acc   -> k acc
    Slice _ acc _       -> k acc
    Map _ _             -> eltType @e
    ZipWith _ _ _       -> eltType @e
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
    Stencil _ _ _       -> eltType @e
    Stencil2 _ _ _ _ _  -> eltType @e


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
    Let _ _           -> eltType @t
    Var _             -> eltType @t
    Const _           -> eltType @t
    Undef             -> eltType @t
    Tuple _           -> eltType @t
    Prj _ _           -> eltType @t
    IndexNil          -> eltType @t
    IndexCons _ _     -> eltType @t
    IndexHead _       -> eltType @t
    IndexTail _       -> eltType @t
    IndexAny          -> eltType @t
    IndexSlice _ _ _  -> eltType @t
    IndexFull _ _ _   -> eltType @t
    ToIndex _ _       -> eltType @t
    FromIndex _ _     -> eltType @t
    Cond _ t _        -> preExpType k t
    While _ _ _       -> eltType @t
    PrimConst _       -> eltType @t
    PrimApp _ _       -> eltType @t
    Index acc _       -> k acc
    LinearIndex acc _ -> k acc
    Shape _           -> eltType @t
    ShapeSize _       -> eltType @t
    Intersect _ _     -> eltType @t
    Union _ _         -> eltType @t
    Foreign _ _ _     -> eltType @t
    Coerce _          -> eltType @t


-- |Size of a tuple type, in bytes
--
sizeOf :: TupleType a -> Int
sizeOf TypeRunit       = 0
sizeOf (TypeRpair a b) = sizeOf a + sizeOf b
sizeOf (TypeRscalar t) = sizeOfScalarType t

sizeOfScalarType :: ScalarType t -> Int
sizeOfScalarType (SingleScalarType t) = sizeOfSingleType t
sizeOfScalarType (VectorScalarType t) = sizeOfVectorType t

sizeOfSingleType :: SingleType t -> Int
sizeOfSingleType (NumSingleType t)    = sizeOfNumType t
sizeOfSingleType (NonNumSingleType t) = sizeOfNonNumType t

sizeOfVectorType :: VectorType t -> Int
sizeOfVectorType (VectorType n t) = n * sizeOfSingleType t

sizeOfNumType :: forall t. NumType t -> Int
sizeOfNumType (IntegralNumType t) | IntegralDict <- integralDict t = F.sizeOf (undefined::t)
sizeOfNumType (FloatingNumType t) | FloatingDict <- floatingDict t = F.sizeOf (undefined::t)

sizeOfNonNumType :: forall t. NonNumType t -> Int
sizeOfNonNumType TypeBool{} = 1 -- stored as Word8
sizeOfNonNumType t | NonNumDict <- nonNumDict t = F.sizeOf (undefined::t)

