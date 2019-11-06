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

  arrayType,
  accType, expType,

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

accType :: forall acc aenv sh e. HasArraysRepr acc => acc aenv (Array sh e) -> TupleType (EltRepr e)
accType acc = case arraysRepr acc of
  ArraysRarray -> eltType @e

-- |Reify the result types of of a scalar expression using the expression AST before tying the
-- knot.
--
expType :: forall acc aenv env t.
              HasArraysRepr acc
           => PreOpenExp acc aenv env t
           -> TupleType (EltRepr t)
expType e =
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
    Cond _ t _        -> expType t
    While _ _ _       -> eltType @t
    PrimConst _       -> eltType @t
    PrimApp _ _       -> eltType @t
    Index acc _       -> accType acc
    LinearIndex acc _ -> accType acc
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

