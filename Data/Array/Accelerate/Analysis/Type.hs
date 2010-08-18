{-# LANGUAGE ScopedTypeVariables, GADTs, TypeFamilies, PatternGuards #-}

-- |Embedded array processing language: type analysis
--
--  Copyright (c) 2009 Manuel M T Chakravarty, Gabriele Keller, Sean Lee
--
--  License: BSD3
--
--- Description ---------------------------------------------------------------
--
--  The Accelerate AST does not explicitly store much type information.  Most of
--  it is only indirectly through type class constraints -especially, Elem
--  constraints- available.  This module provides functions that reify that 
--  type information in the form of a 'TupleType' value.  This is, for example,
--  needed to emit type information in a backend.

module Data.Array.Accelerate.Analysis.Type (

  -- * Query AST types
  accType, accType2, accShapeType, accShapeType2, expType, sizeOf
  
) where
  
-- friends
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.AST

-- neighbours
import qualified Foreign.Storable as F


-- Determine the type of an expressions
-- ------------------------------------

-- |Reify the element type of the result of an array computation.
--
accType :: forall aenv dim e.
           OpenAcc aenv (Array dim e) -> TupleType (ElemRepr e)
accType (Let _ acc)           = accType acc
accType (Let2 _ acc)          = accType acc
accType (Avar _)              = elemType (undefined::e)
accType (Use arr)             = arrayType arr
accType (Unit _)              = elemType (undefined::e)
accType (Reshape _ acc)       = accType acc
accType (Replicate _ _ acc)   = accType acc
accType (Index _ acc _)       = accType acc
accType (Map _ _)             = elemType (undefined::e)
accType (ZipWith _ _ _)       = elemType (undefined::e)
accType (Fold _ _ acc)        = accType acc
accType (FoldSeg _ _ acc _)   = accType acc
accType (Permute _ _ _ acc)   = accType acc
accType (Backpermute _ _ acc) = accType acc
accType (Stencil _ _ _)       = elemType (undefined::e)

-- |Reify the element types of the results of an array computation that yields
-- two arrays.
--
accType2 :: OpenAcc aenv (Array dim1 e1, Array dim2 e2)
         -> (TupleType (ElemRepr e1), TupleType (ElemRepr e2))
accType2 (Scanl _ e acc) = (accType acc, expType e)
accType2 (Scanr _ e acc) = (accType acc, expType e)

-- |Reify the shape type of the result of an array computation
--
accShapeType :: forall aenv dim e.
                OpenAcc aenv (Array dim e) -> TupleType (ElemRepr dim)
accShapeType (Let _ acc)         = accShapeType acc
accShapeType (Let2 _ acc)        = accShapeType acc
accShapeType (Avar _)            = elemType (undefined::dim)
accShapeType (Use (Array _ _))   = elemType (undefined::dim)
accShapeType (Unit _)            = elemType (undefined::DIM0)
accShapeType (Reshape _ _)       = elemType (undefined::dim)
accShapeType (Replicate _ _ _)   = elemType (undefined::dim)
accShapeType (Index _ _ _)       = elemType (undefined::dim)
accShapeType (Map _ acc)         = accShapeType acc
accShapeType (ZipWith _ _ acc)   = accShapeType acc
accShapeType (Fold _ _ _)        = elemType (undefined::DIM0)
accShapeType (FoldSeg _ _ _ _)   = elemType (undefined::DIM1)
accShapeType (Permute _ acc _ _) = accShapeType acc
accShapeType (Backpermute _ _ _) = elemType (undefined::dim)
accShapeType (Stencil _ _ acc)   = accShapeType acc

-- |Reify the shape type of the results of a computation that yields two arrays
--
accShapeType2 :: OpenAcc aenv (Array dim1 e1, Array dim2 e2)
              -> (TupleType (ElemRepr dim1), TupleType (ElemRepr dim2))
accShapeType2 (Scanl _ _ acc) = (accShapeType acc, elemType (undefined::()))
accShapeType2 (Scanr _ _ acc) = (accShapeType acc, elemType (undefined::()))


-- |Reify the result type of a scalar expression.
--
expType :: forall aenv env t. OpenExp aenv env t -> TupleType (ElemRepr t)
expType (Var _)             = elemType (undefined::t)
expType (Const _)           = elemType (undefined::t)
expType (Tuple _)           = elemType (undefined::t)
expType (Prj idx _)         = tupleIdxType idx
expType (Cond _ t _)        = expType t
expType (PrimConst _)       = elemType (undefined::t)
expType (PrimApp _ _)       = elemType (undefined::t)
expType (IndexScalar acc _) = accType acc
expType (Shape _)           = elemType (undefined::t)

-- |Reify the result type of a tuple projection.
--
tupleIdxType :: forall t e. TupleIdx t e -> TupleType (ElemRepr e)
tupleIdxType ZeroTupIdx       = elemType (undefined::e)
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

