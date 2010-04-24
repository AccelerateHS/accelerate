{-# LANGUAGE ScopedTypeVariables, GADTs, TypeFamilies #-}

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
  accType, accType2, expType,
  
) where
  
-- friends
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.AST


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
accType _                     = error "we can never get here"
  -- GHC's exaustivesness checker is not smart enough to exclude alternatives
  -- in a GADT match that are impossible due to equality constraints

-- |Reify the element types of the results of an array computation that yields
-- two arrays.
--
accType2 :: OpenAcc aenv (Array dim1 e1, Array dim2 e2)
         -> (TupleType (ElemRepr e1), TupleType (ElemRepr e2))
accType2 (Scanl _ e acc) = (accType acc, expType e)
accType2 (Scanr _ e acc) = (accType acc, expType e)
accType2 _               = error "we can never get here"
  -- GHC's exaustivesness checker is not smart enough to exclude alternatives
  -- in a GADT match that are impossible due to equality constraints

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
