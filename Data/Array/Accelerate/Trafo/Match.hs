{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo.Simplify
-- Copyright   : [2012] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Trafo.Match (

  -- matching expressions
  (:=:)(..),
  matchOpenExp,

) where

-- standard library
import Prelude                                          hiding ( exp )
import Data.Typeable
import Data.Hashable

-- friends
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Sugar                ( Elt )
import Data.Array.Accelerate.Tuple                      hiding ( Tuple )
import qualified Data.Array.Accelerate.Tuple            as Tuple
import qualified Data.Array.Accelerate.Array.Sugar      as Sugar



-- Witness equality between types. A value of a :=: b is a proof that types a
-- and b are equal. By pattern matching on REFL this fact is introduced to the
-- type checker.
--
data s :=: t where
  REFL :: s :=: s

deriving instance Show (s :=: t)


-- Compute the congruence of two scalar expressions. Two nodes are congruent if
-- either:
--
--  1. The nodes label constants and the contents are equal
--  2. They have the same operator and their operands are congruent
--
-- The below attempts to use real typed equality, but occasionally still needs
-- to use a cast, particularly when we can only match the representation types.
--
matchOpenExp :: forall env aenv s t. OpenExp env aenv s -> OpenExp env aenv t -> Maybe (s :=: t)
matchOpenExp (Let x1 e1) (Let x2 e2)
  | Just REFL <- matchOpenExp x1 x2
  , Just REFL <- matchOpenExp e1 e2
  = Just REFL

matchOpenExp (Var v1) (Var v2)
  = matchIdx v1 v2

matchOpenExp (Const c1) (Const c2)
  | Just REFL <- matchTupleType (Sugar.eltType (undefined::s)) (Sugar.eltType (undefined::t))
  , c1 == c2
  = gcast REFL  -- surface/representation type

matchOpenExp (Tuple t1) (Tuple t2)
  | Just REFL <- matchTuple t1 t2
  = gcast REFL  -- surface/representation type

matchOpenExp (Prj ix1 t1) (Prj ix2 t2)
  | Just REFL <- matchOpenExp  t1  t2
  , Just REFL <- matchTupleIdx ix1 ix2
  = Just REFL

matchOpenExp IndexAny IndexAny
  = gcast REFL  -- ???

matchOpenExp IndexNil IndexNil
  = Just REFL

matchOpenExp (IndexCons sl1 a1) (IndexCons sl2 a2)
  | Just REFL <- matchOpenExp sl1 sl2
  , Just REFL <- matchOpenExp a1 a2
  = Just REFL

matchOpenExp (IndexHead sl1) (IndexHead sl2)
  | Just REFL <- matchOpenExp sl1 sl2
  = Just REFL

matchOpenExp (IndexTail sl1) (IndexTail sl2)
  | Just REFL <- matchOpenExp sl1 sl2
  = Just REFL

matchOpenExp (ToIndex sh1 i1) (ToIndex sh2 i2)
  | Just REFL <- matchOpenExp sh1 sh2
  , Just REFL <- matchOpenExp i1 i2
  = Just REFL

matchOpenExp (FromIndex sh1 i1) (FromIndex sh2 i2)
  = matchOpenExp i1 i2 >> matchOpenExp sh1 sh2

matchOpenExp (Cond p1 t1 e1) (Cond p2 t2 e2)
  = matchOpenExp p1 p2 >> matchOpenExp t1 t2 >> matchOpenExp e1 e2

matchOpenExp (PrimConst c1) (PrimConst c2)
  = matchPrimConst c1 c2

matchOpenExp (PrimApp f1 x1) (PrimApp f2 x2)
  | Just x1'  <- commutes f1 x1
  , Just x2'  <- commutes f2 x2
  , Just REFL <- matchOpenExp x1' x2'
  , Just REFL <- matchPrimFun f1 f2
  = Just REFL

  | Just REFL <- matchOpenExp x1 x2
  , Just REFL <- matchPrimFun f1 f2
  = Just REFL

matchOpenExp (IndexScalar a1 x1) (IndexScalar a2 x2)
  | OpenAcc (Avar v1) <- a1
  , OpenAcc (Avar v2) <- a2
  , Just REFL         <- matchIdx v1 v2
  , Just REFL         <- matchOpenExp x1 x2
  = Just REFL

matchOpenExp (Shape a1) (Shape a2)
  | Just REFL <- matchAvarIdx a1 a2
  = Just REFL

matchOpenExp (ShapeSize sh1) (ShapeSize sh2)
  | Just REFL <- matchOpenExp sh1 sh2
  = Just REFL

matchOpenExp (Intersect sa1 sb1) (Intersect sa2 sb2)
  = matchOpenExp sa1 sa2 >> matchOpenExp sb1 sb2

matchOpenExp _ _
  = Nothing


-- Environment projection indices
--
matchIdx :: Idx env s -> Idx env t -> Maybe (s :=: t)
matchIdx ZeroIdx     ZeroIdx     = Just REFL
matchIdx (SuccIdx u) (SuccIdx v) = matchIdx u v
matchIdx _           _           = Nothing

matchAvarIdx :: OpenAcc aenv s -> OpenAcc aenv t -> Maybe (s :=: t)
matchAvarIdx (OpenAcc (Avar v1)) (OpenAcc (Avar v2))
  | Just REFL <- matchIdx v1 v2
  = Just REFL
matchAvarIdx _ _
  = error "matchAvarIdx: expected array variable"       -- not strictly necessary


-- Tuple projection indices. Given the same tuple expression structure (tup),
-- check that the indices project identical elements.
--
matchTupleIdx :: TupleIdx tup s -> TupleIdx tup t -> Maybe (s :=: t)
matchTupleIdx ZeroTupIdx     ZeroTupIdx     = Just REFL
matchTupleIdx (SuccTupIdx s) (SuccTupIdx t) = matchTupleIdx s t
matchTupleIdx _              _              = Nothing

-- Tuples
--
matchTuple :: Tuple.Tuple (OpenExp env aenv) s
           -> Tuple.Tuple (OpenExp env aenv) t
           -> Maybe (s :=: t)
matchTuple NilTup          NilTup               = Just REFL
matchTuple (SnocTup t1 e1) (SnocTup t2 e2)
  | Just REFL <- matchTuple   t1 t2
  , Just REFL <- matchOpenExp e1 e2
  = Just REFL

matchTuple _               _                    = Nothing


matchPrimConst :: (Elt s, Elt t) => PrimConst s -> PrimConst t -> Maybe (s :=: t)
matchPrimConst (PrimMinBound s) (PrimMinBound t) = matchBoundedType s t
matchPrimConst (PrimMaxBound s) (PrimMaxBound t) = matchBoundedType s t
matchPrimConst (PrimPi s)       (PrimPi t)       = matchFloatingType s t
matchPrimConst _                _                = Nothing


matchPrimFun :: (Elt s, Elt t) => PrimFun (a -> s) -> PrimFun (a -> t) -> Maybe (s :=: t)
matchPrimFun (PrimAdd _)            (PrimAdd _)            = Just REFL
matchPrimFun (PrimSub _)            (PrimSub _)            = Just REFL
matchPrimFun (PrimMul _)            (PrimMul _)            = Just REFL
matchPrimFun (PrimNeg _)            (PrimNeg _)            = Just REFL
matchPrimFun (PrimAbs _)            (PrimAbs _)            = Just REFL
matchPrimFun (PrimSig _)            (PrimSig _)            = Just REFL
matchPrimFun (PrimQuot _)           (PrimQuot _)           = Just REFL
matchPrimFun (PrimRem _)            (PrimRem _)            = Just REFL
matchPrimFun (PrimIDiv _)           (PrimIDiv _)           = Just REFL
matchPrimFun (PrimMod _)            (PrimMod _)            = Just REFL
matchPrimFun (PrimBAnd _)           (PrimBAnd _)           = Just REFL
matchPrimFun (PrimBOr _)            (PrimBOr _)            = Just REFL
matchPrimFun (PrimBXor _)           (PrimBXor _)           = Just REFL
matchPrimFun (PrimBNot _)           (PrimBNot _)           = Just REFL
matchPrimFun (PrimBShiftL _)        (PrimBShiftL _)        = Just REFL
matchPrimFun (PrimBShiftR _)        (PrimBShiftR _)        = Just REFL
matchPrimFun (PrimBRotateL _)       (PrimBRotateL _)       = Just REFL
matchPrimFun (PrimBRotateR _)       (PrimBRotateR _)       = Just REFL
matchPrimFun (PrimFDiv _)           (PrimFDiv _)           = Just REFL
matchPrimFun (PrimRecip _)          (PrimRecip _)          = Just REFL
matchPrimFun (PrimSin _)            (PrimSin _)            = Just REFL
matchPrimFun (PrimCos _)            (PrimCos _)            = Just REFL
matchPrimFun (PrimTan _)            (PrimTan _)            = Just REFL
matchPrimFun (PrimAsin _)           (PrimAsin _)           = Just REFL
matchPrimFun (PrimAcos _)           (PrimAcos _)           = Just REFL
matchPrimFun (PrimAtan _)           (PrimAtan _)           = Just REFL
matchPrimFun (PrimAsinh _)          (PrimAsinh _)          = Just REFL
matchPrimFun (PrimAcosh _)          (PrimAcosh _)          = Just REFL
matchPrimFun (PrimAtanh _)          (PrimAtanh _)          = Just REFL
matchPrimFun (PrimExpFloating _)    (PrimExpFloating _)    = Just REFL
matchPrimFun (PrimSqrt _)           (PrimSqrt _)           = Just REFL
matchPrimFun (PrimLog _)            (PrimLog _)            = Just REFL
matchPrimFun (PrimFPow _)           (PrimFPow _)           = Just REFL
matchPrimFun (PrimLogBase _)        (PrimLogBase _)        = Just REFL
matchPrimFun (PrimAtan2 _)          (PrimAtan2 _)          = Just REFL
matchPrimFun (PrimTruncate _ s)     (PrimTruncate _ t)     = matchIntegralType s t
matchPrimFun (PrimRound _ s)        (PrimRound _ t)        = matchIntegralType s t
matchPrimFun (PrimFloor _ s)        (PrimFloor _ t)        = matchIntegralType s t
matchPrimFun (PrimCeiling _ s)      (PrimCeiling _ t)      = matchIntegralType s t
matchPrimFun (PrimLt _)             (PrimLt _)             = Just REFL
matchPrimFun (PrimGt _)             (PrimGt _)             = Just REFL
matchPrimFun (PrimLtEq _)           (PrimLtEq _)           = Just REFL
matchPrimFun (PrimGtEq _)           (PrimGtEq _)           = Just REFL
matchPrimFun (PrimEq _)             (PrimEq _)             = Just REFL
matchPrimFun (PrimNEq _)            (PrimNEq _)            = Just REFL
matchPrimFun (PrimMax _)            (PrimMax _)            = Just REFL
matchPrimFun (PrimMin _)            (PrimMin _)            = Just REFL
matchPrimFun (PrimFromIntegral _ s) (PrimFromIntegral _ t) = matchNumType s t
matchPrimFun PrimLAnd               PrimLAnd               = Just REFL
matchPrimFun PrimLOr                PrimLOr                = Just REFL
matchPrimFun PrimLNot               PrimLNot               = Just REFL
matchPrimFun PrimOrd                PrimOrd                = Just REFL
matchPrimFun PrimChr                PrimChr                = Just REFL
matchPrimFun PrimBoolToInt          PrimBoolToInt          = Just REFL
matchPrimFun _                      _                      = Nothing


-- Match reified types
--
matchTupleType :: TupleType s -> TupleType t -> Maybe (s :=: t)
matchTupleType UnitTuple         UnitTuple         = Just REFL
matchTupleType (SingleTuple s)   (SingleTuple t)   = matchScalarType s t
matchTupleType (PairTuple s1 s2) (PairTuple t1 t2)
  | Just REFL <- matchTupleType s1 t1
  , Just REFL <- matchTupleType s2 t2
  = Just REFL

matchTupleType _ _
  = Nothing


-- Match reified type dictionaries
--
matchScalarType :: ScalarType s -> ScalarType t -> Maybe (s :=: t)
matchScalarType (NumScalarType s)    (NumScalarType t)    = matchNumType s t
matchScalarType (NonNumScalarType s) (NonNumScalarType t) = matchNonNumType s t
matchScalarType _                    _                    = Nothing

matchNumType :: NumType s -> NumType t -> Maybe (s :=: t)
matchNumType (IntegralNumType s) (IntegralNumType t) = matchIntegralType s t
matchNumType (FloatingNumType s) (FloatingNumType t) = matchFloatingType s t
matchNumType _                   _                   = Nothing

matchBoundedType :: BoundedType s -> BoundedType t -> Maybe (s :=: t)
matchBoundedType (IntegralBoundedType s) (IntegralBoundedType t) = matchIntegralType s t
matchBoundedType (NonNumBoundedType s)   (NonNumBoundedType t)   = matchNonNumType s t
matchBoundedType _                       _                       = Nothing

matchIntegralType :: IntegralType s -> IntegralType t -> Maybe (s :=: t)
matchIntegralType (TypeInt _)     (TypeInt _)     = Just REFL
matchIntegralType (TypeInt8 _)    (TypeInt8 _)    = Just REFL
matchIntegralType (TypeInt16 _)   (TypeInt16 _)   = Just REFL
matchIntegralType (TypeInt32 _)   (TypeInt32 _)   = Just REFL
matchIntegralType (TypeInt64 _)   (TypeInt64 _)   = Just REFL
matchIntegralType (TypeWord _)    (TypeWord _)    = Just REFL
matchIntegralType (TypeWord8 _)   (TypeWord8 _)   = Just REFL
matchIntegralType (TypeWord16 _)  (TypeWord16 _)  = Just REFL
matchIntegralType (TypeWord32 _)  (TypeWord32 _)  = Just REFL
matchIntegralType (TypeWord64 _)  (TypeWord64 _)  = Just REFL
matchIntegralType (TypeCShort _)  (TypeCShort _)  = Just REFL
matchIntegralType (TypeCUShort _) (TypeCUShort _) = Just REFL
matchIntegralType (TypeCInt _)    (TypeCInt _)    = Just REFL
matchIntegralType (TypeCUInt _)   (TypeCUInt _)   = Just REFL
matchIntegralType (TypeCLong _)   (TypeCLong _)   = Just REFL
matchIntegralType (TypeCULong _)  (TypeCULong _)  = Just REFL
matchIntegralType (TypeCLLong _)  (TypeCLLong _)  = Just REFL
matchIntegralType (TypeCULLong _) (TypeCULLong _) = Just REFL
matchIntegralType _               _               = Nothing

matchFloatingType :: FloatingType s -> FloatingType t -> Maybe (s :=: t)
matchFloatingType (TypeFloat _)   (TypeFloat _)   = Just REFL
matchFloatingType (TypeDouble _)  (TypeDouble _)  = Just REFL
matchFloatingType (TypeCFloat _)  (TypeCFloat _)  = Just REFL
matchFloatingType (TypeCDouble _) (TypeCDouble _) = Just REFL
matchFloatingType _               _               = Nothing

matchNonNumType :: NonNumType s -> NonNumType t -> Maybe (s :=: t)
matchNonNumType (TypeBool _)   (TypeBool _)   = Just REFL
matchNonNumType (TypeChar _)   (TypeChar _)   = Just REFL
matchNonNumType (TypeCChar _)  (TypeCChar _)  = Just REFL
matchNonNumType (TypeCSChar _) (TypeCSChar _) = Just REFL
matchNonNumType (TypeCUChar _) (TypeCUChar _) = Just REFL
matchNonNumType _              _              = Nothing


-- Discriminate binary functions that commute, and if so return the operands in
-- a stable ordering such that matching recognises expressions modulo
-- commutativity.
--
commutes :: PrimFun (a -> r) -> OpenExp env aenv a -> Maybe (OpenExp env aenv a)
commutes f x = case f of
  PrimAdd _     -> Just (swizzle x)
  PrimMul _     -> Just (swizzle x)
  PrimBAnd _    -> Just (swizzle x)
  PrimBOr _     -> Just (swizzle x)
  PrimBXor _    -> Just (swizzle x)
  PrimEq _      -> Just (swizzle x)
  PrimNEq _     -> Just (swizzle x)
  PrimMax _     -> Just (swizzle x)
  PrimMin _     -> Just (swizzle x)
  PrimLAnd      -> Just (swizzle x)
  PrimLOr       -> Just (swizzle x)
  _             -> Nothing
  where
    swizzle :: OpenExp env aenv (a,a) -> OpenExp env aenv (a,a)
    swizzle exp
      | Tuple (NilTup `SnocTup` a `SnocTup` b)  <- exp
      , hashOpenExp a > hashOpenExp b           = Tuple (NilTup `SnocTup` b `SnocTup` a)
      --
      | otherwise                               = exp


-- Hashable scalar expressions
--
hashIdx :: Idx env t -> Int
hashIdx = hash . idxToInt

hashTupleIdx :: TupleIdx tup e -> Int
hashTupleIdx = hash . tupleIdxToInt


hashOpenExp :: forall env aenv e. OpenExp env aenv e -> Int
hashOpenExp (Let x e)                   = hash "Let"            `combine` hashOpenExp x  `combine` hashOpenExp e
hashOpenExp (Var ix)                    = hash "Var"            `combine` hashIdx ix
hashOpenExp (Const c)                   = hash "Const"          `hashWithSalt` show (Sugar.toElt c :: e)
hashOpenExp (Tuple t)                   = hash "Tuple"          `combine` hashTuple t
hashOpenExp (Prj ix e)                  = hash "Prj"            `combine` hashTupleIdx ix `combine` hashOpenExp e
hashOpenExp IndexAny                    = hash "IndexAny"
hashOpenExp IndexNil                    = hash "IndexNil"
hashOpenExp (IndexCons sl a)            = hash "IndexCons"      `combine` hashOpenExp sl `combine` hashOpenExp a
hashOpenExp (IndexHead sl)              = hash "IndexHead"      `combine` hashOpenExp sl
hashOpenExp (IndexTail sl)              = hash "IndexTail"      `combine` hashOpenExp sl
hashOpenExp (ToIndex sh i)              = hash "ToIndex"        `combine` hashOpenExp sh `combine` hashOpenExp i
hashOpenExp (FromIndex sh i)            = hash "FromIndex"      `combine` hashOpenExp sh `combine` hashOpenExp i
hashOpenExp (Cond c t e)                = hash "Cond"           `combine` hashOpenExp c  `combine` hashOpenExp t `combine` hashOpenExp e
hashOpenExp (PrimApp f x)               = hash "PrimApp"        `combine` hashPrimFun f  `combine` hashOpenExp (maybe x id (commutes f x))
hashOpenExp (PrimConst c)               = hash "PrimConst"      `combine` hashPrimConst c
hashOpenExp (IndexScalar a ix)
  | OpenAcc (Avar v) <- a               = hash "IndexScalar"    `combine` hashIdx v      `combine` hashOpenExp ix
  | otherwise                           = error "hash: IndexScalar: expected array variable"
--
hashOpenExp (Shape a)
  | OpenAcc (Avar v) <- a               = hash "Shape"          `combine` hashIdx v
  | otherwise                           = error "hash: Shape: expected array variable"
--
hashOpenExp (ShapeSize sh)              = hash "ShapeSize"      `combine` hashOpenExp sh
hashOpenExp (Intersect sa sb)           = hash "Intersect"      `combine` hashOpenExp sa `combine` hashOpenExp sb


hashTuple :: Tuple.Tuple (OpenExp env aenv) e -> Int
hashTuple NilTup                        = hash "NilTup"
hashTuple (SnocTup t e)                 = hash "SnocTup"        `combine` hashTuple t `combine` hashOpenExp e


hashPrimConst :: PrimConst c -> Int
hashPrimConst (PrimMinBound _)          = hash "PrimMinBound"
hashPrimConst (PrimMaxBound _)          = hash "PrimMaxBound"
hashPrimConst (PrimPi _)                = hash "PrimPi"

hashPrimFun :: PrimFun f -> Int
hashPrimFun (PrimAdd _)                 = hash "PrimAdd"
hashPrimFun (PrimSub _)                 = hash "PrimSub"
hashPrimFun (PrimMul _)                 = hash "PrimMul"
hashPrimFun (PrimNeg _)                 = hash "PrimNeg"
hashPrimFun (PrimAbs _)                 = hash "PrimAbs"
hashPrimFun (PrimSig _)                 = hash "PrimSig"
hashPrimFun (PrimQuot _)                = hash "PrimQuot"
hashPrimFun (PrimRem _)                 = hash "PrimRem"
hashPrimFun (PrimIDiv _)                = hash "PrimIDiv"
hashPrimFun (PrimMod _)                 = hash "PrimMod"
hashPrimFun (PrimBAnd _)                = hash "PrimBAnd"
hashPrimFun (PrimBOr _)                 = hash "PrimBOr"
hashPrimFun (PrimBXor _)                = hash "PrimBXor"
hashPrimFun (PrimBNot _)                = hash "PrimBNot"
hashPrimFun (PrimBShiftL _)             = hash "PrimBShiftL"
hashPrimFun (PrimBShiftR _)             = hash "PrimBShiftR"
hashPrimFun (PrimBRotateL _)            = hash "PrimBRotateL"
hashPrimFun (PrimBRotateR _)            = hash "PrimBRotateR"
hashPrimFun (PrimFDiv _)                = hash "PrimFDiv"
hashPrimFun (PrimRecip _)               = hash "PrimRecip"
hashPrimFun (PrimSin _)                 = hash "PrimSin"
hashPrimFun (PrimCos _)                 = hash "PrimCos"
hashPrimFun (PrimTan _)                 = hash "PrimTan"
hashPrimFun (PrimAsin _)                = hash "PrimAsin"
hashPrimFun (PrimAcos _)                = hash "PrimAcos"
hashPrimFun (PrimAtan _)                = hash "PrimAtan"
hashPrimFun (PrimAsinh _)               = hash "PrimAsinh"
hashPrimFun (PrimAcosh _)               = hash "PrimAcosh"
hashPrimFun (PrimAtanh _)               = hash "PrimAtanh"
hashPrimFun (PrimExpFloating _)         = hash "PrimExpFloating"
hashPrimFun (PrimSqrt _)                = hash "PrimSqrt"
hashPrimFun (PrimLog _)                 = hash "PrimLog"
hashPrimFun (PrimFPow _)                = hash "PrimFPow"
hashPrimFun (PrimLogBase _)             = hash "PrimLogBase"
hashPrimFun (PrimAtan2 _)               = hash "PrimAtan2"
hashPrimFun (PrimTruncate _ _)          = hash "PrimTruncate"
hashPrimFun (PrimRound _ _)             = hash "PrimRound"
hashPrimFun (PrimFloor _ _)             = hash "PrimFloor"
hashPrimFun (PrimCeiling _ _)           = hash "PrimCeiling"
hashPrimFun (PrimLt _)                  = hash "PrimLt"
hashPrimFun (PrimGt _)                  = hash "PrimGt"
hashPrimFun (PrimLtEq _)                = hash "PrimLtEq"
hashPrimFun (PrimGtEq _)                = hash "PrimGtEq"
hashPrimFun (PrimEq _)                  = hash "PrimEq"
hashPrimFun (PrimNEq _)                 = hash "PrimNEq"
hashPrimFun (PrimMax _)                 = hash "PrimMax"
hashPrimFun (PrimMin _)                 = hash "PrimMin"
hashPrimFun (PrimFromIntegral _ _)      = hash "PrimFromIntegral"
hashPrimFun PrimLAnd                    = hash "PrimLAnd"
hashPrimFun PrimLOr                     = hash "PrimLOr"
hashPrimFun PrimLNot                    = hash "PrimLNot"
hashPrimFun PrimOrd                     = hash "PrimOrd"
hashPrimFun PrimChr                     = hash "PrimChr"
hashPrimFun PrimBoolToInt               = hash "PrimBoolToInt"

