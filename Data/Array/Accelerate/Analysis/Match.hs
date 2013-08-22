{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Analysis.Match
-- Copyright   : [2012] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Analysis.Match (

  -- matching expressions
  MatchAcc,
  (:=:)(..),
  matchOpenAcc,  matchPreOpenAcc,
  matchOpenAfun, matchPreOpenAfun,
  matchOpenExp,  matchPreOpenExp,
  matchOpenFun,  matchPreOpenFun,
  matchPrimFun,  matchPrimFun',

  -- auxiliary
  matchIdx, matchTupleType,
  matchIntegralType, matchFloatingType, matchNumType, matchScalarType,

  -- hashing expressions
  HashAcc,
  hashPreOpenAcc, hashOpenAcc,
  hashPreOpenExp, hashOpenExp,
  hashPreOpenFun,

) where

-- standard library
import Prelude                                          hiding ( exp )
import Data.Maybe
import Data.Typeable
import Data.Hashable
import System.Mem.StableName
import System.IO.Unsafe                                 ( unsafePerformIO )

-- friends
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Array.Representation       ( SliceIndex(..) )
import Data.Array.Accelerate.Tuple                      hiding ( Tuple )
import qualified Data.Array.Accelerate.Tuple            as Tuple


-- Witness equality between types. A value of a :=: b is a proof that types a
-- and b are equal. By pattern matching on REFL this fact is introduced to the
-- type checker.
--
data s :=: t where
  REFL :: s :=: s

deriving instance Show (s :=: t)


-- The type of matching array computations
--
type MatchAcc acc = forall aenv s t. acc aenv s -> acc aenv t -> Maybe (s :=: t)


-- Compute the congruence of two array computations. The nodes are congruent if
-- they have the same operator and their operands are congruent.
--
matchOpenAcc :: OpenAcc aenv s -> OpenAcc aenv t -> Maybe (s :=: t)
matchOpenAcc (OpenAcc acc1) (OpenAcc acc2) =
  matchPreOpenAcc matchOpenAcc hashOpenAcc acc1 acc2


matchPreOpenAcc
    :: forall acc aenv s t.
       MatchAcc acc
    -> HashAcc  acc
    -> PreOpenAcc acc aenv s
    -> PreOpenAcc acc aenv t
    -> Maybe (s :=: t)
matchPreOpenAcc matchAcc hashAcc = match
  where
    matchFun :: PreOpenFun acc env' aenv' u -> PreOpenFun acc env' aenv' v -> Maybe (u :=: v)
    matchFun = matchPreOpenFun matchAcc hashAcc

    matchExp :: PreOpenExp acc env' aenv' u -> PreOpenExp acc env' aenv' v -> Maybe (u :=: v)
    matchExp = matchPreOpenExp matchAcc hashAcc

    match :: PreOpenAcc acc aenv s -> PreOpenAcc acc aenv t -> Maybe (s :=: t)
    match (Alet x1 a1) (Alet x2 a2)
      | Just REFL <- matchAcc x1 x2
      , Just REFL <- matchAcc a1 a2
      = Just REFL

    match (Avar v1) (Avar v2)
      = matchIdx v1 v2

    match (Atuple t1) (Atuple t2)
      | Just REFL <- matchAtuple matchAcc t1 t2
      = gcast REFL  -- surface/representation type

    match (Aprj ix1 t1) (Aprj ix2 t2)
      | Just REFL <- matchAcc t1 t2
      , Just REFL <- matchTupleIdx ix1 ix2
      = Just REFL

    match (Apply f1 a1) (Apply f2 a2)
      | Just REFL <- matchPreOpenAfun matchAcc f1 f2
      , Just REFL <- matchAcc                  a1 a2
      = Just REFL

    match (Aforeign ff1 _ a1) (Aforeign ff2 _ a2)
      | Just REFL <- matchAcc a1 a2
      , unsafePerformIO $ do
          sn1 <- makeStableName ff1
          sn2 <- makeStableName ff2
          return $! hashStableName sn1 == hashStableName sn2
      = gcast REFL

    match (Acond p1 t1 e1) (Acond p2 t2 e2)
      | Just REFL <- matchExp p1 p2
      , Just REFL <- matchAcc t1 t2
      , Just REFL <- matchAcc e1 e2
      = Just REFL

    match (Awhile p1 f1 a1) (Awhile p2 f2 a2)
      | Just REFL <- matchAcc a1 a2
      , Just REFL <- matchPreOpenAfun matchAcc p1 p2
      , Just REFL <- matchPreOpenAfun matchAcc f1 f2
      = Just REFL

    match (Use a1) (Use a2)
      | Just REFL <- matchArrays (arrays (undefined::s)) (arrays (undefined::t)) a1 a2
      = gcast REFL

    match (Unit e1) (Unit e2)
      | Just REFL <- matchExp e1 e2
      = Just REFL

    match (Reshape sh1 a1) (Reshape sh2 a2)
      | Just REFL <- matchExp sh1 sh2
      , Just REFL <- matchAcc a1  a2
      = Just REFL

    match (Generate sh1 f1) (Generate sh2 f2)
      | Just REFL <- matchExp sh1 sh2
      , Just REFL <- matchFun f1  f2
      = Just REFL

    match (Transform sh1 ix1 f1 a1) (Transform sh2 ix2 f2 a2)
      | Just REFL <- matchExp sh1 sh2
      , Just REFL <- matchFun ix1 ix2
      , Just REFL <- matchFun f1  f2
      , Just REFL <- matchAcc a1  a2
      = Just REFL

    match (Replicate _ ix1 a1) (Replicate _ ix2 a2)
      | Just REFL <- matchExp ix1 ix2
      , Just REFL <- matchAcc a1  a2
      = gcast REFL  -- slice specification ??

    match (Slice _ a1 ix1) (Slice _ a2 ix2)
      | Just REFL <- matchAcc a1  a2
      , Just REFL <- matchExp ix1 ix2
      = gcast REFL  -- slice specification ??

    match (Map f1 a1) (Map f2 a2)
      | Just REFL <- matchFun f1 f2
      , Just REFL <- matchAcc a1 a2
      = Just REFL

    match (ZipWith f1 a1 b1) (ZipWith f2 a2 b2)
      | Just REFL <- matchFun f1 f2
      , Just REFL <- matchAcc a1 a2
      , Just REFL <- matchAcc b1 b2
      = Just REFL

    match (Fold f1 z1 a1) (Fold f2 z2 a2)
      | Just REFL <- matchFun f1 f2
      , Just REFL <- matchExp z1 z2
      , Just REFL <- matchAcc a1 a2
      = Just REFL

    match (Fold1 f1 a1) (Fold1 f2 a2)
      | Just REFL <- matchFun f1 f2
      , Just REFL <- matchAcc a1 a2
      = Just REFL

    match (FoldSeg f1 z1 a1 s1) (FoldSeg f2 z2 a2 s2)
      | Just REFL <- matchFun f1 f2
      , Just REFL <- matchExp z1 z2
      , Just REFL <- matchAcc a1 a2
      , Just REFL <- matchAcc s1 s2
      = Just REFL

    match (Fold1Seg f1 a1 s1) (Fold1Seg f2 a2 s2)
      | Just REFL <- matchFun f1 f2
      , Just REFL <- matchAcc a1 a2
      , Just REFL <- matchAcc s1 s2
      = Just REFL

    match (Scanl f1 z1 a1) (Scanl f2 z2 a2)
      | Just REFL <- matchFun f1 f2
      , Just REFL <- matchExp z1 z2
      , Just REFL <- matchAcc a1 a2
      = Just REFL

    match (Scanl' f1 z1 a1) (Scanl' f2 z2 a2)
      | Just REFL <- matchFun f1 f2
      , Just REFL <- matchExp z1 z2
      , Just REFL <- matchAcc a1 a2
      = Just REFL

    match (Scanl1 f1 a1) (Scanl1 f2 a2)
      | Just REFL <- matchFun f1 f2
      , Just REFL <- matchAcc a1 a2
      = Just REFL

    match (Scanr f1 z1 a1) (Scanr f2 z2 a2)
      | Just REFL <- matchFun f1 f2
      , Just REFL <- matchExp z1 z2
      , Just REFL <- matchAcc a1 a2
      = Just REFL

    match (Scanr' f1 z1 a1) (Scanr' f2 z2 a2)
      | Just REFL <- matchFun f1 f2
      , Just REFL <- matchExp z1 z2
      , Just REFL <- matchAcc a1 a2
      = Just REFL

    match (Scanr1 f1 a1) (Scanr1 f2 a2)
      | Just REFL <- matchFun f1 f2
      , Just REFL <- matchAcc a1 a2
      = Just REFL

    match (Permute f1 d1 p1 a1) (Permute f2 d2 p2 a2)
      | Just REFL <- matchFun f1 f2
      , Just REFL <- matchAcc d1 d2
      , Just REFL <- matchFun p1 p2
      , Just REFL <- matchAcc a1 a2
      = Just REFL

    match (Backpermute sh1 ix1 a1) (Backpermute sh2 ix2 a2)
      | Just REFL <- matchExp sh1 sh2
      , Just REFL <- matchFun ix1 ix2
      , Just REFL <- matchAcc a1  a2
      = Just REFL

    match (Stencil f1 b1 (a1 :: acc aenv (Array sh1 e1)))
          (Stencil f2 b2 (a2 :: acc aenv (Array sh2 e2)))
      | Just REFL <- matchFun f1 f2
      , Just REFL <- matchAcc a1 a2
      , matchBoundary (eltType (undefined::e1)) b1 b2
      = Just REFL

    match (Stencil2 f1 b1  (a1  :: acc aenv (Array sh1  e1 )) b2  (a2 :: acc aenv (Array sh2  e2 )))
          (Stencil2 f2 b1' (a1' :: acc aenv (Array sh1' e1')) b2' (a2':: acc aenv (Array sh2' e2')))
      | Just REFL <- matchFun f1 f2
      , Just REFL <- matchAcc a1 a1'
      , Just REFL <- matchAcc a2 a2'
      , matchBoundary (eltType (undefined::e1)) b1 b1'
      , matchBoundary (eltType (undefined::e2)) b2 b2'
      = Just REFL

    match _ _
      = Nothing


-- Array tuples
--
matchAtuple
    :: MatchAcc acc
    -> Atuple (acc aenv) s
    -> Atuple (acc aenv) t
    -> Maybe (s :=: t)
matchAtuple matchAcc (SnocAtup t1 a1) (SnocAtup t2 a2)
  | Just REFL <- matchAtuple matchAcc t1 t2
  , Just REFL <- matchAcc             a1 a2
  = Just REFL

matchAtuple _ NilAtup NilAtup = Just REFL
matchAtuple _ _       _       = Nothing


-- Array functions
--
matchOpenAfun :: OpenAfun aenv s -> OpenAfun aenv t -> Maybe (s :=: t)
matchOpenAfun = matchPreOpenAfun matchOpenAcc

matchPreOpenAfun :: MatchAcc acc -> PreOpenAfun acc aenv s -> PreOpenAfun acc aenv t -> Maybe (s :=: t)
matchPreOpenAfun m (Alam s) (Alam t)
  | Just REFL <- matchEnvTop        s t
  , Just REFL <- matchPreOpenAfun m s t
  = Just REFL
  where
    matchEnvTop :: (Arrays s, Arrays t)
                => PreOpenAfun acc (aenv, s) f -> PreOpenAfun acc (aenv, t) g -> Maybe (s :=: t)
    matchEnvTop _ _ = gcast REFL  -- ???

matchPreOpenAfun m (Abody s) (Abody t) = m s t
matchPreOpenAfun _ _         _         = Nothing


-- Match stencil boundaries
--
matchBoundary :: TupleType e -> Boundary e -> Boundary e -> Bool
matchBoundary ty (Constant s) (Constant t) = matchConst ty s t
matchBoundary _  Wrap         Wrap         = True
matchBoundary _  Clamp        Clamp        = True
matchBoundary _  Mirror       Mirror       = True
matchBoundary _  _            _            = False


-- Match arrays
--
-- As a convenience, we are just comparing the stable names, but we could also
-- walk the structure comparing the underlying ptrsOfArrayData.
--
matchArrays :: ArraysR s -> ArraysR t -> s -> t -> Maybe (s :=: t)
matchArrays ArraysRunit ArraysRunit () ()
  = Just REFL

matchArrays (ArraysRpair a1 b1) (ArraysRpair a2 b2) (arr1,brr1) (arr2,brr2)
  | Just REFL <- matchArrays a1 a2 arr1 arr2
  , Just REFL <- matchArrays b1 b2 brr1 brr2
  = Just REFL

matchArrays ArraysRarray ArraysRarray (Array _ ad1) (Array _ ad2)
  | unsafePerformIO $ do
      sn1 <- makeStableName ad1
      sn2 <- makeStableName ad2
      return $! hashStableName sn1 == hashStableName sn2
  = gcast REFL

matchArrays _ _ _ _
  = Nothing


-- Compute the congruence of two scalar expressions. Two nodes are congruent if
-- either:
--
--  1. The nodes label constants and the contents are equal
--  2. They have the same operator and their operands are congruent
--
-- The below attempts to use real typed equality, but occasionally still needs
-- to use a cast, particularly when we can only match the representation types.
--
matchOpenExp :: OpenExp env aenv s -> OpenExp env aenv t -> Maybe (s :=: t)
matchOpenExp = matchPreOpenExp matchOpenAcc hashOpenAcc

matchPreOpenExp
    :: forall acc env aenv s t.
       MatchAcc acc
    -> HashAcc  acc
    -> PreOpenExp acc env aenv s
    -> PreOpenExp acc env aenv t
    -> Maybe (s :=: t)
matchPreOpenExp matchAcc hashAcc = match
  where
    match :: forall env' aenv' s' t'. PreOpenExp acc env' aenv' s' -> PreOpenExp acc env' aenv' t' -> Maybe (s' :=: t')
    match (Let x1 e1) (Let x2 e2)
      | Just REFL <- match x1 x2
      , Just REFL <- match e1 e2
      = Just REFL

    match (Var v1) (Var v2)
      = matchIdx v1 v2

    match (Foreign ff1 _ e1) (Foreign ff2 _ e2)
      | Just REFL <- match e1 e2
      , unsafePerformIO $ do
          sn1 <- makeStableName ff1
          sn2 <- makeStableName ff2
          return $! hashStableName sn1 == hashStableName sn2
      = gcast REFL

    match (Const c1) (Const c2)
      | Just REFL <- matchTupleType (eltType (undefined::s')) (eltType (undefined::t'))
      , matchConst (eltType (undefined::s')) c1 c2
      = gcast REFL  -- surface/representation type

    match (Tuple t1) (Tuple t2)
      | Just REFL <- matchTuple matchAcc hashAcc t1 t2
      = gcast REFL  -- surface/representation type

    match (Prj ix1 t1) (Prj ix2 t2)
      | Just REFL <- match         t1  t2
      , Just REFL <- matchTupleIdx ix1 ix2
      = Just REFL

    match IndexAny IndexAny
      = gcast REFL  -- ???

    match IndexNil IndexNil
      = Just REFL

    match (IndexCons sl1 a1) (IndexCons sl2 a2)
      | Just REFL <- match sl1 sl2
      , Just REFL <- match a1 a2
      = Just REFL

    match (IndexHead sl1) (IndexHead sl2)
      | Just REFL <- match sl1 sl2
      = Just REFL

    match (IndexTail sl1) (IndexTail sl2)
      | Just REFL <- match sl1 sl2
      = Just REFL

    match (IndexSlice sliceIndex1 ix1 sh1) (IndexSlice sliceIndex2 ix2 sh2)
      | Just REFL <- match ix1 ix2
      , Just REFL <- match sh1 sh2
      , Just REFL <- matchSliceRestrict sliceIndex1 sliceIndex2
      = gcast REFL  -- SliceIndex representation/surface type

    match (IndexFull sliceIndex1 ix1 sl1) (IndexFull sliceIndex2 ix2 sl2)
      | Just REFL <- match ix1 ix2
      , Just REFL <- match sl1 sl2
      , Just REFL <- matchSliceExtend sliceIndex1 sliceIndex2
      = gcast REFL  -- SliceIndex representation/surface type

    match (ToIndex sh1 i1) (ToIndex sh2 i2)
      | Just REFL <- match sh1 sh2
      , Just REFL <- match i1  i2
      = Just REFL

    match (FromIndex sh1 i1) (FromIndex sh2 i2)
      | Just REFL <- match i1  i2
      , Just REFL <- match sh1 sh2
      = Just REFL

    match (Cond p1 t1 e1) (Cond p2 t2 e2)
      | Just REFL <- match p1 p2
      , Just REFL <- match t1 t2
      , Just REFL <- match e1 e2
      = Just REFL

    match (While p1 f1 x1) (While p2 f2 x2)
      | Just REFL <- match x1 x2
      , Just REFL <- matchPreOpenFun matchAcc hashAcc p1 p2
      , Just REFL <- matchPreOpenFun matchAcc hashAcc f1 f2
      = Just REFL

    match (PrimConst c1) (PrimConst c2)
      = matchPrimConst c1 c2

    match (PrimApp f1 x1) (PrimApp f2 x2)
      | Just x1'  <- commutes hashAcc f1 x1
      , Just x2'  <- commutes hashAcc f2 x2
      , Just REFL <- match        x1' x2'
      , Just REFL <- matchPrimFun f1  f2
      = Just REFL

      | Just REFL <- match x1 x2
      , Just REFL <- matchPrimFun f1 f2
      = Just REFL

    match (Index a1 x1) (Index a2 x2)
      | Just REFL <- matchAcc a1 a2     -- should only be array indices
      , Just REFL <- match    x1 x2
      = Just REFL

    match (LinearIndex a1 x1) (LinearIndex a2 x2)
      | Just REFL <- matchAcc a1 a2
      , Just REFL <- match    x1 x2
      = Just REFL

    match (Shape a1) (Shape a2)
      | Just REFL <- matchAcc a1 a2     -- should only be array indices
      = Just REFL

    match (ShapeSize sh1) (ShapeSize sh2)
      | Just REFL <- match sh1 sh2
      = Just REFL

    match (Intersect sa1 sb1) (Intersect sa2 sb2)
      | Just REFL <- match sa1 sa2
      , Just REFL <- match sb1 sb2
      = Just REFL

    match _ _
      = Nothing


-- Match scalar functions
--
matchOpenFun :: OpenFun env aenv s -> OpenFun env aenv t -> Maybe (s :=: t)
matchOpenFun = matchPreOpenFun matchOpenAcc hashOpenAcc

matchPreOpenFun
    :: MatchAcc acc
    -> HashAcc  acc
    -> PreOpenFun acc env aenv s
    -> PreOpenFun acc env aenv t
    -> Maybe (s :=: t)
matchPreOpenFun m h (Lam s) (Lam t)
  | Just REFL <- matchEnvTop         s t
  , Just REFL <- matchPreOpenFun m h s t
  = Just REFL
  where
    matchEnvTop :: (Elt s, Elt t) => PreOpenFun acc (env, s) aenv f -> PreOpenFun acc (env, t) aenv g -> Maybe (s :=: t)
    matchEnvTop _ _ = gcast REFL  -- ???

matchPreOpenFun m h (Body s) (Body t) = matchPreOpenExp m h s t
matchPreOpenFun _ _ _        _        = Nothing

-- Matching constants
--
matchConst :: TupleType a -> a -> a -> Bool
matchConst UnitTuple         ()      ()      = True
matchConst (SingleTuple ty)  a       b       = evalEq ty (a,b)
matchConst (PairTuple ta tb) (a1,b1) (a2,b2) = matchConst ta a1 a2 && matchConst tb b1 b2

evalEq :: ScalarType a -> (a, a) -> Bool
evalEq (NumScalarType (IntegralNumType ty)) | IntegralDict <- integralDict ty = uncurry (==)
evalEq (NumScalarType (FloatingNumType ty)) | FloatingDict <- floatingDict ty = uncurry (==)
evalEq (NonNumScalarType ty)                | NonNumDict   <- nonNumDict ty   = uncurry (==)


-- Environment projection indices
--
matchIdx :: Idx env s -> Idx env t -> Maybe (s :=: t)
matchIdx ZeroIdx     ZeroIdx     = Just REFL
matchIdx (SuccIdx u) (SuccIdx v) = matchIdx u v
matchIdx _           _           = Nothing


-- Tuple projection indices. Given the same tuple expression structure (tup),
-- check that the indices project identical elements.
--
matchTupleIdx :: TupleIdx tup s -> TupleIdx tup t -> Maybe (s :=: t)
matchTupleIdx ZeroTupIdx     ZeroTupIdx     = Just REFL
matchTupleIdx (SuccTupIdx s) (SuccTupIdx t) = matchTupleIdx s t
matchTupleIdx _              _              = Nothing

-- Tuples
--
matchTuple
    :: MatchAcc acc
    -> HashAcc  acc
    -> Tuple.Tuple (PreOpenExp acc env aenv) s
    -> Tuple.Tuple (PreOpenExp acc env aenv) t
    -> Maybe (s :=: t)
matchTuple _ _ NilTup          NilTup           = Just REFL
matchTuple m h (SnocTup t1 e1) (SnocTup t2 e2)
  | Just REFL <- matchTuple      m h t1 t2
  , Just REFL <- matchPreOpenExp m h e1 e2
  = Just REFL

matchTuple _ _ _               _                = Nothing


-- Slice specifications
--
matchSliceRestrict
    :: SliceIndex slix s co  sh
    -> SliceIndex slix t co' sh
    -> Maybe (s :=: t)
matchSliceRestrict SliceNil SliceNil
  = Just REFL

matchSliceRestrict (SliceAll   sl1) (SliceAll   sl2)
  | Just REFL <- matchSliceRestrict sl1 sl2
  = Just REFL

matchSliceRestrict (SliceFixed sl1) (SliceFixed sl2)
  | Just REFL <- matchSliceRestrict sl1 sl2
  = Just REFL

matchSliceRestrict _ _
  = Nothing


matchSliceExtend
    :: SliceIndex slix sl co  s
    -> SliceIndex slix sl co' t
    -> Maybe (s :=: t)
matchSliceExtend SliceNil SliceNil
  = Just REFL

matchSliceExtend (SliceAll   sl1) (SliceAll   sl2)
  | Just REFL <- matchSliceExtend sl1 sl2
  = Just REFL

matchSliceExtend (SliceFixed sl1) (SliceFixed sl2)
  | Just REFL <- matchSliceExtend sl1 sl2
  = Just REFL

matchSliceExtend _ _
  = Nothing


-- Primitive constants and functions
--
matchPrimConst :: (Elt s, Elt t) => PrimConst s -> PrimConst t -> Maybe (s :=: t)
matchPrimConst (PrimMinBound s) (PrimMinBound t) = matchBoundedType s t
matchPrimConst (PrimMaxBound s) (PrimMaxBound t) = matchBoundedType s t
matchPrimConst (PrimPi s)       (PrimPi t)       = matchFloatingType s t
matchPrimConst _                _                = Nothing


-- Covariant function matching
--
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


-- Contravariant function matching
--
matchPrimFun' :: (Elt s, Elt t) => PrimFun (s -> a) -> PrimFun (t -> a) -> Maybe (s :=: t)
matchPrimFun' (PrimAdd _)            (PrimAdd _)            = Just REFL
matchPrimFun' (PrimSub _)            (PrimSub _)            = Just REFL
matchPrimFun' (PrimMul _)            (PrimMul _)            = Just REFL
matchPrimFun' (PrimNeg _)            (PrimNeg _)            = Just REFL
matchPrimFun' (PrimAbs _)            (PrimAbs _)            = Just REFL
matchPrimFun' (PrimSig _)            (PrimSig _)            = Just REFL
matchPrimFun' (PrimQuot _)           (PrimQuot _)           = Just REFL
matchPrimFun' (PrimRem _)            (PrimRem _)            = Just REFL
matchPrimFun' (PrimIDiv _)           (PrimIDiv _)           = Just REFL
matchPrimFun' (PrimMod _)            (PrimMod _)            = Just REFL
matchPrimFun' (PrimBAnd _)           (PrimBAnd _)           = Just REFL
matchPrimFun' (PrimBOr _)            (PrimBOr _)            = Just REFL
matchPrimFun' (PrimBXor _)           (PrimBXor _)           = Just REFL
matchPrimFun' (PrimBNot _)           (PrimBNot _)           = Just REFL
matchPrimFun' (PrimBShiftL _)        (PrimBShiftL _)        = Just REFL
matchPrimFun' (PrimBShiftR _)        (PrimBShiftR _)        = Just REFL
matchPrimFun' (PrimBRotateL _)       (PrimBRotateL _)       = Just REFL
matchPrimFun' (PrimBRotateR _)       (PrimBRotateR _)       = Just REFL
matchPrimFun' (PrimFDiv _)           (PrimFDiv _)           = Just REFL
matchPrimFun' (PrimRecip _)          (PrimRecip _)          = Just REFL
matchPrimFun' (PrimSin _)            (PrimSin _)            = Just REFL
matchPrimFun' (PrimCos _)            (PrimCos _)            = Just REFL
matchPrimFun' (PrimTan _)            (PrimTan _)            = Just REFL
matchPrimFun' (PrimAsin _)           (PrimAsin _)           = Just REFL
matchPrimFun' (PrimAcos _)           (PrimAcos _)           = Just REFL
matchPrimFun' (PrimAtan _)           (PrimAtan _)           = Just REFL
matchPrimFun' (PrimAsinh _)          (PrimAsinh _)          = Just REFL
matchPrimFun' (PrimAcosh _)          (PrimAcosh _)          = Just REFL
matchPrimFun' (PrimAtanh _)          (PrimAtanh _)          = Just REFL
matchPrimFun' (PrimExpFloating _)    (PrimExpFloating _)    = Just REFL
matchPrimFun' (PrimSqrt _)           (PrimSqrt _)           = Just REFL
matchPrimFun' (PrimLog _)            (PrimLog _)            = Just REFL
matchPrimFun' (PrimFPow _)           (PrimFPow _)           = Just REFL
matchPrimFun' (PrimLogBase _)        (PrimLogBase _)        = Just REFL
matchPrimFun' (PrimAtan2 _)          (PrimAtan2 _)          = Just REFL
matchPrimFun' (PrimTruncate s _)     (PrimTruncate t _)     = matchFloatingType s t
matchPrimFun' (PrimRound s _)        (PrimRound t _)        = matchFloatingType s t
matchPrimFun' (PrimFloor s _)        (PrimFloor t _)        = matchFloatingType s t
matchPrimFun' (PrimCeiling s _)      (PrimCeiling t _)      = matchFloatingType s t
matchPrimFun' (PrimMax _)            (PrimMax _)            = Just REFL
matchPrimFun' (PrimMin _)            (PrimMin _)            = Just REFL
matchPrimFun' (PrimFromIntegral s _) (PrimFromIntegral t _) = matchIntegralType s t
matchPrimFun' PrimLAnd               PrimLAnd               = Just REFL
matchPrimFun' PrimLOr                PrimLOr                = Just REFL
matchPrimFun' PrimLNot               PrimLNot               = Just REFL
matchPrimFun' PrimOrd                PrimOrd                = Just REFL
matchPrimFun' PrimChr                PrimChr                = Just REFL
matchPrimFun' PrimBoolToInt          PrimBoolToInt          = Just REFL
matchPrimFun' (PrimLt s) (PrimLt t)
  | Just REFL <- matchScalarType s t
  = Just REFL

matchPrimFun' (PrimGt s) (PrimGt t)
  | Just REFL <- matchScalarType s t
  = Just REFL

matchPrimFun' (PrimLtEq s) (PrimLtEq t)
  | Just REFL <- matchScalarType s t
  = Just REFL

matchPrimFun' (PrimGtEq s) (PrimGtEq t)
  | Just REFL <- matchScalarType s t
  = Just REFL

matchPrimFun' (PrimEq s) (PrimEq t)
  | Just REFL <- matchScalarType s t
  = Just REFL

matchPrimFun' (PrimNEq s) (PrimNEq t)
  | Just REFL <- matchScalarType s t
  = Just REFL

matchPrimFun' _ _
  = Nothing


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
commutes
    :: forall acc env aenv a r.
       HashAcc acc
    -> PrimFun (a -> r)
    -> PreOpenExp acc env aenv a
    -> Maybe (PreOpenExp acc env aenv a)
commutes h f x = case f of
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
    swizzle :: PreOpenExp acc env aenv (a',a') -> PreOpenExp acc env aenv (a',a')
    swizzle exp
      | Tuple (NilTup `SnocTup` a `SnocTup` b)  <- exp
      , hashPreOpenExp h a > hashPreOpenExp h b = Tuple (NilTup `SnocTup` b `SnocTup` a)
      --
      | otherwise                               = exp


-- Hashing
-- =======

hashIdx :: Idx env t -> Int
hashIdx = hash . idxToInt

hashTupleIdx :: TupleIdx tup e -> Int
hashTupleIdx = hash . tupleIdxToInt


-- Array computations
-- ------------------

type HashAcc acc = forall aenv a. acc aenv a -> Int


hashOpenAcc :: OpenAcc aenv arrs -> Int
hashOpenAcc (OpenAcc pacc) = hashPreOpenAcc hashOpenAcc pacc

hashPreOpenAcc :: forall acc aenv arrs. HashAcc acc -> PreOpenAcc acc aenv arrs -> Int
hashPreOpenAcc hashAcc pacc =
  let
    hashA :: Int -> acc aenv' a -> Int
    hashA salt = hashWithSalt salt . hashAcc

    hashE :: Int -> PreOpenExp acc env' aenv' e -> Int
    hashE salt = hashWithSalt salt . hashPreOpenExp hashAcc

    hashF :: Int -> PreOpenFun acc env' aenv' f -> Int
    hashF salt = hashWithSalt salt . hashPreOpenFun hashAcc

  in case pacc of
    Alet bnd body               -> hash "Alet"          `hashA` bnd `hashA` body
    Avar v                      -> hash "Avar"          `hashWithSalt` hashIdx v
    Atuple t                    -> hash "Atuple"        `hashWithSalt` hashAtuple hashAcc t
    Aprj ix a                   -> hash "Aprj"          `hashWithSalt` hashTupleIdx ix    `hashA` a
    Apply f a                   -> hash "Apply"         `hashWithSalt` hashAfun hashAcc f `hashA` a
    Aforeign _ f a              -> hash "Aforeign"      `hashWithSalt` hashAfun hashAcc f `hashA` a
    Use a                       -> hash "Use"           `hashWithSalt` hashArrays (arrays (undefined::arrs)) a
    Awhile p f a                -> hash "Awhile"        `hashWithSalt` hashAfun hashAcc f `hashWithSalt` hashAfun hashAcc p `hashA` a
    Unit e                      -> hash "Unit"          `hashE` e
    Generate e f                -> hash "Generate"      `hashE` e  `hashF` f
    Acond e a1 a2               -> hash "Acond"         `hashE` e  `hashA` a1 `hashA` a2
    Reshape sh a                -> hash "Reshape"       `hashE` sh `hashA` a
    Transform sh f1 f2 a        -> hash "Transform"     `hashE` sh `hashF` f1 `hashF` f2 `hashA` a
    Replicate spec ix a         -> hash "Replicate"     `hashE` ix `hashA` a  `hashWithSalt` show spec
    Slice spec a ix             -> hash "Slice"         `hashE` ix `hashA` a  `hashWithSalt` show spec
    Map f a                     -> hash "Map"           `hashF` f  `hashA` a
    ZipWith f a1 a2             -> hash "ZipWith"       `hashF` f  `hashA` a1 `hashA` a2
    Fold f e a                  -> hash "Fold"          `hashF` f  `hashE` e  `hashA` a
    Fold1 f a                   -> hash "Fold1"         `hashF` f  `hashA` a
    FoldSeg f e a s             -> hash "FoldSeg"       `hashF` f  `hashE` e  `hashA` a  `hashA` s
    Fold1Seg f a s              -> hash "Fold1Seg"      `hashF` f  `hashA` a  `hashA` s
    Scanl f e a                 -> hash "Scanl"         `hashF` f  `hashE` e  `hashA` a
    Scanl' f e a                -> hash "Scanl'"        `hashF` f  `hashE` e  `hashA` a
    Scanl1 f a                  -> hash "Scanl1"        `hashF` f  `hashA` a
    Scanr f e a                 -> hash "Scanr"         `hashF` f  `hashE` e  `hashA` a
    Scanr' f e a                -> hash "Scanr'"        `hashF` f  `hashE` e  `hashA` a
    Scanr1 f a                  -> hash "Scanr1"        `hashF` f  `hashA` a
    Backpermute sh f a          -> hash "Backpermute"   `hashF` f  `hashE` sh `hashA` a
    Permute f1 a1 f2 a2         -> hash "Permute"       `hashF` f1 `hashA` a1 `hashF` f2 `hashA` a2
    Stencil f b a               -> hash "Stencil"       `hashF` f  `hashA` a             `hashWithSalt` hashBoundary a  b
    Stencil2 f b1 a1 b2 a2      -> hash "Stencil2"      `hashF` f  `hashA` a1 `hashA` a2 `hashWithSalt` hashBoundary a1 b1 `hashWithSalt` hashBoundary a2 b2


hashArrays :: ArraysR a -> a -> Int
hashArrays ArraysRunit         ()       = hash ()
hashArrays (ArraysRpair r1 r2) (a1, a2) = hash ( hashArrays r1 a1, hashArrays r2 a2)
hashArrays ArraysRarray        ad       = unsafePerformIO $! hashStableName `fmap` makeStableName ad

hashAtuple :: HashAcc acc -> Tuple.Atuple (acc aenv) a -> Int
hashAtuple _ NilAtup            = hash "NilAtup"
hashAtuple h (SnocAtup t a)     = hash "SnocAtup"       `hashWithSalt` hashAtuple h t `hashWithSalt` h a

hashAfun :: HashAcc acc -> PreOpenAfun acc aenv f -> Int
hashAfun h (Abody b)            = hash "Abody"          `hashWithSalt` h b
hashAfun h (Alam f)             = hash "Alam"           `hashWithSalt` hashAfun h f

hashBoundary :: forall acc aenv sh e. Elt e => acc aenv (Array sh e) -> Boundary (EltRepr e) -> Int
hashBoundary _ Wrap             = hash "Wrap"
hashBoundary _ Clamp            = hash "Clamp"
hashBoundary _ Mirror           = hash "Mirror"
hashBoundary _ (Constant c)     = hash "Constant"       `hashWithSalt` show (toElt c :: e)


-- Scalar expressions
-- ------------------

hashOpenExp :: OpenExp env aenv exp -> Int
hashOpenExp = hashPreOpenExp hashOpenAcc

hashPreOpenExp :: forall acc env aenv exp. HashAcc acc -> PreOpenExp acc env aenv exp -> Int
hashPreOpenExp hashAcc exp =
  let
    hashA :: Int -> acc aenv' a -> Int
    hashA salt = hashWithSalt salt . hashAcc

    hashE :: Int -> PreOpenExp acc env' aenv' e -> Int
    hashE salt = hashWithSalt salt . hashPreOpenExp hashAcc

  in case exp of
    Let bnd body                -> hash "Let"           `hashE` bnd `hashE` body
    Var ix                      -> hash "Var"           `hashWithSalt` hashIdx ix
    Const c                     -> hash "Const"         `hashWithSalt` show (toElt c :: exp)
    Tuple t                     -> hash "Tuple"         `hashWithSalt` hashTuple hashAcc t
    Prj i e                     -> hash "Prj"           `hashWithSalt` hashTupleIdx i `hashE` e
    IndexAny                    -> hash "IndexAny"
    IndexNil                    -> hash "IndexNil"
    IndexCons sl a              -> hash "IndexCons"     `hashE` sl `hashE` a
    IndexHead sl                -> hash "IndexHead"     `hashE` sl
    IndexTail sl                -> hash "IndexTail"     `hashE` sl
    IndexSlice spec ix sh       -> hash "IndexSlice"    `hashE` ix `hashE` sh `hashWithSalt` show spec
    IndexFull  spec ix sl       -> hash "IndexFull"     `hashE` ix `hashE` sl `hashWithSalt` show spec
    ToIndex sh i                -> hash "ToIndex"       `hashE` sh `hashE` i
    FromIndex sh i              -> hash "FromIndex"     `hashE` sh `hashE` i
    Cond c t e                  -> hash "Cond"          `hashE` c  `hashE` t  `hashE` e
    While p f x                 -> hash "While"         `hashWithSalt` hashPreOpenFun hashAcc p  `hashWithSalt` hashPreOpenFun hashAcc f  `hashE` x
    PrimApp f x                 -> hash "PrimApp"       `hashWithSalt` hashPrimFun f `hashE` fromMaybe x (commutes hashAcc f x)
    PrimConst c                 -> hash "PrimConst"     `hashWithSalt` hashPrimConst c
    Index a ix                  -> hash "Index"         `hashA` a  `hashE` ix
    LinearIndex a ix            -> hash "LinearIndex"   `hashA` a  `hashE` ix
    Shape a                     -> hash "Shape"         `hashA` a
    ShapeSize sh                -> hash "ShapeSize"     `hashE` sh
    Intersect sa sb             -> hash "Intersect"     `hashE` sa `hashE` sb
    Foreign _ f e               -> hash "Foreign"       `hashWithSalt` hashPreOpenFun hashAcc f `hashE` e


hashPreOpenFun :: HashAcc acc -> PreOpenFun acc env aenv f -> Int
hashPreOpenFun h (Body e)       = hash "Body"           `hashWithSalt` hashPreOpenExp h e
hashPreOpenFun h (Lam f)        = hash "Lam"            `hashWithSalt` hashPreOpenFun h f

hashTuple :: HashAcc acc -> Tuple.Tuple (PreOpenExp acc env aenv) e -> Int
hashTuple _ NilTup              = hash "NilTup"
hashTuple h (SnocTup t e)       = hash "SnocTup"        `hashWithSalt` hashTuple h t `hashWithSalt` hashPreOpenExp h e


hashPrimConst :: PrimConst c -> Int
hashPrimConst (PrimMinBound _)  = hash "PrimMinBound"
hashPrimConst (PrimMaxBound _)  = hash "PrimMaxBound"
hashPrimConst (PrimPi _)        = hash "PrimPi"

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

