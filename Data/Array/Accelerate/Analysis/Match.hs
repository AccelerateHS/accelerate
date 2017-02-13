{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Analysis.Match
-- Copyright   : [2012..2014] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Analysis.Match (

  -- matching expressions
  MatchAcc,
  (:~:)(..),
  matchOpenAcc,  matchPreOpenAcc,
  matchOpenAfun, matchPreOpenAfun,
  matchOpenExp,  matchPreOpenExp,
  matchOpenFun,  matchPreOpenFun,
  matchPrimFun,  matchPrimFun',
  matchOpenSeq,

  -- auxiliary
  matchIdx, matchTupleType,
  matchIntegralType, matchFloatingType, matchNumType, matchScalarType,
  matchSource,

  -- hashing expressions
  HashAcc,
  hashPreOpenAcc, hashOpenAcc,
  hashPreOpenExp, hashOpenExp,
  hashPreOpenFun,

) where

-- standard library
import Prelude                                          hiding ( exp )
import Control.Applicative                              ( liftA2 )
import Control.Monad                                    ( join )
import Data.Maybe
import Data.Typeable
import Data.Hashable
import System.Mem.StableName
import System.IO.Unsafe                                 ( unsafePerformIO )

#if __GLASGOW_HASKELL__ <= 708
import Control.Applicative                               ( (<$>), (<*>) )
#endif

-- friends
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Array.Representation       ( SliceIndex(..) )
import Data.Array.Accelerate.Product


-- The type of matching array computations
--
type MatchAcc acc = forall aenv s t. acc aenv s -> acc aenv t -> Maybe (s :~: t)


-- Compute the congruence of two array computations. The nodes are congruent if
-- they have the same operator and their operands are congruent.
--
{-# INLINEABLE matchOpenAcc #-}
matchOpenAcc
    :: OpenAcc aenv s
    -> OpenAcc aenv t
    -> Maybe (s :~: t)
matchOpenAcc (OpenAcc acc1) (OpenAcc acc2) =
  matchPreOpenAcc matchOpenAcc hashOpenAcc acc1 acc2

matchPreOpenAcc
    :: forall acc aenv s t.
       MatchAcc acc
    -> HashAcc  acc
    -> PreOpenAcc acc aenv s
    -> PreOpenAcc acc aenv t
    -> Maybe (s :~: t)
matchPreOpenAcc matchAcc hashAcc = match
  where
    matchFun :: PreOpenFun acc env' aenv' u -> PreOpenFun acc env' aenv' v -> Maybe (u :~: v)
    matchFun = matchPreOpenFun matchAcc hashAcc

    matchExp :: PreOpenExp acc env' aenv' u -> PreOpenExp acc env' aenv' v -> Maybe (u :~: v)
    matchExp = matchPreOpenExp matchAcc hashAcc

    match :: PreOpenAcc acc aenv s -> PreOpenAcc acc aenv t -> Maybe (s :~: t)
    match (Alet x1 a1) (Alet x2 a2)
      | Just Refl <- matchAcc x1 x2
      , Just Refl <- matchAcc a1 a2
      = Just Refl

    match (Avar v1) (Avar v2)
      = matchIdx v1 v2

    match (Atuple t1) (Atuple t2)
      | Just Refl <- matchAtuple matchAcc t1 t2
      = gcast Refl  -- surface/representation type

    match (Aprj ix1 t1) (Aprj ix2 t2)
      | Just Refl <- matchAcc t1 t2
      , Just Refl <- matchTupleIdx ix1 ix2
      = Just Refl

    match (Apply f1 a1) (Apply f2 a2)
      | Just Refl <- matchPreOpenAfun matchAcc f1 f2
      , Just Refl <- matchAcc                  a1 a2
      = Just Refl

    match (Aforeign ff1 _ a1) (Aforeign ff2 _ a2)
      | Just Refl <- matchAcc a1 a2
      , unsafePerformIO $ do
          sn1 <- makeStableName ff1
          sn2 <- makeStableName ff2
          return $! hashStableName sn1 == hashStableName sn2
      = gcast Refl

    match (Acond p1 t1 e1) (Acond p2 t2 e2)
      | Just Refl <- matchExp p1 p2
      , Just Refl <- matchAcc t1 t2
      , Just Refl <- matchAcc e1 e2
      = Just Refl

    match (Awhile p1 f1 a1) (Awhile p2 f2 a2)
      | Just Refl <- matchAcc a1 a2
      , Just Refl <- matchPreOpenAfun matchAcc p1 p2
      , Just Refl <- matchPreOpenAfun matchAcc f1 f2
      = Just Refl

    match (Use a1) (Use a2)
      | Just Refl <- matchArrays (arrays (undefined::s)) (arrays (undefined::t)) a1 a2
      = gcast Refl

    match (Subarray ix1 sh1 a1) (Subarray ix2 sh2 a2)
      | Just Refl <- matchExp ix1 ix2
      , Just Refl <- matchExp sh1 sh2
      , Just Refl <- matchArrays ArraysRarray ArraysRarray a1 a2
      = gcast Refl

    match (Unit e1) (Unit e2)
      | Just Refl <- matchExp e1 e2
      = Just Refl

    match (Reshape sh1 a1) (Reshape sh2 a2)
      | Just Refl <- matchExp sh1 sh2
      , Just Refl <- matchAcc a1  a2
      = Just Refl

    match (Generate sh1 f1) (Generate sh2 f2)
      | Just Refl <- matchExp sh1 sh2
      , Just Refl <- matchFun f1  f2
      = Just Refl

    match (Transform sh1 ix1 f1 a1) (Transform sh2 ix2 f2 a2)
      | Just Refl <- matchExp sh1 sh2
      , Just Refl <- matchFun ix1 ix2
      , Just Refl <- matchFun f1  f2
      , Just Refl <- matchAcc a1  a2
      = Just Refl

    match (Replicate _ ix1 a1) (Replicate _ ix2 a2)
      | Just Refl <- matchExp ix1 ix2
      , Just Refl <- matchAcc a1  a2
      = gcast Refl  -- slice specification ??

    match (Slice _ a1 ix1) (Slice _ a2 ix2)
      | Just Refl <- matchAcc a1  a2
      , Just Refl <- matchExp ix1 ix2
      = gcast Refl  -- slice specification ??

    match (Map f1 a1) (Map f2 a2)
      | Just Refl <- matchFun f1 f2
      , Just Refl <- matchAcc a1 a2
      = Just Refl

    match (ZipWith f1 a1 b1) (ZipWith f2 a2 b2)
      | Just Refl <- matchFun f1 f2
      , Just Refl <- matchAcc a1 a2
      , Just Refl <- matchAcc b1 b2
      = Just Refl

    match (Fold f1 z1 a1) (Fold f2 z2 a2)
      | Just Refl <- matchFun f1 f2
      , Just Refl <- matchExp z1 z2
      , Just Refl <- matchAcc a1 a2
      = Just Refl

    match (Fold1 f1 a1) (Fold1 f2 a2)
      | Just Refl <- matchFun f1 f2
      , Just Refl <- matchAcc a1 a2
      = Just Refl

    match (FoldSeg f1 z1 a1 s1) (FoldSeg f2 z2 a2 s2)
      | Just Refl <- matchFun f1 f2
      , Just Refl <- matchExp z1 z2
      , Just Refl <- matchAcc a1 a2
      , Just Refl <- matchAcc s1 s2
      = Just Refl

    match (Fold1Seg f1 a1 s1) (Fold1Seg f2 a2 s2)
      | Just Refl <- matchFun f1 f2
      , Just Refl <- matchAcc a1 a2
      , Just Refl <- matchAcc s1 s2
      = Just Refl

    match (Scanl f1 z1 a1) (Scanl f2 z2 a2)
      | Just Refl <- matchFun f1 f2
      , Just Refl <- matchExp z1 z2
      , Just Refl <- matchAcc a1 a2
      = Just Refl

    match (Scanl' f1 z1 a1) (Scanl' f2 z2 a2)
      | Just Refl <- matchFun f1 f2
      , Just Refl <- matchExp z1 z2
      , Just Refl <- matchAcc a1 a2
      = Just Refl

    match (Scanl1 f1 a1) (Scanl1 f2 a2)
      | Just Refl <- matchFun f1 f2
      , Just Refl <- matchAcc a1 a2
      = Just Refl

    match (Scanr f1 z1 a1) (Scanr f2 z2 a2)
      | Just Refl <- matchFun f1 f2
      , Just Refl <- matchExp z1 z2
      , Just Refl <- matchAcc a1 a2
      = Just Refl

    match (Scanr' f1 z1 a1) (Scanr' f2 z2 a2)
      | Just Refl <- matchFun f1 f2
      , Just Refl <- matchExp z1 z2
      , Just Refl <- matchAcc a1 a2
      = Just Refl

    match (Scanr1 f1 a1) (Scanr1 f2 a2)
      | Just Refl <- matchFun f1 f2
      , Just Refl <- matchAcc a1 a2
      = Just Refl

    match (Permute f1 d1 p1 a1) (Permute f2 d2 p2 a2)
      | Just Refl <- matchFun f1 f2
      , Just Refl <- matchAcc d1 d2
      , Just Refl <- matchFun p1 p2
      , Just Refl <- matchAcc a1 a2
      = Just Refl

    match (Backpermute sh1 ix1 a1) (Backpermute sh2 ix2 a2)
      | Just Refl <- matchExp sh1 sh2
      , Just Refl <- matchFun ix1 ix2
      , Just Refl <- matchAcc a1  a2
      = Just Refl

    match (Stencil f1 b1 (a1 :: acc aenv (Array sh1 e1)))
          (Stencil f2 b2 (a2 :: acc aenv (Array sh2 e2)))
      | Just Refl <- matchFun f1 f2
      , Just Refl <- matchAcc a1 a2
      , matchBoundary (eltType (undefined::e1)) b1 b2
      = Just Refl

    match (Stencil2 f1 b1  (a1  :: acc aenv (Array sh1  e1 )) b2  (a2 :: acc aenv (Array sh2  e2 )))
          (Stencil2 f2 b1' (a1' :: acc aenv (Array sh1' e1')) b2' (a2':: acc aenv (Array sh2' e2')))
      | Just Refl <- matchFun f1 f2
      , Just Refl <- matchAcc a1 a1'
      , Just Refl <- matchAcc a2 a2'
      , matchBoundary (eltType (undefined::e1)) b1 b1'
      , matchBoundary (eltType (undefined::e2)) b2 b2'
      = Just Refl

    match (Collect min1 max1 i1 s1) (Collect min2 max2 i2 s2)
      | Just Refl <- matchExp min1 min2
      , Just Refl <- join $ liftA2 matchExp max1 max2
      , Just Refl <- join $ liftA2 matchExp i1 i2
      , Just Refl <- eqT3 s1 s2 -- index ~ index'
      , Just Refl <- matchSeq matchAcc hashAcc s1 s2
      = Just Refl

    match _ _
      = Nothing


-- Array tuples
--
matchAtuple
    :: MatchAcc acc
    -> Atuple (acc aenv) s
    -> Atuple (acc aenv) t
    -> Maybe (s :~: t)
matchAtuple matchAcc (SnocAtup t1 a1) (SnocAtup t2 a2)
  | Just Refl <- matchAtuple matchAcc t1 t2
  , Just Refl <- matchAcc             a1 a2
  = Just Refl

matchAtuple _ NilAtup NilAtup = Just Refl
matchAtuple _ _       _       = Nothing


-- Array functions
--
matchOpenAfun
    :: OpenAfun aenv s
    -> OpenAfun aenv t
    -> Maybe (s :~: t)
matchOpenAfun = matchPreOpenAfun matchOpenAcc

matchPreOpenAfun
    :: MatchAcc acc
    -> PreOpenAfun acc aenv s
    -> PreOpenAfun acc aenv t
    -> Maybe (s :~: t)
matchPreOpenAfun m (Alam s) (Alam t)
  | Just Refl <- matchEnvTop        s t
  , Just Refl <- matchPreOpenAfun m s t
  = Just Refl
  where
    matchEnvTop :: (Arrays s, Arrays t)
                => PreOpenAfun acc (aenv, s) f -> PreOpenAfun acc (aenv, t) g -> Maybe (s :~: t)
    matchEnvTop _ _ = gcast Refl  -- ???

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


-- Match sequences
--
matchSeq
    :: forall idx acc aenv s t.
       MatchAcc acc
    -> HashAcc acc
    -> PreOpenSeq idx acc aenv s
    -> PreOpenSeq idx acc aenv t
    -> Maybe (s :~: t)
matchSeq m h = match
  where
    matchExp :: PreOpenExp acc env' aenv' u -> PreOpenExp acc env' aenv' v -> Maybe (u :~: v)
    matchExp = matchPreOpenExp m h

    match :: forall aenv u v. PreOpenSeq idx acc aenv u -> PreOpenSeq idx acc aenv v -> Maybe (u :~: v)
    match (Producer p1 s1)   (Producer p2 s2)
      | Just Refl <- matchP p1 p2
      , Just Refl <- match s1 s2
      = Just Refl
    match (Consumer c1)   (Consumer c2)
      | Just Refl <- matchC c1 c2
      = Just Refl
    match (Reify _ ix1) (Reify _ ix2)
      | Just Refl <- m ix1 ix2
      = gcast Refl
    match _ _
      = Nothing

    matchP :: forall aenv u v. Producer idx acc aenv u -> Producer idx acc aenv v -> Maybe (u :~: v)
    matchP (Pull src1) (Pull src2)
      | Just Refl <- matchSource src1 src2
      = Just Refl
    matchP (Subarrays sh1 a1) (Subarrays sh2 a2)
      | Just Refl <- matchExp sh1 sh2
      , Just Refl <- matchArrays ArraysRarray ArraysRarray a1 a2
      = Just Refl
    matchP (FromSegs s1 n1 vs1) (FromSegs s2 n2 vs2)
      | Just Refl <- m s1 s2
      , Just Refl <- matchExp n1 n2
      , Just Refl <- m vs1 vs2
      = Just Refl
    matchP (Produce l1 f1) (Produce l2 f2)
      | Just Refl <- join $ liftA2 matchExp l1 l2
      , Just Refl <- matchPreOpenAfun m f1 f2
      = Just Refl
    -- matchP (MapBatch f1 c1 c1' a1 x1) (MapBatch f2 c2 c2' a2 x2)
    --   | Just Refl <- matchPreOpenAfun m f1 f2
    --   , Just Refl <- matchPreOpenAfun m c1 c2
    --   , Just Refl <- matchPreOpenAfun m c1' c2'
    --   , Just Refl <- m a1 a2
    --   , Just Refl <- m x1 x2
    --   = Just Refl
    matchP (ProduceAccum l1 f1 a1) (ProduceAccum l2 f2 a2)
      | Just Refl <- join $ liftA2 matchExp l1 l2
      , Just Refl <- matchPreOpenAfun m f1 f2
      , Just Refl <- m a1 a2
      = Just Refl
    matchP _ _
      = Nothing

    matchC :: forall aenv u v. Consumer idx acc aenv u -> Consumer idx acc aenv v -> Maybe (u :~: v)
    matchC (Last a1 d1) (Last a2 d2)
      | Just Refl <- m a1 a2
      , Just Refl <- m d1 d2
      = Just Refl
    matchC (FoldBatch f1 a1 x1) (FoldBatch f2 a2 x2)
      | Just Refl <- matchPreOpenAfun m f1 f2
      , Just Refl <- m a1 a2
      , Just Refl <- m x1 x2
      = Just Refl
    matchC (Stuple s1) (Stuple s2)
      | Just Refl <- matchAtuple (matchSeq m h) s1 s2
      = gcast Refl
    matchC _ _
      = Nothing

matchSource :: forall s t. Source s -> Source t -> Maybe (s :~: t)
matchSource (List arrs1) (List arrs2)
      | unsafePerformIO $ do
          sn1 <- makeStableName arrs1
          sn2 <- makeStableName arrs2
          return $! hashStableName sn1 == hashStableName sn2
      = gcast Refl
matchSource (RegularList _ arrs1) (RegularList _ arrs2)
      | unsafePerformIO $ do
          sn1 <- makeStableName arrs1
          sn2 <- makeStableName arrs2
          return $! hashStableName sn1 == hashStableName sn2
      = gcast Refl
matchSource (Function f1 a1) (Function f2 a2)
      | unsafePerformIO $ do
          sn1 <- makeStableName f1
          sn2 <- makeStableName f2
          return $! hashStableName sn1 == hashStableName sn2
      , unsafePerformIO $ do
          sn1 <- makeStableName a1
          sn2 <- makeStableName a2
          return $! hashStableName sn1 == hashStableName sn2
      = gcast Refl
matchSource _ _ = Nothing


-- Match arrays
--
-- As a convenience, we are just comparing the stable names, but we could also
-- walk the structure comparing the underlying ptrsOfArrayData.
--
matchArrays :: ArraysR s -> ArraysR t -> s -> t -> Maybe (s :~: t)
matchArrays ArraysRunit ArraysRunit () ()
  = Just Refl

matchArrays (ArraysRpair a1 b1) (ArraysRpair a2 b2) (arr1,brr1) (arr2,brr2)
  | Just Refl <- matchArrays a1 a2 arr1 arr2
  , Just Refl <- matchArrays b1 b2 brr1 brr2
  = Just Refl

matchArrays ArraysRarray ArraysRarray (Array _ ad1) (Array _ ad2)
  | unsafePerformIO $ do
      sn1 <- makeStableName ad1
      sn2 <- makeStableName ad2
      return $! hashStableName sn1 == hashStableName sn2
  = gcast Refl

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
matchOpenExp
    :: OpenExp env aenv s
    -> OpenExp env aenv t
    -> Maybe (s :~: t)
matchOpenExp = matchPreOpenExp matchOpenAcc hashOpenAcc

matchPreOpenExp
    :: forall acc env aenv s t.
       MatchAcc acc
    -> HashAcc  acc
    -> PreOpenExp acc env aenv s
    -> PreOpenExp acc env aenv t
    -> Maybe (s :~: t)
matchPreOpenExp matchAcc hashAcc = match
  where
    match :: forall env' aenv' s' t'.
             PreOpenExp acc env' aenv' s'
          -> PreOpenExp acc env' aenv' t'
          -> Maybe (s' :~: t')
    match (Let x1 e1) (Let x2 e2)
      | Just Refl <- match x1 x2
      , Just Refl <- match e1 e2
      = Just Refl

    match (Var v1) (Var v2)
      = matchIdx v1 v2

    match (Foreign ff1 _ e1) (Foreign ff2 _ e2)
      | Just Refl <- match e1 e2
      , unsafePerformIO $ do
          sn1 <- makeStableName ff1
          sn2 <- makeStableName ff2
          return $! hashStableName sn1 == hashStableName sn2
      = gcast Refl

    match (Const c1) (Const c2)
      | Just Refl <- matchTupleType (eltType (undefined::s')) (eltType (undefined::t'))
      , matchConst (eltType (undefined::s')) c1 c2
      = gcast Refl  -- surface/representation type

    match (Tuple t1) (Tuple t2)
      | Just Refl <- matchTuple matchAcc hashAcc t1 t2
      = gcast Refl  -- surface/representation type

    match (Prj ix1 t1) (Prj ix2 t2)
      | Just Refl <- match         t1  t2
      , Just Refl <- matchTupleIdx ix1 ix2
      = Just Refl

    match IndexAny IndexAny
      = gcast Refl  -- ???

    match IndexNil IndexNil
      = Just Refl

    match (IndexCons sl1 a1) (IndexCons sl2 a2)
      | Just Refl <- match sl1 sl2
      , Just Refl <- match a1 a2
      = Just Refl

    match (IndexHead sl1) (IndexHead sl2)
      | Just Refl <- match sl1 sl2
      = Just Refl

    match (IndexTail sl1) (IndexTail sl2)
      | Just Refl <- match sl1 sl2
      = Just Refl

    match (IndexTrans sl1) (IndexTrans sl2)
      | Just Refl <- match sl1 sl2
      = Just Refl

    match (IndexSlice sliceIndex1 _ sh1) (IndexSlice sliceIndex2 _ sh2)
      | Just Refl <- match sh1 sh2
      , Just Refl <- matchSliceRestrict sliceIndex1 sliceIndex2
      = gcast Refl  -- SliceIndex representation/surface type

    match (IndexFull sliceIndex1 ix1 sl1) (IndexFull sliceIndex2 ix2 sl2)
      | Just Refl <- match ix1 ix2
      , Just Refl <- match sl1 sl2
      , Just Refl <- matchSliceExtend sliceIndex1 sliceIndex2
      = gcast Refl  -- SliceIndex representation/surface type

    match (ToIndex sh1 i1) (ToIndex sh2 i2)
      | Just Refl <- match sh1 sh2
      , Just Refl <- match i1  i2
      = Just Refl

    match (FromIndex sh1 i1) (FromIndex sh2 i2)
      | Just Refl <- match i1  i2
      , Just Refl <- match sh1 sh2
      = Just Refl

    match (ToSlice _ sh1 i1) (ToSlice _ sh2 i2)
      | Just Refl <- match sh1 sh2
      , Just Refl <- match i1  i2
      = gcast Refl

    match (Cond p1 t1 e1) (Cond p2 t2 e2)
      | Just Refl <- match p1 p2
      , Just Refl <- match t1 t2
      , Just Refl <- match e1 e2
      = Just Refl

    match (While p1 f1 x1) (While p2 f2 x2)
      | Just Refl <- match x1 x2
      , Just Refl <- matchPreOpenFun matchAcc hashAcc p1 p2
      , Just Refl <- matchPreOpenFun matchAcc hashAcc f1 f2
      = Just Refl

    match (PrimConst c1) (PrimConst c2)
      = matchPrimConst c1 c2

    match (PrimApp f1 x1) (PrimApp f2 x2)
      | Just x1'  <- commutes hashAcc f1 x1
      , Just x2'  <- commutes hashAcc f2 x2
      , Just Refl <- match        x1' x2'
      , Just Refl <- matchPrimFun f1  f2
      = Just Refl

      | Just Refl <- match x1 x2
      , Just Refl <- matchPrimFun f1 f2
      = Just Refl

    match (Index a1 x1) (Index a2 x2)
      | Just Refl <- matchAcc a1 a2     -- should only be array indices
      , Just Refl <- match    x1 x2
      = Just Refl

    match (LinearIndex a1 x1) (LinearIndex a2 x2)
      | Just Refl <- matchAcc a1 a2
      , Just Refl <- match    x1 x2
      = Just Refl

    match (Shape a1) (Shape a2)
      | Just Refl <- matchAcc a1 a2     -- should only be array indices
      = Just Refl

    match (ShapeSize sh1) (ShapeSize sh2)
      | Just Refl <- match sh1 sh2
      = Just Refl

    match (Intersect sa1 sb1) (Intersect sa2 sb2)
      | Just Refl <- match sa1 sa2
      , Just Refl <- match sb1 sb2
      = Just Refl

    match (Union sa1 sb1) (Union sa2 sb2)
      | Just Refl <- match sa1 sa2
      , Just Refl <- match sb1 sb2
      = Just Refl

    match _ _
      = Nothing


-- Match scalar functions
--
matchOpenFun
    :: OpenFun env aenv s
    -> OpenFun env aenv t
    -> Maybe (s :~: t)
matchOpenFun = matchPreOpenFun matchOpenAcc hashOpenAcc

matchPreOpenFun
    :: MatchAcc acc
    -> HashAcc  acc
    -> PreOpenFun acc env aenv s
    -> PreOpenFun acc env aenv t
    -> Maybe (s :~: t)
matchPreOpenFun m h (Lam s) (Lam t)
  | Just Refl <- matchEnvTop         s t
  , Just Refl <- matchPreOpenFun m h s t
  = Just Refl
  where
    matchEnvTop :: (Elt s, Elt t) => PreOpenFun acc (env, s) aenv f -> PreOpenFun acc (env, t) aenv g -> Maybe (s :~: t)
    matchEnvTop _ _ = gcast Refl  -- ???

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
matchIdx :: Idx env s -> Idx env t -> Maybe (s :~: t)
matchIdx ZeroIdx     ZeroIdx     = Just Refl
matchIdx (SuccIdx u) (SuccIdx v) = matchIdx u v
matchIdx _           _           = Nothing


-- Tuple projection indices. Given the same tuple expression structure (tup),
-- check that the indices project identical elements.
--
matchTupleIdx :: TupleIdx tup s -> TupleIdx tup t -> Maybe (s :~: t)
matchTupleIdx ZeroTupIdx     ZeroTupIdx     = Just Refl
matchTupleIdx (SuccTupIdx s) (SuccTupIdx t) = matchTupleIdx s t
matchTupleIdx _              _              = Nothing

-- Tuples
--
matchTuple
    :: MatchAcc acc
    -> HashAcc  acc
    -> Tuple (PreOpenExp acc env aenv) s
    -> Tuple (PreOpenExp acc env aenv) t
    -> Maybe (s :~: t)
matchTuple _ _ NilTup          NilTup           = Just Refl
matchTuple m h (SnocTup t1 e1) (SnocTup t2 e2)
  | Just Refl <- matchTuple      m h t1 t2
  , Just Refl <- matchPreOpenExp m h e1 e2
  = Just Refl

matchTuple _ _ _               _                = Nothing


-- Slice specifications
--
matchSliceRestrict
    :: SliceIndex slix s co  sh
    -> SliceIndex slix' t co' sh
    -> Maybe (s :~: t)
matchSliceRestrict SliceNil SliceNil
  = Just Refl

matchSliceRestrict (SliceAll   sl1) (SliceAll   sl2)
  | Just Refl <- matchSliceRestrict sl1 sl2
  = Just Refl

matchSliceRestrict (SliceFixed sl1) (SliceFixed sl2)
  | Just Refl <- matchSliceRestrict sl1 sl2
  = Just Refl

matchSliceRestrict _ _
  = Nothing


matchSliceExtend
    :: SliceIndex slix sl co  s
    -> SliceIndex slix sl co' t
    -> Maybe (s :~: t)
matchSliceExtend SliceNil SliceNil
  = Just Refl

matchSliceExtend (SliceAll   sl1) (SliceAll   sl2)
  | Just Refl <- matchSliceExtend sl1 sl2
  = Just Refl

matchSliceExtend (SliceFixed sl1) (SliceFixed sl2)
  | Just Refl <- matchSliceExtend sl1 sl2
  = Just Refl

matchSliceExtend _ _
  = Nothing


-- Primitive constants and functions
--
matchPrimConst :: PrimConst s -> PrimConst t -> Maybe (s :~: t)
matchPrimConst (PrimMinBound s) (PrimMinBound t) = matchBoundedType s t
matchPrimConst (PrimMaxBound s) (PrimMaxBound t) = matchBoundedType s t
matchPrimConst (PrimPi s)       (PrimPi t)       = matchFloatingType s t
matchPrimConst _                _                = Nothing


-- Covariant function matching
--
matchPrimFun :: PrimFun (a -> s) -> PrimFun (a -> t) -> Maybe (s :~: t)
matchPrimFun (PrimAdd _)                (PrimAdd _)                = Just Refl
matchPrimFun (PrimSub _)                (PrimSub _)                = Just Refl
matchPrimFun (PrimMul _)                (PrimMul _)                = Just Refl
matchPrimFun (PrimNeg _)                (PrimNeg _)                = Just Refl
matchPrimFun (PrimAbs _)                (PrimAbs _)                = Just Refl
matchPrimFun (PrimSig _)                (PrimSig _)                = Just Refl
matchPrimFun (PrimQuot _)               (PrimQuot _)               = Just Refl
matchPrimFun (PrimRem _)                (PrimRem _)                = Just Refl
matchPrimFun (PrimQuotRem _)            (PrimQuotRem _)            = Just Refl
matchPrimFun (PrimIDiv _)               (PrimIDiv _)               = Just Refl
matchPrimFun (PrimMod _)                (PrimMod _)                = Just Refl
matchPrimFun (PrimDivMod _)             (PrimDivMod _)             = Just Refl
matchPrimFun (PrimBAnd _)               (PrimBAnd _)               = Just Refl
matchPrimFun (PrimBOr _)                (PrimBOr _)                = Just Refl
matchPrimFun (PrimBXor _)               (PrimBXor _)               = Just Refl
matchPrimFun (PrimBNot _)               (PrimBNot _)               = Just Refl
matchPrimFun (PrimBShiftL _)            (PrimBShiftL _)            = Just Refl
matchPrimFun (PrimBShiftR _)            (PrimBShiftR _)            = Just Refl
matchPrimFun (PrimBRotateL _)           (PrimBRotateL _)           = Just Refl
matchPrimFun (PrimBRotateR _)           (PrimBRotateR _)           = Just Refl
matchPrimFun (PrimPopCount _)           (PrimPopCount _)           = Just Refl
matchPrimFun (PrimCountLeadingZeros _)  (PrimCountLeadingZeros _)  = Just Refl
matchPrimFun (PrimCountTrailingZeros _) (PrimCountTrailingZeros _) = Just Refl
matchPrimFun (PrimFDiv _)               (PrimFDiv _)               = Just Refl
matchPrimFun (PrimRecip _)              (PrimRecip _)              = Just Refl
matchPrimFun (PrimSin _)                (PrimSin _)                = Just Refl
matchPrimFun (PrimCos _)                (PrimCos _)                = Just Refl
matchPrimFun (PrimTan _)                (PrimTan _)                = Just Refl
matchPrimFun (PrimAsin _)               (PrimAsin _)               = Just Refl
matchPrimFun (PrimAcos _)               (PrimAcos _)               = Just Refl
matchPrimFun (PrimAtan _)               (PrimAtan _)               = Just Refl
matchPrimFun (PrimSinh _)               (PrimSinh _)               = Just Refl
matchPrimFun (PrimCosh _)               (PrimCosh _)               = Just Refl
matchPrimFun (PrimTanh _)               (PrimTanh _)               = Just Refl
matchPrimFun (PrimAsinh _)              (PrimAsinh _)              = Just Refl
matchPrimFun (PrimAcosh _)              (PrimAcosh _)              = Just Refl
matchPrimFun (PrimAtanh _)              (PrimAtanh _)              = Just Refl
matchPrimFun (PrimExpFloating _)        (PrimExpFloating _)        = Just Refl
matchPrimFun (PrimSqrt _)               (PrimSqrt _)               = Just Refl
matchPrimFun (PrimLog _)                (PrimLog _)                = Just Refl
matchPrimFun (PrimFPow _)               (PrimFPow _)               = Just Refl
matchPrimFun (PrimLogBase _)            (PrimLogBase _)            = Just Refl
matchPrimFun (PrimAtan2 _)              (PrimAtan2 _)              = Just Refl
matchPrimFun (PrimTruncate _ s)         (PrimTruncate _ t)         = matchIntegralType s t
matchPrimFun (PrimRound _ s)            (PrimRound _ t)            = matchIntegralType s t
matchPrimFun (PrimFloor _ s)            (PrimFloor _ t)            = matchIntegralType s t
matchPrimFun (PrimCeiling _ s)          (PrimCeiling _ t)          = matchIntegralType s t
matchPrimFun (PrimIsNaN _)              (PrimIsNaN _)              = Just Refl
matchPrimFun (PrimLt _)                 (PrimLt _)                 = Just Refl
matchPrimFun (PrimGt _)                 (PrimGt _)                 = Just Refl
matchPrimFun (PrimLtEq _)               (PrimLtEq _)               = Just Refl
matchPrimFun (PrimGtEq _)               (PrimGtEq _)               = Just Refl
matchPrimFun (PrimEq _)                 (PrimEq _)                 = Just Refl
matchPrimFun (PrimNEq _)                (PrimNEq _)                = Just Refl
matchPrimFun (PrimMax _)                (PrimMax _)                = Just Refl
matchPrimFun (PrimMin _)                (PrimMin _)                = Just Refl
matchPrimFun (PrimFromIntegral _ s)     (PrimFromIntegral _ t)     = matchNumType s t
matchPrimFun (PrimToFloating _ s)       (PrimToFloating _ t)       = matchFloatingType s t
matchPrimFun (PrimCoerce _ s)           (PrimCoerce _ t)           = matchScalarType s t
matchPrimFun PrimLAnd                   PrimLAnd                   = Just Refl
matchPrimFun PrimLOr                    PrimLOr                    = Just Refl
matchPrimFun PrimLNot                   PrimLNot                   = Just Refl
matchPrimFun PrimOrd                    PrimOrd                    = Just Refl
matchPrimFun PrimChr                    PrimChr                    = Just Refl
matchPrimFun PrimBoolToInt              PrimBoolToInt              = Just Refl
matchPrimFun _                          _                          = Nothing


-- Contravariant function matching
--
matchPrimFun' :: PrimFun (s -> a) -> PrimFun (t -> a) -> Maybe (s :~: t)
matchPrimFun' (PrimAdd _)                (PrimAdd _)                = Just Refl
matchPrimFun' (PrimSub _)                (PrimSub _)                = Just Refl
matchPrimFun' (PrimMul _)                (PrimMul _)                = Just Refl
matchPrimFun' (PrimNeg _)                (PrimNeg _)                = Just Refl
matchPrimFun' (PrimAbs _)                (PrimAbs _)                = Just Refl
matchPrimFun' (PrimSig _)                (PrimSig _)                = Just Refl
matchPrimFun' (PrimQuot _)               (PrimQuot _)               = Just Refl
matchPrimFun' (PrimRem _)                (PrimRem _)                = Just Refl
matchPrimFun' (PrimQuotRem _)            (PrimQuotRem _)            = Just Refl
matchPrimFun' (PrimIDiv _)               (PrimIDiv _)               = Just Refl
matchPrimFun' (PrimMod _)                (PrimMod _)                = Just Refl
matchPrimFun' (PrimDivMod _)             (PrimDivMod _)             = Just Refl
matchPrimFun' (PrimBAnd _)               (PrimBAnd _)               = Just Refl
matchPrimFun' (PrimBOr _)                (PrimBOr _)                = Just Refl
matchPrimFun' (PrimBXor _)               (PrimBXor _)               = Just Refl
matchPrimFun' (PrimBNot _)               (PrimBNot _)               = Just Refl
matchPrimFun' (PrimBShiftL _)            (PrimBShiftL _)            = Just Refl
matchPrimFun' (PrimBShiftR _)            (PrimBShiftR _)            = Just Refl
matchPrimFun' (PrimBRotateL _)           (PrimBRotateL _)           = Just Refl
matchPrimFun' (PrimBRotateR _)           (PrimBRotateR _)           = Just Refl
matchPrimFun' (PrimPopCount s)           (PrimPopCount t)           = matchIntegralType s t
matchPrimFun' (PrimCountLeadingZeros s)  (PrimCountLeadingZeros t)  = matchIntegralType s t
matchPrimFun' (PrimCountTrailingZeros s) (PrimCountTrailingZeros t) = matchIntegralType s t
matchPrimFun' (PrimFDiv _)               (PrimFDiv _)               = Just Refl
matchPrimFun' (PrimRecip _)              (PrimRecip _)              = Just Refl
matchPrimFun' (PrimSin _)                (PrimSin _)                = Just Refl
matchPrimFun' (PrimCos _)                (PrimCos _)                = Just Refl
matchPrimFun' (PrimTan _)                (PrimTan _)                = Just Refl
matchPrimFun' (PrimAsin _)               (PrimAsin _)               = Just Refl
matchPrimFun' (PrimAcos _)               (PrimAcos _)               = Just Refl
matchPrimFun' (PrimAtan _)               (PrimAtan _)               = Just Refl
matchPrimFun' (PrimSinh _)               (PrimSinh _)               = Just Refl
matchPrimFun' (PrimCosh _)               (PrimCosh _)               = Just Refl
matchPrimFun' (PrimTanh _)               (PrimTanh _)               = Just Refl
matchPrimFun' (PrimAsinh _)              (PrimAsinh _)              = Just Refl
matchPrimFun' (PrimAcosh _)              (PrimAcosh _)              = Just Refl
matchPrimFun' (PrimAtanh _)              (PrimAtanh _)              = Just Refl
matchPrimFun' (PrimExpFloating _)        (PrimExpFloating _)        = Just Refl
matchPrimFun' (PrimSqrt _)               (PrimSqrt _)               = Just Refl
matchPrimFun' (PrimLog _)                (PrimLog _)                = Just Refl
matchPrimFun' (PrimFPow _)               (PrimFPow _)               = Just Refl
matchPrimFun' (PrimLogBase _)            (PrimLogBase _)            = Just Refl
matchPrimFun' (PrimAtan2 _)              (PrimAtan2 _)              = Just Refl
matchPrimFun' (PrimTruncate s _)         (PrimTruncate t _)         = matchFloatingType s t
matchPrimFun' (PrimRound s _)            (PrimRound t _)            = matchFloatingType s t
matchPrimFun' (PrimFloor s _)            (PrimFloor t _)            = matchFloatingType s t
matchPrimFun' (PrimCeiling s _)          (PrimCeiling t _)          = matchFloatingType s t
matchPrimFun' (PrimIsNaN s)              (PrimIsNaN t)              = matchFloatingType s t
matchPrimFun' (PrimMax _)                (PrimMax _)                = Just Refl
matchPrimFun' (PrimMin _)                (PrimMin _)                = Just Refl
matchPrimFun' (PrimFromIntegral s _)     (PrimFromIntegral t _)     = matchIntegralType s t
matchPrimFun' (PrimToFloating s _)       (PrimToFloating t _)       = matchNumType s t
matchPrimFun' (PrimCoerce s _)           (PrimCoerce t _)           = matchScalarType s t
matchPrimFun' PrimLAnd                   PrimLAnd                   = Just Refl
matchPrimFun' PrimLOr                    PrimLOr                    = Just Refl
matchPrimFun' PrimLNot                   PrimLNot                   = Just Refl
matchPrimFun' PrimOrd                    PrimOrd                    = Just Refl
matchPrimFun' PrimChr                    PrimChr                    = Just Refl
matchPrimFun' PrimBoolToInt              PrimBoolToInt              = Just Refl
matchPrimFun' (PrimLt s) (PrimLt t)
  | Just Refl <- matchScalarType s t
  = Just Refl

matchPrimFun' (PrimGt s) (PrimGt t)
  | Just Refl <- matchScalarType s t
  = Just Refl

matchPrimFun' (PrimLtEq s) (PrimLtEq t)
  | Just Refl <- matchScalarType s t
  = Just Refl

matchPrimFun' (PrimGtEq s) (PrimGtEq t)
  | Just Refl <- matchScalarType s t
  = Just Refl

matchPrimFun' (PrimEq s) (PrimEq t)
  | Just Refl <- matchScalarType s t
  = Just Refl

matchPrimFun' (PrimNEq s) (PrimNEq t)
  | Just Refl <- matchScalarType s t
  = Just Refl

matchPrimFun' _ _
  = Nothing

matchOpenSeq :: PreOpenSeq idx OpenAcc aenv s -> PreOpenSeq idx OpenAcc aenv t -> Maybe (s :~: t)
matchOpenSeq = matchSeq matchOpenAcc hashOpenAcc

-- Match reified types
--
matchTupleType :: TupleType s -> TupleType t -> Maybe (s :~: t)
matchTupleType UnitTuple         UnitTuple         = Just Refl
matchTupleType (SingleTuple s)   (SingleTuple t)   = matchScalarType s t
matchTupleType (PairTuple s1 s2) (PairTuple t1 t2)
  | Just Refl <- matchTupleType s1 t1
  , Just Refl <- matchTupleType s2 t2
  = Just Refl

matchTupleType _ _
  = Nothing


-- Match reified type dictionaries
--
matchScalarType :: ScalarType s -> ScalarType t -> Maybe (s :~: t)
matchScalarType (NumScalarType s)    (NumScalarType t)    = matchNumType s t
matchScalarType (NonNumScalarType s) (NonNumScalarType t) = matchNonNumType s t
matchScalarType _                    _                    = Nothing

matchNumType :: NumType s -> NumType t -> Maybe (s :~: t)
matchNumType (IntegralNumType s) (IntegralNumType t) = matchIntegralType s t
matchNumType (FloatingNumType s) (FloatingNumType t) = matchFloatingType s t
matchNumType _                   _                   = Nothing

matchBoundedType :: BoundedType s -> BoundedType t -> Maybe (s :~: t)
matchBoundedType (IntegralBoundedType s) (IntegralBoundedType t) = matchIntegralType s t
matchBoundedType (NonNumBoundedType s)   (NonNumBoundedType t)   = matchNonNumType s t
matchBoundedType _                       _                       = Nothing

matchIntegralType :: IntegralType s -> IntegralType t -> Maybe (s :~: t)
matchIntegralType (TypeInt _)     (TypeInt _)     = Just Refl
matchIntegralType (TypeInt8 _)    (TypeInt8 _)    = Just Refl
matchIntegralType (TypeInt16 _)   (TypeInt16 _)   = Just Refl
matchIntegralType (TypeInt32 _)   (TypeInt32 _)   = Just Refl
matchIntegralType (TypeInt64 _)   (TypeInt64 _)   = Just Refl
matchIntegralType (TypeWord _)    (TypeWord _)    = Just Refl
matchIntegralType (TypeWord8 _)   (TypeWord8 _)   = Just Refl
matchIntegralType (TypeWord16 _)  (TypeWord16 _)  = Just Refl
matchIntegralType (TypeWord32 _)  (TypeWord32 _)  = Just Refl
matchIntegralType (TypeWord64 _)  (TypeWord64 _)  = Just Refl
matchIntegralType (TypeCShort _)  (TypeCShort _)  = Just Refl
matchIntegralType (TypeCUShort _) (TypeCUShort _) = Just Refl
matchIntegralType (TypeCInt _)    (TypeCInt _)    = Just Refl
matchIntegralType (TypeCUInt _)   (TypeCUInt _)   = Just Refl
matchIntegralType (TypeCLong _)   (TypeCLong _)   = Just Refl
matchIntegralType (TypeCULong _)  (TypeCULong _)  = Just Refl
matchIntegralType (TypeCLLong _)  (TypeCLLong _)  = Just Refl
matchIntegralType (TypeCULLong _) (TypeCULLong _) = Just Refl
matchIntegralType _               _               = Nothing

matchFloatingType :: FloatingType s -> FloatingType t -> Maybe (s :~: t)
matchFloatingType (TypeFloat _)   (TypeFloat _)   = Just Refl
matchFloatingType (TypeDouble _)  (TypeDouble _)  = Just Refl
matchFloatingType (TypeCFloat _)  (TypeCFloat _)  = Just Refl
matchFloatingType (TypeCDouble _) (TypeCDouble _) = Just Refl
matchFloatingType _               _               = Nothing

matchNonNumType :: NonNumType s -> NonNumType t -> Maybe (s :~: t)
matchNonNumType (TypeBool _)   (TypeBool _)   = Just Refl
matchNonNumType (TypeChar _)   (TypeChar _)   = Just Refl
matchNonNumType (TypeCChar _)  (TypeCChar _)  = Just Refl
matchNonNumType (TypeCSChar _) (TypeCSChar _) = Just Refl
matchNonNumType (TypeCUChar _) (TypeCUChar _) = Just Refl
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

-- | Equality over @k1 -> k2 -> k3 -> k4@
eqT3 :: forall t t' (a :: k1) (b :: k2) (c ::k3) (a' :: k1) (b' :: k2) (c' :: k3).
        (Typeable t, Typeable t')
     => t a b c -> t' a' b' c' -> Maybe (t :~: t')
eqT3 _ _ = eqT :: Maybe (t :~: t')

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

hashPreOpenSeq :: forall idx acc aenv arrs. HashAcc acc -> PreOpenSeq idx acc aenv arrs -> Int
hashPreOpenSeq hashAcc s =
  let
    hashA :: Int -> acc aenv' a -> Int
    hashA salt = hashWithSalt salt . hashAcc

    hashE :: Int -> PreOpenExp acc env' aenv' e -> Int
    hashE salt = hashWithSalt salt . hashPreOpenExp hashAcc

    hashAF :: Int -> PreOpenAfun acc aenv' f -> Int
    hashAF salt = hashWithSalt salt . hashAfun hashAcc

    hashS :: Int -> PreOpenSeq idx acc aenv' arrs' -> Int
    hashS salt = hashWithSalt salt . hashPreOpenSeq hashAcc

    hashL :: Int -> Maybe (PreExp acc aenv' e) -> Int
    hashL salt Nothing = salt
    hashL salt (Just l) = hashE salt l

    hashP :: Int -> Producer idx acc aenv' a -> Int
    hashP salt p =
      case p of
        Pull src            -> hashWithSalt salt "Pull"         `hashSource` src
        Subarrays sh a      -> hashWithSalt salt "Subarrays"    `hashE` sh `hashWithSalt` hashArrays ArraysRarray a
        FromSegs s n vs     -> hashWithSalt salt "FromSegs"     `hashA` s `hashE` n `hashA` vs
        Produce l f         -> hashWithSalt salt "Produce"      `hashL` l  `hashAF` f
        -- MapBatch f c c' a x -> hashWithSalt salt "MapBatch"     `hashAF` f `hashAF` c `hashAF` c' `hashA`  a `hashA` x
        ProduceAccum l f a  -> hashWithSalt salt "ProduceAccum" `hashL` l `hashAF` f `hashA` a


    hashSource :: Int -> Source t -> Int
    hashSource salt a =
      case a of
        List arrs          -> hashWithSalt salt "List"        `hashWithSalt` (unsafePerformIO $! hashStableName `fmap` makeStableName arrs)
        RegularList _ arrs -> hashWithSalt salt "RegularList" `hashWithSalt` (unsafePerformIO $! hashStableName `fmap` makeStableName arrs)
        Function f a       -> hashWithSalt salt "Function"    `hashWithSalt` (unsafePerformIO $! hashStableName `fmap` makeStableName f) `hashWithSalt` (unsafePerformIO $! hashStableName `fmap` makeStableName a)

    hashC :: Int -> Consumer idx acc aenv' a -> Int
    hashC salt c =
      case c of
        FoldBatch f a x -> hashWithSalt salt "FoldBatch" `hashAF` f `hashA` a `hashA` x
        Last a d        -> hashWithSalt salt "Last"      `hashA` a `hashA` d
        Stuple t        -> hash "Stuple"                 `hashWithSalt` hashAtuple (hashS salt) t
        Elements x      -> hashWithSalt salt "Elements"  `hashA` x
        Tabulate x      -> hashWithSalt salt "Tabulate"  `hashA` x

  in case s of
    Producer   p s' -> hash "Producer"   `hashP` p `hashS` s'
    Consumer   c    -> hash "Consumer"   `hashC` c
    Reify _    ix   -> hash "Reify"      `hashA` ix


hashPreOpenAcc :: forall acc aenv arrs. HashAcc acc -> PreOpenAcc acc aenv arrs -> Int
hashPreOpenAcc hashAcc pacc =
  let
    hashA :: Int -> acc aenv' a -> Int
    hashA salt = hashWithSalt salt . hashAcc

    hashE :: Int -> PreOpenExp acc env' aenv' e -> Int
    hashE salt = hashWithSalt salt . hashPreOpenExp hashAcc

    hashF :: Int -> PreOpenFun acc env' aenv' f -> Int
    hashF salt = hashWithSalt salt . hashPreOpenFun hashAcc

    hashS :: Int -> PreOpenSeq idx acc aenv arrs -> Int
    hashS salt = hashWithSalt salt . hashPreOpenSeq hashAcc

    hashL :: Int -> Maybe (PreExp acc aenv' e) -> Int
    hashL salt Nothing = salt
    hashL salt (Just l) = hashE salt l

  in case pacc of
    Alet bnd body               -> hash "Alet"          `hashA` bnd `hashA` body
    Avar v                      -> hash "Avar"          `hashWithSalt` hashIdx v
    Atuple t                    -> hash "Atuple"        `hashWithSalt` hashAtuple hashAcc t
    Aprj ix a                   -> hash "Aprj"          `hashWithSalt` hashTupleIdx ix    `hashA` a
    Apply f a                   -> hash "Apply"         `hashWithSalt` hashAfun hashAcc f `hashA` a
    Aforeign _ f a              -> hash "Aforeign"      `hashWithSalt` hashAfun hashAcc f `hashA` a
    Use a                       -> hash "Use"           `hashWithSalt` hashArrays (arrays (undefined::arrs)) a
    Subarray ix sh a            -> hash "Subarray"      `hashE` ix `hashE` sh `hashWithSalt` hashArrays (arrays (undefined::arrs)) a
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
    Collect min max i s         -> hash "Seq"           `hashE` min `hashL` max `hashL` i `hashS` s


hashArrays :: ArraysR a -> a -> Int
hashArrays ArraysRunit         ()       = hash ()
hashArrays (ArraysRpair r1 r2) (a1, a2) = hash ( hashArrays r1 a1, hashArrays r2 a2)
hashArrays ArraysRarray        ad       = unsafePerformIO $! hashStableName `fmap` makeStableName ad

hashAtuple :: HashAcc acc -> Atuple (acc aenv) a -> Int
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
    IndexTrans sl               -> hash "IndexTrans"    `hashE` sl
    IndexSlice spec _ sh        -> hash "IndexSlice"    `hashE` sh            `hashWithSalt` show spec
    IndexFull  spec ix sl       -> hash "IndexFull"     `hashE` ix `hashE` sl `hashWithSalt` show spec
    ToIndex sh i                -> hash "ToIndex"       `hashE` sh `hashE` i
    FromIndex sh i              -> hash "FromIndex"     `hashE` sh `hashE` i
    ToSlice spec sh i           -> hash "ToSlice"       `hashE` sh `hashE` i  `hashWithSalt` show spec
    Cond c t e                  -> hash "Cond"          `hashE` c  `hashE` t  `hashE` e
    While p f x                 -> hash "While"         `hashWithSalt` hashPreOpenFun hashAcc p  `hashWithSalt` hashPreOpenFun hashAcc f  `hashE` x
    PrimApp f x                 -> hash "PrimApp"       `hashWithSalt` hashPrimFun f `hashE` fromMaybe x (commutes hashAcc f x)
    PrimConst c                 -> hash "PrimConst"     `hashWithSalt` hashPrimConst c
    Index a ix                  -> hash "Index"         `hashA` a  `hashE` ix
    LinearIndex a ix            -> hash "LinearIndex"   `hashA` a  `hashE` ix
    Shape a                     -> hash "Shape"         `hashA` a
    ShapeSize sh                -> hash "ShapeSize"     `hashE` sh
    Intersect sa sb             -> hash "Intersect"     `hashE` sa `hashE` sb
    Union sa sb                 -> hash "Union"         `hashE` sa `hashE` sb
    Foreign _ f e               -> hash "Foreign"       `hashWithSalt` hashPreOpenFun hashAcc f `hashE` e


hashPreOpenFun :: HashAcc acc -> PreOpenFun acc env aenv f -> Int
hashPreOpenFun h (Body e)       = hash "Body"           `hashWithSalt` hashPreOpenExp h e
hashPreOpenFun h (Lam f)        = hash "Lam"            `hashWithSalt` hashPreOpenFun h f

hashTuple :: HashAcc acc -> Tuple (PreOpenExp acc env aenv) e -> Int
hashTuple _ NilTup              = hash "NilTup"
hashTuple h (SnocTup t e)       = hash "SnocTup"        `hashWithSalt` hashTuple h t `hashWithSalt` hashPreOpenExp h e


hashPrimConst :: PrimConst c -> Int
hashPrimConst PrimMinBound{}    = hash "PrimMinBound"
hashPrimConst PrimMaxBound{}    = hash "PrimMaxBound"
hashPrimConst PrimPi{}          = hash "PrimPi"

hashPrimFun :: PrimFun f -> Int
hashPrimFun PrimAdd{}                = hash "PrimAdd"
hashPrimFun PrimSub{}                = hash "PrimSub"
hashPrimFun PrimMul{}                = hash "PrimMul"
hashPrimFun PrimNeg{}                = hash "PrimNeg"
hashPrimFun PrimAbs{}                = hash "PrimAbs"
hashPrimFun PrimSig{}                = hash "PrimSig"
hashPrimFun PrimQuot{}               = hash "PrimQuot"
hashPrimFun PrimRem{}                = hash "PrimRem"
hashPrimFun PrimQuotRem{}            = hash "PrimQuotRem"
hashPrimFun PrimIDiv{}               = hash "PrimIDiv"
hashPrimFun PrimMod{}                = hash "PrimMod"
hashPrimFun PrimDivMod{}             = hash "PrimDivMod"
hashPrimFun PrimBAnd{}               = hash "PrimBAnd"
hashPrimFun PrimBOr{}                = hash "PrimBOr"
hashPrimFun PrimBXor{}               = hash "PrimBXor"
hashPrimFun PrimBNot{}               = hash "PrimBNot"
hashPrimFun PrimBShiftL{}            = hash "PrimBShiftL"
hashPrimFun PrimBShiftR{}            = hash "PrimBShiftR"
hashPrimFun PrimBRotateL{}           = hash "PrimBRotateL"
hashPrimFun PrimBRotateR{}           = hash "PrimBRotateR"
hashPrimFun PrimPopCount{}           = hash "PrimPopCount"
hashPrimFun PrimCountLeadingZeros{}  = hash "PrimCountLeadingZeros"
hashPrimFun PrimCountTrailingZeros{} = hash "PrimCountTrailingZeros"
hashPrimFun PrimFDiv{}               = hash "PrimFDiv"
hashPrimFun PrimRecip{}              = hash "PrimRecip"
hashPrimFun PrimSin{}                = hash "PrimSin"
hashPrimFun PrimCos{}                = hash "PrimCos"
hashPrimFun PrimTan{}                = hash "PrimTan"
hashPrimFun PrimAsin{}               = hash "PrimAsin"
hashPrimFun PrimAcos{}               = hash "PrimAcos"
hashPrimFun PrimAtan{}               = hash "PrimAtan"
hashPrimFun PrimSinh{}               = hash "PrimSinh"
hashPrimFun PrimCosh{}               = hash "PrimCosh"
hashPrimFun PrimTanh{}               = hash "PrimTanh"
hashPrimFun PrimAsinh{}              = hash "PrimAsinh"
hashPrimFun PrimAcosh{}              = hash "PrimAcosh"
hashPrimFun PrimAtanh{}              = hash "PrimAtanh"
hashPrimFun PrimExpFloating{}        = hash "PrimExpFloating"
hashPrimFun PrimSqrt{}               = hash "PrimSqrt"
hashPrimFun PrimLog{}                = hash "PrimLog"
hashPrimFun PrimFPow{}               = hash "PrimFPow"
hashPrimFun PrimLogBase{}            = hash "PrimLogBase"
hashPrimFun PrimAtan2{}              = hash "PrimAtan2"
hashPrimFun PrimTruncate{}           = hash "PrimTruncate"
hashPrimFun PrimRound{}              = hash "PrimRound"
hashPrimFun PrimFloor{}              = hash "PrimFloor"
hashPrimFun PrimCeiling{}            = hash "PrimCeiling"
hashPrimFun PrimIsNaN{}              = hash "PrimIsNaN"
hashPrimFun PrimLt{}                 = hash "PrimLt"
hashPrimFun PrimGt{}                 = hash "PrimGt"
hashPrimFun PrimLtEq{}               = hash "PrimLtEq"
hashPrimFun PrimGtEq{}               = hash "PrimGtEq"
hashPrimFun PrimEq{}                 = hash "PrimEq"
hashPrimFun PrimNEq{}                = hash "PrimNEq"
hashPrimFun PrimMax{}                = hash "PrimMax"
hashPrimFun PrimMin{}                = hash "PrimMin"
hashPrimFun PrimFromIntegral{}       = hash "PrimFromIntegral"
hashPrimFun PrimToFloating{}         = hash "PrimToFloating"
hashPrimFun PrimCoerce{}             = hash "PrimCoerce"
hashPrimFun PrimLAnd                 = hash "PrimLAnd"
hashPrimFun PrimLOr                  = hash "PrimLOr"
hashPrimFun PrimLNot                 = hash "PrimLNot"
hashPrimFun PrimOrd                  = hash "PrimOrd"
hashPrimFun PrimChr                  = hash "PrimChr"
hashPrimFun PrimBoolToInt            = hash "PrimBoolToInt"

