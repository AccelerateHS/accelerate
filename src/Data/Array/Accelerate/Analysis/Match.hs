{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Analysis.Match
-- Copyright   : [2012..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Analysis.Match (

  -- matching expressions
  MatchAcc,
  (:~:)(..),
  matchPreOpenAcc,
  matchPreOpenAfun,
  matchOpenExp,
  matchOpenFun,
  matchPrimFun,  matchPrimFun',

  -- auxiliary
  matchIdx, matchVar, matchVars, matchArrayR, matchArraysR, matchTypeR, matchShapeR,
  matchShapeType, matchIntegralType, matchFloatingType, matchNumType, matchScalarType,
  matchLeftHandSide, matchALeftHandSide, matchELeftHandSide, matchSingleType, matchTupR

) where

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.AST.Idx
import Data.Array.Accelerate.AST.LeftHandSide
import Data.Array.Accelerate.AST.Var
import Data.Array.Accelerate.Analysis.Hash
import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.Representation.Shape
import Data.Array.Accelerate.Representation.Slice
import Data.Array.Accelerate.Representation.Stencil
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Type
import Data.Primitive.Vec
import qualified Data.Array.Accelerate.Sugar.Shape      as Sugar

import Data.Maybe
import Data.Typeable
import Unsafe.Coerce                                    ( unsafeCoerce )
import System.IO.Unsafe                                 ( unsafePerformIO )
import System.Mem.StableName
import Prelude                                          hiding ( exp )


-- TODO: Should we match annotations? If we do, then we should probably only
--       compare the optimization flags.

-- The type of matching array computations
--
type MatchAcc acc = forall aenv s t. acc aenv s -> acc aenv t -> Maybe (s :~: t)


-- Compute the congruence of two array computations. The nodes are congruent if
-- they have the same operator and their operands are congruent.
--
{-# INLINEABLE matchPreOpenAcc #-}
matchPreOpenAcc
    :: forall acc aenv s t. HasArraysR acc
    => MatchAcc  acc
    -> PreOpenAcc acc aenv s
    -> PreOpenAcc acc aenv t
    -> Maybe (s :~: t)
matchPreOpenAcc matchAcc = match
  where
    matchFun :: OpenFun env' aenv' u -> OpenFun env' aenv' v -> Maybe (u :~: v)
    matchFun = matchOpenFun

    matchExp :: OpenExp env' aenv' u -> OpenExp env' aenv' v -> Maybe (u :~: v)
    matchExp = matchOpenExp

    match :: PreOpenAcc acc aenv s -> PreOpenAcc acc aenv t -> Maybe (s :~: t)
    match (Alet lhs1 x1 a1) (Alet lhs2 x2 a2)
      | Just Refl <- matchALeftHandSide lhs1 lhs2
      , Just Refl <- matchAcc x1 x2
      , Just Refl <- matchAcc a1 a2
      = Just Refl

    match (Avar v1) (Avar v2)
      = matchVar v1 v2

    match (Apair a1 a2) (Apair b1 b2)
      | Just Refl <- matchAcc a1 b1
      , Just Refl <- matchAcc a2 b2
      = Just Refl

    match Anil Anil
      = Just Refl

    match (Apply _ f1 a1) (Apply _ f2 a2)
      | Just Refl <- matchPreOpenAfun matchAcc f1 f2
      , Just Refl <- matchAcc                  a1 a2
      = Just Refl

    match (Aforeign _ ff1 f1 a1) (Aforeign _ ff2 f2 a2)
      | Just Refl <- matchAcc a1 a2
      , unsafePerformIO $ do
          sn1 <- makeStableName ff1
          sn2 <- makeStableName ff2
          return $! hashStableName sn1 == hashStableName sn2
      , Just Refl <- matchPreOpenAfun matchAcc f1 f2
      = Just Refl

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

    match (Use repr1 a1) (Use repr2 a2)
      | Just Refl <- matchArray repr1 repr2 a1 a2
      = Just Refl

    match (Unit t1 e1) (Unit t2 e2)
      | Just Refl <- matchTypeR t1 t2
      , Just Refl <- matchExp e1 e2
      = Just Refl

    match (Reshape _ sh1 a1) (Reshape _ sh2 a2)
      | Just Refl <- matchExp sh1 sh2
      , Just Refl <- matchAcc a1  a2
      = Just Refl

    match (Generate _ sh1 f1) (Generate _ sh2 f2)
      | Just Refl <- matchExp sh1 sh2
      , Just Refl <- matchFun f1  f2
      = Just Refl

    match (Transform _ sh1 ix1 f1 a1) (Transform _ sh2 ix2 f2 a2)
      | Just Refl <- matchExp sh1 sh2
      , Just Refl <- matchFun ix1 ix2
      , Just Refl <- matchFun f1  f2
      , Just Refl <- matchAcc a1  a2
      = Just Refl

    match (Replicate si1 ix1 a1) (Replicate si2 ix2 a2)
      | Just Refl <- matchSliceIndex si1 si2
      , Just Refl <- matchExp ix1 ix2
      , Just Refl <- matchAcc a1  a2
      = Just Refl

    match (Slice si1 a1 ix1) (Slice si2 a2 ix2)
      | Just Refl <- matchSliceIndex si1 si2
      , Just Refl <- matchAcc a1  a2
      , Just Refl <- matchExp ix1 ix2
      = Just Refl

    match (Map _ f1 a1) (Map _ f2 a2)
      | Just Refl <- matchFun f1 f2
      , Just Refl <- matchAcc a1 a2
      = Just Refl

    match (ZipWith _ f1 a1 b1) (ZipWith _ f2 a2 b2)
      | Just Refl <- matchFun f1 f2
      , Just Refl <- matchAcc a1 a2
      , Just Refl <- matchAcc b1 b2
      = Just Refl

    match (Fold f1 z1 a1) (Fold f2 z2 a2)
      | Just Refl <- matchFun f1 f2
      , matchMaybe matchExp z1 z2
      , Just Refl <- matchAcc a1 a2
      = Just Refl

    match (FoldSeg _ f1 z1 a1 s1) (FoldSeg _ f2 z2 a2 s2)
      | Just Refl <- matchFun f1 f2
      , matchMaybe matchExp z1 z2
      , Just Refl <- matchAcc a1 a2
      , Just Refl <- matchAcc s1 s2
      = Just Refl

    match (Scan d1 f1 z1 a1) (Scan d2 f2 z2 a2)
      | d1 == d2
      , Just Refl <- matchFun f1 f2
      , matchMaybe matchExp z1 z2
      , Just Refl <- matchAcc a1 a2
      = Just Refl

    match (Scan' d1 f1 z1 a1) (Scan' d2 f2 z2 a2)
      | d1 == d2
      , Just Refl <- matchFun f1 f2
      , Just Refl <- matchExp z1 z2
      , Just Refl <- matchAcc a1 a2
      = Just Refl

    match (Permute f1 d1 p1 a1) (Permute f2 d2 p2 a2)
      | Just Refl <- matchFun f1 f2
      , Just Refl <- matchAcc d1 d2
      , Just Refl <- matchFun p1 p2
      , Just Refl <- matchAcc a1 a2
      = Just Refl

    match (Backpermute _ sh1 ix1 a1) (Backpermute _ sh2 ix2 a2)
      | Just Refl <- matchExp sh1 sh2
      , Just Refl <- matchFun ix1 ix2
      , Just Refl <- matchAcc a1  a2
      = Just Refl

    match (Stencil s1 _ f1 b1 a1) (Stencil _ _ f2 b2 a2)
      | Just Refl <- matchFun f1 f2
      , Just Refl <- matchAcc a1 a2
      , matchBoundary (stencilEltR s1) b1 b2
      = Just Refl

    match (Stencil2 s1 s2 _ f1 b1  a1  b2  a2) (Stencil2 _ _ _ f2 b1' a1' b2' a2')
      | Just Refl <- matchFun f1 f2
      , Just Refl <- matchAcc a1 a1'
      , Just Refl <- matchAcc a2 a2'
      , matchBoundary (stencilEltR s1) b1 b1'
      , matchBoundary (stencilEltR s2) b2 b2'
      = Just Refl

    -- match (Collect s1) (Collect s2)
    --   = matchSeq matchAcc encodeAcc s1 s2

    match _ _
      = Nothing

-- Array functions
--
{-# INLINEABLE matchPreOpenAfun #-}
matchPreOpenAfun
    :: MatchAcc acc
    -> PreOpenAfun acc aenv s
    -> PreOpenAfun acc aenv t
    -> Maybe (s :~: t)
matchPreOpenAfun m (Alam lhs1 s) (Alam lhs2 t)
  | Just Refl <- matchALeftHandSide lhs1 lhs2
  , Just Refl <- matchPreOpenAfun m s t
  = Just Refl

matchPreOpenAfun m (Abody s) (Abody t) = m s t
matchPreOpenAfun _ _           _           = Nothing

matchALeftHandSide
    :: forall aenv aenv1 aenv2 t1 t2.
       ALeftHandSide t1 aenv aenv1
    -> ALeftHandSide t2 aenv aenv2
    -> Maybe (ALeftHandSide t1 aenv aenv1 :~: ALeftHandSide t2 aenv aenv2)
matchALeftHandSide = matchLeftHandSide matchArrayR

matchELeftHandSide
    :: forall env env1 env2 t1 t2.
       ELeftHandSide t1 env env1
    -> ELeftHandSide t2 env env2
    -> Maybe (ELeftHandSide t1 env env1 :~: ELeftHandSide t2 env env2)
matchELeftHandSide = matchLeftHandSide matchScalarType

matchLeftHandSide
    :: forall s env env1 env2 t1 t2.
      (forall x y. s x -> s y -> Maybe (x :~: y))
    -> LeftHandSide s t1 env env1
    -> LeftHandSide s t2 env env2
    -> Maybe (LeftHandSide s t1 env env1 :~: LeftHandSide s t2 env env2)
matchLeftHandSide f (LeftHandSideWildcard repr1) (LeftHandSideWildcard repr2)
  | Just Refl <- matchTupR f repr1 repr2
  = Just Refl
matchLeftHandSide f (LeftHandSideSingle x) (LeftHandSideSingle y)
  | Just Refl <- f x y
  = Just Refl
matchLeftHandSide f (LeftHandSidePair a1 a2) (LeftHandSidePair b1 b2)
  | Just Refl <- matchLeftHandSide f a1 b1
  , Just Refl <- matchLeftHandSide f a2 b2
  = Just Refl
matchLeftHandSide _ _ _ = Nothing

-- Match stencil boundaries
--
matchBoundary
    :: TypeR t
    -> Boundary aenv (Array sh t)
    -> Boundary aenv (Array sh t)
    -> Bool
matchBoundary _  Clamp        Clamp        = True
matchBoundary _  Mirror       Mirror       = True
matchBoundary _  Wrap         Wrap         = True
matchBoundary tp (Constant s) (Constant t) = matchConst tp s t
matchBoundary _  (Function f) (Function g)
  | Just Refl <- matchOpenFun f g
  = True
matchBoundary _ _ _
  = False

matchMaybe :: (s1 -> s2 -> Maybe (t1 :~: t2)) -> Maybe s1 -> Maybe s2 -> Bool
matchMaybe _ Nothing  Nothing  = True
matchMaybe f (Just x) (Just y)
  | Just Refl <- f x y         = True
matchMaybe _ _        _        = False

{--
-- Match sequences
--
matchSeq
    :: forall acc aenv senv s t.
       MatchAcc  acc
    -> EncodeAcc acc
    -> PreOpenSeq acc aenv senv s
    -> PreOpenSeq acc aenv senv t
    -> Maybe (s :~: t)
matchSeq m h = match
  where
    matchFun :: OpenFun env' aenv' u -> OpenFun env' aenv' v -> Maybe (u :~: v)
    matchFun = matchOpenFun m h

    matchExp :: OpenExp env' aenv' u -> OpenExp env' aenv' v -> Maybe (u :~: v)
    matchExp = matchOpenExp m h

    match :: PreOpenSeq acc aenv senv' u -> PreOpenSeq acc aenv senv' v -> Maybe (u :~: v)
    match (Producer p1 s1)   (Producer p2 s2)
      | Just Refl <- matchP p1 p2
      , Just Refl <- match s1 s2
      = Just Refl
    match (Consumer c1)   (Consumer c2)
      | Just Refl <- matchC c1 c2
      = Just Refl
    match (Reify ix1) (Reify ix2)
      | Just Refl <- matchIdx ix1 ix2
      = Just Refl
    match _ _
      = Nothing

    matchP :: Producer acc aenv senv' u -> Producer acc aenv senv' v -> Maybe (u :~: v)
    matchP (StreamIn arrs1) (StreamIn arrs2)
      | unsafePerformIO $ do
          sn1 <- makeStableName arrs1
          sn2 <- makeStableName arrs2
          return $! hashStableName sn1 == hashStableName sn2
      = gcast Refl
    matchP (ToSeq _ (_::proxy1 slix1) a1) (ToSeq _ (_::proxy2 slix2) a2)
      | Just Refl <- gcast Refl :: Maybe (slix1 :~: slix2) -- Divisions are singleton.
      , Just Refl <- m a1 a2
      = gcast Refl
    matchP (MapSeq f1 x1) (MapSeq f2 x2)
      | Just Refl <- matchPreOpenAfun m f1 f2
      , Just Refl <- matchIdx x1 x2
      = Just Refl
    matchP (ZipWithSeq f1 x1 y1) (ZipWithSeq f2 x2 y2)
      | Just Refl <- matchPreOpenAfun m f1 f2
      , Just Refl <- matchIdx x1 x2
      , Just Refl <- matchIdx y1 y2
      = Just Refl
    matchP (ScanSeq f1 e1 x1) (ScanSeq f2 e2 x2)
      | Just Refl <- matchFun f1 f2
      , Just Refl <- matchIdx x1 x2
      , Just Refl <- matchExp e1 e2
      = Just Refl
    matchP _ _
      = Nothing

    matchC :: Consumer acc aenv senv' u -> Consumer acc aenv senv' v -> Maybe (u :~: v)
    matchC (FoldSeq f1 e1 x1) (FoldSeq f2 e2 x2)
      | Just Refl <- matchIdx x1 x2
      , Just Refl <- matchFun f1 f2
      , Just Refl <- matchExp e1 e2
      = Just Refl
    matchC (FoldSeqFlatten f1 acc1 x1) (FoldSeqFlatten f2 acc2 x2)
      | Just Refl <- matchIdx x1 x2
      , Just Refl <- matchPreOpenAfun m f1 f2
      , Just Refl <- m acc1 acc2
      = Just Refl
    matchC (Stuple s1) (Stuple s2)
      | Just Refl <- matchAtuple matchC s1 s2
      = gcast Refl
    matchC _ _
      = Nothing
--}

-- Match arrays
--
-- As a convenience, we are just comparing the stable names, but we could also
-- walk the structure comparing the underlying ptrsOfArrayData.
--
matchArray :: ArrayR (Array sh1 e1)
           -> ArrayR (Array sh2 e2)
           -> Array sh1 e1
           -> Array sh2 e2
           -> Maybe (Array sh1 e1 :~: Array sh2 e2)
matchArray repr1 repr2 (Array _ ad1) (Array _ ad2)
  | Just Refl <- matchArrayR repr1 repr2
  , unsafePerformIO $ do
      sn1 <- makeStableName ad1
      sn2 <- makeStableName ad2
      return $! hashStableName sn1 == hashStableName sn2
  = Just Refl

matchArray _ _ _ _
  = Nothing

matchTupR :: (forall u1 u2. s u1 -> s u2 -> Maybe (u1 :~: u2)) -> TupR s t1 -> TupR s t2 -> Maybe (t1 :~: t2)
matchTupR _ TupRunit         TupRunit         = Just Refl
matchTupR f (TupRsingle x)   (TupRsingle y)   = f x y
matchTupR f (TupRpair x1 x2) (TupRpair y1 y2)
  | Just Refl <- matchTupR f x1 y1
  , Just Refl <- matchTupR f x2 y2            = Just Refl
matchTupR _ _                _                = Nothing

matchArraysR :: ArraysR s -> ArraysR t -> Maybe (s :~: t)
matchArraysR = matchTupR matchArrayR

matchArrayR :: ArrayR s -> ArrayR t -> Maybe (s :~: t)
matchArrayR (ArrayR shr1 tp1) (ArrayR shr2 tp2)
  | Just Refl <- matchShapeR shr1 shr2
  , Just Refl <- matchTypeR tp1 tp2 = Just Refl
matchArrayR _ _ = Nothing


-- Compute the congruence of two scalar expressions. Two nodes are congruent if
-- either:
--
--  1. The nodes label constants and the contents are equal
--  2. They have the same operator and their operands are congruent
--
-- The below attempts to use real typed equality, but occasionally still needs
-- to use a cast, particularly when we can only match the representation types.
--
{-# INLINEABLE matchOpenExp #-}
matchOpenExp
    :: forall env aenv s t.
       OpenExp env aenv s
    -> OpenExp env aenv t
    -> Maybe (s :~: t)

matchOpenExp (Let lhs1 x1 e1) (Let lhs2 x2 e2)
  | Just Refl <- matchELeftHandSide lhs1 lhs2
  , Just Refl <- matchOpenExp x1 x2
  , Just Refl <- matchOpenExp e1 e2
  = Just Refl

matchOpenExp (Evar v1) (Evar v2)
  = matchVar v1 v2

matchOpenExp (Foreign _ ff1 f1 e1) (Foreign _ ff2 f2 e2)
  | Just Refl <- matchOpenExp e1 e2
  , unsafePerformIO $ do
      sn1 <- makeStableName ff1
      sn2 <- makeStableName ff2
      return $! hashStableName sn1 == hashStableName sn2
  , Just Refl <- matchOpenFun f1 f2
  = Just Refl

matchOpenExp (Const _ t1 c1) (Const _ t2 c2)
  | Just Refl <- matchScalarType t1 t2
  , matchConst (TupRsingle t1) c1 c2
  = Just Refl

matchOpenExp (Undef t1) (Undef t2) = matchScalarType t1 t2

matchOpenExp (Coerce _ t1 e1) (Coerce _ t2 e2)
  | Just Refl <- matchScalarType t1 t2
  , Just Refl <- matchOpenExp e1 e2
  = Just Refl

matchOpenExp (Pair _ a1 b1) (Pair _ a2 b2)
  | Just Refl <- matchOpenExp a1 a2
  , Just Refl <- matchOpenExp b1 b2
  = Just Refl

matchOpenExp (Nil _) (Nil _)
  = Just Refl

matchOpenExp (IndexSlice sliceIndex1 ix1 sh1) (IndexSlice sliceIndex2 ix2 sh2)
  | Just Refl <- matchOpenExp ix1 ix2
  , Just Refl <- matchOpenExp sh1 sh2
  , Just Refl <- matchSliceIndex sliceIndex1 sliceIndex2
  = Just Refl

matchOpenExp (IndexFull sliceIndex1 ix1 sl1) (IndexFull sliceIndex2 ix2 sl2)
  | Just Refl <- matchOpenExp ix1 ix2
  , Just Refl <- matchOpenExp sl1 sl2
  , Just Refl <- matchSliceIndex sliceIndex1 sliceIndex2
  = Just Refl

matchOpenExp (ToIndex _ sh1 i1) (ToIndex _ sh2 i2)
  | Just Refl <- matchOpenExp sh1 sh2
  , Just Refl <- matchOpenExp i1  i2
  = Just Refl

matchOpenExp (FromIndex _ sh1 i1) (FromIndex _ sh2 i2)
  | Just Refl <- matchOpenExp i1  i2
  , Just Refl <- matchOpenExp sh1 sh2
  = Just Refl

matchOpenExp (Cond p1 t1 e1) (Cond p2 t2 e2)
  | Just Refl <- matchOpenExp p1 p2
  , Just Refl <- matchOpenExp t1 t2
  , Just Refl <- matchOpenExp e1 e2
  = Just Refl

matchOpenExp (While p1 f1 x1) (While p2 f2 x2)
  | Just Refl <- matchOpenExp x1 x2
  , Just Refl <- matchOpenFun p1 p2
  , Just Refl <- matchOpenFun f1 f2
  = Just Refl

matchOpenExp (PrimConst c1) (PrimConst c2)
  = matchPrimConst c1 c2

matchOpenExp (PrimApp f1 x1) (PrimApp f2 x2)
  | Just x1'  <- commutes f1 x1
  , Just x2'  <- commutes f2 x2
  , Just Refl <- matchOpenExp x1' x2'
  , Just Refl <- matchPrimFun f1  f2
  = Just Refl

  | Just Refl <- matchOpenExp x1 x2
  , Just Refl <- matchPrimFun f1 f2
  = Just Refl

matchOpenExp (Index a1 x1) (Index a2 x2)
  | Just Refl <- matchVar a1 a2
  , Just Refl <- matchOpenExp x1 x2
  = Just Refl

matchOpenExp (LinearIndex a1 x1) (LinearIndex a2 x2)
  | Just Refl <- matchVar a1 a2
  , Just Refl <- matchOpenExp x1 x2
  = Just Refl

matchOpenExp (Shape a1) (Shape a2)
  | Just Refl <- matchVar a1 a2
  = Just Refl

matchOpenExp (ShapeSize _ sh1) (ShapeSize _ sh2)
  | Just Refl <- matchOpenExp sh1 sh2
  = Just Refl

matchOpenExp _ _
  = Nothing


-- Match scalar functions
--
{-# INLINEABLE matchOpenFun #-}
matchOpenFun
    :: OpenFun env aenv s
    -> OpenFun env aenv t
    -> Maybe (s :~: t)
matchOpenFun (Lam lhs1 s) (Lam lhs2 t)
  | Just Refl <- matchELeftHandSide lhs1 lhs2
  , Just Refl <- matchOpenFun s t
  = Just Refl

matchOpenFun (Body s) (Body t) = matchOpenExp s t
matchOpenFun _        _        = Nothing

-- Matching constants
--
matchConst :: TypeR a -> a -> a -> Bool
matchConst TupRunit         ()      ()      = True
matchConst (TupRsingle ty)  a       b       = evalEq ty (a,b)
matchConst (TupRpair ta tb) (a1,b1) (a2,b2) = matchConst ta a1 a2 && matchConst tb b1 b2

evalEq :: ScalarType a -> (a, a) -> Bool
evalEq (SingleScalarType t) = evalEqSingle t
evalEq (VectorScalarType t) = evalEqVector t

evalEqSingle :: SingleType a -> (a, a) -> Bool
evalEqSingle (NumSingleType t) = evalEqNum t

evalEqVector :: VectorType a -> (a, a) -> Bool
evalEqVector VectorType{} = uncurry (==)

evalEqNum :: NumType a -> (a, a) -> Bool
evalEqNum (IntegralNumType t) | IntegralDict <- integralDict t  = uncurry (==)
evalEqNum (FloatingNumType t) | FloatingDict <- floatingDict t  = uncurry (==)


-- Environment projection indices
--
{-# INLINEABLE matchIdx #-}
matchIdx :: Idx env s -> Idx env t -> Maybe (s :~: t)
matchIdx ZeroIdx     ZeroIdx     = Just Refl
matchIdx (SuccIdx u) (SuccIdx v) = matchIdx u v
matchIdx _           _           = Nothing

{-# INLINEABLE matchVar #-}
matchVar :: Var s env t1 -> Var s env t2 -> Maybe (t1 :~: t2)
matchVar (Var _ v1) (Var _ v2) = matchIdx v1 v2

{-# INLINEABLE matchVars #-}
matchVars :: Vars s env t1 -> Vars s env t2 -> Maybe (t1 :~: t2)
matchVars TupRunit         TupRunit = Just Refl
matchVars (TupRsingle v1) (TupRsingle v2)
  | Just Refl <- matchVar v1 v2 = Just Refl
matchVars (TupRpair v w) (TupRpair x y)
  | Just Refl <- matchVars v x
  , Just Refl <- matchVars w y  = Just Refl
matchVars _ _ = Nothing


-- Slice specifications
--
matchSliceIndex :: SliceIndex slix1 sl1 co1 sh1 -> SliceIndex slix2 sl2 co2 sh2 -> Maybe (SliceIndex slix1 sl1 co1 sh1 :~: SliceIndex slix2 sl2 co2 sh2)
matchSliceIndex SliceNil SliceNil
  = Just Refl

matchSliceIndex (SliceAll   sl1) (SliceAll   sl2)
  | Just Refl <- matchSliceIndex sl1 sl2
  = Just Refl

matchSliceIndex (SliceFixed sl1) (SliceFixed sl2)
  | Just Refl <- matchSliceIndex sl1 sl2
  = Just Refl

matchSliceIndex _ _
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
{-# INLINEABLE matchPrimFun #-}
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
matchPrimFun (PrimIsInfinite _)         (PrimIsInfinite _)         = Just Refl
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
matchPrimFun PrimLAnd                   PrimLAnd                   = Just Refl
matchPrimFun PrimLOr                    PrimLOr                    = Just Refl
matchPrimFun PrimLNot                   PrimLNot                   = Just Refl

matchPrimFun _ _
  = Nothing


-- Contravariant function matching
--
{-# INLINEABLE matchPrimFun' #-}
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
matchPrimFun' (PrimIsInfinite s)         (PrimIsInfinite t)         = matchFloatingType s t
matchPrimFun' (PrimMax _)                (PrimMax _)                = Just Refl
matchPrimFun' (PrimMin _)                (PrimMin _)                = Just Refl
matchPrimFun' (PrimFromIntegral s _)     (PrimFromIntegral t _)     = matchIntegralType s t
matchPrimFun' (PrimToFloating s _)       (PrimToFloating t _)       = matchNumType s t
matchPrimFun' PrimLAnd                   PrimLAnd                   = Just Refl
matchPrimFun' PrimLOr                    PrimLOr                    = Just Refl
matchPrimFun' PrimLNot                   PrimLNot                   = Just Refl

matchPrimFun' (PrimLt s) (PrimLt t)
  | Just Refl <- matchSingleType s t
  = Just Refl

matchPrimFun' (PrimGt s) (PrimGt t)
  | Just Refl <- matchSingleType s t
  = Just Refl

matchPrimFun' (PrimLtEq s) (PrimLtEq t)
  | Just Refl <- matchSingleType s t
  = Just Refl

matchPrimFun' (PrimGtEq s) (PrimGtEq t)
  | Just Refl <- matchSingleType s t
  = Just Refl

matchPrimFun' (PrimEq s) (PrimEq t)
  | Just Refl <- matchSingleType s t
  = Just Refl

matchPrimFun' (PrimNEq s) (PrimNEq t)
  | Just Refl <- matchSingleType s t
  = Just Refl

matchPrimFun' _ _
  = Nothing


-- Match reified types
--
{-# INLINEABLE matchTypeR #-}
matchTypeR :: TypeR s -> TypeR t -> Maybe (s :~: t)
matchTypeR = matchTupR matchScalarType


-- Match shapes (dimensionality)
--
-- XXX: Matching shapes is sort of a special case because the representation
-- types really are isomorphic to the surface type. However, 'gcast' does not
-- inline here, meaning that it will always do the fingerprint check, even if
-- the dimensions are known statically and thus the check could be elided as
-- a known branch.
--
{-# INLINEABLE matchShapeType #-}
matchShapeType :: forall s t. (Sugar.Shape s, Sugar.Shape t) => Maybe (s :~: t)
matchShapeType
  | Just Refl <- matchShapeR (Sugar.shapeR @s) (Sugar.shapeR @t)
#ifdef ACCELERATE_INTERNAL_CHECKS
  = gcast Refl
#else
  = Just (unsafeCoerce Refl)
#endif
  | otherwise
  = Nothing

{-# INLINEABLE matchShapeR #-}
matchShapeR :: forall s t. ShapeR s -> ShapeR t -> Maybe (s :~: t)
matchShapeR ShapeRz ShapeRz = Just Refl
matchShapeR (ShapeRsnoc shr1) (ShapeRsnoc shr2)
  | Just Refl <- matchShapeR shr1 shr2
  = Just Refl
matchShapeR _ _ = Nothing


-- Match reified type dictionaries
--
{-# INLINEABLE matchScalarType #-}
matchScalarType :: ScalarType s -> ScalarType t -> Maybe (s :~: t)
matchScalarType (SingleScalarType s) (SingleScalarType t) = matchSingleType s t
matchScalarType (VectorScalarType s) (VectorScalarType t) = matchVectorType s t
matchScalarType _                    _                    = Nothing

{-# INLINEABLE matchSingleType #-}
matchSingleType :: SingleType s -> SingleType t -> Maybe (s :~: t)
matchSingleType (NumSingleType s) (NumSingleType t) = matchNumType s t

{-# INLINEABLE matchVectorType #-}
matchVectorType :: forall m n s t. VectorType (Vec n s) -> VectorType (Vec m t) -> Maybe (Vec n s :~: Vec m t)
matchVectorType (VectorType n s) (VectorType m t)
  | Just Refl <- if n == m
                   then Just (unsafeCoerce Refl :: n :~: m) -- XXX: we don't have an embedded KnownNat constraint, but
                   else Nothing                             -- this implementation is the same as 'GHC.TypeLits.sameNat'
  , Just Refl <- matchSingleType s t
  = Just Refl
matchVectorType _ _
  = Nothing

{-# INLINEABLE matchNumType #-}
matchNumType :: NumType s -> NumType t -> Maybe (s :~: t)
matchNumType (IntegralNumType s) (IntegralNumType t) = matchIntegralType s t
matchNumType (FloatingNumType s) (FloatingNumType t) = matchFloatingType s t
matchNumType _                   _                   = Nothing

{-# INLINEABLE matchBoundedType #-}
matchBoundedType :: BoundedType s -> BoundedType t -> Maybe (s :~: t)
matchBoundedType (IntegralBoundedType s) (IntegralBoundedType t) = matchIntegralType s t

{-# INLINEABLE matchIntegralType #-}
matchIntegralType :: IntegralType s -> IntegralType t -> Maybe (s :~: t)
matchIntegralType TypeInt    TypeInt    = Just Refl
matchIntegralType TypeInt8   TypeInt8   = Just Refl
matchIntegralType TypeInt16  TypeInt16  = Just Refl
matchIntegralType TypeInt32  TypeInt32  = Just Refl
matchIntegralType TypeInt64  TypeInt64  = Just Refl
matchIntegralType TypeWord   TypeWord   = Just Refl
matchIntegralType TypeWord8  TypeWord8  = Just Refl
matchIntegralType TypeWord16 TypeWord16 = Just Refl
matchIntegralType TypeWord32 TypeWord32 = Just Refl
matchIntegralType TypeWord64 TypeWord64 = Just Refl
matchIntegralType _            _            = Nothing

{-# INLINEABLE matchFloatingType #-}
matchFloatingType :: FloatingType s -> FloatingType t -> Maybe (s :~: t)
matchFloatingType TypeHalf   TypeHalf   = Just Refl
matchFloatingType TypeFloat  TypeFloat  = Just Refl
matchFloatingType TypeDouble TypeDouble = Just Refl
matchFloatingType _            _            = Nothing


-- Auxiliary
-- ---------

-- Discriminate binary functions that commute, and if so return the operands in
-- a stable ordering such that matching recognises expressions modulo
-- commutativity.
--
commutes
    :: forall env aenv a r.
       PrimFun (a -> r)
    -> OpenExp env aenv a
    -> Maybe (OpenExp env aenv a)
commutes f x = case f of
  PrimAdd{}     -> Just (swizzle x)
  PrimMul{}     -> Just (swizzle x)
  PrimBAnd{}    -> Just (swizzle x)
  PrimBOr{}     -> Just (swizzle x)
  PrimBXor{}    -> Just (swizzle x)
  PrimEq{}      -> Just (swizzle x)
  PrimNEq{}     -> Just (swizzle x)
  PrimMax{}     -> Just (swizzle x)
  PrimMin{}     -> Just (swizzle x)
  PrimLAnd      -> Just (swizzle x)
  PrimLOr       -> Just (swizzle x)
  _             -> Nothing
  where
    swizzle :: OpenExp env aenv (a',a') -> OpenExp env aenv (a',a')
    swizzle exp
      | (Pair ann a b) <- exp
      , hashOpenExp a > hashOpenExp b = Pair ann b a
      --
      | otherwise                               = exp

