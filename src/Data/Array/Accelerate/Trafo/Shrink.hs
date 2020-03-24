{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo.Shrink
-- Copyright   : [2012..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The shrinking substitution arises as a restriction of beta-reduction to cases
-- where the bound variable is used zero (dead-code elimination) or one (linear
-- inlining) times. By simplifying terms, the shrinking reduction can expose
-- opportunities for further optimisation.
--
-- TODO: replace with a linear shrinking algorithm; e.g.
--
--   * Andrew Appel & Trevor Jim, "Shrinking lambda expressions in linear time".
--
--   * Nick Benton, Andrew Kennedy, Sam Lindley and Claudio Russo, "Shrinking
--     Reductions in SML.NET"
--

module Data.Array.Accelerate.Trafo.Shrink (

  -- Shrinking
  Shrink(..),
  ShrinkAcc,

  -- Occurrence counting
  UsesOfAcc, usesOfPreAcc, usesOfExp,

) where

-- standard library
import Data.Monoid
import Control.Applicative                              hiding ( Const )
import Prelude                                          hiding ( exp, seq )

-- friends
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Array.Sugar               hiding ( Any )
import Data.Array.Accelerate.Trafo.Base
import Data.Array.Accelerate.Trafo.Substitution

import qualified Data.Array.Accelerate.Debug.Stats      as Stats


class Shrink f where
  shrink  :: f -> f
  shrink' :: f -> (Bool, f)

  shrink = snd . shrink'

instance Kit acc => Shrink (PreOpenExp acc env aenv e) where
  shrink' = shrinkExp

instance Kit acc => Shrink (PreOpenFun acc env aenv f) where
  shrink' = shrinkFun

data VarsRange = VarsRange !Int !Int !RangeTuple -- first, count, tuple

data RangeTuple
  = RTNil
  | RTSingle
  | RTPair !RangeTuple !RangeTuple

lhsVarsRange :: LeftHandSide s v env env' -> Maybe VarsRange
lhsVarsRange (LeftHandSideWildcard TupRunit)    = Just $ VarsRange 0 0           RTNil
lhsVarsRange (LeftHandSideSingle _)             = Just $ VarsRange 0 1           RTSingle
lhsVarsRange (LeftHandSidePair l1 l2)
  | Just (VarsRange _ n1 t1) <- lhsVarsRange l1
  , Just (VarsRange _ n2 t2) <- lhsVarsRange l2 = Just $ VarsRange 0 (n1 + n2) $ RTPair t1 t2
lhsVarsRange _                                  = Nothing

lhsSize :: LeftHandSide s v env env' -> Int
lhsSize (LeftHandSideWildcard _) = 0
lhsSize (LeftHandSideSingle _)   = 1
lhsSize (LeftHandSidePair l1 l2) = lhsSize l1 + lhsSize l2

weakenVarsRange :: LeftHandSide s v env env' -> VarsRange -> VarsRange
weakenVarsRange lhs (VarsRange i n t) = VarsRange (i + lhsSize lhs) n t

matchEVarsRange :: VarsRange -> PreOpenExp acc env aenv t -> Bool
matchEVarsRange (VarsRange _  _ RTNil)    Nil                = True
matchEVarsRange (VarsRange i' _ RTSingle) (Evar (Var _ ix')) = go i' ix'
  where
    go :: Int -> Idx env t ->  Bool
    go 0 ZeroIdx = True
    go i (SuccIdx ix) = go (i - 1) ix
    go _ _ = False
matchEVarsRange (VarsRange i _ (RTPair t1 t2)) (Pair e1 e2)
  =  matchEVarsRange (VarsRange i 0 t1) e1
  && matchEVarsRange (VarsRange i 0 t2) e2
matchEVarsRange _ _ = False

varInRange :: VarsRange -> Var s env t -> Bool
varInRange (VarsRange i n _) (Var _ ix) = i <= j && j < i + n
  where
    j = idxToInt ix

data Count
  = Impossible -- Cannot inline this definition. This happens when the definition declares multiple variables (the right hand side returns a tuple) and the variables are used seperately.
  | Infinity   -- The variable is used in a loop. Inlining should only proceed if the computation is cheap.
  | Finite {-# UNPACK #-} !Int

instance Semigroup Count where
  Impossible <> _          = Impossible
  _          <> Impossible = Impossible
  Infinity   <> _          = Infinity
  _          <> Infinity   = Infinity
  Finite a   <> Finite b   = Finite $ a + b

loopCount :: Count -> Count
loopCount (Finite n) | n > 0 = Infinity
loopCount c                  = c

-- Shrinking
-- =========

-- The shrinking substitution for scalar expressions. This is a restricted
-- instance of beta-reduction to cases where the bound variable is used zero
-- (dead-code elimination) or one (linear inlining) times.
--
shrinkExp :: Kit acc => PreOpenExp acc env aenv t -> (Bool, PreOpenExp acc env aenv t)
shrinkExp = Stats.substitution "shrinkE" . first getAny . shrinkE
  where
    -- If the bound variable is used at most this many times, it will be inlined
    -- into the body. In cases where it is not used at all, this is equivalent
    -- to dead-code elimination.
    --
    lIMIT :: Int
    lIMIT = 1

    cheap :: PreOpenExp acc env aenv t -> Bool
    cheap (Evar _) = True
    cheap (Pair e1 e2) = cheap e1 && cheap e2
    cheap Nil = True
    cheap Const{} = True
    cheap PrimConst{} = True
    cheap Undef{} = True
    cheap (Coerce _ _ e) = cheap e
    cheap _ = False

    shrinkE :: Kit acc => PreOpenExp acc env aenv t -> (Any, PreOpenExp acc env aenv t)
    shrinkE exp = case exp of
      Let (LeftHandSideSingle _) bnd@Evar{} body -> Stats.inline "Var"   . yes $ shrinkE (inline body bnd)
      Let lhs bnd body
        | shouldInline -> case inlineVars lhs (snd body') (snd bnd') of
            Just inlined -> Stats.betaReduce msg . yes $ shrinkE inlined
            _            -> error "shrinkExp: Unexpected failure while trying to inline some expression."
        | otherwise    -> Let lhs <$> bnd' <*> body'
        where
          shouldInline = case uses of
            Finite n   -> n <= lIMIT || cheap (snd bnd')
            Infinity   ->               cheap (snd bnd')
            Impossible -> False

          bnd'  = shrinkE bnd
          body' = shrinkE body
          uses  = case lhsVarsRange lhs of
            Nothing    -> Impossible
            Just range -> usesOfExp range (snd body')

          msg = case uses of
            Finite 0 -> "dead exp"
            _        -> "inline exp"   -- forced inlining when lIMIT > 1
      --
      Evar v                    -> pure (Evar v)
      Const t c                 -> pure (Const t c)
      Undef t                   -> pure (Undef t)
      Nil                       -> pure Nil
      Pair x y                  -> Pair <$> shrinkE x <*> shrinkE y
      VecPack   vec e           -> VecPack   vec <$> shrinkE e
      VecUnpack vec e           -> VecUnpack vec <$> shrinkE e
      IndexSlice x ix sh        -> IndexSlice x <$> shrinkE ix <*> shrinkE sh
      IndexFull x ix sl         -> IndexFull x <$> shrinkE ix <*> shrinkE sl
      ToIndex shr sh ix         -> ToIndex shr <$> shrinkE sh <*> shrinkE ix
      FromIndex shr sh i        -> FromIndex shr <$> shrinkE sh <*> shrinkE i
      Cond p t e                -> Cond <$> shrinkE p <*> shrinkE t <*> shrinkE e
      While p f x               -> While <$> shrinkF p <*> shrinkF f <*> shrinkE x
      PrimConst c               -> pure (PrimConst c)
      PrimApp f x               -> PrimApp f <$> shrinkE x
      Index a sh                -> Index a <$> shrinkE sh
      LinearIndex a i           -> LinearIndex a <$> shrinkE i
      Shape a                   -> pure (Shape a)
      ShapeSize shr sh          -> ShapeSize shr <$> shrinkE sh
      Foreign ff f e            -> Foreign ff <$> shrinkF f <*> shrinkE e
      Coerce t1 t2 e            -> Coerce t1 t2 <$> shrinkE e

    shrinkF :: Kit acc => PreOpenFun acc env aenv t -> (Any, PreOpenFun acc env aenv t)
    shrinkF = first Any . shrinkFun

    first :: (a -> a') -> (a,b) -> (a',b)
    first f (x,y) = (f x, y)

    yes :: (Any, x) -> (Any, x)
    yes (_, x) = (Any True, x)

shrinkFun :: Kit acc => PreOpenFun acc env aenv f -> (Bool, PreOpenFun acc env aenv f)
shrinkFun (Lam l f) = Lam l <$> shrinkFun f
shrinkFun (Body  b) = Body  <$> shrinkExp b

-- The shrinking substitution for array computations. This is further limited to
-- dead-code elimination only, primarily because linear inlining may inline
-- array computations into scalar expressions, which is generally not desirable.
--
type ShrinkAcc acc = forall aenv a.   acc aenv a -> acc aenv a

{-
type ReduceAcc acc = forall aenv s t. acc aenv s -> acc (aenv,s) t -> Maybe (PreOpenAcc acc aenv t)

shrinkPreAcc
    :: forall acc aenv arrs. ShrinkAcc acc -> ReduceAcc acc
    -> PreOpenAcc acc aenv arrs
    -> PreOpenAcc acc aenv arrs
shrinkPreAcc shrinkAcc reduceAcc = Stats.substitution "shrinkA" shrinkA
  where
    shrinkA :: PreOpenAcc acc aenv' a -> PreOpenAcc acc aenv' a
    shrinkA pacc = case pacc of
      Alet lhs bnd body
        | Just reduct <- reduceAcc bnd' body'   -> shrinkA reduct
        | otherwise                             -> Alet lhs bnd' body'
        where
          bnd'  = shrinkAcc bnd
          body' = shrinkAcc body
      --
      Avar ix                   -> Avar ix
      Apair a1 a2               -> Apair (shrinkAcc a1) (shrinkAcc a2)
      Anil                      -> Anil
      Apply repr f a            -> Apply repr (shrinkAF f) (shrinkAcc a)
      Aforeign ff af a          -> Aforeign ff af (shrinkAcc a)
      Acond p t e               -> Acond (shrinkE p) (shrinkAcc t) (shrinkAcc e)
      Awhile p f a              -> Awhile (shrinkAF p) (shrinkAF f) (shrinkAcc a)
      Use repr a                -> Use repr a
      Unit e                    -> Unit (shrinkE e)
      Reshape e a               -> Reshape (shrinkE e) (shrinkAcc a)
      Generate e f              -> Generate (shrinkE e) (shrinkF f)
      Transform sh ix f a       -> Transform (shrinkE sh) (shrinkF ix) (shrinkF f) (shrinkAcc a)
      Replicate sl slix a       -> Replicate sl (shrinkE slix) (shrinkAcc a)
      Slice sl a slix           -> Slice sl (shrinkAcc a) (shrinkE slix)
      Map f a                   -> Map (shrinkF f) (shrinkAcc a)
      ZipWith f a1 a2           -> ZipWith (shrinkF f) (shrinkAcc a1) (shrinkAcc a2)
      Fold f z a                -> Fold (shrinkF f) (shrinkE z) (shrinkAcc a)
      Fold1 f a                 -> Fold1 (shrinkF f) (shrinkAcc a)
      FoldSeg f z a b           -> FoldSeg (shrinkF f) (shrinkE z) (shrinkAcc a) (shrinkAcc b)
      Fold1Seg f a b            -> Fold1Seg (shrinkF f) (shrinkAcc a) (shrinkAcc b)
      Scanl f z a               -> Scanl (shrinkF f) (shrinkE z) (shrinkAcc a)
      Scanl' f z a              -> Scanl' (shrinkF f) (shrinkE z) (shrinkAcc a)
      Scanl1 f a                -> Scanl1 (shrinkF f) (shrinkAcc a)
      Scanr f z a               -> Scanr (shrinkF f) (shrinkE z) (shrinkAcc a)
      Scanr' f z a              -> Scanr' (shrinkF f) (shrinkE z) (shrinkAcc a)
      Scanr1 f a                -> Scanr1 (shrinkF f) (shrinkAcc a)
      Permute f1 a1 f2 a2       -> Permute (shrinkF f1) (shrinkAcc a1) (shrinkF f2) (shrinkAcc a2)
      Backpermute sh f a        -> Backpermute (shrinkE sh) (shrinkF f) (shrinkAcc a)
      Stencil f b a             -> Stencil (shrinkF f) b (shrinkAcc a)
      Stencil2 f b1 a1 b2 a2    -> Stencil2 (shrinkF f) b1 (shrinkAcc a1) b2 (shrinkAcc a2)
      -- Collect s                 -> Collect (shrinkS s)

{--
    shrinkS :: PreOpenSeq acc aenv' senv a -> PreOpenSeq acc aenv' senv a
    shrinkS seq =
      case seq of
        Producer p s -> Producer (shrinkP p) (shrinkS s)
        Consumer c   -> Consumer (shrinkC c)
        Reify ix     -> Reify ix

    shrinkP :: Producer acc aenv' senv a -> Producer acc aenv' senv a
    shrinkP p =
      case p of
        StreamIn arrs        -> StreamIn arrs
        ToSeq sl slix a      -> ToSeq sl slix (shrinkAcc a)
        MapSeq f x           -> MapSeq (shrinkAF f) x
        ChunkedMapSeq f x    -> ChunkedMapSeq (shrinkAF f) x
        ZipWithSeq f x y     -> ZipWithSeq (shrinkAF f) x y
        ScanSeq f e x        -> ScanSeq (shrinkF f) (shrinkE e) x

    shrinkC :: Consumer acc aenv' senv a -> Consumer acc aenv' senv a
    shrinkC c =
      case c of
        FoldSeq f e x        -> FoldSeq (shrinkF f) (shrinkE e) x
        FoldSeqFlatten f a x -> FoldSeqFlatten (shrinkAF f) (shrinkAcc a) x
        Stuple t             -> Stuple (shrinkCT t)

    shrinkCT :: Atuple (Consumer acc aenv' senv) t -> Atuple (Consumer acc aenv' senv) t
    shrinkCT NilAtup        = NilAtup
    shrinkCT (SnocAtup t c) = SnocAtup (shrinkCT t) (shrinkC c)
--}

    shrinkE :: PreOpenExp acc env aenv' t -> PreOpenExp acc env aenv' t
    shrinkE exp = case exp of
      Let bnd body              -> Let (shrinkE bnd) (shrinkE body)
      Var idx                   -> Var idx
      Const c                   -> Const c
      Undef                     -> Undef
      Tuple t                   -> Tuple (shrinkT t)
      Prj tup e                 -> Prj tup (shrinkE e)
      IndexNil                  -> IndexNil
      IndexCons sl sz           -> IndexCons (shrinkE sl) (shrinkE sz)
      IndexHead sh              -> IndexHead (shrinkE sh)
      IndexTail sh              -> IndexTail (shrinkE sh)
      IndexSlice x ix sh        -> IndexSlice x (shrinkE ix) (shrinkE sh)
      IndexFull x ix sl         -> IndexFull x (shrinkE ix) (shrinkE sl)
      IndexAny                  -> IndexAny
      ToIndex sh ix             -> ToIndex (shrinkE sh) (shrinkE ix)
      FromIndex sh i            -> FromIndex (shrinkE sh) (shrinkE i)
      Cond p t e                -> Cond (shrinkE p) (shrinkE t) (shrinkE e)
      While p f x               -> While (shrinkF p) (shrinkF f) (shrinkE x)
      PrimConst c               -> PrimConst c
      PrimApp f x               -> PrimApp f (shrinkE x)
      Index a sh                -> Index (shrinkAcc a) (shrinkE sh)
      LinearIndex a i           -> LinearIndex (shrinkAcc a) (shrinkE i)
      Shape a                   -> Shape (shrinkAcc a)
      ShapeSize sh              -> ShapeSize (shrinkE sh)
      Intersect sh sz           -> Intersect (shrinkE sh) (shrinkE sz)
      Union sh sz               -> Union (shrinkE sh) (shrinkE sz)
      Foreign ff f e            -> Foreign ff (shrinkF f) (shrinkE e)
      Coerce e                  -> Coerce (shrinkE e)

    shrinkF :: PreOpenFun acc env aenv' f -> PreOpenFun acc env aenv' f
    shrinkF (Lam f)  = Lam (shrinkF f)
    shrinkF (Body b) = Body (shrinkE b)

    shrinkT :: Tuple (PreOpenExp acc env aenv') t -> Tuple (PreOpenExp acc env aenv') t
    shrinkT NilTup        = NilTup
    shrinkT (SnocTup t e) = shrinkT t `SnocTup` shrinkE e

    shrinkAF :: PreOpenAfun acc aenv' f -> PreOpenAfun acc aenv' f
    shrinkAF (Alam lhs f) = Alam lhs (shrinkAF f)
    shrinkAF (Abody a) = Abody (shrinkAcc a)
-}
-- Occurrence Counting
-- ===================

-- Count the number of occurrences an in-scope scalar expression bound at the
-- given variable index recursively in a term.
--
usesOfExp :: forall acc env aenv t. VarsRange -> PreOpenExp acc env aenv t -> Count
usesOfExp range = countE
  where
    countE :: PreOpenExp acc env aenv e -> Count
    countE exp | matchEVarsRange range exp = Finite 1
    countE exp = case exp of
      Evar v
        | varInRange range v    -> Impossible
        | otherwise             -> Finite 0
      --
      Let lhs  bnd body         -> countE bnd <> usesOfExp (weakenVarsRange lhs range) body
      Const _ _                 -> Finite 0
      Undef _                   -> Finite 0
      Nil                       -> Finite 0
      Pair e1 e2                -> countE e1 <> countE e2
      VecPack   _ e             -> countE e
      VecUnpack _ e             -> countE e
      IndexSlice _ ix sh        -> countE ix <> countE sh
      IndexFull _ ix sl         -> countE ix <> countE sl
      FromIndex _ sh i          -> countE sh <> countE i
      ToIndex _ sh e            -> countE sh <> countE e
      Cond p t e                -> countE p  <> countE t <> countE e
      While p f x               -> countE x  <> loopCount (countF range p) <> countF range f
      PrimConst _               -> Finite 0
      PrimApp _ x               -> countE x
      Index _ sh                -> countE sh
      LinearIndex _ i           -> countE i
      Shape _                   -> Finite 0
      ShapeSize _ sh            -> countE sh
      Foreign _ _ e             -> countE e
      Coerce _ _ e              -> countE e

    countF :: VarsRange -> PreOpenFun acc env' aenv f -> Count
    countF range' (Lam lhs f) = countF (weakenVarsRange lhs range') f
    countF range' (Body b)    = usesOfExp range' b

-- Count the number of occurrences of the array term bound at the given
-- environment index. If the first argument is 'True' then it includes in the
-- total uses of the variable for 'Shape' information, otherwise not.
--
type UsesOfAcc acc = forall aenv s t. Bool -> Idx aenv s -> acc aenv t -> Int

usesOfPreAcc
    :: forall acc aenv s t.
       Bool
    -> UsesOfAcc  acc
    -> Idx            aenv s
    -> PreOpenAcc acc aenv t
    -> Int
usesOfPreAcc withShape countAcc idx = count
  where
    countIdx :: Idx aenv a -> Int
    countIdx this
        | Just Refl <- match this idx   = 1
        | otherwise                     = 0

    count :: PreOpenAcc acc aenv a -> Int
    count pacc = case pacc of
      Avar (Var _ this)          -> countIdx this
      --
      Alet lhs bnd body          -> countA bnd + countAcc withShape (weakenWithLHS lhs >:> idx) body
      Apair a1 a2                -> countA a1 + countA a2
      Anil                       -> 0
      Apply _ _ a                -> countA a --- XXX: It is suspicious that we don't descend into the function here. Same for awhile.
      Aforeign _ _ a             -> countA a
      Acond p t e                -> countE p  + countA t + countA e
      Awhile _ _ a               -> countA a
      Use _ _                    -> 0
      Unit _ e                   -> countE e
      Reshape _ e a              -> countE e  + countA a
      Generate _ e f             -> countE e  + countF f
      Transform _ sh ix f a      -> countE sh + countF ix + countF f  + countA a
      Replicate _ sh a           -> countE sh + countA a
      Slice _ a sl               -> countE sl + countA a
      Map _ f a                  -> countF f  + countA a
      ZipWith _ f a1 a2          -> countF f  + countA a1 + countA a2
      Fold f z a                 -> countF f  + countE z  + countA a
      Fold1 f a                  -> countF f  + countA a
      FoldSeg _ f z a s          -> countF f  + countE z  + countA a  + countA s
      Fold1Seg _ f a s           -> countF f  + countA a  + countA s
      Scanl f z a                -> countF f  + countE z  + countA a
      Scanl' f z a               -> countF f  + countE z  + countA a
      Scanl1 f a                 -> countF f  + countA a
      Scanr f z a                -> countF f  + countE z  + countA a
      Scanr' f z a               -> countF f  + countE z  + countA a
      Scanr1 f a                 -> countF f  + countA a
      Permute f1 a1 f2 a2        -> countF f1 + countA a1 + countF f2 + countA a2
      Backpermute _ sh f a       -> countE sh + countF f  + countA a
      Stencil _ _ f _ a          -> countF f  + countA a
      Stencil2 _ _ _ f _ a1 _ a2 -> countF f  + countA a1 + countA a2
      -- Collect s                 -> countS s

    countE :: PreOpenExp acc env aenv e -> Int
    countE exp = case exp of
      Let _ bnd body             -> countE bnd + countE body
      Evar _                     -> 0
      Const _ _                  -> 0
      Undef _                    -> 0
      Nil                        -> 0
      Pair x y                   -> countE x + countE y
      VecPack   _ e              -> countE e
      VecUnpack _ e              -> countE e
      IndexSlice _ ix sh         -> countE ix + countE sh
      IndexFull _ ix sl          -> countE ix + countE sl
      ToIndex _ sh ix            -> countE sh + countE ix
      FromIndex _ sh i           -> countE sh + countE i
      Cond p t e                 -> countE p  + countE t + countE e
      While p f x                -> countF p  + countF f + countE x
      PrimConst _                -> 0
      PrimApp _ x                -> countE x
      Index a sh                 -> countA a + countE sh
      LinearIndex a i            -> countA a + countE i
      ShapeSize _ sh             -> countE sh
      Shape a
        | withShape              -> countA a
        | otherwise              -> 0
      Foreign _ _ e              -> countE e
      Coerce _ _ e               -> countE e

    countA :: acc aenv a -> Int
    countA = countAcc withShape idx

    -- countAF :: PreOpenAfun acc aenv' f
    --         -> Idx aenv' s
    --         -> Int
    -- countAF (Alam f)  v = countAF f (SuccIdx v)
    -- countAF (Abody a) v = countAcc withShape v a

    countF :: PreOpenFun acc env aenv f -> Int
    countF (Lam _ f) = countF f
    countF (Body  b) = countE b

{--
    countS :: PreOpenSeq acc aenv senv arrs -> Int
    countS seq =
      case seq of
        Producer p s -> countP p + countS s
        Consumer c   -> countC c
        Reify _      -> 0

    countP :: Producer acc aenv senv arrs -> Int
    countP p =
      case p of
        StreamIn _           -> 0
        ToSeq _ _ a          -> countA a
        MapSeq f _           -> countAF f idx
        ChunkedMapSeq f _    -> countAF f idx
        ZipWithSeq f _ _     -> countAF f idx
        ScanSeq f e _        -> countF f + countE e

    countC :: Consumer acc aenv senv arrs -> Int
    countC c =
      case c of
        FoldSeq f e _        -> countF f + countE e
        FoldSeqFlatten f a _ -> countAF f idx + countA a
        Stuple t             -> countCT t

    countCT :: Atuple (Consumer acc aenv senv) t' -> Int
    countCT NilAtup        = 0
    countCT (SnocAtup t c) = countCT t + countC c
--}
