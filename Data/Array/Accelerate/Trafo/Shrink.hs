{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo.Shrink
-- Copyright   : [2012..2014] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
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
  ShrinkAcc, shrinkPreAcc, basicReduceAcc,

  -- Occurrence counting
  UsesOfAcc, usesOfPreAcc, usesOfExp,
  Use(..), zeroUse, oneUse, allUse,

  -- Array access merging
  reduceAccessExp, reduceAccessFun, reduceAccessOpenAcc, reduceAccessPreAcc,
  reduceAccessAfun,

) where

-- standard library
import Data.Function                                    ( on )
import Data.Maybe                                       ( fromMaybe )
import Data.Monoid                                      hiding ( Last )
import Control.Applicative                              hiding ( Const )
import Prelude                                          hiding ( exp, seq )

-- friends
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Array.Sugar               hiding ( Any )
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Product                   ( ProdR(..), TupleIdx(..) )
import Data.Array.Accelerate.Trafo.Base
import Data.Array.Accelerate.Trafo.Substitution

import qualified Data.Array.Accelerate.Debug            as Stats


class Shrink f where
  shrink  :: f -> f
  shrink' :: f -> (Bool, f)

  shrink = snd . shrink'

instance Kit acc => Shrink (PreOpenExp acc env aenv e) where
  shrink' = shrinkExp

instance Kit acc => Shrink (PreOpenFun acc env aenv f) where
  shrink' = shrinkFun


-- Shrinking
-- =========

-- The shrinking substitution for scalar expressions. This is a restricted
-- instance of beta-reduction to cases where the bound variable is used zero
-- (dead-code elimination) or one (linear inlining) times.
--
shrinkExp :: Kit acc => PreOpenExp acc env aenv t -> (Bool, PreOpenExp acc env aenv t)
shrinkExp = Stats.substitution "shrink exp" . first getAny . shrinkE
  where
    -- If the bound variable is used at most this many times, it will be inlined
    -- into the body. In cases where it is not used at all, this is equivalent
    -- to dead-code elimination.
    --
    lIMIT :: Int
    lIMIT = 1

    shrinkE :: Kit acc => PreOpenExp acc env aenv t -> (Any, PreOpenExp acc env aenv t)
    shrinkE exp = case exp of
      Let bnd body
        | Var _ <- bnd  -> Stats.inline "Var"   . yes $ shrinkE (inline body bnd)
        | allUseElt (<= lIMIT) uses -> Stats.betaReduce msg . yes $ shrinkE (inline (snd body') (snd bnd'))
        | otherwise     -> Let <$> bnd' <*> body'
        where
          bnd'  = shrinkE bnd
          body' = shrinkE body
          uses  = usesOfExp ZeroIdx (snd body')

          msg   = if allUseElt (==0) uses
                  then "dead exp"
                  else "inline exp"   -- forced inlining when lIMIT > 1
      --
      Var idx                   -> pure (Var idx)
      Const c                   -> pure (Const c)
      Tuple t                   -> Tuple <$> shrinkT t
      Prj tup e                 -> Prj tup <$> shrinkE e
      IndexNil                  -> pure IndexNil
      IndexCons sl sz           -> IndexCons <$> shrinkE sl <*> shrinkE sz
      IndexHead sh              -> IndexHead <$> shrinkE sh
      IndexTail sh              -> IndexTail <$> shrinkE sh
      IndexTrans sh             -> IndexTrans <$> shrinkE sh
      IndexSlice x ix sh        -> IndexSlice x ix <$> shrinkE sh
      IndexFull x ix sl         -> IndexFull x <$> shrinkE ix <*> shrinkE sl
      IndexAny                  -> pure IndexAny
      ToIndex sh ix             -> ToIndex <$> shrinkE sh <*> shrinkE ix
      FromIndex sh i            -> FromIndex <$> shrinkE sh <*> shrinkE i
      ToSlice x sh i            -> ToSlice x <$> shrinkE sh <*> shrinkE i
      Cond p t e                -> Cond <$> shrinkE p <*> shrinkE t <*> shrinkE e
      While p f x               -> While <$> shrinkF p <*> shrinkF f <*> shrinkE x
      PrimConst c               -> pure (PrimConst c)
      PrimApp f x               -> PrimApp f <$> shrinkE x
      Index a sh                -> Index a <$> shrinkE sh
      LinearIndex a i           -> LinearIndex a <$> shrinkE i
      Shape a                   -> pure (Shape a)
      ShapeSize sh              -> ShapeSize <$> shrinkE sh
      Intersect sh sz           -> Intersect <$> shrinkE sh <*> shrinkE sz
      Union sh sz               -> Union <$> shrinkE sh <*> shrinkE sz
      Foreign ff f e            -> Foreign ff <$> shrinkF f <*> shrinkE e

    shrinkT :: Kit acc => Tuple (PreOpenExp acc env aenv) t -> (Any, Tuple (PreOpenExp acc env aenv) t)
    shrinkT NilTup        = pure NilTup
    shrinkT (SnocTup t e) = SnocTup <$> shrinkT t <*> shrinkE e

    shrinkF :: Kit acc => PreOpenFun acc env aenv t -> (Any, PreOpenFun acc env aenv t)
    shrinkF = first Any . shrinkFun

    first :: (a -> a') -> (a,b) -> (a',b)
    first f (x,y) = (f x, y)

    yes :: (Any, x) -> (Any, x)
    yes (_, x) = (Any True, x)

shrinkFun :: Kit acc => PreOpenFun acc env aenv f -> (Bool, PreOpenFun acc env aenv f)
shrinkFun (Lam f)  = Lam  <$> shrinkFun f
shrinkFun (Body b) = Body <$> shrinkExp b


-- The shrinking substitution for array computations. This is further limited to
-- dead-code elimination only, primarily because linear inlining may inline
-- array computations into scalar expressions, which is generally not desirable.
--
type ShrinkAcc acc = forall aenv a.   acc aenv a -> acc aenv a
type ReduceAcc acc = forall aenv s t. Arrays s => acc aenv s -> acc (aenv,s) t -> Maybe (PreOpenAcc acc aenv t)

shrinkPreAcc
    :: forall acc aenv arrs. ShrinkAcc acc -> ReduceAcc acc
    -> PreOpenAcc acc aenv arrs
    -> PreOpenAcc acc aenv arrs
shrinkPreAcc shrinkAcc reduceAcc = Stats.substitution "shrink acc" shrinkA
  where
    shrinkA :: PreOpenAcc acc aenv' a -> PreOpenAcc acc aenv' a
    shrinkA pacc = case pacc of
      Alet bnd body
        | Just reduct <- reduceAcc bnd' body'   -> shrinkA reduct
        | otherwise                             -> Alet bnd' body'
        where
          bnd'  = shrinkAcc bnd
          body' = shrinkAcc body
      --
      Avar ix                   -> Avar ix
      Atuple tup                -> Atuple (shrinkAT tup)
      Aprj tup a                -> Aprj tup (shrinkAcc a)
      Apply f a                 -> Apply (shrinkAF f) (shrinkAcc a)
      Aforeign ff af a          -> Aforeign ff af (shrinkAcc a)
      Acond p t e               -> Acond (shrinkE p) (shrinkAcc t) (shrinkAcc e)
      Awhile p f a              -> Awhile (shrinkAF p) (shrinkAF f) (shrinkAcc a)
      Use a                     -> Use a
      Subarray ix sh a          -> Subarray (shrinkE ix) (shrinkE sh) a
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
      Collect min max i s       -> Collect (shrinkE min) (shrinkE <$> max) (shrinkE <$> i) (shrinkS s)

    shrinkS :: PreOpenSeq index acc aenv' a -> PreOpenSeq index acc aenv' a
    shrinkS seq =
      case seq of
        Producer p s -> Producer (shrinkP p) (shrinkS s)
        Consumer c   -> Consumer (shrinkC c)
        Reify ty a   -> Reify ty (shrinkAcc a)

    shrinkP :: Producer index acc aenv' a -> Producer index acc aenv' a
    shrinkP p =
      case p of
        Pull src            -> Pull src
        Subarrays sh arr    -> Subarrays (shrinkE sh) arr
        FromSegs s n vs     -> FromSegs (shrinkAcc s) (shrinkE n) (shrinkAcc vs)
        Produce l f         -> Produce (shrinkE <$> l) (shrinkAF f)
        -- MapBatch f c c' a x -> MapBatch (shrinkAF f) (shrinkAF c) (shrinkAF c') (shrinkAcc a) (shrinkAcc x)
        ProduceAccum l f a  -> ProduceAccum (shrinkE <$> l) (shrinkAF f) (shrinkAcc a)

    shrinkC :: Consumer index acc aenv' a -> Consumer index acc aenv' a
    shrinkC c =
      case c of
        FoldBatch f a x -> FoldBatch (shrinkAF f) (shrinkAcc a) (shrinkAcc x)
        Last a d        -> Last (shrinkAcc a) (shrinkAcc d)
        Elements x      -> Elements (shrinkAcc x)
        Tabulate x      -> Tabulate (shrinkAcc x)
        Stuple t        -> Stuple (shrinkCT t)

    shrinkCT :: Atuple (PreOpenSeq index acc aenv') t -> Atuple (PreOpenSeq index acc aenv') t
    shrinkCT NilAtup        = NilAtup
    shrinkCT (SnocAtup t c) = SnocAtup (shrinkCT t) (shrinkS c)

    shrinkE :: PreOpenExp acc env aenv' t -> PreOpenExp acc env aenv' t
    shrinkE exp = case exp of
      Let bnd body              -> Let (shrinkE bnd) (shrinkE body)
      Var idx                   -> Var idx
      Const c                   -> Const c
      Tuple t                   -> Tuple (shrinkT t)
      Prj tup e                 -> Prj tup (shrinkE e)
      IndexNil                  -> IndexNil
      IndexCons sl sz           -> IndexCons (shrinkE sl) (shrinkE sz)
      IndexHead sh              -> IndexHead (shrinkE sh)
      IndexTail sh              -> IndexTail (shrinkE sh)
      IndexTrans sh             -> IndexTrans (shrinkE sh)
      IndexSlice x ix sh        -> IndexSlice x ix (shrinkE sh)
      IndexFull x ix sl         -> IndexFull x (shrinkE ix) (shrinkE sl)
      IndexAny                  -> IndexAny
      ToIndex sh ix             -> ToIndex (shrinkE sh) (shrinkE ix)
      FromIndex sh i            -> FromIndex (shrinkE sh) (shrinkE i)
      ToSlice x sh i            -> ToSlice x (shrinkE sh) (shrinkE i)
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

    shrinkF :: PreOpenFun acc env aenv' f -> PreOpenFun acc env aenv' f
    shrinkF (Lam f)  = Lam (shrinkF f)
    shrinkF (Body b) = Body (shrinkE b)

    shrinkT :: Tuple (PreOpenExp acc env aenv') t -> Tuple (PreOpenExp acc env aenv') t
    shrinkT NilTup        = NilTup
    shrinkT (SnocTup t e) = shrinkT t `SnocTup` shrinkE e

    shrinkAT :: Atuple (acc aenv') t -> Atuple (acc aenv') t
    shrinkAT NilAtup        = NilAtup
    shrinkAT (SnocAtup t a) = shrinkAT t `SnocAtup` shrinkAcc a

    shrinkAF :: PreOpenAfun acc aenv' f -> PreOpenAfun acc aenv' f
    shrinkAF (Alam  f) = Alam (shrinkAF f)
    shrinkAF (Abody a) = Abody (shrinkAcc a)


-- A somewhat hacky example implementation of the reduction step. It requires a
-- function to open the recursive closure of an array term.
--
basicReduceAcc
    :: Kit acc
    => (forall aenv a. acc aenv a -> PreOpenAcc acc aenv a)
    -> UsesOfAcc acc
    -> ReduceAcc acc
basicReduceAcc unwrapAcc countAcc (unwrapAcc -> bnd) body@(unwrapAcc -> pbody)
  | Avar _ <- bnd                      = Stats.inline "Avar"  . Just $ rebuildA (subAtop bnd) pbody
  | allUse (on (&&) (<=lIMIT)) uses = Stats.betaReduce msg . Just $ rebuildA (subAtop bnd) pbody
  | otherwise                          = Nothing
  where
    -- If the bound variable is used at most this many times, it will be inlined
    -- into the body. Since this implies an array computation could be inlined
    -- into a scalar expression, we limit the shrinking reduction for array
    -- computations to dead-code elimination only.
    --
    lIMIT = 0

    uses  = countAcc ZeroIdx body
    msg   = if allUse (on (&&) (<=lIMIT)) uses
            then "dead acc"
            else "inline acc"         -- forced inlining when lIMIT > 1


-- Occurrence Counting
-- ===================

data UseElt e where
  UseElt      :: Elt e => Int -> UseElt e
  UseEltTuple :: Tuple UseElt (TupleRepr t) -> UseElt t

-- Combine the uses of an array variable.
--
(<+.>) :: UseElt a -> UseElt a -> UseElt a
(<+.>) = zipWithUelt (+)

zipWithUelt :: (Int -> Int -> Int) -> UseElt a -> UseElt a -> UseElt a
zipWithUelt f (UseElt c1)      (UseElt c2)      = UseElt (c1 `f` c2)
zipWithUelt f (UseEltTuple t1) (UseEltTuple t2) = UseEltTuple (t1 `tup` t2)
  where
    tup :: Tuple UseElt t -> Tuple UseElt t -> Tuple UseElt t
    tup NilTup          NilTup          = NilTup
    tup (SnocTup t1 a1) (SnocTup t2 a2) = tup t1 t2 `SnocTup` zipWithUelt f a1 a2
#if __GLASGOW_HASKELL__ < 800
    tup _               _               = error "Chewie, we're home."
#endif
zipWithUelt _ _                 _           = error "Aaarrrrhhggg!"

-- Update use at a specific index.
--
updateUseElt :: UseElt s -> TupleIdx (TupleRepr s) a -> UseElt a -> UseElt s
updateUseElt (UseEltTuple ut) ix = UseEltTuple . tup ut ix
  where
    tup :: Tuple UseElt t -> TupleIdx t a -> UseElt a -> Tuple UseElt t
    tup (SnocTup t s) ZeroTupIdx      a = SnocTup t (s <+.> a)
    tup (SnocTup t s) (SuccTupIdx ix) a = SnocTup (tup t ix a) s
    tup _             _               _ = error "Gentlemen, you can't fight in here!"
updateUseElt _                _  = error "This is the war room!"

-- A variable's components each occur `n` times in total.
--
nUseElt :: forall t. Elt t => Int -> UseElt t
nUseElt n =
  case eltFlavour (undefined :: t) of
    EltBase  -> UseElt n
    EltTuple -> UseEltTuple (nt (tuple (undefined :: t)))
  where
    nt :: ProdR Elt t' -> Tuple UseElt t'
    nt ProdRunit = NilTup
    nt (ProdRsnoc t) = nt t `SnocTup` nUseElt n

zeroUseElt, oneUseElt :: Elt t => UseElt t
zeroUseElt = nUseElt 0
oneUseElt  = nUseElt 1

instance Eq (UseElt a) where
  UseElt c1      == UseElt c2      = c1 == c2
  UseEltTuple t1 == UseEltTuple t2 = t1 == t2
  _              == _              = error "Impossible usage"

instance Eq (Tuple UseElt t) where
  NilTup        == NilTup        = True
  SnocTup t1 a1 == SnocTup t2 a2 = t1 == t2 && a1 == a2
#if __GLASGOW_HASKELL__ < 800
  _             == _             = error "Impossible usage"
#endif

-- Check if a condition is try for the use of all components.
--
allUseElt :: (Int -> Bool) -> UseElt t -> Bool
allUseElt p (UseElt c) = p c
allUseElt p (UseEltTuple t) = aT t
  where
    aT :: Tuple UseElt t -> Bool
    aT NilTup = True
    aT (SnocTup t u) = aT t && allUseElt p u

-- Specify a certain component of a variable has been used.
--
useComponentElt :: Elt a => TupleIdx (TupleRepr t) a -> UseElt t -> UseElt t
useComponentElt tix u = updateUseElt u tix oneUseElt

-- Count the number of occurrences an in-scope scalar expression bound at the
-- given variable index recursively in a term.
--
usesOfExp :: forall acc env aenv s t. Elt s => Idx env s -> PreOpenExp acc env aenv t -> UseElt s
usesOfExp idx = countE
  where
    countE :: PreOpenExp acc env aenv e -> UseElt s
    countE exp = case exp of
      Var this
        | Just Refl <- match this idx   -> oneUseElt
        | otherwise                     -> zeroUseElt
      --
      Let bnd body              -> countE bnd <+.> usesOfExp (SuccIdx idx) body
      Const _                   -> zeroUseElt
      Tuple t                   -> countT t
      Prj ix (Var v)            | Just Refl <- match v idx
                                -> useComponentElt ix zeroUseElt
      Prj _ e                   -> countE e
      IndexNil                  -> zeroUseElt
      IndexCons sl sz           -> countE sl <+.> countE sz
      IndexHead sh              -> countE sh
      IndexTail sh              -> countE sh
      IndexTrans sh             -> countE sh
      IndexSlice _ _ sh         -> countE sh
      IndexFull _ ix sl         -> countE ix <+.> countE sl
      IndexAny                  -> zeroUseElt
      ToIndex sh ix             -> countE sh <+.> countE ix
      FromIndex sh i            -> countE sh <+.> countE i
      ToSlice _ sh i            -> countE sh <+.> countE i
      Cond p t e                -> countE p  <+.> countE t <+.> countE e
      While p f x               -> countE x  <+.> countF idx p <+.> countF idx f
      PrimConst _               -> zeroUseElt
      PrimApp _ x               -> countE x
      Index _ sh                -> countE sh
      LinearIndex _ i           -> countE i
      Shape _                   -> zeroUseElt
      ShapeSize sh              -> countE sh
      Intersect sh sz           -> countE sh <+.> countE sz
      Union sh sz               -> countE sh <+.> countE sz
      Foreign _ _ e             -> countE e

    countF :: Idx env' s -> PreOpenFun acc env' aenv f -> UseElt s
    countF idx' (Lam  f) = countF (SuccIdx idx') f
    countF idx' (Body b) = usesOfExp idx' b

    countT :: Tuple (PreOpenExp acc env aenv) e -> UseElt s
    countT NilTup        = zeroUseElt
    countT (SnocTup t e) = countT t <+.> countE e


-- Count the number of uses of the array term bound at the given environment
-- index.
--
type UsesOfAcc acc = forall aenv s t. Arrays s => Idx aenv s -> acc aenv t -> Use s

-- |How an array variable is used in its context. If it is of a product type, we
-- track the usage of each component.
--
data Use a where
  UseArray :: Int                -- How often the shape of the array is used
           -> Int                -- How often the contents of the array is used
           -> Use (Array sh e)
  UseTuple :: Atuple Use (TupleRepr t) -> Use t

-- Combine the uses of an array variable.
--
(<+>) :: Use a -> Use a -> Use a
(<+>) = zipWithU (+) (+)

zipWithU :: (Int -> Int -> Int) -> (Int -> Int -> Int)-> Use a -> Use a -> Use a
zipWithU f g (UseArray s1 c1) (UseArray s2 c2) = UseArray (s1 `f` s2) (c1 `g` c2)
zipWithU f g (UseTuple t1)    (UseTuple t2)    = UseTuple (t1 `tup` t2)
  where
    tup :: Atuple Use t -> Atuple Use t -> Atuple Use t
    tup NilAtup          NilAtup          = NilAtup
    tup (SnocAtup t1 a1) (SnocAtup t2 a2) = tup t1 t2 `SnocAtup` zipWithU f g a1 a2
#if __GLASGOW_HASKELL__ < 800
    tup _                _                = error "Chewie, we're home."
#endif
zipWithU _ _ _                _                = error "Aaarrrrhhggg!"

-- Update use at a specific index.
--
updateUse :: Use s -> TupleIdx (TupleRepr s) a -> Use a -> Use s
updateUse (UseTuple ut) ix = UseTuple . tup ut ix
  where
    tup :: Atuple Use t -> TupleIdx t a -> Use a -> Atuple Use t
    tup (SnocAtup t s) ZeroTupIdx      a = SnocAtup t (s <+> a)
    tup (SnocAtup t s) (SuccTupIdx ix) a = SnocAtup (tup t ix a) s
    tup _              _               _ = error "All these facts and opinions look the same. I can't tell them apart."
updateUse _             _  = error "Happens to me all the time. Don't worry about it."

-- A variable's components each occur `n` times in total.
--
nUse :: forall t. Arrays t => Int -> Use t
nUse n =
  case flavour (undefined :: t) of
    ArraysFunit  -> UseTuple NilAtup
    ArraysFarray -> UseArray n n
    ArraysFtuple -> UseTuple (nt (atuple (undefined :: t)))
  where
    nt :: ProdR Arrays t' -> Atuple Use t'
    nt ProdRunit = NilAtup
    nt (ProdRsnoc t) = nt t `SnocAtup` nUse n

zeroUse, oneUse :: Arrays t => Use t
zeroUse = nUse 0
oneUse  = nUse 1

instance Eq (Use a) where
  UseArray s1 c1 == UseArray s2 c2 = s1 == s2 && c1 == c2
  UseTuple t1    == UseTuple t2    = t1 == t2
  _              == _              = error "It can camouflage!"

instance Eq (Atuple Use t) where
  NilAtup        == NilAtup        = True
  SnocAtup t1 a1 == SnocAtup t2 a2 = t1 == t2 && a1 == a2
#if __GLASGOW_HASKELL__ < 800
  _              == _              = error "That thing out there... That is no dinosaur"
#endif

-- Check if a condition is try for the use of all components.
--
allUse :: (Int -> Int -> Bool) -> Use t -> Bool
allUse p (UseArray s c) = p s c
allUse p (UseTuple t)   = aT t
  where
    aT :: Atuple Use t -> Bool
    aT NilAtup = True
    aT (SnocAtup t u) = aT t && allUse p u

usesOfPreAcc
    :: forall acc aenv s t. (Kit acc, Arrays s)
    => UsesOfAcc  acc
    -> Idx            aenv s
    -> PreOpenAcc acc aenv t
    -> Use s
usesOfPreAcc countAcc idx = count
  where
    countIdx :: Idx aenv a -> Use s
    countIdx this
        | Just Refl <- match this idx   = oneUse
        | otherwise                     = zeroUse

    count :: PreOpenAcc acc aenv a -> Use s
    count pacc = case pacc of
      Avar this                 -> countIdx this
      --
      Alet bnd body             -> countA bnd <+> countAcc (SuccIdx idx) body

      Atuple tup                -> countAT tup
      Aprj ix a                 | Just u <- prjChain idx (inject $ Aprj ix a) oneUse
                                -> u
                                | Atuple t <- extract a
                                -> countA (prj t ix)
                                | otherwise
                                -> countA a
      Apply f a                 -> countAF f idx <+> countA a
      Aforeign _ _ a            -> countA a
      Acond p t e               -> countE p  <+> countA t <+> countA e
      Awhile p f a              -> countAF p idx <+> countAF f idx <+> countA a
      Use _                     -> zeroUse
      Subarray ix sh _          -> countE ix <+> countE sh
      Unit e                    -> countE e
      Reshape e a               -> countE e  <+> countA a
      Generate e f              -> countE e  <+> countF f
      Transform sh ix f a       -> countE sh <+> countF ix <+> countF f  <+> countA a
      Replicate _ sh a          -> countE sh <+> countA a
      Slice _ a sl              -> countE sl <+> countA a
      Map f a                   -> countF f  <+> countA a
      ZipWith f a1 a2           -> countF f  <+> countA a1 <+> countA a2
      Fold f z a                -> countF f  <+> countE z  <+> countA a
      Fold1 f a                 -> countF f  <+> countA a
      FoldSeg f z a s           -> countF f  <+> countE z  <+> countA a  <+> countA s
      Fold1Seg f a s            -> countF f  <+> countA a  <+> countA s
      Scanl f z a               -> countF f  <+> countE z  <+> countA a
      Scanl' f z a              -> countF f  <+> countE z  <+> countA a
      Scanl1 f a                -> countF f  <+> countA a
      Scanr f z a               -> countF f  <+> countE z  <+> countA a
      Scanr' f z a              -> countF f  <+> countE z  <+> countA a
      Scanr1 f a                -> countF f  <+> countA a
      Permute f1 a1 f2 a2       -> countF f1 <+> countA a1 <+> countF f2 <+> countA a2
      Backpermute sh f a        -> countE sh <+> countF f  <+> countA a
      Stencil f _ a             -> countF f  <+> countA a
      Stencil2 f _ a1 _ a2      -> countF f  <+> countA a1 <+> countA a2
      Collect min max i s       -> foldl (<+>) zeroUse (map (fromMaybe zeroUse . fmap countE) [Just min,max,i])
                                <+> usesOfPreSeq countAcc idx s

    countA :: acc aenv a -> Use s
    countA = countAcc idx

    countAF :: PreOpenAfun acc aenv' f
            -> Idx aenv' s
            -> Use s
    countAF (Alam f)  v = countAF f (SuccIdx v)
    countAF (Abody a) v = countAcc v a

    countF :: PreOpenFun acc env aenv f -> Use s
    countF (Lam  f) = countF f
    countF (Body b) = countE b

    countAT :: Atuple (acc aenv) a -> Use s
    countAT NilAtup        = zeroUse
    countAT (SnocAtup t a) = countAT t <+> countA a

    countE :: PreOpenExp acc env aenv e -> Use s
    countE | ArraysFarray <- flavour (undefined :: s)
           = usesOfExpA countAcc idx . reduceAccessExp idx
           | otherwise
           = usesOfExpA countAcc idx

    prj :: Atuple k t' -> TupleIdx t' a -> k a
    prj (SnocAtup _ a) ZeroTupIdx      = a
    prj (SnocAtup t _) (SuccTupIdx ix) = prj t ix
    prj NilAtup        _               = error "That'll do, pig. That'll do"

prjChain :: Kit acc => Idx aenv s -> acc aenv t' -> Use t' -> Maybe (Use s)
prjChain idx a u =
  case extract a of
    Avar x    | Just Refl <- match idx x
              -> Just u
    Aprj ix a -> prjChain idx a (updateUse zeroUse ix u)
    _         -> Nothing


usesOfPreSeq :: forall acc index aenv s t. (Kit acc, Arrays s)
             => UsesOfAcc acc
             -> Idx aenv s
             -> PreOpenSeq index acc aenv t
             -> Use s
usesOfPreSeq countAcc idx seq =
  case seq of
    Producer p s -> countP p <+> usesOfPreSeq countAcc (SuccIdx idx) s
    Consumer c   -> countC c
    Reify _ a    -> countA a
  where
    countP :: Producer index acc aenv arrs -> Use s
    countP p =
      case p of
        Pull _              -> zeroUse
        Subarrays sh _      -> countE sh
        FromSegs s n vs     -> countA s <+> countE n <+> countA vs
        Produce l f         -> maybe zeroUse countE l <+> countAF f idx
        -- MapBatch f c c' a x -> countAF f idx <+> countAF c idx <+> countAF c' idx <+> countA a <+> countA x
        ProduceAccum l f a  -> maybe zeroUse countE l <+> countAF f idx <+> countA a

    countC :: Consumer index acc aenv arrs -> Use s
    countC c =
      case c of
        FoldBatch f a x -> countAF f idx <+> countA a <+> countA x
        Last a d        -> countA a <+> countA d
        Elements x      -> countA x
        Tabulate x      -> countA x
        Stuple t        -> countCT t

    countCT :: Atuple (PreOpenSeq index acc aenv) t' -> Use s
    countCT NilAtup        = zeroUse
    countCT (SnocAtup t c) = countCT t <+> usesOfPreSeq countAcc idx c

    countA :: acc aenv a -> Use s
    countA = countAcc idx

    countAF :: PreOpenAfun acc aenv' f
            -> Idx aenv' s
            -> Use s
    countAF (Alam f)  v = countAF f (SuccIdx v)
    countAF (Abody a) v = countAcc v a

    countE :: PreOpenExp acc env aenv e -> Use s
    countE = usesOfExpA countAcc idx

usesOfExpA :: forall acc env aenv t e. (Kit acc, Arrays t)
           => UsesOfAcc acc
           -> Idx aenv t
           -> PreOpenExp acc env aenv e
           -> Use t
usesOfExpA countAcc idx exp =
  case exp of
    Let bnd body              -> countE bnd <+> countE body
    Var _                     -> zeroUse
    Const _                   -> zeroUse
    Tuple t                   -> countT t
    Prj _ e                   -> countE e
    IndexNil                  -> zeroUse
    IndexCons sl sz           -> countE sl <+> countE sz
    IndexHead sh              -> countE sh
    IndexTail sh              -> countE sh
    IndexTrans sh             -> countE sh
    IndexSlice _ _ sh         -> countE sh
    IndexFull _ ix sl         -> countE ix <+> countE sl
    IndexAny                  -> zeroUse
    ToIndex sh ix             -> countE sh <+> countE ix
    FromIndex sh i            -> countE sh <+> countE i
    ToSlice _ sh i            -> countE sh <+> countE i
    Cond p t e                -> countE p  <+> countE t <+> countE e
    While p f x               -> countF p  <+> countF f <+> countE x
    PrimConst _               -> zeroUse
    PrimApp _ x               -> countE x
    Index a sh                | Just u <- prjChain idx a oneUse
                              -> zipWithU const (+) u (countE sh)
                              | otherwise
                              -> countA a <+> countE sh
    LinearIndex a i           -> countA a <+> countE i
    ShapeSize sh              -> countE sh
    Intersect sh sz           -> countE sh <+> countE sz
    Union sh sz               -> countE sh <+> countE sz
    Shape a                   | Avar v    <- extract a
                              , Just Refl <- match v idx
                              -> UseArray 1 0
                              | otherwise
                              -> countA a
    Foreign _ _ e             -> countE e

  where
    countE :: PreOpenExp acc env' aenv e' -> Use t
    countE = usesOfExpA countAcc idx

    countT :: Tuple (PreOpenExp acc env aenv) e' -> Use t
    countT NilTup        = zeroUse
    countT (SnocTup t e) = countT t <+> countE e

    countF :: PreOpenFun acc env' aenv f -> Use t
    countF (Lam  f) = countF f
    countF (Body b) = countE b

    countA :: acc aenv a -> Use t
    countA = countAcc idx


-- Find and merge common array accesses
--
type ReduceAccess acc = forall aenv sh e a. (Shape sh, Elt e) => Idx aenv (Array sh e) -> acc aenv a -> acc aenv a

reduceAccessOpenAcc :: (Shape sh, Elt e)
                    => Idx aenv (Array sh e)
                    -> OpenAcc aenv a
                    -> OpenAcc aenv a
reduceAccessOpenAcc idx (OpenAcc pacc) = OpenAcc (reduceAccessPreAcc reduceAccessOpenAcc idx pacc)

reduceAccessPreAcc :: forall acc aenv a sh e. (Kit acc, Shape sh, Elt e)
                   => ReduceAccess acc
                   -> Idx aenv (Array sh e)
                   -> PreOpenAcc acc aenv a
                   -> PreOpenAcc acc aenv a
reduceAccessPreAcc reduceAcc idx pacc =
  case pacc of
    Alet bnd body             -> Alet (cvtA bnd) (reduceAcc (SuccIdx idx) body)
    Avar ix                   -> Avar ix
    Atuple tup                -> Atuple (cvtT tup)
    Aprj tup a                -> Aprj tup (cvtA a)
    Apply f a                 -> Apply (cvtAfun f) (cvtA a)
    Aforeign ff afun acc      -> Aforeign ff afun (cvtA acc)
    Acond p t e               -> Acond (cvtE p) (cvtA t) (cvtA e)
    Awhile p f a              -> Awhile (cvtAfun p) (cvtAfun f) (cvtA a)
    Use a                     -> Use a
    Subarray ix sh arr        -> Subarray (cvtE ix) (cvtE sh) arr
    Unit e                    -> Unit (cvtE e)
    Reshape e a               -> Reshape (cvtE e) (cvtA a)
    Generate e f              -> Generate (cvtE e) (cvtF f)
    Transform sh ix f a       -> Transform (cvtE sh) (cvtF ix) (cvtF f) (cvtA a)
    Replicate sl slix a       -> Replicate sl (cvtE slix) (cvtA a)
    Slice sl a slix           -> Slice sl (cvtA a) (cvtE slix)
    Map f a                   -> Map (cvtF f) (cvtA a)
    ZipWith f a1 a2           -> ZipWith (cvtF f) (cvtA a1) (cvtA a2)
    Fold f z a                -> Fold (cvtF f) (cvtE z) (cvtA a)
    Fold1 f a                 -> Fold1 (cvtF f) (cvtA a)
    FoldSeg f z a s           -> FoldSeg (cvtF f) (cvtE z) (cvtA a) (cvtA s)
    Fold1Seg f a s            -> Fold1Seg (cvtF f) (cvtA a) (cvtA s)
    Scanl f z a               -> Scanl (cvtF f) (cvtE z) (cvtA a)
    Scanl' f z a              -> Scanl' (cvtF f) (cvtE z) (cvtA a)
    Scanl1 f a                -> Scanl1 (cvtF f) (cvtA a)
    Scanr f z a               -> Scanr (cvtF f) (cvtE z) (cvtA a)
    Scanr' f z a              -> Scanr' (cvtF f) (cvtE z) (cvtA a)
    Scanr1 f a                -> Scanr1 (cvtF f) (cvtA a)
    Permute f1 a1 f2 a2       -> Permute (cvtF f1) (cvtA a1) (cvtF f2) (cvtA a2)
    Backpermute sh f a        -> Backpermute (cvtE sh) (cvtF f) (cvtA a)
    Stencil f b a             -> Stencil (cvtF f) b (cvtA a)
    Stencil2 f b1 a1 b2 a2    -> Stencil2 (cvtF f) b1 (cvtA a1) b2 (cvtA a2)
    Collect min max i s       -> Collect (cvtE min) (cvtE <$> max) (cvtE <$> i) (reduceAccessSeq reduceAcc idx s)

  where
    cvtA :: acc aenv a' -> acc aenv a'
    cvtA = reduceAcc idx

    cvtE :: PreOpenExp acc env aenv e' -> PreOpenExp acc env aenv e'
    cvtE = reduceAccessExp idx

    cvtF :: PreOpenFun acc env aenv e' -> PreOpenFun acc env aenv e'
    cvtF = reduceAccessFun idx

    cvtT :: Atuple (acc aenv) t -> Atuple (acc aenv) t
    cvtT NilAtup        = NilAtup
    cvtT (SnocAtup t a) = SnocAtup (cvtT t) (cvtA a)

    cvtAfun :: PreOpenAfun acc aenv t -> PreOpenAfun acc aenv t
    cvtAfun = reduceAccessAfun reduceAcc idx

reduceAccessAfun :: (Shape sh, Elt e)
                 => ReduceAccess acc
                 -> Idx aenv (Array sh e)
                 -> PreOpenAfun acc aenv f
                 -> PreOpenAfun acc aenv f
reduceAccessAfun reduceAcc idx (Abody a) = Abody (reduceAcc idx a)
reduceAccessAfun reduceAcc idx (Alam f)  = Alam (reduceAccessAfun reduceAcc (SuccIdx idx) f)

reduceAccessSeq :: forall index acc aenv a sh e. (Kit acc, Shape sh, Elt e)
                => ReduceAccess acc
                -> Idx aenv (Array sh e)
                -> PreOpenSeq index acc aenv a
                -> PreOpenSeq index acc aenv a
reduceAccessSeq reduceAcc idx seq =
  case seq of
    Producer p s -> Producer (cvtP p) (reduceAccessSeq reduceAcc (SuccIdx idx) s)
    Consumer c   -> Consumer (cvtC c)
    Reify ty a   -> Reify ty (cvtA a)
  where
    cvtA :: acc aenv a' -> acc aenv a'
    cvtA = reduceAcc idx

    cvtE :: PreOpenExp acc env aenv e' -> PreOpenExp acc env aenv e'
    cvtE = reduceAccessExp idx

    cvtP :: Producer index acc aenv a' -> Producer index acc aenv a'
    cvtP (Pull src)           = Pull src
    cvtP (Subarrays sh a)     = Subarrays    (cvtE sh) a
    cvtP (FromSegs s n vs)    = FromSegs     (cvtA s) (cvtE n) (cvtA vs)
    cvtP (Produce l f)        = Produce      (cvtE <$> l) (reduceAccessAfun reduceAcc idx f)
    cvtP (ProduceAccum l f a) = ProduceAccum (cvtE <$> l) (reduceAccessAfun reduceAcc idx f) (cvtA a)
    cvtP _                    = stageError

    cvtC :: Consumer index acc aenv a' -> Consumer index acc aenv a'
    cvtC (Last a d)        = Last (cvtA a) (cvtA d)
    cvtC (Stuple t)        = Stuple (cvtT t)
    cvtC (FoldBatch f a x) = FoldBatch (reduceAccessAfun reduceAcc idx f) (cvtA a) (cvtA x)
    cvtC (Elements x)      = Elements (cvtA x)
    cvtC (Tabulate x)      = Tabulate (cvtA x)
    cvtC _                 = stageError

    cvtT :: Atuple (PreOpenSeq index acc aenv) t -> Atuple (PreOpenSeq index acc aenv) t
    cvtT NilAtup        = NilAtup
    cvtT (SnocAtup t s) = SnocAtup (cvtT t) (reduceAccessSeq reduceAcc idx s)

    stageError = $internalError "reduceAccessSeq" "Sequence AST is at an unexpected stage"

reduceAccessExp :: (Kit acc, Shape sh, Elt e)
                => Idx aenv (Array sh e)
                -> PreOpenExp acc env aenv e'
                -> PreOpenExp acc env aenv e'
reduceAccessExp idx e =
  case elimArrayAccess idx e of
    Left (sh, e) -> inline e (Index (inject $ Avar idx) sh)
    Right e      -> e

reduceAccessFun :: (Kit acc, Shape sh, Elt e)
                => Idx aenv (Array sh e)
                -> PreOpenFun acc env aenv f
                -> PreOpenFun acc env aenv f
reduceAccessFun ix (Body b) = Body (reduceAccessExp ix b)
reduceAccessFun ix (Lam f)  = Lam (reduceAccessFun ix f)

elimArrayAccess :: forall acc env aenv sh e e'. (Kit acc, Elt e, Shape sh)
                => Idx aenv (Array sh e)
                -> PreOpenExp acc env aenv e'
                -> Either (PreOpenExp acc env aenv sh, PreOpenExp acc (env,e) aenv e') (PreOpenExp acc env aenv e')
elimArrayAccess idx exp =
  case exp of
    Let a b             -> cvtLet (cvtE a) (cvtE b)
    Var ix              -> noAccess $ Var ix
    Const c             -> noAccess $ Const c
    Tuple tup           -> cvtTup tup
    Prj tup e           -> Prj tup `cvtE1` cvtE e
    IndexNil            -> noAccess IndexNil
    IndexCons sh sz     -> cvtE2 IndexCons (cvtE sh) (cvtE sz)
    IndexHead sh        -> IndexHead `cvtE1` cvtE sh
    IndexTail sh        -> IndexTail `cvtE1` cvtE sh
    IndexTrans sh       -> IndexTrans `cvtE1` cvtE sh
    IndexAny            -> noAccess IndexAny
    IndexSlice x ix sh  -> IndexSlice x ix `cvtE1` cvtE sh
    IndexFull x ix sl   -> cvtE2 (IndexFull x)  (cvtE ix) (cvtE sl)
    ToIndex sh ix       -> cvtE2 ToIndex (cvtE sh) (cvtE ix)
    FromIndex sh ix     -> cvtE2 FromIndex (cvtE sh) (cvtE ix)
    ToSlice x sh ix     -> cvtE2 (ToSlice x) (cvtE sh) (cvtE ix)
    Cond p t e          -> cvtE3 Cond (cvtE p) (cvtE t) (cvtE e)
    While p f x         -> cvtE3 While (cvtF p) (cvtF f) (cvtE x)
    PrimConst c         -> noAccess $ PrimConst c
    PrimApp f x         -> PrimApp f `cvtE1` cvtE x
    Index a sh          | Avar idx' <- extract a
                        , Just Refl <- match idx idx'
                        -> Left (sh, Var ZeroIdx)
                        | otherwise
                        -> Index a `cvtE1` cvtE sh
    LinearIndex a i     | Avar idx' <- extract a
                        , Just Refl <- match idx idx'
                        -> Left (FromIndex (Shape a) i, Var ZeroIdx)
                        | otherwise
                        -> LinearIndex a `cvtE1` cvtE i
    Shape a             -> noAccess $ Shape a
    ShapeSize sh        -> ShapeSize `cvtE1` cvtE sh
    Intersect s t       -> cvtE2 Intersect (cvtE s) (cvtE t)
    Union s t           -> cvtE2 Union (cvtE s) (cvtE t)
    Foreign ff f e      -> Foreign ff f `cvtE1` cvtE e

  where
    cvtE :: forall env t. PreOpenExp acc env aenv t -> Either (PreOpenExp acc env aenv sh, PreOpenExp acc (env,e) aenv t) (PreOpenExp acc env aenv t)
    cvtE = elimArrayAccess idx

    cvtF :: PreOpenFun acc env aenv (a -> b) -> Either (PreOpenExp acc env aenv sh, PreOpenFun acc (env,e) aenv (a -> b)) (PreOpenFun acc env aenv (a -> b))
    cvtF (Lam (Body b)) =
      case cvtE b of
        Left (sh, e) | Just sh' <- strengthenE noTop sh
                     -> Left (sh', Lam (Body (weakenE swapTop e)))
                     | otherwise
                     -> Right (Lam (Body (inline e (access sh))))
        Right b'     -> Right (Lam (Body b'))
    cvtF _ = error "Impossible function"

    cvtE1 :: forall s t.
             (forall env. PreOpenExp acc env aenv s -> PreOpenExp acc env aenv t)
          -> Either (PreOpenExp acc env aenv sh, PreOpenExp acc (env,e) aenv s) (PreOpenExp acc env aenv s)
          -> Either (PreOpenExp acc env aenv sh, PreOpenExp acc (env,e) aenv t) (PreOpenExp acc env aenv t)
    cvtE1 f (Left (sh, e)) = Left (sh, f e)
    cvtE1 f (Right e)      = Right (f e)

    cvtE2 :: forall s t a. (Elt a, Elt s, Elt t)
          => (forall env. PreOpenExp acc env aenv s -> PreOpenExp acc env aenv t -> PreOpenExp acc env aenv a)
          -> Either (PreOpenExp acc env aenv sh, PreOpenExp acc (env,e) aenv s) (PreOpenExp acc env aenv s)
          -> Either (PreOpenExp acc env aenv sh, PreOpenExp acc (env,e) aenv t) (PreOpenExp acc env aenv t)
          -> Either (PreOpenExp acc env aenv sh, PreOpenExp acc (env,e) aenv a) (PreOpenExp acc env aenv a)
    cvtE2 f (Right s)     (Right t)      = Right (f s t)
    cvtE2 f (Right s)     (Left (e, t))  = Left (e, f (weakenE SuccIdx s) t)
    cvtE2 f (Left (e, s)) (Left (e', t)) | Just Refl <- match e e'
                                         = Left (e, Let (Var ZeroIdx) $ f (weakenE oneBelow s) (weakenE oneBelow t))
                                         | otherwise
                                         = Right (f (inline s (access e)) (inline t (access e')))
    cvtE2 f s             t              = cvtE2 (flip f) t s

    cvtE3 :: forall f g h r s t a. (Elt a, SinkExp f, SinkExp g, SinkExp h, RebuildableExp f, acc ~ AccCloE f, RebuildableExp g, acc ~ AccCloE g, RebuildableExp h, acc ~ AccCloE h)
          => (forall env. f env aenv r -> g env aenv s -> h env aenv t -> PreOpenExp acc env aenv a)
          -> Either (PreOpenExp acc env aenv sh, f (env,e) aenv r) (f env aenv r)
          -> Either (PreOpenExp acc env aenv sh, g (env,e) aenv s) (g env aenv s)
          -> Either (PreOpenExp acc env aenv sh, h (env,e) aenv t) (h env aenv t)
          -> Either (PreOpenExp acc env aenv sh, PreOpenExp acc (env,e) aenv a) (PreOpenExp acc env aenv a)
    cvtE3 f (Right r) (Right s)     (Right t)      = Right (f r s t)
    cvtE3 f (Right r) (Right s)     (Left (e, t))  = Left (e, f (weakenE SuccIdx r) (weakenE SuccIdx s) t)
    cvtE3 f r@Right{} s@Left{}      t@Right{}      = cvtE3 (flip . f) r t s
    cvtE3 f r@Left{}  s@Right{}     t@Right{}      = cvtE3 (flip f) s r t
    cvtE3 f (Right r) (Left (e, s)) (Left (e', t)) | Just Refl <- match e e'
                                                   = Left (e, Let (Var ZeroIdx) $ f (weakenE (SuccIdx . SuccIdx) r) (weakenE oneBelow s) (weakenE oneBelow t))
                                                   | otherwise
                                                   = Right (f r (inline s (access e)) (inline t (access e')))
    cvtE3 f r@Left{}  s@Right{}     t@Left{}       = cvtE3 (flip f) s r t
    cvtE3 f r@Left{}  s@Left{}      t@Right{}      = cvtE3 (flip . f) r t s
    cvtE3 f (Left (e,r)) (Left (e', s)) (Left (e'', t))
      | Just Refl <- match e e'
      , Just Refl <- match e' e''
      = Left (e, Let (Var ZeroIdx) $ f (weakenE oneBelow r) (weakenE oneBelow s) (weakenE oneBelow t))
      | otherwise
      = Right (f (inline r (access e)) (inline s (access e)) (inline t (access e')))

    cvtLet :: forall s t. (Elt s, Elt t)
           => Either (PreOpenExp acc env aenv sh,     PreOpenExp acc (env,e) aenv s)     (PreOpenExp acc env aenv s)
           -> Either (PreOpenExp acc (env,s) aenv sh, PreOpenExp acc ((env,s),e) aenv t) (PreOpenExp acc (env,s) aenv t)
           -> Either (PreOpenExp acc env aenv sh,     PreOpenExp acc (env,e) aenv t)     (PreOpenExp acc env aenv t)
    cvtLet (Right s)     (Right t)      = Right (Let s t)
    cvtLet (Right s)     (Left (e, t))  | Just e' <- strengthenE noTop e
                                        = Left (e', Let (weakenE SuccIdx s) (weakenE swapTop t))
                                        | otherwise
                                        = Right (Let s (inline t (access e)))
    cvtLet (Left (e, s)) (Right t)      = Left (e, Let s (weakenE (swapTop . SuccIdx) t))
    cvtLet (Left (e, s)) (Left (e', t)) | Just e''  <- strengthenE noTop e'
                                        , Just Refl <- match e e''
                                        = Left (e, Let (Var ZeroIdx) $ Let (weakenE oneBelow s) (weakenE (under oneBelow . swapTop) t))
                                        | otherwise
                                        = Right (Let (inline s (access e)) (inline t (access e')))

    cvtTup :: (Elt t, IsTuple t)
           => Tuple (PreOpenExp acc env aenv) (TupleRepr t)
           -> Either (PreOpenExp acc env aenv sh, PreOpenExp acc (env,e) aenv t) (PreOpenExp acc env aenv t)
    cvtTup t =
      case cvtT t of
        Left (sh, t', True)  -> Left (sh, Let (Var ZeroIdx) (weakenE oneBelow $ Tuple t'))
        Left (sh, t', False) -> Left (sh, Tuple t')
        Right t'             -> Right (Tuple t')
      where
        cvtT :: Tuple (PreOpenExp acc env aenv) t
             -> Either (PreOpenExp acc env aenv sh, Tuple (PreOpenExp acc (env,e) aenv) t, Bool) (Tuple (PreOpenExp acc env aenv) t)
        cvtT NilTup = Right NilTup
        cvtT (SnocTup t e) =
          case (cvtT t, cvtE e) of
            (Right t', Right e')             -> Right (t' `SnocTup` e')
            (Right t', Left (sh, e'))  -> Left (sh, (unRTup . weakenE SuccIdx . RebuildTup) t' `SnocTup` e', False)
            (Left (sh, t', dups), Right e')  -> Left (sh, t' `SnocTup` weakenE SuccIdx e', dups)
            (Left (sh, t', _), Left (sh', e')) | Just Refl <- match sh sh'
                                            -> Left (sh, t' `SnocTup` e', True)
                                            | otherwise
                                            -> Right (unRTup (inline (RebuildTup t') (access sh)) `SnocTup` inline e' (access sh'))

    oneBelow :: forall env a b. (env,a) :> ((env,b),a)
    oneBelow ZeroIdx = ZeroIdx
    oneBelow (SuccIdx ix) = SuccIdx (SuccIdx ix)

    swapTop :: forall env a b. ((env,a),b) :> ((env,b),a)
    swapTop ZeroIdx                = SuccIdx ZeroIdx
    swapTop (SuccIdx ZeroIdx)      = ZeroIdx
    swapTop (SuccIdx (SuccIdx ix)) = SuccIdx (SuccIdx ix)

    under :: forall env env' a. env :> env' -> (env,a) :> (env',a)
    under _ ZeroIdx = ZeroIdx
    under v (SuccIdx ix) = SuccIdx (v ix)

    noTop :: forall env a. (env,a) :?> env
    noTop ZeroIdx      = Nothing
    noTop (SuccIdx ix) = Just ix

    access :: forall env. PreOpenExp acc env aenv sh -> PreOpenExp acc env aenv e
    access = Index (inject (Avar idx))

    noAccess = Right
