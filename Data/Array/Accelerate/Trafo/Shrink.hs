{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

  -- Dependency analysis
  (::>)(..), Stronger(..), weakIn, weakOut,
  DependenciesAcc, dependenciesOpenAcc,
  dependenciesPreAcc, dependenciesAfun,
  dependenciesProducer, dependenciesConsumer,

) where

-- standard library
import Data.Function                                    ( on )
import Data.Monoid
import Control.Applicative                              hiding ( Const )
import Prelude                                          hiding ( exp, seq )

-- friends
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Array.Sugar               hiding ( Any )
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
      Collect s cs              -> Collect (shrinkS s) (shrinkS <$> cs)

    shrinkS :: PreOpenSeq index acc aenv' a -> PreOpenSeq index acc aenv' a
    shrinkS seq =
      case seq of
        Producer p s -> Producer (shrinkP p) (shrinkS s)
        Consumer c   -> Consumer (shrinkC c)
        Reify a      -> Reify (shrinkAcc a)

    shrinkP :: Producer index acc aenv' a -> Producer index acc aenv' a
    shrinkP p =
      case p of
        Pull src           -> Pull src
        Subarrays sh arr   -> Subarrays (shrinkE sh) arr
        Produce l f        -> Produce (shrinkE <$> l) (shrinkAF f)
        MapAccumFlat f a x -> MapAccumFlat (shrinkAF f) (shrinkAcc a) (shrinkAcc x)
        ProduceAccum l f a -> ProduceAccum (shrinkE <$> l) (shrinkAF f) (shrinkAcc a)

    shrinkC :: Consumer index acc aenv' a -> Consumer index acc aenv' a
    shrinkC c =
      case c of
        FoldSeqFlatten f a x -> FoldSeqFlatten (shrinkAF f) (shrinkAcc a) (shrinkAcc x)
        Iterate l f a        -> Iterate (shrinkE <$> l) (shrinkAF f) (shrinkAcc a)
        Conclude a d         -> Conclude (shrinkAcc a) (shrinkAcc d)
        Stuple t             -> Stuple (shrinkCT t)

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
    tup _               _               = error "Chewie, we're home."
zipWithUelt _ _                 _           = error "Aaarrrrhhggg!"

-- Update use at a specific index.
--
updateUseElt :: UseElt s -> TupleIdx (TupleRepr s) a -> UseElt a -> UseElt s
updateUseElt (UseEltTuple ut) ix = UseEltTuple . tup ut ix
  where
    tup :: Tuple UseElt t -> TupleIdx t a -> UseElt a -> Tuple UseElt t
    tup (SnocTup t s) ZeroTupIdx      a = SnocTup t (s <+.> a)
    tup (SnocTup t s) (SuccTupIdx ix) a = SnocTup (tup t ix a) s

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

instance Eq (Tuple UseElt t) where
  NilTup        == NilTup        = True
  SnocTup t1 a1 == SnocTup t2 a2 = t1 == t2 && a1 == a2

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
        | Just REFL <- match this idx   -> oneUseElt
        | otherwise                     -> zeroUseElt
      --
      Let bnd body              -> countE bnd <+.> usesOfExp (SuccIdx idx) body
      Const _                   -> zeroUseElt
      Tuple t                   -> countT t
      Prj ix (Var v)            | Just REFL <- match v idx
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
(<+>) = zipWithU (+)

zipWithU :: (Int -> Int -> Int) -> Use a -> Use a -> Use a
zipWithU f (UseArray s1 c1) (UseArray s2 c2) = UseArray (s1 `f` s2) (c1 `f` c2)
zipWithU f (UseTuple t1)    (UseTuple t2)    = UseTuple (t1 `tup` t2)
  where
    tup :: Atuple Use t -> Atuple Use t -> Atuple Use t
    tup NilAtup          NilAtup          = NilAtup
    tup (SnocAtup t1 a1) (SnocAtup t2 a2) = tup t1 t2 `SnocAtup` zipWithU f a1 a2
    tup _                _                = error "Chewie, we're home."
zipWithU _ _             _           = error "Aaarrrrhhggg!"

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
  _              == _              = error "That thing out there... That is no dinosaur"

-- Check if a condition is try for the use of all components.
--
allUse :: (Int -> Int -> Bool) -> Use t -> Bool
allUse p (UseArray s c) = p s c
allUse p (UseTuple t)   = aT t
  where
    aT :: Atuple Use t -> Bool
    aT NilAtup = True
    aT (SnocAtup t u) = aT t && allUse p u

-- Specify a certain component of a variable has been used.
--
useComponent :: Arrays a => TupleIdx (TupleRepr t) a -> Use t -> Use t
useComponent ix u = updateUse u ix oneUse

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
        | Just REFL <- match this idx   = oneUse
        | otherwise                     = zeroUse

    count :: PreOpenAcc acc aenv a -> Use s
    count pacc = case pacc of
      Avar this                 -> countIdx this
      --
      Alet bnd body             | Aprj ix a <- extract bnd
                                , Aprj ix' a' <- extract a
                                , Avar v    <- extract a'
                                , Just REFL <- match v idx
                                , u <-  countAcc ZeroIdx body
                                -> updateUse (countAcc (SuccIdx idx) body) ix' (updateUse zeroUse ix u)
                                | Aprj ix a <- extract bnd
                                , Avar v    <- extract a
                                , Just REFL <- match v idx
                                -> updateUse (countAcc (SuccIdx idx) body) ix (countAcc ZeroIdx body)
                                | otherwise
                                -> countA bnd <+> countAcc (SuccIdx idx) body

      Atuple tup                -> countAT tup
      Aprj ix a                 | Avar v <- extract a
                                , Just REFL <- match v idx
                                -> useComponent ix zeroUse
                                | otherwise
                                -> countA a
      Apply f a                 -> countAF f idx <+> countA a
      Aforeign _ _ a            -> countA a
      Acond p t e               -> countE p  <+> zipWithU max (countA t) (countA e)
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
      Collect s cs              -> maybe (usesOfPreSeq countAcc idx s) (usesOfPreSeq countAcc idx) cs

    countA :: acc aenv a -> Use s
    countA = countAcc idx

    countAF :: Kit acc
            => PreOpenAfun acc aenv' f
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
    countE = usesOfExpA countAcc idx

usesOfPreSeq :: forall acc index aenv s t. (Kit acc, Arrays s)
             => UsesOfAcc acc
             -> Idx aenv s
             -> PreOpenSeq index acc aenv t
             -> Use s
usesOfPreSeq countAcc idx seq =
  case seq of
    Producer p s -> countP p <+> usesOfPreSeq countAcc (SuccIdx idx) s
    Consumer c   -> countC c
    Reify a      -> countA a
  where
    countP :: Producer index acc aenv arrs -> Use s
    countP p =
      case p of
        Pull _             -> zeroUse
        Subarrays sh _     -> countE sh
        Produce l f        -> maybe zeroUse countE l <+> countAF f idx
        MapAccumFlat f a x -> countAF f idx <+> countA a <+> countA x
        ProduceAccum l f a -> maybe zeroUse countE l <+> countAF f idx <+> countA a

    countC :: Consumer index acc aenv arrs -> Use s
    countC c =
      case c of
        FoldSeqFlatten f a x -> countAF f idx <+> countA a <+> countA x
        Iterate l f a        -> maybe zeroUse countE l <+> countAF f idx <+> countA a
        Conclude a _         -> countA a
        Stuple t             -> countCT t

    countCT :: Atuple (PreOpenSeq index acc aenv) t' -> Use s
    countCT NilAtup        = zeroUse
    countCT (SnocAtup t c) = countCT t <+> usesOfPreSeq countAcc idx c

    countA :: acc aenv a -> Use s
    countA = countAcc idx

    countAF :: Kit acc
            => PreOpenAfun acc aenv' f
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
    Index a sh                -> countA a <+> countE sh
    LinearIndex a i           -> countA a <+> countE i
    ShapeSize sh              -> countE sh
    Intersect sh sz           -> countE sh <+> countE sz
    Union sh sz               -> countE sh <+> countE sz
    Shape a                   | Avar v    <- extract a
                              , Just REFL <- match v idx
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

-- Given an array term in environment 'aenv', determine what variables, are
-- actually referenced. This yields a new environment,
--
type DependenciesAcc acc = forall aenv t. acc aenv t -> Stronger aenv

-- A reified proof that aenv' is weaker than aenv.
--
data aenv ::> aenv' where
  WeakEmpty :: ()   ::> aenv
  WeakBase  :: aenv ::> aenv
  WeakIn    :: aenv ::> aenv' -> (aenv,a) ::> (aenv',a)
  WeakOut   :: aenv ::> aenv' -> aenv     ::> (aenv', a)

-- Existentially captures a stronger environment than aenv
--
data Stronger aenv where
  Stronger :: aenv' ::> aenv -> Stronger aenv

weakIn, weakOut :: Stronger aenv -> Stronger (aenv,a)
weakIn  (Stronger v) = Stronger (WeakIn v)
weakOut (Stronger v) = Stronger (WeakOut v)

dropTop :: Stronger (aenv, a) -> Stronger aenv
dropTop (Stronger WeakEmpty) = Stronger WeakEmpty
dropTop (Stronger WeakBase) = Stronger WeakBase
dropTop (Stronger (WeakIn rv)) = Stronger rv
dropTop (Stronger (WeakOut rv)) = Stronger rv

single :: Idx aenv t -> Stronger aenv
single ZeroIdx = weakIn mempty
single (SuccIdx ix) = weakOut (single ix)

instance Monoid (Stronger env) where
  mempty = Stronger WeakEmpty

  mappend (Stronger WeakEmpty) (Stronger v) = Stronger v
  mappend (Stronger v) (Stronger WeakEmpty) = Stronger v
  mappend (Stronger WeakBase) (Stronger _)  = Stronger WeakBase
  mappend (Stronger _) (Stronger WeakBase) = Stronger WeakBase
  mappend (Stronger (WeakIn v)) (Stronger (WeakIn v')) = weakIn (Stronger v <> Stronger v')
  mappend (Stronger (WeakIn v)) (Stronger (WeakOut v')) = weakIn (Stronger v <> Stronger v')
  mappend (Stronger (WeakOut v)) (Stronger (WeakIn v')) = weakIn (Stronger v <> Stronger v')
  mappend (Stronger (WeakOut v)) (Stronger (WeakOut v')) = weakOut (Stronger v <> Stronger v')

dependenciesOpenAcc :: OpenAcc aenv t -> Stronger aenv
dependenciesOpenAcc (OpenAcc acc) = dependenciesPreAcc dependenciesOpenAcc acc

dependenciesPreAcc
    :: forall acc aenv t. Kit acc
    => DependenciesAcc  acc
    -> PreOpenAcc acc aenv t
    -> Stronger aenv
dependenciesPreAcc depsAcc = deps
  where
    deps :: PreOpenAcc acc aenv a -> Stronger aenv
    deps pacc = case pacc of
      Avar this                 -> single this
      --
      Alet bnd body             -> depsAcc bnd <> dropTop (depsAcc body)
      Atuple tup                -> depsAT tup
      Aprj _ a                  -> depsAcc a
      Apply f a                 -> depsAF f <> depsAcc a
      Aforeign _ _ a            -> depsAcc a
      Acond p t e               -> depsE p  <> depsAcc t <> depsAcc e
      Awhile p f a              -> depsAF p <> depsAF f <> depsAcc a
      Use _                     -> mempty
      Subarray ix sh _          -> depsE ix <> depsE sh
      Unit e                    -> depsE e
      Reshape e a               -> depsE e  <> depsAcc a
      Generate e f              -> depsE e  <> depsF f
      Transform sh ix f a       -> depsE sh <> depsF ix <> depsF f  <> depsAcc a
      Replicate _ sh a          -> depsE sh <> depsAcc a
      Slice _ a sl              -> depsE sl <> depsAcc a
      Map f a                   -> depsF f  <> depsAcc a
      ZipWith f a1 a2           -> depsF f  <> depsAcc a1 <> depsAcc a2
      Fold f z a                -> depsF f  <> depsE z  <> depsAcc a
      Fold1 f a                 -> depsF f  <> depsAcc a
      FoldSeg f z a s           -> depsF f  <> depsE z  <> depsAcc a  <> depsAcc s
      Fold1Seg f a s            -> depsF f  <> depsAcc a  <> depsAcc s
      Scanl f z a               -> depsF f  <> depsE z  <> depsAcc a
      Scanl' f z a              -> depsF f  <> depsE z  <> depsAcc a
      Scanl1 f a                -> depsF f  <> depsAcc a
      Scanr f z a               -> depsF f  <> depsE z  <> depsAcc a
      Scanr' f z a              -> depsF f  <> depsE z  <> depsAcc a
      Scanr1 f a                -> depsF f  <> depsAcc a
      Permute f1 a1 f2 a2       -> depsF f1 <> depsAcc a1 <> depsF f2 <> depsAcc a2
      Backpermute sh f a        -> depsE sh <> depsF f  <> depsAcc a
      Stencil f _ a             -> depsF f  <> depsAcc a
      Stencil2 f _ a1 _ a2      -> depsF f  <> depsAcc a1 <> depsAcc a2
      Collect s cs              -> dependenciesPreSeq depsAcc s  <> maybe mempty (dependenciesPreSeq depsAcc) cs

    depsAF :: Kit acc
           => PreOpenAfun acc aenv' f
           -> Stronger aenv'
    depsAF = dependenciesAfun depsAcc

    depsF :: PreOpenFun acc env aenv f -> Stronger aenv
    depsF (Lam  f) = depsF f
    depsF (Body b) = depsE b

    depsAT :: Atuple (acc aenv) a -> Stronger aenv
    depsAT NilAtup        = mempty
    depsAT (SnocAtup t a) = depsAT t <> depsAcc a

    depsE :: PreOpenExp acc env aenv e -> Stronger aenv
    depsE = dependenciesExp depsAcc

dependenciesAfun :: DependenciesAcc acc
                    -> PreOpenAfun acc aenv t
                    -> Stronger aenv
dependenciesAfun depsAcc (Alam f)  = dropTop (dependenciesAfun depsAcc f)
dependenciesAfun depsAcc (Abody a) = depsAcc a

dependenciesPreSeq :: forall acc index aenv t. Kit acc
                   => DependenciesAcc acc
                   -> PreOpenSeq index acc aenv t
                   -> Stronger aenv
dependenciesPreSeq depsAcc seq =
  case seq of
    Producer p s -> dependenciesProducer depsAcc p <> dropTop (dependenciesPreSeq depsAcc s)
    Consumer c   -> dependenciesConsumer depsAcc c
    Reify a      -> depsAcc a

dependenciesProducer :: forall acc index aenv arrs. Kit acc
                     => DependenciesAcc acc
                     -> Producer index acc aenv arrs
                     -> Stronger aenv
dependenciesProducer depsAcc p =
  case p of
    Pull _             -> mempty
    Subarrays sh _     -> depsE sh
    Produce l f        -> maybe mempty depsE l <> depsAF f
    MapAccumFlat f a x -> depsAF f <> depsAcc a <> depsAcc x
    ProduceAccum l f a -> maybe mempty depsE l <> depsAF f <> depsAcc a
  where
    depsAF :: Kit acc
            => PreOpenAfun acc aenv' f
            -> Stronger aenv'
    depsAF = dependenciesAfun depsAcc

    depsE :: PreOpenExp acc env aenv e -> Stronger aenv
    depsE = dependenciesExp depsAcc

dependenciesConsumer :: forall acc index aenv arrs. Kit acc
                     => DependenciesAcc acc
                     -> Consumer index acc aenv arrs
                     -> Stronger aenv
dependenciesConsumer depsAcc c =
  case c of
    FoldSeqFlatten f a x -> depsAF f <> depsAcc a <> depsAcc x
    Iterate l f a        -> maybe mempty depsE l <> depsAF f <> depsAcc a
    Conclude a d         -> depsAcc a <> depsAcc d
    Stuple t             -> depsCT t
  where
    depsCT :: Atuple (PreOpenSeq index acc aenv) t' -> Stronger aenv
    depsCT NilAtup        = mempty
    depsCT (SnocAtup t c) = depsCT t <> dependenciesPreSeq depsAcc c

    depsAF :: Kit acc
            => PreOpenAfun acc aenv' f
            -> Stronger aenv'
    depsAF = dependenciesAfun depsAcc

    depsE :: PreOpenExp acc env aenv e -> Stronger aenv
    depsE = dependenciesExp depsAcc

dependenciesExp :: forall acc env aenv e.
                   DependenciesAcc acc
                -> PreOpenExp acc env aenv e
                -> Stronger aenv
dependenciesExp depsAcc exp =
  case exp of
    Let bnd body              -> depsE bnd <> depsE body
    Var _                     -> mempty
    Const _                   -> mempty
    Tuple t                   -> depsT t
    Prj _ e                   -> depsE e
    IndexNil                  -> mempty
    IndexCons sl sz           -> depsE sl <> depsE sz
    IndexHead sh              -> depsE sh
    IndexTail sh              -> depsE sh
    IndexTrans sh             -> depsE sh
    IndexSlice _ _ sh         -> depsE sh
    IndexFull _ ix sl         -> depsE ix <> depsE sl
    IndexAny                  -> mempty
    ToIndex sh ix             -> depsE sh <> depsE ix
    FromIndex sh i            -> depsE sh <> depsE i
    ToSlice _ sh i            -> depsE sh <> depsE i
    Cond p t e                -> depsE p  <> depsE t <> depsE e
    While p f x               -> depsF p  <> depsF f <> depsE x
    PrimConst _               -> mempty
    PrimApp _ x               -> depsE x
    Index a sh                -> depsAcc a <> depsE sh
    LinearIndex a i           -> depsAcc a <> depsE i
    ShapeSize sh              -> depsE sh
    Intersect sh sz           -> depsE sh <> depsE sz
    Union sh sz               -> depsE sh <> depsE sz
    Shape a                   -> depsAcc a
    Foreign _ _ e             -> depsE e

  where
    depsE :: PreOpenExp acc env' aenv e' -> Stronger aenv
    depsE = dependenciesExp depsAcc

    depsT :: Tuple (PreOpenExp acc env aenv) e' -> Stronger aenv
    depsT NilTup        = mempty
    depsT (SnocTup t e) = depsT t <> depsE e

    depsF :: PreOpenFun acc env' aenv f -> Stronger aenv
    depsF (Lam  f) = depsF f
    depsF (Body b) = depsE b
