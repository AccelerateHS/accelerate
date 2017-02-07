{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo.Dependency
-- Copyright   : [2016] Robert Clifton-Everest
-- License     : BSD3
--
-- Maintainer  : Robert Clifton-Everest <robertce@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
--

module Data.Array.Accelerate.Trafo.Dependency (

  -- Dependency analysis
  (::>)(..), Stronger(..), weakIn, weakOut,
  DependenciesAcc, dependenciesOpenAcc,
  dependenciesPreAcc, dependenciesAfun,
  dependenciesProducer, dependenciesConsumer,
  dependenciesExp, dependenciesFun,

) where

-- standard library
import Data.Monoid                                      hiding ( Last )
import Prelude                                          hiding ( exp, seq )

-- friends
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Array.Sugar               hiding ( Any )

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
    :: forall acc aenv t. DependenciesAcc  acc
    -> PreOpenAcc acc aenv t
    -> Stronger aenv
dependenciesPreAcc depsAcc = deps
  where
    deps :: PreOpenAcc acc aenv a -> Stronger aenv
    deps pacc = case pacc of
      Avar this              -> single this
      --
      Alet bnd body          -> depsAcc bnd <> dropTop (depsAcc body)
      Atuple tup             -> depsAT tup
      Aprj _ a               -> depsAcc a
      Apply f a              -> depsAF f <> depsAcc a
      Aforeign _ _ a         -> depsAcc a
      Acond p t e            -> depsE p  <> depsAcc t <> depsAcc e
      Awhile p f a           -> depsAF p <> depsAF f <> depsAcc a
      Use _                  -> mempty
      Subarray ix sh _       -> depsE ix <> depsE sh
      Unit e                 -> depsE e
      Reshape e a            -> depsE e  <> depsAcc a
      Generate e f           -> depsE e  <> depsF f
      Transform sh ix f a    -> depsE sh <> depsF ix <> depsF f  <> depsAcc a
      Replicate _ sh a       -> depsE sh <> depsAcc a
      Slice _ a sl           -> depsE sl <> depsAcc a
      Map f a                -> depsF f  <> depsAcc a
      ZipWith f a1 a2        -> depsF f  <> depsAcc a1 <> depsAcc a2
      Fold f z a             -> depsF f  <> depsE z  <> depsAcc a
      Fold1 f a              -> depsF f  <> depsAcc a
      FoldSeg f z a s        -> depsF f  <> depsE z  <> depsAcc a  <> depsAcc s
      Fold1Seg f a s         -> depsF f  <> depsAcc a  <> depsAcc s
      Scanl f z a            -> depsF f  <> depsE z  <> depsAcc a
      Scanl' f z a           -> depsF f  <> depsE z  <> depsAcc a
      Scanl1 f a             -> depsF f  <> depsAcc a
      Scanr f z a            -> depsF f  <> depsE z  <> depsAcc a
      Scanr' f z a           -> depsF f  <> depsE z  <> depsAcc a
      Scanr1 f a             -> depsF f  <> depsAcc a
      Permute f1 a1 f2 a2    -> depsF f1 <> depsAcc a1 <> depsF f2 <> depsAcc a2
      Backpermute sh f a     -> depsE sh <> depsF f  <> depsAcc a
      Stencil f _ a          -> depsF f  <> depsAcc a
      Stencil2 f _ a1 _ a2   -> depsF f  <> depsAcc a1 <> depsAcc a2
      Collect min max i s    -> depsE min <> maybe mempty depsE max <> maybe mempty depsE i
                             <> dependenciesPreSeq depsAcc s

    depsAF :: PreOpenAfun acc aenv' f
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

dependenciesPreSeq :: forall acc index aenv t. DependenciesAcc acc
                   -> PreOpenSeq index acc aenv t
                   -> Stronger aenv
dependenciesPreSeq depsAcc seq =
  case seq of
    Producer p s -> dependenciesProducer depsAcc p <> dropTop (dependenciesPreSeq depsAcc s)
    Consumer c   -> dependenciesConsumer depsAcc c
    Reify _ a    -> depsAcc a

dependenciesProducer :: forall acc index aenv arrs. DependenciesAcc acc
                     -> Producer index acc aenv arrs
                     -> Stronger aenv
dependenciesProducer depsAcc p =
  case p of
    Pull _              -> mempty
    Subarrays sh _      -> depsE sh
    FromSegs s n vs     -> depsAcc s <> depsE n <> depsAcc vs
    Produce l f         -> maybe mempty depsE l <> depsAF f
    -- MapBatch f c c' a x -> depsAF f <> depsAF c <> depsAF c' <> depsAcc a <> depsAcc x
    ProduceAccum l f a  -> maybe mempty depsE l <> depsAF f <> depsAcc a
  where
    depsAF :: PreOpenAfun acc aenv' f
           -> Stronger aenv'
    depsAF = dependenciesAfun depsAcc

    depsE :: PreOpenExp acc env aenv e -> Stronger aenv
    depsE = dependenciesExp depsAcc

dependenciesConsumer :: forall acc index aenv arrs. DependenciesAcc acc
                     -> Consumer index acc aenv arrs
                     -> Stronger aenv
dependenciesConsumer depsAcc c =
  case c of
    FoldBatch f a x -> depsAF f <> depsAcc a <> depsAcc x
    Last a d        -> depsAcc a <> depsAcc d
    Stuple t        -> depsCT t
    Tabulate x      -> depsAcc x
    Elements x      -> depsAcc x
  where
    depsCT :: Atuple (PreOpenSeq index acc aenv) t' -> Stronger aenv
    depsCT NilAtup        = mempty
    depsCT (SnocAtup t c) = depsCT t <> dependenciesPreSeq depsAcc c

    depsAF :: PreOpenAfun acc aenv' f
           -> Stronger aenv'
    depsAF = dependenciesAfun depsAcc

dependenciesExp :: forall acc env aenv e. DependenciesAcc acc
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
    depsF = dependenciesFun depsAcc

dependenciesFun :: DependenciesAcc acc -> PreOpenFun acc env' aenv f -> Stronger aenv
dependenciesFun depsAcc (Lam  f) = dependenciesFun depsAcc f
dependenciesFun depsAcc (Body b) = dependenciesExp depsAcc b
