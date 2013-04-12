{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE PatternGuards        #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing      #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo.Fusion
-- Copyright   : [2012..2013] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module implements producer/producer and consumer/producer fusion as a
-- term rewriting of the Accelerate AST.
--
-- The function 'quench' perform the source-to-source fusion transformation,
-- while 'anneal' additionally makes the representation of embedded producers
-- explicit by representing the AST as a 'DelayedAcc' of manifest and delayed
-- nodes.
--

module Data.Array.Accelerate.Trafo.Fusion (

  -- * Types
  DelayedAcc, DelayedAfun, DelayedOpenAcc(..),
  DelayedExp, DelayedFun, DelayedOpenExp, DelayedOpenFun,

  -- * Fusion
  annealAcc, annealAfun,
  quenchAcc, quenchAfun,

) where

-- standard library
import Prelude                                          hiding ( exp, until )
import Text.PrettyPrint

-- friends
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Pretty.Print
import Data.Array.Accelerate.Trafo.Common
import Data.Array.Accelerate.Trafo.Shrink
import Data.Array.Accelerate.Trafo.Simplify
import Data.Array.Accelerate.Trafo.Substitution
import Data.Array.Accelerate.Array.Representation       ( SliceIndex(..) )
import Data.Array.Accelerate.Array.Sugar                ( Array, Arrays(..), ArraysR(..), ArrRepr', Elt, EltRepr, Shape )
import Data.Array.Accelerate.Tuple

import qualified Data.Array.Accelerate.Debug            as Stats

#include "accelerate.h"


-- Delayed Arrays
-- ==============

-- The type of delayed arrays. This representation is used to annotate the AST
-- in the recursive knot to distinguish standard AST terms from operand arrays
-- that should be embedded into their consumers.
--
type DelayedAcc a       = DelayedOpenAcc () a
type DelayedAfun        = PreOpenAfun DelayedOpenAcc

type DelayedExp         = DelayedOpenExp ()
type DelayedFun         = DelayedOpenFun ()
type DelayedOpenAfun    = PreOpenAfun DelayedOpenAcc
type DelayedOpenExp     = PreOpenExp DelayedOpenAcc
type DelayedOpenFun     = PreOpenFun DelayedOpenAcc

data DelayedOpenAcc aenv a where
  Manifest              :: PreOpenAcc DelayedOpenAcc aenv a -> DelayedOpenAcc aenv a

  Delayed               :: (Shape sh, Elt e) =>
    { extentD           :: PreExp DelayedOpenAcc aenv sh
    , indexD            :: PreFun DelayedOpenAcc aenv (sh  -> e)
    , linearIndexD      :: PreFun DelayedOpenAcc aenv (Int -> e)
    }                   -> DelayedOpenAcc aenv (Array sh e)

instance Kit DelayedOpenAcc where
  termOut       = Manifest
  rebuildAcc    = error "DelayedAcc.rebuildAcc"
  matchAcc      = error "DelayedAcc.matchAcc"
  hashAcc       = error "DelayedAcc.hashAcc"
  prettyAcc     = error "DelayedAcc.prettyAcc"


-- | Apply the fusion transformation _and also_ embed the delayed representation
-- of producers into the AST. This makes fused consumer/producer terms explicit.
--
quenchAcc :: Arrays arrs => OpenAcc aenv arrs -> DelayedOpenAcc aenv arrs
quenchAcc = cvtA . annealAcc
  where
    -- Convert array computations into an embeddable delayed representation.
    -- This is essentially the reverse of 'compute'. While this is defined
    -- recursively on the array arguments (for map, etc), this is guaranteed to
    -- be an Avar.
    --
    embed :: (Shape sh, Elt e) => OpenAcc aenv (Array sh e) -> DelayedOpenAcc aenv (Array sh e)
    embed (OpenAcc pacc) =
      case pacc of
        Avar v
          -> Delayed (arrayShape v) (indexArray v) (linearIndex v)

        Generate (cvtE -> sh) (cvtF -> f)
          -> Delayed sh f (f `compose` fromIndex sh)

        Map (cvtF -> f) (embed -> Delayed{..})
          -> Delayed extentD (f `compose` indexD) (f `compose` linearIndexD)

        Backpermute (cvtE -> sh) (cvtF -> p) (embed -> Delayed{..})
          -> let p' = indexD `compose` p
             in  Delayed sh p'(p' `compose` fromIndex sh)

        Transform (cvtE -> sh) (cvtF -> p) (cvtF -> f) (embed -> Delayed{..})
          -> let f' = f `compose` indexD `compose` p
             in  Delayed sh f' (f' `compose` fromIndex sh)

        _ -> INTERNAL_ERROR(error) "quench" "tried to consume a non-embeddable term"

    fusionError = INTERNAL_ERROR(error) "quench" "unexpected fusible materials"

    -- Convert array programs as manifest terms.
    --
    cvtA :: OpenAcc aenv a -> DelayedOpenAcc aenv a
    cvtA (OpenAcc pacc) = Manifest $
      case pacc of
        -- Non-fusible terms
        -- -----------------
        Avar ix                 -> Avar ix
        Use arr                 -> Use arr
        Unit e                  -> Unit (cvtE e)
        Alet bnd body           -> Alet (cvtA bnd) (cvtA body)
        Acond p t e             -> Acond (cvtE p) (cvtA t) (cvtA e)
        Atuple tup              -> Atuple (cvtAT tup)
        Aprj ix tup             -> Aprj ix (cvtA tup)
        Apply f a               -> Apply (cvtAF f) (cvtA a)
        Aforeign ff f a         -> Aforeign ff (cvtAF f) (cvtA a)

        -- Producers
        -- ---------
        --
        -- Some producers might still exist as a manifest array. Typically
        -- this is because they are the last stage of the computation, or the
        -- result of a let-binding to be used multiple times.
        --
        Map f a                 -> Map (cvtF f) (embed a)
        Generate sh f           -> Generate (cvtE sh) (cvtF f)
        Transform sh p f a      -> Transform (cvtE sh) (cvtF p) (cvtF f) (embed a)
        Backpermute sh p a      -> Backpermute (cvtE sh) (cvtF p) (embed a)
        Reshape{}               -> fusionError
        Replicate{}             -> fusionError
        Slice{}                 -> fusionError
        ZipWith{}               -> fusionError

        -- Consumers
        -- ---------
        --
        -- Embed producers directly into the representation. For stencils we
        -- make an exception. Since these consumers access elements of the
        -- argument array multiple times, we are careful not to duplicate work
        -- and instead force the argument to be a manifest array.
        --
        Fold f z a              -> Fold     (cvtF f) (cvtE z) (embed a)
        Fold1 f a               -> Fold1    (cvtF f) (embed a)
        FoldSeg f z a s         -> FoldSeg  (cvtF f) (cvtE z) (embed a) (embed s)
        Fold1Seg f a s          -> Fold1Seg (cvtF f) (embed a) (embed s)
        Scanl f z a             -> Scanl    (cvtF f) (cvtE z) (embed a)
        Scanl1 f a              -> Scanl1   (cvtF f) (embed a)
        Scanl' f z a            -> Scanl'   (cvtF f) (cvtE z) (embed a)
        Scanr f z a             -> Scanr    (cvtF f) (cvtE z) (embed a)
        Scanr1 f a              -> Scanr1   (cvtF f) (embed a)
        Scanr' f z a            -> Scanr'   (cvtF f) (cvtE z) (embed a)
        Permute f d p a         -> Permute  (cvtF f) (embed d) (cvtF p) (embed a)
        Stencil f x a           -> Stencil  (cvtF f) x (cvtA a)
        Stencil2 f x a y b      -> Stencil2 (cvtF f) x (cvtA a) y (cvtA b)

    cvtAT :: Atuple (OpenAcc aenv) a -> Atuple (DelayedOpenAcc aenv) a
    cvtAT NilAtup        = NilAtup
    cvtAT (SnocAtup t a) = cvtAT t `SnocAtup` cvtA a

    cvtAF :: OpenAfun aenv f -> PreOpenAfun DelayedOpenAcc aenv f
    cvtAF (Alam f)  = Alam  (cvtAF f)
    cvtAF (Abody b) = Abody (cvtA b)

    -- Conversions for closed scalar functions and expressions
    --
    cvtF :: OpenFun env aenv f -> DelayedOpenFun env aenv f
    cvtF (Lam f)  = Lam (cvtF f)
    cvtF (Body b) = Body (cvtE b)

    cvtE :: OpenExp env aenv t -> DelayedOpenExp env aenv t
    cvtE exp =
      case exp of
        Let bnd body            -> Let (cvtE bnd) (cvtE body)
        Var ix                  -> Var ix
        Const c                 -> Const c
        Tuple tup               -> Tuple (cvtT tup)
        Prj ix t                -> Prj ix (cvtE t)
        IndexNil                -> IndexNil
        IndexCons sh sz         -> IndexCons (cvtE sh) (cvtE sz)
        IndexHead sh            -> IndexHead (cvtE sh)
        IndexTail sh            -> IndexTail (cvtE sh)
        IndexAny                -> IndexAny
        IndexSlice x ix sh      -> IndexSlice x (cvtE ix) (cvtE sh)
        IndexFull x ix sl       -> IndexFull x (cvtE ix) (cvtE sl)
        ToIndex sh ix           -> ToIndex (cvtE sh) (cvtE ix)
        FromIndex sh ix         -> FromIndex (cvtE sh) (cvtE ix)
        Cond p t e              -> Cond (cvtE p) (cvtE t) (cvtE e)
        Iterate n f x           -> Iterate (cvtE n) (cvtE f) (cvtE x)
        PrimConst c             -> PrimConst c
        PrimApp f x             -> PrimApp f (cvtE x)
        Index a sh              -> Index (cvtA a) (cvtE sh)
        LinearIndex a i         -> LinearIndex (cvtA a) (cvtE i)
        Shape a                 -> Shape (cvtA a)
        ShapeSize sh            -> ShapeSize (cvtE sh)
        Intersect s t           -> Intersect (cvtE s) (cvtE t)
        Foreign ff f e          -> Foreign ff (cvtF f) (cvtE e)

    cvtT :: Tuple (OpenExp env aenv) t -> Tuple (DelayedOpenExp env aenv) t
    cvtT NilTup        = NilTup
    cvtT (SnocTup t e) = cvtT t `SnocTup` cvtE e


quenchAfun :: OpenAfun aenv f -> DelayedAfun aenv f
quenchAfun (Alam  f) = Alam  (quenchAfun f)
quenchAfun (Abody b) = Abody (quenchAcc b)


-- | Apply the fusion transformation to the AST to combine and simplify terms.
-- This combines producer/producer terms and makes consumer/producer nodes
-- adjacent.
--
annealAcc :: Arrays arrs => OpenAcc aenv arrs -> OpenAcc aenv arrs
annealAcc = shrinkAcc . computeAcc . delayAcc
  where
    delayAcc :: Arrays a => OpenAcc aenv a -> Delayed OpenAcc aenv a
    delayAcc (OpenAcc pacc) = delayPreAcc delayAcc elimAcc pacc

    countAcc :: UsesOfAcc OpenAcc
    countAcc ok idx (OpenAcc pacc) = usesOfPreAcc ok countAcc idx pacc

    shrinkAcc :: ShrinkAcc OpenAcc
    shrinkAcc (OpenAcc pacc) = OpenAcc (shrinkPreAcc shrinkAcc (basicReduceAcc unwrapAcc countAcc) pacc)
    unwrapAcc (OpenAcc pacc) = pacc

    -- When does the cost of re-computation outweigh that of memory access? For
    -- the moment only do the substitution on a single use of the bound array
    -- into the use site, but it is likely advantageous to be far more
    -- aggressive here.
    --
    elimAcc :: Idx aenv s -> OpenAcc aenv t -> Bool
    elimAcc v acc = countAcc False v acc <= lIMIT
      where
        lIMIT = 1


annealAfun :: OpenAfun aenv f -> OpenAfun aenv f
annealAfun (Alam  f) = Alam  (annealAfun f)
annealAfun (Abody b) = Abody (annealAcc b)


-- | Recast terms into the internal fusion delayed array representation to be
-- forged into combined terms. Using the reduced internal form limits the number
-- of combinations that need to be considered.
--
type DelayAcc acc = forall aenv arrs. Arrays arrs => acc aenv arrs -> Delayed acc aenv arrs
type ElimAcc  acc = forall aenv s t. Idx aenv s -> acc aenv t -> Bool

{-# SPECIALISE
      delayPreAcc :: Arrays a
                  => DelayAcc   OpenAcc
                  -> ElimAcc    OpenAcc
                  -> PreOpenAcc OpenAcc aenv a
                  -> Delayed    OpenAcc aenv a
 #-}

delayPreAcc
    :: forall acc aenv arrs. (Kit acc, Arrays arrs)
    => DelayAcc   acc
    -> ElimAcc    acc
    -> PreOpenAcc acc aenv arrs
    -> Delayed    acc aenv arrs
delayPreAcc delayAcc elimAcc pacc =
  case pacc of

    -- Non-fusible terms
    -- -----------------
    --
    -- Solid and semi-solid terms that we generally do not which to fuse, such
    -- as control flow (|?), array introduction (use, unit), array tupling and
    -- projection, and foreign function operations. Generally we also do not
    -- want to fuse past array let bindings, as this would imply work
    -- duplication. SEE: [Sharing vs. Fusion]
    --
    Alet bnd body       -> aletD delayAcc elimAcc bnd body
    Acond p at ae       -> acondD delayAcc (cvtE p) at ae
    Atuple tup          -> done $ Atuple (cvtAT tup)
    Aprj ix tup         -> done $ Aprj ix (cvtA tup)
    Apply f a           -> done $ Apply (cvtAF f) (cvtA a)
    Aforeign ff f a     -> done $ Aforeign ff (cvtAF f) (cvtA a)

    -- Array injection
    Avar v              -> done $ Avar v
    Use arrs            -> done $ Use arrs
    Unit e              -> done $ Unit (cvtE e)

    -- Producers
    -- ---------
    --
    -- The class of operations that given a set of zero or more input arrays,
    -- produce a _single_ element for the output array by manipulating a
    -- _single_ element from each input array. These can be further classified
    -- as value (map, zipWith) or index space (backpermute, slice, replicate)
    -- transformations.
    --
    -- The critical feature is that each element of the output is produced
    -- independently of all others, and so we can aggressively fuse arbitrary
    -- sequences of these operations.
    --
    Generate sh f       -> generateD (cvtE sh) (cvtF f)

    Map f a             -> fuse  (into  mapD              (cvtF f)) a
    ZipWith f a b       -> fuse2 (into  zipWithD          (cvtF f)) a b
    Transform sh p f a  -> fuse  (into3 transformD        (cvtE sh) (cvtF p) (cvtF f)) a

    Backpermute sl p a  -> fuse  (into2 backpermuteD      (cvtE sl) (cvtF p)) a
    Slice slix a sl     -> fuse  (into  (sliceD slix)     (cvtE sl)) a
    Replicate slix sh a -> fuse  (into  (replicateD slix) (cvtE sh)) a
    Reshape sl a        -> fuse  (into  reshapeD          (cvtE sl)) a

    -- Consumers
    -- ---------
    --
    -- Operations where each element of the output array depends on multiple
    -- elements of the input array. To implement these operations efficiently in
    -- parallel, we need to know how elements of the array depend on each other:
    -- a parallel scan is implemented very differently from a parallel fold, for
    -- example.
    --
    -- In order to avoid obfuscating this crucial information required for
    -- parallel implementation, fusion is separated into to phases:
    -- producer/producer, implemented above, and consumer/producer, which is
    -- implemented below. This will place producers adjacent to the consumer
    -- node, so that the producer can be directly embedded into the consumer
    -- during the code generation phase.
    --
    Fold f z a          -> embed  (into2 Fold          (cvtF f) (cvtE z)) a
    Fold1 f a           -> embed  (into  Fold1         (cvtF f)) a
    FoldSeg f z a s     -> embed2 (into2 FoldSeg       (cvtF f) (cvtE z)) a s
    Fold1Seg f a s      -> embed2 (into  Fold1Seg      (cvtF f)) a s
    Scanl f z a         -> embed  (into2 Scanl         (cvtF f) (cvtE z)) a
    Scanl1 f a          -> embed  (into  Scanl1        (cvtF f)) a
    Scanl' f z a        -> embed  (into2 Scanl'        (cvtF f) (cvtE z)) a
    Scanr f z a         -> embed  (into2 Scanr         (cvtF f) (cvtE z)) a
    Scanr1 f a          -> embed  (into  Scanr1        (cvtF f)) a
    Scanr' f z a        -> embed  (into2 Scanr'        (cvtF f) (cvtE z)) a
    Permute f d p a     -> embed2 (into2 permute       (cvtF f) (cvtF p)) d a
    Stencil f x a       -> embed  (into (stencil x)    (cvtF f)) a
    Stencil2 f x a y b  -> embed2 (into (stencil2 x y) (cvtF f)) a b

  where
    cvtA :: Arrays a => acc aenv' a -> acc aenv' a
    cvtA = computeAcc . delayAcc

    cvtAT :: Atuple (acc aenv') a -> Atuple (acc aenv') a
    cvtAT NilAtup          = NilAtup
    cvtAT (SnocAtup tup a) = cvtAT tup `SnocAtup` cvtA a

    cvtAF :: PreOpenAfun acc aenv' f -> PreOpenAfun acc aenv' f
    cvtAF (Alam  f) = Alam  (cvtAF f)
    cvtAF (Abody a) = Abody (cvtA a)

    -- Helpers to shuffle the order of arguments to a constructor
    --
    permute f p d a     = Permute f d p a
    stencil x f a       = Stencil f x a
    stencil2 x y f a b  = Stencil2 f x a y b

    -- Conversions for closed scalar functions and expressions, with
    -- pre-simplification.
    --
    cvtF :: PreFun acc aenv t -> PreFun acc aenv t
    cvtF = cvtF' . simplify

    cvtE :: PreExp acc aenv t -> PreExp acc aenv t
    cvtE = cvtE' . simplify

    -- Conversions for scalar functions and expressions without
    -- pre-simplification. Hence we can operate on open expressions.
    --
    cvtF' :: PreOpenFun acc env aenv' t -> PreOpenFun acc env aenv' t
    cvtF' (Lam f)  = Lam  (cvtF' f)
    cvtF' (Body b) = Body (cvtE' b)

    cvtE' :: PreOpenExp acc env aenv' t -> PreOpenExp acc env aenv' t
    cvtE' exp =
      case exp of
        Let bnd body            -> Let (cvtE' bnd) (cvtE' body)
        Var ix                  -> Var ix
        Const c                 -> Const c
        Tuple tup               -> Tuple (cvtT tup)
        Prj tup ix              -> Prj tup (cvtE' ix)
        IndexNil                -> IndexNil
        IndexCons sh sz         -> IndexCons (cvtE' sh) (cvtE' sz)
        IndexHead sh            -> IndexHead (cvtE' sh)
        IndexTail sh            -> IndexTail (cvtE' sh)
        IndexAny                -> IndexAny
        IndexSlice x ix sh      -> IndexSlice x (cvtE' ix) (cvtE' sh)
        IndexFull x ix sl       -> IndexFull x (cvtE' ix) (cvtE' sl)
        ToIndex sh ix           -> ToIndex (cvtE' sh) (cvtE' ix)
        FromIndex sh ix         -> FromIndex (cvtE' sh) (cvtE' ix)
        Cond p t e              -> Cond (cvtE' p) (cvtE' t) (cvtE' e)
        Iterate n f x           -> Iterate (cvtE' n) (cvtE' f) (cvtE' x)
        PrimConst c             -> PrimConst c
        PrimApp f x             -> PrimApp f (cvtE' x)
        Index a sh              -> Index (cvtA a) (cvtE' sh)
        LinearIndex a i         -> LinearIndex (cvtA a) (cvtE' i)
        Shape a                 -> Shape (cvtA a)
        ShapeSize sh            -> ShapeSize (cvtE' sh)
        Intersect s t           -> Intersect (cvtE' s) (cvtE' t)
        Foreign ff f e          -> Foreign ff (cvtF' f) (cvtE' e)

    cvtT :: Tuple (PreOpenExp acc env aenv') t -> Tuple (PreOpenExp acc env aenv') t
    cvtT NilTup          = NilTup
    cvtT (SnocTup tup e) = cvtT tup `SnocTup` cvtE' e

    -- Helpers to embed and fuse delayed terms
    --
    into :: Sink f => (f env' a -> b) -> f env a -> Extend acc env env' -> b
    into op a env = op (sink env a)

    into2 :: (Sink f1, Sink f2)
          => (f1 env' a -> f2 env' b -> c) -> f1 env a -> f2 env b -> Extend acc env env' -> c
    into2 op a b env = op (sink env a) (sink env b)

    into3 :: (Sink f1, Sink f2, Sink f3)
          => (f1 env' a -> f2 env' b -> f3 env' c -> d) -> f1 env a -> f2 env b -> f3 env c -> Extend acc env env' -> d
    into3 op a b c env = op (sink env a) (sink env b) (sink env c)

    fuse :: Arrays as
         => (forall aenv'. Extend acc aenv aenv' -> Cunctation acc aenv' as -> Cunctation acc aenv' bs)
         ->         acc aenv as
         -> Delayed acc aenv bs
    fuse op (delayAcc -> Term env cc) = Term env (op env cc)

    fuse2 :: (Arrays as, Arrays bs)
          => (forall aenv'. Extend acc aenv aenv' -> Cunctation acc aenv' as -> Cunctation acc aenv' bs -> Cunctation acc aenv' cs)
          ->         acc aenv as
          ->         acc aenv bs
          -> Delayed acc aenv cs
    fuse2 op a1 a0
      | Term env1 cc1   <- delayAcc a1
      , Term env0 cc0   <- delayAcc (sink env1 a0)
      , env             <- env1 `join` env0
      = Term env (op env (sink env0 cc1) cc0)

    embed :: (Arrays as, Arrays bs)
          => (forall aenv'. Extend acc aenv aenv' -> acc aenv' as -> PreOpenAcc acc aenv' bs)
          ->         acc aenv as
          -> Delayed acc aenv bs
    embed op (delayAcc -> Term env cc) = case cc of
      Done v        -> Term (env `PushEnv` op env (avarIn v)) (Done ZeroIdx)
      Step sh p f v -> Term (env `PushEnv` op env (computeAcc (Term BaseEnv (Step sh p f v)))) (Done ZeroIdx)
      Yield sh f    -> Term (env `PushEnv` op env (computeAcc (Term BaseEnv (Yield sh f)))) (Done ZeroIdx)

    embed2 :: forall aenv as bs cs. (Arrays as, Arrays bs, Arrays cs)
           => (forall aenv'. Extend acc aenv aenv' -> acc aenv' as -> acc aenv' bs -> PreOpenAcc acc aenv' cs)
           ->         acc aenv as
           ->         acc aenv bs
           -> Delayed acc aenv cs
    embed2 op (delayAcc -> Term env1 cc1) a0 = case cc1 of
      Done v        -> inner env1 v a0
      Step sh p f v -> inner (env1 `PushEnv` compute (Term BaseEnv (Step sh p f v))) ZeroIdx a0
      Yield sh f    -> inner (env1 `PushEnv` compute (Term BaseEnv (Yield sh f))) ZeroIdx a0
      where
        inner :: Extend acc aenv aenv' -> Idx aenv' as -> acc aenv bs -> Delayed acc aenv cs
        inner env1 v1 (delayAcc . sink env1 -> Term env0 cc0) = case cc0 of
          Done v0       -> let env = env1 `join` env0 in Term (env `PushEnv` op env (avarIn (sink env0 v1)) (avarIn v0)) (Done ZeroIdx)
          Step sh p f v -> let env = env1 `join` env0 in Term (env `PushEnv` op env (avarIn (sink env0 v1)) (computeAcc (Term BaseEnv (Step sh p f v)))) (Done ZeroIdx)
          Yield sh f    -> let env = env1 `join` env0 in Term (env `PushEnv` op env (avarIn (sink env0 v1)) (computeAcc (Term BaseEnv (Yield sh f)))) (Done ZeroIdx)


-- Internal representation
-- =======================

-- Note: [Representing delayed array]
--
-- During the fusion transformation we represent terms as a pair consisting of
-- a collection of supplementary environment bindings and a description of how
-- to construct the array.
--
-- It is critical to separate these two. To create a real AST node we need both
-- the environment and array term, but analysis of how to fuse terms requires
-- only the array description. If the additional bindings are bundled as part of
-- the representation, the existentially quantified extended environment type
-- will be untouchable. This is problematic because the terms of the two arrays
-- are defined with respect to this existentially quantified type, and there is
-- no way to directly combine these two environments:
--
--   join :: Extend env env1 -> Extend env env2 -> Extend env ???
--
-- And hence, no way to combine the terms of the delayed representation.
--
-- The only way to bring terms into the same scope is to operate via the
-- manifest terms. This entails a great deal of conversion between delayed and
-- AST terms, but is certainly possible.
--
-- However, because of the limited scope into which this existential type is
-- available, we ultimately perform this process many times. In fact, complexity
-- of the fusion algorithm for an AST of N terms becomes O(r^n), where r is the
-- number of different rules we have for combining terms.
--
data Delayed acc aenv a where
  Term  :: Extend     acc aenv aenv'
        -> Cunctation acc      aenv' a
        -> Delayed    acc aenv       a


-- Cunctation (n): the action or an instance of delaying; a tardy action.
--
-- This describes the ways in which the fusion transformation represents
-- intermediate arrays. The fusion process operates by recasting producer array
-- computations in terms of a set of scalar functions used to construct an
-- element at each index, and fusing successive producers by combining these
-- scalar functions.
--
data Cunctation acc aenv a where

  -- The base case is just a real (manifest) array term. No fusion happens here.
  -- Note that the array is referenced by an index into the extended
  -- environment, making the term non-recursive.
  --
  Done  :: Arrays a
        => Idx            aenv a
        -> Cunctation acc aenv a

  -- We can represent an array by its shape and a function to compute an element
  -- at each index.
  --
  Yield :: (Shape sh, Elt e)
        => PreExp     acc aenv sh
        -> PreFun     acc aenv (sh -> e)
        -> Cunctation acc aenv (Array sh e)

  -- A more restrictive form than 'Yield' may afford greater opportunities for
  -- optimisation by a backend. This more structured form applies an index and
  -- value transform to an input array. Note that the transform is applied to an
  -- array stored as an environment index, so that the term is non-recursive and
  -- it is always possible to embed into a collective operation.
  --
  Step  :: (Shape sh, Shape sh', Elt a, Elt b)
        => PreExp     acc aenv sh'
        -> PreFun     acc aenv (sh' -> sh)
        -> PreFun     acc aenv (a   -> b)
        -> Idx            aenv (Array sh  a)
        -> Cunctation acc aenv (Array sh' b)


-- Convert a real AST node into the internal representation
--
done :: Arrays a => PreOpenAcc acc aenv a -> Delayed acc aenv a
done pacc
  | Avar v <- pacc      = Term BaseEnv                  (Done v)
  | otherwise           = Term (BaseEnv `PushEnv` pacc) (Done ZeroIdx)


-- Recast a cunctation into a mapping from indices to elements.
--
yield :: Kit acc
      => Cunctation acc aenv (Array sh e)
      -> Cunctation acc aenv (Array sh e)
yield cc =
  case cc of
    Yield{}                             -> cc
    Step sh p f v                       -> Yield sh (f `compose` indexArray v `compose` p)
    Done v
      | ArraysRarray <- accType' cc     -> Yield (arrayShape v) (indexArray v)
      | otherwise                       -> error "yield: impossible case"


-- Recast a cunctation into transformation step form. Not possible if the source
-- was in the Yield formulation.
--
step :: Kit acc
     => Cunctation acc aenv (Array sh e)
     -> Maybe (Cunctation acc aenv (Array sh e))
step cc =
  case cc of
    Yield{}                             -> Nothing
    Step{}                              -> Just cc
    Done v
      | ArraysRarray <- accType' cc     -> Just $ Step (arrayShape v) identity identity v
      | otherwise                       -> error "step: impossible case"


-- Get the shape of a delayed array
--
shape :: Kit acc => Cunctation acc aenv (Array sh e) -> PreExp acc aenv sh
shape cc
  | Just (Step sh _ _ _) <- step cc     = sh
  | Yield sh _           <- yield cc    = sh


-- Reified type of a delayed array representation. This way we don't require
-- additional class constraints on 'step' and 'yield'.
--
accType' :: forall acc aenv a. Arrays a => Cunctation acc aenv a -> ArraysR (ArrRepr' a)
accType' _ = arrays' (undefined :: a)


-- Environment manipulation
-- ========================

-- NOTE: [Extend]
--
-- As part of the fusion transformation we often need to lift out array valued
-- inputs to be let-bound at a higher point. We can't add these directly to the
-- output array term because these would interfere with further fusion steps.
--
-- The Extend type is a heterogeneous snoc-list of array terms that witnesses
-- how the array environment is extend by binding these additional terms.
--
data Extend acc aenv aenv' where
  BaseEnv :: Extend acc aenv aenv

  PushEnv :: Arrays a
          => Extend acc aenv aenv' -> PreOpenAcc acc aenv' a -> Extend acc aenv (aenv', a)


-- Append two environment witnesses
--
join :: Extend acc env env' -> Extend acc env' env'' -> Extend acc env env''
join x BaseEnv        = x
join x (PushEnv as a) = x `join` as `PushEnv` a

-- Bring into scope all of the array terms in the Extend environment list. This
-- converts a term in the inner environment (aenv') into the outer (aenv).
--
bind :: (Kit acc, Arrays a)
     => Extend acc aenv aenv'
     -> PreOpenAcc acc aenv' a
     -> PreOpenAcc acc aenv  a
bind BaseEnv         = id
bind (PushEnv env a) = bind env . Alet (termOut a) . termOut


prjExtend :: Kit acc => Idx env' t -> Extend acc env env' -> PreOpenAcc acc env' t
prjExtend ZeroIdx       (PushEnv _   v) = weakenA rebuildAcc SuccIdx v
prjExtend (SuccIdx idx) (PushEnv env _) = weakenA rebuildAcc SuccIdx $ prjExtend idx env
prjExtend _             _               = INTERNAL_ERROR(error) "prjExtend" "inconsistent valuation"


-- Sink a term from one array environment into another, where additional
-- bindings have come into scope according to the witness and no old things have
-- vanished.
--
class Sink f where
  sink :: Extend acc env env' -> f env t -> f env' t

instance Sink Idx where
  sink BaseEnv       = Stats.substitution "sink" id
  sink (PushEnv e _) = SuccIdx . sink e

instance Kit acc => Sink (PreOpenExp acc env) where
  sink env = weakenEA rebuildAcc (sink env)

instance Kit acc => Sink (PreOpenFun acc env) where
  sink env = weakenFA rebuildAcc (sink env)

instance Kit acc => Sink (PreOpenAcc acc) where
  sink env = weakenA rebuildAcc (sink env)

instance Kit acc => Sink acc where      -- overlapping, undecidable, incoherent
  sink env = rebuildAcc (Avar . sink env)

instance Kit acc => Sink (Cunctation acc) where
  sink env cc = case cc of
    Done v              -> Done (sink env v)
    Step sh p f v       -> Step (sink env sh) (sink env p) (sink env f) (sink env v)
    Yield sh f          -> Yield (sink env sh) (sink env f)


class Sink1 f where
  sink1 :: Extend acc env env' -> f (env,s) t -> f (env',s) t

instance Sink1 Idx where
  sink1 BaseEnv         = Stats.substitution "sink1" id
  sink1 (PushEnv e _)   = split . sink1 e
    where
      split :: Idx (env,s) t -> Idx ((env,u),s) t
      split ZeroIdx      = ZeroIdx
      split (SuccIdx ix) = SuccIdx (SuccIdx ix)

instance Kit acc => Sink1 (PreOpenExp acc env) where
  sink1 env = weakenEA rebuildAcc (sink1 env)

instance Kit acc => Sink1 (PreOpenFun acc env) where
  sink1 env = weakenFA rebuildAcc (sink1 env)

instance Kit acc => Sink1 (PreOpenAcc acc) where
  sink1 env = weakenA rebuildAcc (sink1 env)

instance Kit acc => Sink1 acc where     -- overlapping, undecidable, incoherent
  sink1 env = rebuildAcc (Avar . sink1 env)



-- Array fusion of a de Bruijn computation AST
-- ===========================================

-- Array computations
-- ------------------

-- Recast the internal representation of delayed arrays into a real AST node.
-- Use the most specific version of a combinator whenever possible.
--
compute :: (Kit acc, Arrays arrs) => Delayed acc aenv arrs -> PreOpenAcc acc aenv arrs
compute (Term env cc)
  = bind env
  $ case cc of
      Done v                                    -> Avar v
      Yield (simplify -> sh) (simplify -> f)    -> Generate sh f
      Step  (simplify -> sh) (simplify -> p) (simplify -> f) v
        | Just REFL <- identShape
        , Just REFL <- isIdentity p
        , Just REFL <- isIdentity f             -> Avar v
        | Just REFL <- identShape
        , Just REFL <- isIdentity p             -> Map f acc
        | Just REFL <- isIdentity f             -> Backpermute sh p acc
        | otherwise                             -> Transform sh p f acc
        where
          identShape    = match sh (arrayShape v)
          acc           = avarIn v


-- Evaluate a delayed computation and tie the recursive knot
--
computeAcc :: (Kit acc, Arrays arrs) => Delayed acc aenv arrs -> acc aenv arrs
computeAcc = termOut . compute


-- Representation of a generator as a delayed array
--
generateD :: (Shape sh, Elt e)
          => PreExp  acc aenv sh
          -> PreFun  acc aenv (sh -> e)
          -> Delayed acc aenv (Array sh e)
generateD sh f
  = Stats.ruleFired "generateD"
  $ Term BaseEnv (Yield sh f)


-- Fuse a unary function into a delayed array.
--
mapD :: (Kit acc, Elt b)
     => PreFun     acc aenv (a -> b)
     -> Cunctation acc aenv (Array sh a)
     -> Cunctation acc aenv (Array sh b)
mapD f = Stats.ruleFired "mapD" . go
  where
    go (step  -> Just (Step sh ix g v)) = Step sh ix (f `compose` g) v
    go (yield -> Yield sh g)            = Yield sh (f `compose` g)


-- Fuse an index space transformation function that specifies where elements in
-- the destination array read there data from in the source array.
--
backpermuteD
    :: (Kit acc, Shape sh')
    => PreExp     acc aenv sh'
    -> PreFun     acc aenv (sh' -> sh)
    -> Cunctation acc aenv (Array sh  e)
    -> Cunctation acc aenv (Array sh' e)
backpermuteD sh' p = Stats.ruleFired "backpermuteD" . go
  where
    go (step  -> Just (Step _ q f v))   = Step sh' (q `compose` p) f v
    go (yield -> Yield _ g)             = Yield sh' (g `compose` p)


-- Transform as a combined map and backwards permutation
--
transformD
    :: (Kit acc, Shape sh', Elt b)
    => PreExp     acc aenv sh'
    -> PreFun     acc aenv (sh' -> sh)
    -> PreFun     acc aenv (a   -> b)
    -> Cunctation acc aenv (Array sh  a)
    -> Cunctation acc aenv (Array sh' b)
transformD sh' p f
  = Stats.ruleFired "transformD"
  . backpermuteD sh' p
  . mapD f


-- Replicate as a backwards permutation
--
-- TODO: If we have a pattern such as `replicate sh (map f xs)` then in some
--       cases it might be beneficial to not fuse these terms, if `f` is
--       expensive and/or `sh` is large.
--
replicateD
    :: (Kit acc, Shape sh, Shape sl, Elt slix, Elt e)
    => SliceIndex (EltRepr slix) (EltRepr sl) co (EltRepr sh)
    -> PreExp     acc aenv slix
    -> Cunctation acc aenv (Array sl e)
    -> Cunctation acc aenv (Array sh e)
replicateD sliceIndex slix cc
  = Stats.ruleFired "replicateD"
  $ backpermuteD (IndexFull sliceIndex slix (shape cc)) (extend sliceIndex slix) cc


-- Dimensional slice as a backwards permutation
--
sliceD
    :: (Kit acc, Shape sh, Shape sl, Elt slix, Elt e)
    => SliceIndex (EltRepr slix) (EltRepr sl) co (EltRepr sh)
    -> PreExp     acc aenv slix
    -> Cunctation acc aenv (Array sh e)
    -> Cunctation acc aenv (Array sl e)
sliceD sliceIndex slix cc
  = Stats.ruleFired "sliceD"
  $ backpermuteD (IndexSlice sliceIndex slix (shape cc)) (restrict sliceIndex slix) cc


-- Reshape an array
--
-- TLM: there was a runtime check to ensure the old and new shapes contained the
--      same number of elements: this has been lost!
--
reshapeD
    :: (Kit acc, Shape sh, Shape sl)
    => PreExp     acc aenv sl
    -> Cunctation acc aenv (Array sh e)
    -> Cunctation acc aenv (Array sl e)
reshapeD sl cc
  = Stats.ruleFired "reshapeD"
  $ backpermuteD sl (reindex (shape cc) sl) cc


-- Combine two arrays element-wise with a binary function to produce a delayed
-- array.
--
zipWithD :: (Kit acc, Shape sh, Elt a, Elt b, Elt c)
         => PreFun     acc aenv (a -> b -> c)
         -> Cunctation acc aenv (Array sh a)
         -> Cunctation acc aenv (Array sh b)
         -> Cunctation acc aenv (Array sh c)
zipWithD f cc1 cc0
  -- Two stepper functions identically accessing the same array can be kept in
  -- stepping form. This might yield a simpler final term.
  --
  | Just (Step sh1 p1 f1 v1)    <- step cc1
  , Just (Step sh0 p0 f0 v0)    <- step cc0
  , Just REFL                   <- match v1 v0
  , Just REFL                   <- match p1 p0
  = Stats.ruleFired "zipWithD/step"
  $ Step (sh1 `Intersect` sh0) p0 (combine f f1 f0) v0

  -- Otherwise transform both delayed terms into (index -> value) mappings and
  -- combine the two indexing functions that way.
  --
  | Yield sh1 f1                <- yield cc1
  , Yield sh0 f0                <- yield cc0
  = Stats.ruleFired "zipWithD"
  $ Yield (sh1 `Intersect` sh0) (combine f f1 f0)

  where
    combine :: forall acc aenv a b c e. (Elt a, Elt b, Elt c)
            => PreFun acc aenv (a -> b -> c)
            -> PreFun acc aenv (e -> a)
            -> PreFun acc aenv (e -> b)
            -> PreFun acc aenv (e -> c)
    combine c ixa ixb
      | Lam (Lam (Body c'))     <- weakenFE SuccIdx c   :: PreOpenFun acc ((),e) aenv (a -> b -> c)
      , Lam (Body ixa')         <- ixa                          -- ^ else the soklem 'e' will escape
      , Lam (Body ixb')         <- ixb
      = Lam $ Body $ Let ixa' $ Let (weakenE SuccIdx ixb') c'


-- NOTE: [Sharing vs. Fusion]
--
-- The approach to array fusion is similar to that the first generation of Repa.
-- It was discovered that the most immediately pressing problem with delayed
-- arrays in Repa-1 was that it did not preserve sharing of collective
-- operations, leading to excessive recomputation and severe repercussions on
-- performance if the user did not explicitly intervene.
--
-- However, as we have explicit sharing information in the term tree, so it is
-- straightforward to respect sharing by not fusing let-bindings, as that
-- introduces work duplication. However, sometimes we can be cleverer.
--
-- let-floating:
-- -------------
--
-- If the binding is of manifest data, we can instead move the let-binding to a
-- different point in the program and then continue to fuse into the body. This
-- is done by adding the bound term to the Extend environment. In essence this
-- is covering a different occurrence of the same problem Extend was introduced
-- to handle: let bindings of manifest data unnecessarily get in the way of the
-- fusion process. For example:
--
--   map f (zipWith g xs (map h xs))
--
-- after sharing recovery results in:
--
--   map f (let a0 = xs in zipWith g a0 (map h a0))
--
-- Without allowing the binding for a0 to float outwards, `map f` will not be
-- fused into the rest of the program.
--
-- let-elimination:
-- ----------------
--
-- Array binding points appear in the program because the array data _or_ shape
-- was accessed multiple times in the source program. In general we want to fuse
-- arbitrary sequences of array _data_, irrespective of how the shape component
-- is used. For example, reverse is defined in the prelude as:
--
--   reverse xs = let len   = unindex1 (shape xs)
--                    pf i  = len - i - 1
--                in
--                backpermute (shape xs) (ilift1 pf) xs
--
-- Sharing recovery introduces a let-binding for the input `xs` since it is used
-- thrice in the definition, which impedes subsequent fusion. However the actual
-- array data is only accessed once, with the remaining two uses querying the
-- array shape. Since the delayed terms contain the shape of the array they
-- represent as a scalar term, if the data component otherwise satisfies the
-- rules for fusing terms, as it does in this example, we can eliminate the
-- let-binding by pushing the scalar shape and value generation terms directly
-- into the body.
--
-- Let-elimination can also be used to _introduce_ work duplication, which may
-- be beneficial if we can estimate that the cost of recomputation is less than
-- the cost of completely evaluating the array and subsequently retrieving the
-- data from memory.
--
-- TODO: instead of relying on later shrinking, remove the eliminated binding
--       from the environment straight away.
--
aletD :: forall acc aenv arrs brrs. (Kit acc, Arrays arrs, Arrays brrs)
      => DelayAcc acc
      -> ElimAcc  acc
      ->          acc aenv        arrs
      ->          acc (aenv,arrs) brrs
      -> Delayed  acc aenv        brrs
aletD delayAcc elimAcc (delayAcc -> Term env1 cc1) body@(delayAcc . sink1 env1 -> Term env0 cc0)

  -- let-floating
  -- ------------
  --
  -- Immediately inline the variable referring to the bound expression into the
  -- body, instead of adding to the environments and creating an indirection
  -- that must be later eliminated by the simplifier. If we don't, repeated
  -- evaluations of the forging process will delay termination.
  --
  | Done v1             <- cc1
  , Term env0 cc0       <- delayAcc $ rebuildAcc (subTop (Avar v1) . sink1 env1) body
  = Stats.ruleFired "aletD/float"
  $ Term (env1 `join` env0) cc0

  -- let-elimination: step/step
  -- --------------------------
  --
  -- A special case for step/step that combines successive index-space
  -- transformations that do not have an intermediate value transform. This is
  -- able to keep the node in Step form, which we can do a bit more
  -- simplification to.
  --
  | shouldInline
  , Step sh1 p1 f1 v1   <- cc1
  , Step sh0 p0 f0 v0   <- cc0
  , Just REFL           <- match v0 (sink env0 ZeroIdx)
  , Just REFL           <- isIdentity f1
  , sh1'                <- sink env0 (weakenEA rebuildAcc SuccIdx sh1)
  , f1'                 <- sink env0 (weakenFA rebuildAcc SuccIdx (f1 `compose` indexArray v1 `compose` p1))
  = Stats.ruleFired "aletD/step-step"
  $ Term env'
  $ Step (replaceE sh1' f1' v0 sh0)
         (replaceF sh1' f1' v0 (sink env0 (weakenFA rebuildAcc SuccIdx p1) `compose` p0))
         (replaceF sh1' f1' v0 f0)
         (sink env0 (SuccIdx v1))

  -- let-elimination: delayed/permute
  -- --------------------------------
  --
  -- If the let binding is a producer and the body is forward permutation, we
  -- might be able to fuse into the shape and index transformation of a forward
  -- permutation.
  --
  | shouldInline
  , Just (Yield sh1 f1) <- yield' cc1
  , Done v0             <- cc0
  , Permute c0 d0 p0 a0 <- prjExtend v0 env0
  , sh1'                <- sink env0 (weakenEA rebuildAcc SuccIdx sh1)
  , f1'                 <- sink env0 (weakenFA rebuildAcc SuccIdx f1)
  , v1'                 <- sink env0 ZeroIdx
  = Stats.ruleFired "aletD/permute"
  $ Term (env' `PushEnv` Permute (replaceF sh1' f1' v1' c0) d0 (replaceF sh1' f1' v1' p0) a0)
         (Done ZeroIdx)

  -- let-elimination: general cases
  -- ------------------------------
  --
  -- We need to enumerate all possibilities because we don't have a plain Array
  -- constraint to use the helpers `step` and `yield`. If successful, the
  -- binding is merged into the body.
  --
  | Step sh1 p1 f1 v1   <- cc1
  = case cc0 of
      Done v0           -> Term env (Done v0)
      Step sh0 p0 f0 v0 -> intoStep  env1 sh1 (f1 `compose` indexArray v1 `compose` p1) bnd' env0 sh0 p0 f0 v0
      Yield sh0 f0      -> intoYield env1 sh1 (f1 `compose` indexArray v1 `compose` p1) bnd' env0 sh0 f0

  | Yield sh1 f1        <- cc1
  = case cc0 of
      Done v0           -> Term env (Done v0)
      Step sh0 p0 f0 v0 -> intoStep  env1 sh1 f1 bnd' env0 sh0 p0 f0 v0
      Yield sh0 f0      -> intoYield env1 sh1 f1 bnd' env0 sh0 f0

  where
    subTop :: forall aenv s t. Arrays t => PreOpenAcc acc aenv s -> Idx (aenv,s) t -> PreOpenAcc acc aenv t
    subTop t ZeroIdx       = t
    subTop _ (SuccIdx idx) = Avar idx

    yield' :: Kit acc => Cunctation acc aenv' a -> Maybe (Cunctation acc aenv' a)
    yield' cc = case cc of
      Done{}    -> Nothing              -- TLM: can't determine if (a ~ Array sh e)
      Step{}    -> Just (yield cc)
      Yield{}   -> Just cc

    -- The body term, optimised and then re-made manifest. This is fine because
    -- we only call delay once, either here or in the let-floating branch.
    --
    body'               = computeAcc (Term env0 cc0)
    bnd'                = compute (Term BaseEnv cc1)
    shouldInline        = elimAcc ZeroIdx body'

    env'                = env1 `PushEnv` eliminated `join` env0
    env                 = env1 `PushEnv` bnd'       `join` env0

    -- If for some reason we have inlined a binding that isn't later removed as
    -- dead code, report this as an error.
    --
    -- We need to bundle the error message inside a Use node to stop the error
    -- being reported prematurely. This works because the transformations don't
    -- inspect the contents of Use nodes.
    --
    eliminated :: forall aenv a. Arrays a => PreOpenAcc acc aenv a
    eliminated = Use $ INTERNAL_ERROR(error) "aletD" "let binding not eliminated"

    -- Combine a bound term into a body that was represented as a Step function
    --
    intoStep :: (Kit acc, Shape sh1, Shape sh0, Shape sh0', Elt e1, Elt e0, Elt e0')
             => Extend acc aenv aenv'
             -> PreExp acc aenv' sh1
             -> PreFun acc aenv' (sh1 -> e1)
             -> PreOpenAcc acc aenv' (Array sh1 e1)
             -> Extend acc (aenv', Array sh1 e1) aenv''
             -> PreExp acc aenv'' sh0'
             -> PreFun acc aenv'' (sh0' -> sh0)
             -> PreFun acc aenv'' (e0   -> e0')
             -> Idx aenv'' (Array sh0 e0)
             -> Delayed acc aenv (Array sh0' e0')
    intoStep env1 sh1 f1 bnd1 env0 sh0 p0 f0 v0
      | shouldInline
      , sh1'            <- sink env0 (weakenEA rebuildAcc SuccIdx sh1)
      , f1'             <- sink env0 (weakenFA rebuildAcc SuccIdx f1)
      , v1'             <- sink env0 ZeroIdx
      , f0'             <- f0 `compose` indexArray v0 `compose` p0
      = Stats.ruleFired "aletD/eliminate"
      $ Term (env1 `PushEnv` eliminated `join` env0)
      $ Yield (replaceE sh1' f1' v1' sh0) (replaceF sh1' f1' v1' f0')

      | otherwise
      = Term (env1 `PushEnv` bnd1 `join` env0)
      $ Step sh0 p0 f0 v0

    -- Combine a bound term into a body that was represented as a Yield function
    --
    intoYield :: (Kit acc, Shape sh1, Shape sh0, Elt e1, Elt e0)
              => Extend acc aenv aenv'
              -> PreExp acc aenv' sh1
              -> PreFun acc aenv' (sh1 -> e1)
              -> PreOpenAcc acc aenv' (Array sh1 e1)
              -> Extend acc (aenv', Array sh1 e1) aenv''
              -> PreExp acc aenv'' sh0
              -> PreFun acc aenv'' (sh0 -> e0)
              -> Delayed acc aenv (Array sh0 e0)
    intoYield env1 sh1 f1 bnd1 env0 sh0 f0
      | shouldInline
      , sh1'          <- sink env0 (weakenEA rebuildAcc SuccIdx sh1)
      , f1'           <- sink env0 (weakenFA rebuildAcc SuccIdx f1)
      , v1'           <- sink env0 ZeroIdx
      = Stats.ruleFired "aletD/eliminate"
      $ Term (env1 `PushEnv` eliminated `join` env0)
      $ Yield (replaceE sh1' f1' v1' sh0) (replaceF sh1' f1' v1' f0)

      | otherwise
      = Term (env1 `PushEnv` bnd1 `join` env0)
      $ Yield sh0 f0

    -- As part of let-elimination, we need to replace uses of array variables in
    -- scalar expressions with an equivalent expression that generates the
    -- result directly
    --
    replaceE :: forall acc env aenv sh e t. (Kit acc, Shape sh, Elt e)
             => PreOpenExp acc env aenv sh -> PreOpenFun acc env aenv (sh -> e) -> Idx aenv (Array sh e)
             -> PreOpenExp acc env aenv t
             -> PreOpenExp acc env aenv t
    replaceE sh' f' avar exp =
      case exp of
        Let x y                         -> Let (travE x) (replaceE (weakenE SuccIdx sh') (weakenFE SuccIdx f') avar y)
        Var i                           -> Var i
        Foreign ff f e                  -> Foreign ff f (travE e)
        Const c                         -> Const c
        Tuple t                         -> Tuple (travT t)
        Prj ix e                        -> Prj ix (travE e)
        IndexNil                        -> IndexNil
        IndexCons sl sz                 -> IndexCons (travE sl) (travE sz)
        IndexHead sh                    -> IndexHead (travE sh)
        IndexTail sz                    -> IndexTail (travE sz)
        IndexAny                        -> IndexAny
        IndexSlice x ix sh              -> IndexSlice x (travE ix) (travE sh)
        IndexFull x ix sl               -> IndexFull x (travE ix) (travE sl)
        ToIndex sh ix                   -> ToIndex (travE sh) (travE ix)
        FromIndex sh i                  -> FromIndex (travE sh) (travE i)
        Cond p t e                      -> Cond (travE p) (travE t) (travE e)
        Iterate n f x                   -> Iterate (travE n) (replaceE (weakenE SuccIdx sh') (weakenFE SuccIdx f') avar f) (travE x)
        PrimConst c                     -> PrimConst c
        PrimApp g x                     -> PrimApp g (travE x)
        ShapeSize sh                    -> ShapeSize (travE sh)
        Intersect sh sl                 -> Intersect (travE sh) (travE sl)
        Shape a
          | Just REFL <- match a a'     -> Stats.substitution "replaceE/shape" sh'
          | otherwise                   -> exp

        Index a sh
          | Just REFL    <- match a a'
          , Lam (Body b) <- f'          -> Stats.substitution "replaceE/!" $ Let sh b
          | otherwise                   -> Index a (travE sh)

        LinearIndex a i
          | Just REFL    <- match a a'
          , Lam (Body b) <- f'          -> Stats.substitution "replaceE/!!" $ Let (Let i (FromIndex (weakenE SuccIdx sh') (Var ZeroIdx))) b
          | otherwise                   -> LinearIndex a (travE i)

      where
        a' = avarIn avar

        travE :: PreOpenExp acc env aenv s -> PreOpenExp acc env aenv s
        travE = replaceE sh' f' avar

        travT :: Tuple (PreOpenExp acc env aenv) s -> Tuple (PreOpenExp acc env aenv) s
        travT NilTup        = NilTup
        travT (SnocTup t e) = travT t `SnocTup` travE e

    replaceF :: forall acc env aenv sh e t. (Kit acc, Shape sh, Elt e)
             => PreOpenExp acc env aenv sh -> PreOpenFun acc env aenv (sh -> e) -> Idx aenv (Array sh e)
             -> PreOpenFun acc env aenv t
             -> PreOpenFun acc env aenv t
    replaceF sh' f' avar fun =
      case fun of
        Body e          -> Body (replaceE sh' f' avar e)
        Lam f           -> Lam  (replaceF (weakenE SuccIdx sh') (weakenFE SuccIdx f') avar f)


-- Array conditionals, in particular eliminate branches when the predicate
-- reduces to a known constant.
--
-- Note that we take the raw unprocessed terms as input. If instead we had the
-- terms for each branch in the delayed representation, this would require that
-- each term has been sunk into a common environment, which implies the
-- conditional has been pushed underneath the intersection of bound terms for
-- both branches. This would result in redundant work processing the bindings
-- for the branch not taken.
--
acondD :: (Kit acc, Arrays arrs)
       => DelayAcc acc
       -> PreExp   acc aenv Bool
       ->          acc aenv arrs
       ->          acc aenv arrs
       -> Delayed  acc aenv arrs
acondD delayAcc p t e
  | Const ((),True)  <- p   = Stats.knownBranch "True"      $ delayAcc t
  | Const ((),False) <- p   = Stats.knownBranch "False"     $ delayAcc e
  | Just REFL <- match t e  = Stats.knownBranch "redundant" $ delayAcc e
  | otherwise               = done $ Acond p (computeAcc (delayAcc t))
                                             (computeAcc (delayAcc e))


-- Scalar expressions
-- ------------------

isIdentity :: PreFun acc aenv (a -> b) -> Maybe (a :=: b)
isIdentity f
  | Lam (Body (Var ZeroIdx)) <- f       = Just REFL
  | otherwise                           = Nothing

identity :: Elt a => PreOpenFun acc env aenv (a -> a)
identity = Lam (Body (Var ZeroIdx))

toIndex :: Shape sh => PreOpenExp acc env aenv sh -> PreOpenFun acc env aenv (sh -> Int)
toIndex sh = Lam (Body (ToIndex (weakenE SuccIdx sh) (Var ZeroIdx)))

fromIndex :: Shape sh => PreOpenExp acc env aenv sh -> PreOpenFun acc env aenv (Int -> sh)
fromIndex sh = Lam (Body (FromIndex (weakenE SuccIdx sh) (Var ZeroIdx)))

reindex :: (Kit acc, Shape sh, Shape sh')
        => PreOpenExp acc env aenv sh'
        -> PreOpenExp acc env aenv sh
        -> PreOpenFun acc env aenv (sh -> sh')
reindex sh' sh
  | Just REFL <- match sh sh'   = identity
  | otherwise                   = fromIndex sh' `compose` toIndex sh

extend :: (Shape sh, Shape sl, Elt slix)
       => SliceIndex (EltRepr slix) (EltRepr sl) co (EltRepr sh)
       -> PreExp acc aenv slix
       -> PreFun acc aenv (sh -> sl)
extend sliceIndex slix = Lam (Body (IndexSlice sliceIndex (weakenE SuccIdx slix) (Var ZeroIdx)))

restrict :: (Shape sh, Shape sl, Elt slix)
         => SliceIndex (EltRepr slix) (EltRepr sl) co (EltRepr sh)
         -> PreExp acc aenv slix
         -> PreFun acc aenv (sl -> sh)
restrict sliceIndex slix = Lam (Body (IndexFull sliceIndex (weakenE SuccIdx slix) (Var ZeroIdx)))

arrayShape :: (Kit acc, Shape sh, Elt e) => Idx aenv (Array sh e) -> PreExp acc aenv sh
arrayShape = Shape . avarIn

indexArray :: (Kit acc, Shape sh, Elt e) => Idx aenv (Array sh e) -> PreFun acc aenv (sh -> e)
indexArray v = Lam (Body (Index (avarIn v) (Var ZeroIdx)))

linearIndex :: (Kit acc, Shape sh, Elt e) => Idx aenv (Array sh e) -> PreFun acc aenv (Int -> e)
linearIndex v = Lam (Body (LinearIndex (avarIn v) (Var ZeroIdx)))


-- Pretty Printing
-- ===============

wide :: Style
wide = style { lineLength = 150 }

prettyDelayedAcc :: PrettyAcc DelayedOpenAcc
prettyDelayedAcc alvl wrap acc = case acc of
  Manifest pacc         -> prettyPreAcc prettyDelayedAcc alvl wrap pacc
  Delayed sh f _        ->
    wrap $ hang (text "Delayed") 2
         $ sep [ prettyPreExp prettyDelayedAcc 0 alvl parens sh
               , parens (prettyPreFun prettyDelayedAcc alvl f)
               ]

instance Show (DelayedOpenAcc aenv a) where
  show c = renderStyle wide $ prettyDelayedAcc 0 noParens c

instance Show (DelayedOpenAfun aenv f) where
  show f = renderStyle wide $ prettyPreAfun prettyDelayedAcc 0 f

instance Show (DelayedOpenExp env aenv t) where
  show e = renderStyle wide $ prettyPreExp prettyDelayedAcc 0 0 noParens e

instance Show (DelayedOpenFun env aenv t) where
  show f = renderStyle wide $ prettyPreFun prettyDelayedAcc 0 f


