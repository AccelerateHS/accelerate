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
  DelayedAcc(..),
  Delayed, DelayedAfun, DelayedExp, DelayedFun, DelayedOpenExp, DelayedOpenFun,

  -- * Fusion
  anneal, annealAfun,
  quench, quenchAfun,

) where

-- standard library
import Prelude                                          hiding ( exp, until )

-- friends
import Data.Array.Accelerate.AST
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
type Delayed a          = DelayedAcc () a
type DelayedAfun        = PreOpenAfun DelayedAcc

type DelayedExp         = DelayedOpenExp ()
type DelayedFun         = DelayedOpenFun ()
type DelayedOpenExp     = PreOpenExp DelayedAcc
type DelayedOpenFun     = PreOpenFun DelayedAcc

data DelayedAcc aenv a where
  Manifest              :: PreOpenAcc DelayedAcc aenv a -> DelayedAcc aenv a

  Delayed               :: (Shape sh, Elt e) =>
    { extentD           :: PreExp DelayedAcc aenv sh
    , indexD            :: PreFun DelayedAcc aenv (sh  -> e)
    , linearIndexD      :: PreFun DelayedAcc aenv (Int -> e)
    }                   -> DelayedAcc aenv (Array sh e)

instance Kit DelayedAcc where
  termOut       = Manifest
  rebuildAcc    = error "DelayedAcc.rebuildAcc"
  matchAcc      = error "DelayedAcc.matchAcc"
  hashAcc       = error "DelayedAcc.hashAcc"
  prettyAcc     = error "DelayedAcc.prettyAcc"


-- | Apply the fusion transformation _and also_ embed the delayed representation
-- of producers into the AST. This makes fused consumer/producer terms explicit.
--
quench :: Arrays arrs => OpenAcc aenv arrs -> DelayedAcc aenv arrs
quench = cvtA . anneal
  where
    -- Convert array computations into an embeddable delayed representation.
    -- This is essentially the reverse of 'compute'. While this is defined
    -- recursively on the array arguments (for map, etc), this is guaranteed to
    -- be an Avar.
    --
    embed :: (Shape sh, Elt e) => OpenAcc aenv (Array sh e) -> DelayedAcc aenv (Array sh e)
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
    cvtA :: OpenAcc aenv a -> DelayedAcc aenv a
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

    cvtAT :: Atuple (OpenAcc aenv) a -> Atuple (DelayedAcc aenv) a
    cvtAT NilAtup        = NilAtup
    cvtAT (SnocAtup t a) = cvtAT t `SnocAtup` cvtA a

    cvtAF :: OpenAfun aenv f -> PreOpenAfun DelayedAcc aenv f
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

    cvtT :: Tuple (OpenExp env aenv) t -> Tuple (DelayedOpenExp env aenv) t
    cvtT NilTup        = NilTup
    cvtT (SnocTup t e) = cvtT t `SnocTup` cvtE e


quenchAfun :: OpenAfun aenv f -> DelayedAfun aenv f
quenchAfun (Alam  f) = Alam  (quenchAfun f)
quenchAfun (Abody b) = Abody (quench b)


-- | Apply the fusion transformation to the AST to combine and simplify terms.
-- This combines producer/producer terms and makes consumer/producer nodes
-- adjacent.
--
anneal :: Arrays arrs => OpenAcc aenv arrs -> OpenAcc aenv arrs
anneal = shrinkAcc . computeAcc . delayAcc
  where
    delayAcc :: Arrays a => OpenAcc aenv a -> Cunctation OpenAcc aenv a
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
annealAfun (Abody b) = Abody (anneal b)


-- | Recast terms into the internal fusion delayed array representation to be
-- forged into combined terms. Using the reduced internal form limits the number
-- of combinations that need to be considered.
--
type DelayAcc acc = forall aenv arrs. Arrays arrs => acc aenv arrs -> Cunctation acc aenv arrs
type ElimAcc  acc = forall aenv s t. Idx aenv s -> acc aenv t -> Bool

{-# SPECIALISE
      delayPreAcc :: Arrays a
                  => DelayAcc OpenAcc
                  -> ElimAcc OpenAcc
                  -> PreOpenAcc OpenAcc aenv a
                  -> Cunctation OpenAcc aenv a
 #-}

delayPreAcc
    :: forall acc aenv arrs. (Kit acc, Arrays arrs)
    => DelayAcc   acc
    -> ElimAcc    acc
    -> PreOpenAcc acc aenv arrs
    -> Cunctation acc aenv arrs
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
    Alet bnd body       -> aletD delayAcc elimAcc (delayAcc bnd) body
    Acond p at ae       -> acondD delayAcc (cvtE p) at ae
    Atuple tup          -> done $ Atuple (cvtAT tup)
    Aprj ix tup         -> done $ Aprj ix (cvtA tup)
    Apply f a           -> done $ Apply (cvtAF f) (cvtA a)

    -- Array injection
    Avar v              -> done $ Avar v
    Use arrs            -> done $ Use arrs
    Unit e              -> done $ Unit e

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
    Generate sh f       -> Yield BaseEnv (cvtE sh) (cvtF f)

    Map f a             -> mapD (cvtF f) (delayAcc a)
    ZipWith f a1 a0     -> zipWithD delayAcc (cvtF f) (delayAcc a1) a0
    Transform sh p f a  -> backpermuteD (cvtE sh) (cvtF p) $ mapD (cvtF f) (delayAcc a)

    Backpermute sl p a  -> backpermuteD (cvtE sl) (cvtF p) (delayAcc a)
    Slice slix a sl     -> sliceD slix (delayAcc a) (cvtE sl)
    Replicate slix sh a -> replicateD slix (cvtE sh) (delayAcc a)
    Reshape sl a        -> reshapeD (cvtE sl) (delayAcc a)

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
    Fold f z a          -> embed  (into2 Fold          (cvtF f)  (cvtE z)) (cvtA a)
    Fold1 f a           -> embed  (into  Fold1         (cvtF f)) (cvtA a)
    FoldSeg f z a s     -> embed2 (into2 FoldSeg       (cvtF f)  (cvtE z)) (cvtA a) (cvtA s)
    Fold1Seg f a s      -> embed2 (into  Fold1Seg      (cvtF f)) (cvtA a)  (cvtA s)
    Scanl f z a         -> embed  (into2 Scanl         (cvtF f)  (cvtE z)) (cvtA a)
    Scanl1 f a          -> embed  (into  Scanl1        (cvtF f)) (cvtA a)
    Scanl' f z a        -> embed  (into2 Scanl'        (cvtF f)  (cvtE z)) (cvtA a)
    Scanr f z a         -> embed  (into2 Scanr         (cvtF f)  (cvtE z)) (cvtA a)
    Scanr1 f a          -> embed  (into  Scanr1        (cvtF f)) (cvtA a)
    Scanr' f z a        -> embed  (into2 Scanr'        (cvtF f)  (cvtE z)) (cvtA a)
    Permute f d p a     -> embed2 (into2 permute       (cvtF f)  (cvtF p)) (cvtA d) (cvtA a)
    Stencil f x a       -> embed  (into (stencil x)    (cvtF f)) (cvtA a)
    Stencil2 f x a y b  -> embed2 (into (stencil2 x y) (cvtF f)) (cvtA a)  (cvtA b)

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
    cvtF' :: PreOpenFun acc env aenv t -> PreOpenFun acc env aenv t
    cvtF' (Lam f)  = Lam  (cvtF' f)
    cvtF' (Body b) = Body (cvtE' b)

    cvtE' :: PreOpenExp acc env aenv t -> PreOpenExp acc env aenv t
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

    cvtT :: Tuple (PreOpenExp acc env aenv) t -> Tuple (PreOpenExp acc env aenv) t
    cvtT NilTup          = NilTup
    cvtT (SnocTup tup e) = cvtT tup `SnocTup` cvtE' e

    -- Embedding producers into consumers
    --
    into :: Sink f => (f env' a -> b) -> f env a -> Extend acc env env' -> b
    into op a env = op (sink env a)

    into2 :: (Sink f1, Sink f2) => (f1 env' a -> f2 env' b -> c) -> f1 env a -> f2 env b -> Extend acc env env' -> c
    into2 op a b env = op (sink env a) (sink env b)

    embed :: (Arrays as, Arrays bs)
          => (forall aenv'. Extend acc aenv aenv' -> acc aenv' as -> PreOpenAcc acc aenv' bs)
          ->            acc aenv as
          -> Cunctation acc aenv bs
    embed op a0 = case delayAcc a0 of
      Done env v        -> Done (env `PushEnv` op env (avarIn v)) ZeroIdx
      Step env sh p f v -> Done (env `PushEnv` op env (computeAcc (Step BaseEnv sh p f v))) ZeroIdx
      Yield env sh f    -> Done (env `PushEnv` op env (computeAcc (Yield BaseEnv sh f))) ZeroIdx

    embed2 :: forall aenv as bs cs. (Arrays as, Arrays bs, Arrays cs)
           => (forall aenv'. Extend acc aenv aenv' -> acc aenv' as -> acc aenv' bs -> PreOpenAcc acc aenv' cs)
           ->            acc aenv as
           ->            acc aenv bs
           -> Cunctation acc aenv cs
    embed2 op a1 a0 = case delayAcc a1 of
      Done env v        -> inner env v
      Step env sh p f v -> inner (env `PushEnv` compute (Step BaseEnv sh p f v)) ZeroIdx
      Yield env sh f    -> inner (env `PushEnv` compute (Yield BaseEnv sh f)) ZeroIdx
      where
        inner :: Extend acc aenv aenv' -> Idx aenv' as -> Cunctation acc aenv cs
        inner env1 v1 = case delayAcc (sink env1 a0) of
          Done env0 v0          -> let env' = env1 `join` env0 in Done (env' `PushEnv` op env' (avarIn (sink env0 v1)) (avarIn v0)) ZeroIdx
          Step env0 sh p f v    -> let env' = env1 `join` env0 in Done (env' `PushEnv` op env' (avarIn (sink env0 v1)) (computeAcc (Step BaseEnv sh p f v))) ZeroIdx
          Yield env0 sh f       -> let env' = env1 `join` env0 in Done (env' `PushEnv` op env' (avarIn (sink env0 v1)) (computeAcc (Yield BaseEnv sh f))) ZeroIdx


-- Internal representation
-- =======================

-- Cunctation (n): the action or an instance of delaying; a tardy action.
--
-- This describes the ways in which the fusion transformation represents
-- intermediate arrays. The fusion process operates by recasting producer array
-- computations in terms of a set of scalar functions used to construct an
-- element at each index, and fusing successive producers by combining these
-- scalar functions.
--
data Cunctation acc aenv a where

  -- The base case is just real (manifest) array term. No fusion happens here.
  -- Note that we still need to keep track of additional array bindings, and the
  -- array is referenced by an index into the extended environment, making the
  -- term non-recursive.
  --
  Done  :: Arrays a
        => Extend     acc aenv aenv'
        -> Idx            aenv' a
        -> Cunctation acc aenv  a

  -- We can represent an array by its shape and a function to compute an element
  -- at each index.
  --
  -- TLM: Maybe we should also carry the linear indexing term? w.r.t. zipWith
  --      probably only useful if we can determine that the arrays are of
  --      identical shape, which we don't for manifest arrays. Alt: need a
  --      better simplifier.
  --
  Yield :: (Shape sh, Elt e)
        => Extend     acc aenv aenv'
        -> PreExp     acc aenv' sh
        -> PreFun     acc aenv' (sh -> e)
        -> Cunctation acc aenv  (Array sh e)

  -- A more restrictive form than 'Yield' may afford greater opportunities for
  -- optimisation by a backend. This more structured form applies an index and
  -- value transform to an input array. Note that the transform is applied to an
  -- array stored as an environment index, so that the term is non-recursive and
  -- it is always possible to embed into a collective operation.
  --
  Step  :: (Shape sh, Shape sh', Elt a, Elt b)
        => Extend     acc aenv aenv'
        -> PreExp     acc aenv' sh'
        -> PreFun     acc aenv' (sh' -> sh)
        -> PreFun     acc aenv' (a   -> b)
        -> Idx            aenv' (Array sh  a)
        -> Cunctation acc aenv  (Array sh' b)


-- Convert a real AST node into the internal representation
--
done :: Arrays a => PreOpenAcc acc aenv a -> Cunctation acc aenv a
done pacc
  | Avar v <- pacc      = Done BaseEnv v
  | otherwise           = Done (BaseEnv `PushEnv` pacc) ZeroIdx


-- Recast a cunctation into a mapping from indices to elements.
--
yield :: Kit acc
      => Cunctation acc aenv (Array sh e)
      -> Cunctation acc aenv (Array sh e)
yield cc =
  case cc of
    Yield{}                             -> cc
    Step env sh p f v                   -> Yield env sh (f `compose` indexArray v `compose` p)
    Done env v
      | ArraysRarray <- accType' cc     -> Yield env (arrayShape v) (indexArray v)
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
    Done env v
      | ArraysRarray <- accType' cc     -> Just $ Step env (arrayShape v) identity identity v
      | otherwise                       -> error "step: impossible case"

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


class Sink1 f where
  sink1 :: Extend acc env env' -> f (env,s) t -> f (env',s) t

instance Sink1 Idx where
  sink1 BaseEnv         = Stats.substitution "sink1" id
  sink1 (PushEnv e _)   = split . sink1 e
    where
      split :: Idx (env,s) t -> Idx ((env,u),s) t
      split ZeroIdx      = ZeroIdx
      split (SuccIdx ix) = SuccIdx (SuccIdx ix)

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
compute :: (Kit acc, Arrays a) => Cunctation acc aenv a -> PreOpenAcc acc aenv a
compute (Done env v)                                    = bind env $ Avar v
compute (Yield env (simplify -> sh) (simplify -> f))    = bind env $ Generate sh f
compute (Step  env (simplify -> sh) (simplify -> p) (simplify -> f) v)
  | Just REFL <- identShape
  , Just REFL <- isIdentity p
  , Just REFL <- isIdentity f   = bind env $ Avar v

  | Just REFL <- identShape
  , Just REFL <- isIdentity p   = bind env $ Map f acc

  | Just REFL <- isIdentity f   = bind env $ Backpermute sh p acc
  | otherwise                   = bind env $ Transform sh p f acc
  where
    identShape  = match sh (arrayShape v)
    acc         = avarIn v

-- Evaluate a delayed computation and tie the recursive knot
--
computeAcc :: (Kit acc, Arrays arrs) => Cunctation acc aenv arrs -> acc aenv arrs
computeAcc = termOut . compute


-- Fuse a unary function into a delayed array.
--
mapD :: (Kit acc, Elt b)
     => PreFun     acc aenv (a -> b)
     -> Cunctation acc aenv (Array sh a)
     -> Cunctation acc aenv (Array sh b)
mapD f = Stats.ruleFired "mapD" go
  where
    go (step  -> Just (Step env sh ix g v))     = Step env sh ix (sink env f `compose` g) v
    go (yield -> Yield env sh g)                = Yield env sh (sink env f `compose` g)


-- Fuse an index space transformation function that specifies where elements in
-- the destination array read there data from in the source array.
--
backpermuteD
    :: (Kit acc, Shape sh')
    => PreExp     acc aenv sh'
    -> PreFun     acc aenv (sh' -> sh)
    -> Cunctation acc aenv (Array sh  e)
    -> Cunctation acc aenv (Array sh' e)
backpermuteD sh' p = Stats.ruleFired "backpermuteD" go
  where
    go (step  -> Just (Step env _ q f v))       = Step env (sink env sh') (q `compose` sink env p) f v
    go (yield -> Yield env _ g)                 = Yield env (sink env sh') (g `compose` sink env p)


-- Replicate as a backwards permutation
--
-- TODO: If we have a pattern such as `replicate sh (map f xs)` then in some
--       cases it might be beneficial to not fuse these terms, if `f` is
--       expensive and/or `sh` is large.
--
replicateD
    :: forall acc slix sl co sh aenv e. (Kit acc, Shape sh, Shape sl, Elt slix, Elt e)
    => SliceIndex (EltRepr slix) (EltRepr sl) co (EltRepr sh)
    -> PreExp     acc aenv slix
    -> Cunctation acc aenv (Array sl e)
    -> Cunctation acc aenv (Array sh e)
replicateD sliceIndex slix = Stats.ruleFired "replicateD" go
  where
    go (step  -> Just (Step env sl p f v))      = Step env (fullshape env sl) (p `compose` extend env) f v
    go (yield -> Yield env sl f)                = Yield env (fullshape env sl) (f `compose` extend env)

    fullshape :: Extend acc aenv aenv' -> PreExp acc aenv' sl -> PreExp acc aenv' sh
    fullshape env = IndexFull sliceIndex (sink env slix)

    extend :: Extend acc aenv aenv' -> PreFun acc aenv' (sh -> sl)
    extend env = Lam (Body (IndexSlice sliceIndex (weakenE SuccIdx (sink env slix)) (Var ZeroIdx)))


-- Dimensional slice as a backwards permutation
--
sliceD
    :: forall acc slix sl co sh aenv e. (Kit acc, Shape sh, Shape sl, Elt slix, Elt e)
    => SliceIndex (EltRepr slix) (EltRepr sl) co (EltRepr sh)
    -> Cunctation acc aenv (Array sh e)
    -> PreExp     acc aenv slix
    -> Cunctation acc aenv (Array sl e)
sliceD sliceIndex cc slix = Stats.ruleFired "sliceD" $ go cc
  where
    go (step  -> Just (Step env sl p f v))      = Step env (sliceshape env sl) (p `compose` restrict env) f v
    go (yield -> Yield env sl f)                = Yield env (sliceshape env sl) (f `compose` restrict env)

    sliceshape :: Extend acc aenv aenv' -> PreExp acc aenv' sh -> PreExp acc aenv' sl
    sliceshape env = IndexSlice sliceIndex (sink env slix)

    restrict :: Extend acc aenv aenv' -> PreFun acc aenv' (sl -> sh)
    restrict env = Lam (Body (IndexFull sliceIndex (weakenE SuccIdx (sink env slix)) (Var ZeroIdx)))


-- Reshape an array
--
-- TLM: there was a runtime check to ensure the old and new shapes contained the
--      same number of elements: this has been lost!
--
reshapeD
    :: forall acc aenv sh sh' e. (Kit acc, Shape sh, Shape sh')
    => PreExp     acc aenv sh'
    -> Cunctation acc aenv (Array sh  e)
    -> Cunctation acc aenv (Array sh' e)
reshapeD = Stats.ruleFired "reshapeD" $ flip go
  where
    go :: Cunctation acc aenv (Array sh e) -> PreExp acc aenv sh' -> Cunctation acc aenv (Array sh' e)
    go (step  -> Just (Step env sh p f v)) (sink env -> sl)     = Step env sl (p `compose` reindex sh sl) f v
    go (yield -> Yield env sh f)           (sink env -> sl)     = Yield env sl (f `compose` reindex sh sl)


-- Combine two arrays element-wise with a binary function to produce a delayed
-- array. Note that we can not take two delayed terms as input, as we do with
-- 'mapD' and 'backpermuteD'. This is because the extended environment types are
-- untouchable, and so there is no way to combine them:
--
--   join :: Extend env env1 -> Extend env env2 -> Extend env ???
--
-- Instead, the trick to keep the skolem types under control is to recast one
-- array to delayed form, bring these additional terms into scope in the second
-- term, and then recast the other. This ensures that the extend environments
-- are built atop one another and all terms are in scope.
--
zipWithD :: (Kit acc, Shape sh, Elt a, Elt b, Elt c)
         => DelayAcc acc
         -> PreFun     acc aenv (a -> b -> c)
         -> Cunctation acc aenv (Array sh a)
         ->            acc aenv (Array sh b)
         -> Cunctation acc aenv (Array sh c)
zipWithD delayAcc f cc1 a0
  -- Two stepper functions identically accessing the same array can be kept in
  -- stepping form. This might yield a simpler final term.
  --
  | Just (Step env1 sh1 p1 f1 v1) <- step cc1
  , Just (Step env0 sh0 p0 f0 v0) <- step (delayAcc (sink env1 a0))
  , Just REFL           <- match (sink env0 v1) v0
  , Just REFL           <- match (sink env0 p1) p0
  , env                 <- env1 `join` env0
  = Stats.ruleFired "zipWithD/step"
  $ Step env (sink env0 sh1 `Intersect` sh0) p0 (combine (sink env f) (sink env0 f1) f0) v0

  -- Otherwise transform both delayed terms into (index -> value) mappings and
  -- combine the two indexing functions that way.
  --
  | Yield env1 sh1 f1   <- yield cc1
  , Yield env0 sh0 f0   <- yield (delayAcc (sink env1 a0))
  , env                 <- env1 `join` env0
  = Stats.ruleFired "zipWithD"
  $ Yield env (sink env0 sh1 `Intersect` sh0) (combine (sink env f) (sink env0 f1) f0)

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
-- TODO: if a let is eliminated, we should shrink immediately to eliminate the
--       bound term, with an error if this is term not completely removed.
--
aletD :: forall acc aenv arrs brrs. (Kit acc, Arrays arrs, Arrays brrs)
      => DelayAcc   acc
      -> ElimAcc    acc
      -> Cunctation acc aenv        arrs
      ->            acc (aenv,arrs) brrs
      -> Cunctation acc aenv        brrs
aletD delayAcc shouldInline cc1 body

  -- let-floating
  -- ------------
  --
  -- Immediately inline the variable referring to the bound expression into the
  -- body, instead of adding to the environments and creating an indirection
  -- that must be later eliminated by the simplifier. If we don't, repeated
  -- evaluations of the forging process will delay termination.
  --
  | Done env1 v1        <- cc1
  , body'               <- rebuildAcc (subTop (Avar v1) . sink1 env1) body
  = Stats.ruleFired "aletD/float"
  $ case delayAcc body' of
      Done env0 v0              -> Done  (env1 `join` env0) v0
      Step env0 sh0 p0 f0 v0    -> Step  (env1 `join` env0) sh0 p0 f0 v0
      Yield env0 sh0 f0         -> Yield (env1 `join` env0) sh0 f0

  -- let-elimination: step/step
  -- --------------------------
  --
  -- A special case for step/step that combines successive index-space
  -- transformations that do not have an intermediate value transform. This is
  -- able to keep the node in Step form, which we can do a bit more
  -- simplification to.
  --
  | Step env1 sh1 p1 f1 v1      <- cc1
  , Step env0 sh0 p0 f0 v0      <- delayAcc (sink1 env1 body)
  , Just REFL                   <- match v0 (sink env0 ZeroIdx)
  , Just REFL                   <- isIdentity f1
  , shouldInline ZeroIdx (computeAcc (Step env0 sh0 p0 f0 v0))
  , env1'                       <- env1 `PushEnv` eliminated
  , sh1'                        <- sink env0 (weakenEA rebuildAcc SuccIdx sh1)
  , f1'                         <- sink env0 (weakenFA rebuildAcc SuccIdx (f1 `compose` indexArray v1 `compose` p1))
  = Stats.ruleFired "AletD/step-step"
  $ Step (env1' `join` env0)
         (replaceE sh1' f1' v0 sh0)
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
  | Just (Yield env1 sh1 f1)    <- yield' cc1
  , Done env0 v0                <- delayAcc (sink1 env1 body)
  , Permute c0 d0 p0 a0         <- prjExtend v0 env0
  , shouldInline ZeroIdx (computeAcc (Done env0 v0))
  , env1'                       <- env1 `PushEnv` eliminated
  , sh1'                        <- sink env0 (weakenEA rebuildAcc SuccIdx sh1)
  , f1'                         <- sink env0 (weakenFA rebuildAcc SuccIdx f1)
  , v1'                         <- sink env0 ZeroIdx
  = Stats.ruleFired "AletD/permute"
  $ Done (env1' `join` env0 `PushEnv` Permute (replaceF sh1' f1' v1' c0) d0 (replaceF sh1' f1' v1' p0) a0)
         ZeroIdx

  -- let-elimination: general cases
  -- ------------------------------
  --
  -- We need to enumerate all possibilities because we don't have a plain Array
  -- constraint to use the helpers `step` and `yield`. If successful, the
  -- binding is merged into the body.
  --
  | Step env1 sh1 p1 f1 v1      <- cc1
  , bnd                         <- compute (Step BaseEnv sh1 p1 f1 v1)
  = case delayAcc (sink1 env1 body) of
      Done env0 v0              -> Done (env1 `PushEnv` bnd `join` env0) v0
      Step env0 sh0 p0 f0 v0    -> intoStep  env1 sh1 (f1 `compose` indexArray v1 `compose` p1) bnd env0 sh0 p0 f0 v0
      Yield env0 sh0 f0         -> intoYield env1 sh1 (f1 `compose` indexArray v1 `compose` p1) bnd env0 sh0 f0

  | Yield env1 sh1 f1           <- cc1
  , bnd                         <- compute (Yield BaseEnv sh1 f1)
  = case delayAcc (sink1 env1 body) of
      Done env0 v0              -> Done (env1 `PushEnv` bnd `join` env0) v0
      Step env0 sh0 p0 f0 v0    -> intoStep  env1 sh1 f1 bnd env0 sh0 p0 f0 v0
      Yield env0 sh0 f0         -> intoYield env1 sh1 f1 bnd env0 sh0 f0

  where
    subTop :: forall aenv s t. Arrays t => PreOpenAcc acc aenv s -> Idx (aenv,s) t -> PreOpenAcc acc aenv t
    subTop t ZeroIdx       = t
    subTop _ (SuccIdx idx) = Avar idx

    yield' :: Kit acc => Cunctation acc aenv a -> Maybe (Cunctation acc aenv a)
    yield' cc = case cc of
      Done{}    -> Nothing              -- TLM: can't determine if (a ~ Array sh e)
      Step{}    -> Just (yield cc)
      Yield{}   -> Just cc

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
             -> Cunctation acc aenv (Array sh0' e0')
    intoStep env1 sh1 f1 bnd1 env0 sh0 p0 f0 v0
      | shouldInline ZeroIdx (computeAcc (Step env0 sh0 p0 f0 v0))
      , env1'           <- env1 `PushEnv` eliminated
      , sh1'            <- sink env0 (weakenEA rebuildAcc SuccIdx sh1)
      , f1'             <- sink env0 (weakenFA rebuildAcc SuccIdx f1)
      , v1'             <- sink env0 ZeroIdx
      , f0'             <- f0 `compose` indexArray v0 `compose` p0
      = Stats.ruleFired "AletD/eliminate"
      $ Yield (env1' `join` env0) (replaceE sh1' f1' v1' sh0) (replaceF sh1' f1' v1' f0')

      | otherwise
      = Step (env1 `PushEnv` bnd1 `join` env0) sh0 p0 f0 v0

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
              -> Cunctation acc aenv (Array sh0 e0)
    intoYield env1 sh1 f1 bnd1 env0 sh0 f0
      | shouldInline ZeroIdx (computeAcc (Yield env0 sh0 f0))
      , env1'         <- env1 `PushEnv` eliminated
      , sh1'          <- sink env0 (weakenEA rebuildAcc SuccIdx sh1)
      , f1'           <- sink env0 (weakenFA rebuildAcc SuccIdx f1)
      , v1'           <- sink env0 ZeroIdx
      = Stats.ruleFired "AletD/eliminate"
      $ Yield (env1' `join` env0) (replaceE sh1' f1' v1' sh0) (replaceF sh1' f1' v1' f0)

      | otherwise
      = Yield (env1 `PushEnv` bnd1 `join` env0) sh0 f0

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
          | Just REFL <- match a a'     -> sh'
          | otherwise                   -> exp

        Index a sh
          | Just REFL    <- match a a'
          , Lam (Body b) <- f'          -> Let sh b
          | otherwise                   -> Index a (travE sh)

        LinearIndex a i
          | Just REFL    <- match a a'
          , Lam (Body b) <- f'          -> Let (Let i (FromIndex (weakenE SuccIdx sh') (Var ZeroIdx))) b
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
acondD :: (Kit acc, Arrays arrs)
       => DelayAcc   acc
       -> PreExp     acc aenv Bool
       ->            acc aenv arrs
       ->            acc aenv arrs
       -> Cunctation acc aenv arrs
acondD delayAcc p t@(computeAcc . delayAcc -> t') e@(computeAcc . delayAcc -> e')
  | Const ((),True)  <- p       = Stats.knownBranch "True"      $ delayAcc t
  | Const ((),False) <- p       = Stats.knownBranch "False"     $ delayAcc e
  | Just REFL <- match t' e'    = Stats.knownBranch "redundant" $ delayAcc e'
  | otherwise                   = done $ Acond p t' e'


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

arrayShape :: (Kit acc, Shape sh, Elt e) => Idx aenv (Array sh e) -> PreExp acc aenv sh
arrayShape = Shape . avarIn

indexArray :: (Kit acc, Shape sh, Elt e) => Idx aenv (Array sh e) -> PreFun acc aenv (sh -> e)
indexArray v = Lam (Body (Index (avarIn v) (Var ZeroIdx)))

linearIndex :: (Kit acc, Shape sh, Elt e) => Idx aenv (Array sh e) -> PreFun acc aenv (Int -> e)
linearIndex v = Lam (Body (LinearIndex (avarIn v) (Var ZeroIdx)))

