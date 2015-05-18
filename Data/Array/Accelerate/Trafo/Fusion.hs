{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE PatternGuards        #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing      #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo.Fusion
-- Copyright   : [2012..2014] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
--               [2014..2014] Frederik M. Madsen
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

  -- ** Types
  DelayedAcc, DelayedOpenAcc(..),
  DelayedAfun, DelayedOpenAfun,
  DelayedExp, DelayedFun, DelayedOpenExp, DelayedOpenFun,

  -- ** Conversion
  convertAcc, convertAfun, convertSeq,

) where

-- TODO remove
import Debug.Trace (trace)

-- standard library
import Prelude                                          hiding ( exp, until )
import Control.Applicative                              ( pure, (<$>), (<*>) )

-- friends
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Trafo.Base
import Data.Array.Accelerate.Trafo.Shrink
import Data.Array.Accelerate.Trafo.Simplify
import Data.Array.Accelerate.Trafo.Substitution
import Data.Array.Accelerate.Array.Lifted               ( Regular, isArraysFlat, IsConstrained(..) )
import Data.Array.Accelerate.Array.Representation       ( SliceIndex(..) )
import Data.Array.Accelerate.Array.Sugar                ( Array, Arrays(..), ArraysR(..), ArrRepr
                                                        , Elt, EltRepr, Shape, Tuple(..), Atuple(..)
                                                        , IsAtuple, TupleRepr, Scalar )
import Data.Array.Accelerate.Product

import qualified Data.Array.Accelerate.Debug            as Stats
#ifdef ACCELERATE_DEBUG
import System.IO.Unsafe -- for debugging
#endif


-- Delayed Array Fusion
-- ====================

-- | Apply the fusion transformation to a closed de Bruijn AST
--
convertAcc :: Arrays arrs => Bool -> Acc arrs -> DelayedAcc arrs
convertAcc fuseAcc = withSimplStats . convertOpenAcc fuseAcc

-- | Apply the fusion transformation to a function of array arguments
--
convertAfun :: Bool -> Afun f -> DelayedAfun f
convertAfun fuseAcc = withSimplStats . convertOpenAfun fuseAcc

-- | Apply the fusion transformation to the array computations embedded
--   in a sequence computation.
convertSeq :: Bool -> Seq a -> DelayedSeq a
convertSeq fuseAcc (embedSeq (embedOpenAcc fuseAcc) -> ExtendSeq aenv s)
  = withSimplStats (DelayedSeq (cvtE aenv) (convertOpenSeq fuseAcc (fuseSeq s)))
  where
    cvtE :: Extend OpenAcc aenv aenv' -> Extend DelayedOpenAcc aenv aenv'
    cvtE BaseEnv         = BaseEnv
    cvtE (PushEnv env a) | a' <- convertOpenAcc fuseAcc a
                         = PushEnv (cvtE env) a'

withSimplStats :: a -> a
#ifdef ACCELERATE_DEBUG
withSimplStats x = unsafePerformIO Stats.resetSimplCount `seq` x
#else
withSimplStats x = x
#endif


-- | Apply the fusion transformation to an AST. This consists of two phases:
--
--    1. A bottom-up traversal that converts nodes into the internal delayed
--       representation, merging adjacent producer/producer pairs.
--
--    2. A top-down traversal that makes the representation of fused
--       consumer/producer pairs explicit as a 'DelayedAcc' of manifest and
--       delayed nodes.
--
-- TLM: Note that there really is no ambiguity as to which state an array will
--      be in following this process: an array will be either delayed or
--      manifest, and the two helper functions are even named as such! We should
--      encode this property in the type somehow...
--
convertOpenAcc :: Arrays arrs => Bool -> OpenAcc aenv arrs -> DelayedOpenAcc aenv arrs
convertOpenAcc fuseAcc = manifest fuseAcc . computeAcc . embedOpenAcc fuseAcc

-- Convert array computations into an embeddable delayed representation.
-- Reapply the embedding function from the first pass and unpack the
-- representation. It is safe to match on BaseEnv because the first pass
-- will put producers adjacent to the term consuming it.
--
delayed :: (Shape sh, Elt e) => Bool -> OpenAcc aenv (Array sh e) -> DelayedOpenAcc aenv (Array sh e)
delayed fuseAcc (embedOpenAcc fuseAcc -> Embed BaseEnv cc) =
  case cc of
    Done v                                -> Delayed (arrayShape v) (indexArray v) (linearIndex v)
    Yield (cvtE -> sh) (cvtF -> f)        -> Delayed sh f (f `compose` fromIndex sh)
    Step  (cvtE -> sh) (cvtF -> p) (cvtF -> f) v
      | Just REFL <- match sh (arrayShape v)
      , Just REFL <- isIdentity p
      -> Delayed sh (f `compose` indexArray v) (f `compose` linearIndex v)

      | f'        <- f `compose` indexArray v `compose` p
      -> Delayed sh f' (f' `compose` fromIndex sh)
  where
    cvtE :: OpenExp env aenv t -> DelayedOpenExp env aenv t
    cvtE = convertOpenExp fuseAcc

    cvtF :: OpenFun env aenv f -> DelayedOpenFun env aenv f
    cvtF (Lam f)  = Lam (cvtF f)
    cvtF (Body b) = Body (cvtE b)

-- Convert array programs as manifest terms.
--
manifest :: Bool -> OpenAcc aenv a -> DelayedOpenAcc aenv a
manifest fuseAcc (OpenAcc pacc) =
  let fusionError = $internalError "manifest" "unexpected fusible materials"
  in
  Manifest $ case pacc of
    -- Non-fusible terms
    -- -----------------
    Avar ix                 -> Avar ix
    Use arr                 -> Use arr
    Unit e                  -> Unit (cvtE e)
    Alet bnd body           -> alet (manifest fuseAcc bnd) (manifest fuseAcc body)
    Acond p t e             -> Acond (cvtE p) (manifest fuseAcc t) (manifest fuseAcc e)
    Awhile p f a            -> Awhile (cvtAF p) (cvtAF f) (manifest fuseAcc a)
    Atuple tup              -> Atuple (cvtAT tup)
    Aprj ix tup             -> Aprj ix (manifest fuseAcc tup)
    Apply f a               -> Apply (cvtAF f) (manifest fuseAcc a)
    Aforeign ff f a         -> Aforeign ff (cvtAF f) (manifest fuseAcc a)

    -- Producers
    -- ---------
    --
    -- Some producers might still exist as a manifest array. Typically
    -- this is because they are the last stage of the computation, or the
    -- result of a let-binding to be used multiple times. The input array
    -- here should be an array variable, else something went wrong.
    --
    Map f a                 -> Map (cvtF f) (delayed fuseAcc a)
    Generate sh f           -> Generate (cvtE sh) (cvtF f)
    Transform sh p f a      -> Transform (cvtE sh) (cvtF p) (cvtF f) (delayed fuseAcc a)
    Backpermute sh p a      -> Backpermute (cvtE sh) (cvtF p) (delayed fuseAcc a)
    Reshape sl a            -> Reshape (cvtE sl) (manifest fuseAcc a)

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
    Fold f z a              -> Fold     (cvtF f) (cvtE z) (delayed fuseAcc a)
    Fold1 f a               -> Fold1    (cvtF f) (delayed fuseAcc a)
    FoldSeg f z a s         -> FoldSeg  (cvtF f) (cvtE z) (delayed fuseAcc a) (delayed fuseAcc s)
    Fold1Seg f a s          -> Fold1Seg (cvtF f) (delayed fuseAcc a) (delayed fuseAcc s)
    Scanl f z a             -> Scanl    (cvtF f) (cvtE z) (delayed fuseAcc a)
    Scanl1 f a              -> Scanl1   (cvtF f) (delayed fuseAcc a)
    Scanl' f z a            -> Scanl'   (cvtF f) (cvtE z) (delayed fuseAcc a)
    Scanr f z a             -> Scanr    (cvtF f) (cvtE z) (delayed fuseAcc a)
    Scanr1 f a              -> Scanr1   (cvtF f) (delayed fuseAcc a)
    Scanr' f z a            -> Scanr'   (cvtF f) (cvtE z) (delayed fuseAcc a)
    Permute f d p a         -> Permute  (cvtF f) (manifest fuseAcc d) (cvtF p) (delayed fuseAcc a)
    Stencil f x a           -> Stencil  (cvtF f) x (manifest fuseAcc a)
    Stencil2 f x a y b      -> Stencil2 (cvtF f) x (manifest fuseAcc a) y (manifest fuseAcc b)

    -- Seq operations
    Collect seq             -> Collect (convertOpenSeq fuseAcc (fuseSeq seq))

    where
      -- Flatten needless let-binds, which can be introduced by the conversion to
      -- the internal embeddable representation.
      --
      alet bnd body
        | Manifest (Avar ZeroIdx) <- body
        , Manifest x              <- bnd
        = x

        | Manifest (Aprj tupix (Manifest (Avar ix))) <- bnd
        , Manifest (Alet bnd' body')           <- body
        , Just REFL                            <- matchAcc (weaken SuccIdx bnd) bnd'
        = Alet (Manifest $ Aprj tupix (Manifest (Avar ix))) (inlineA body' (Avar ZeroIdx))

        | otherwise
        = Alet bnd body

      cvtAT :: Atuple (OpenAcc aenv) a -> Atuple (DelayedOpenAcc aenv) a
      cvtAT NilAtup        = NilAtup
      cvtAT (SnocAtup t a) = cvtAT t `SnocAtup` manifest fuseAcc a

      cvtAF :: OpenAfun aenv f -> PreOpenAfun DelayedOpenAcc aenv f
      cvtAF (Alam f)  = Alam  (cvtAF f)
      cvtAF (Abody b) = Abody (manifest fuseAcc b)

      -- Conversions for closed scalar functions and expressions
      --
      cvtF :: OpenFun env aenv f -> DelayedOpenFun env aenv f
      cvtF (Lam f)  = Lam (cvtF f)
      cvtF (Body b) = Body (cvtE b)

      cvtE :: OpenExp env aenv t -> DelayedOpenExp env aenv t
      cvtE = convertOpenExp fuseAcc

convertOpenExp :: Bool -> OpenExp env aenv t -> DelayedOpenExp env aenv t
convertOpenExp fuseAcc exp =
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
    IndexTrans sh           -> IndexTrans (cvtE sh)
    IndexAny                -> IndexAny
    IndexSlice x ix sh      -> IndexSlice x (cvtE ix) (cvtE sh)
    IndexFull x ix sl       -> IndexFull x (cvtE ix) (cvtE sl)
    ToIndex sh ix           -> ToIndex (cvtE sh) (cvtE ix)
    FromIndex sh ix         -> FromIndex (cvtE sh) (cvtE ix)
    Cond p t e              -> Cond (cvtE p) (cvtE t) (cvtE e)
    While p f x             -> While (cvtF p) (cvtF f) (cvtE x)
    PrimConst c             -> PrimConst c
    PrimApp f x             -> PrimApp f (cvtE x)
    Index a sh              -> Index (manifest fuseAcc a) (cvtE sh)
    LinearIndex a i         -> LinearIndex (manifest fuseAcc a) (cvtE i)
    Shape a                 -> Shape (manifest fuseAcc a)
    ShapeSize sh            -> ShapeSize (cvtE sh)
    Intersect s t           -> Intersect (cvtE s) (cvtE t)
    Union s t               -> Union (cvtE s) (cvtE t)
    Foreign ff f e          -> Foreign ff (cvtF f) (cvtE e)
  where
    cvtT :: Tuple (OpenExp env aenv) t -> Tuple (DelayedOpenExp env aenv) t
    cvtT NilTup        = NilTup
    cvtT (SnocTup t e) = cvtT t `SnocTup` cvtE e

    -- Conversions for closed scalar functions and expressions
    --
    cvtF :: OpenFun env aenv f -> DelayedOpenFun env aenv f
    cvtF (Lam f)  = Lam (cvtF f)
    cvtF (Body b) = Body (cvtE b)

    cvtE :: OpenExp env aenv t -> DelayedOpenExp env aenv t
    cvtE = convertOpenExp fuseAcc


convertOpenAfun :: Bool -> OpenAfun aenv f -> DelayedOpenAfun aenv f
convertOpenAfun c (Alam  f) = Alam  (convertOpenAfun c f)
convertOpenAfun c (Abody b) = Abody (convertOpenAcc  c b)

convertOpenSeq :: Bool -> PreOpenSeq OpenAcc aenv senv a -> PreOpenSeq DelayedOpenAcc aenv senv a
convertOpenSeq fuseAcc s =
  case s of
    Consumer c          -> Consumer (cvtC c)
    Reify f ix          -> Reify (cvtAF `fmap` f) ix
    Producer p s'       -> Producer p' (convertOpenSeq fuseAcc s')
      where
        p' = case p of
               StreamIn arrs       -> StreamIn arrs
               ToSeq f slix sh a   -> ToSeq (cvtAF `fmap` f) slix sh $
                                      case a of -- Enable "use lazy" optimization.
                                        OpenAcc (Use _) -> manifest fuseAcc a
                                        _               -> delayed fuseAcc a
               MapSeq f f' x       -> MapSeq (cvtAF f) (cvtAF `fmap` f') x
               ZipWithSeq f f' x y -> ZipWithSeq (cvtAF f) (cvtAF `fmap` f') x y
               ScanSeq f e x       -> ScanSeq (cvtF f) (cvtE e) x
               GeneralMapSeq pre a a' -> GeneralMapSeq pre (manifest fuseAcc a) (manifest fuseAcc <$> a')
  where
    cvtC :: Consumer OpenAcc aenv senv a -> Consumer DelayedOpenAcc aenv senv a
    cvtC c =
      case c of
        FoldSeqRegular pre f a  -> FoldSeqRegular pre (cvtAF f) (manifest fuseAcc a)
        FoldSeqFlatten f' f a x -> FoldSeqFlatten (cvtAF `fmap` f') (cvtAF f) (manifest fuseAcc a) x
        Stuple t                -> Stuple (cvtCT t)

    cvtCT :: Atuple (Consumer OpenAcc aenv senv) t -> Atuple (Consumer DelayedOpenAcc aenv senv) t
    cvtCT NilAtup        = NilAtup
    cvtCT (SnocAtup t c) = SnocAtup (cvtCT t) (cvtC c)

    cvtAF :: OpenAfun aenv f -> PreOpenAfun DelayedOpenAcc aenv f
    cvtAF (Alam f)  = Alam  (cvtAF f)
    cvtAF (Abody b) = Abody (manifest fuseAcc b)

    cvtE :: OpenExp env aenv t -> DelayedOpenExp env aenv t
    cvtE = convertOpenExp fuseAcc

    cvtF :: OpenFun env aenv f -> DelayedOpenFun env aenv f
    cvtF (Lam f)  = Lam (cvtF f)
    cvtF (Body b) = Body (cvtE b)


-- | Apply the fusion transformation to the AST to combine and simplify terms.
-- This converts terms into the internal delayed array representation and merges
-- adjacent producer/producer terms. Using the reduced internal form limits the
-- number of combinations that need to be considered.
--
type EmbedAcc acc = forall aenv arrs. Arrays arrs => acc aenv arrs -> Embed acc aenv arrs
type ElimAcc  acc = forall aenv s t. acc aenv s -> acc (aenv,s) t -> Bool

embedOpenAcc :: Arrays arrs => Bool -> OpenAcc aenv arrs -> Embed OpenAcc aenv arrs
embedOpenAcc fuseAcc (OpenAcc pacc) =
  embedPreAcc fuseAcc (embedOpenAcc fuseAcc) elimOpenAcc pacc
  where
    -- When does the cost of re-computation outweigh that of memory access? For
    -- the moment only do the substitution on a single use of the bound array
    -- into the use site, but it is likely advantageous to be far more
    -- aggressive here. SEE: [Sharing vs. Fusion]
    --
    -- As a special case, look for the definition of 'unzip' applied to manifest
    -- data, which is defined in the prelude as a map projecting out the
    -- appropriate element.
    --
    elimOpenAcc :: ElimAcc OpenAcc
    elimOpenAcc bnd body
      | Map f a                 <- extract bnd
      , Avar _                  <- extract a
      , Lam (Body (Prj _ _))    <- f
      = Stats.ruleFired "unzipD" True

      | count False ZeroIdx body <= lIMIT
      = True

      | Unit e <- extract bnd
      = Stats.ruleFired "simpleScalar" (simpleExp e)

      | otherwise
      = False
      where
        lIMIT = 1

        count :: UsesOfAcc OpenAcc
        count ok idx (OpenAcc pacc) = usesOfPreAcc ok count idx pacc

embedPreAcc
    :: forall acc aenv arrs. (Kit acc, Arrays arrs)
    => Bool
    -> EmbedAcc   acc
    -> ElimAcc    acc
    -> PreOpenAcc acc aenv arrs
    -> Embed      acc aenv arrs
embedPreAcc fuseAcc embedAcc elimAcc pacc
  = unembed
  $ case pacc of

    -- Non-fusible terms
    -- -----------------
    --
    -- Solid and semi-solid terms that we generally do not wish to fuse, such
    -- as control flow (|?), array introduction (use, unit), array tupling and
    -- projection, and foreign function operations. Generally we also do not
    -- want to fuse past array let bindings, as this would imply work
    -- duplication. SEE: [Sharing vs. Fusion]
    --
    Apply f a           -> applyD (cvtAF f) (cvtA a)
    Alet bnd body       -> aletD embedAcc elimAcc bnd body
    Aprj ix tup         -> aprjD embedAcc ix tup
    Acond p at ae       -> acondD embedAcc (cvtE p) at ae
    Awhile p f a        -> done $ Awhile (cvtAF p) (cvtAF f) (cvtA a)
    Atuple tup          -> done $ Atuple (cvtAT tup)
    Aforeign ff f a     -> done $ Aforeign ff (cvtAF f) (cvtA a)
    Collect s           -> collectD s

    -- Array injection
    Avar v              -> done $ Avar v
    Use arrs            -> done $ Use arrs
    Unit e              -> unitD (cvtE e)

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
    Reshape sl a        -> reshapeD (embedAcc a) (cvtE sl)

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
    -- If fusion is not enabled, force terms to the manifest representation
    --
    unembed :: Embed acc aenv arrs -> Embed acc aenv arrs
    unembed x
      | fuseAcc         = x
      | otherwise       = done (compute x)

    cvtA :: Arrays a => acc aenv' a -> acc aenv' a
    cvtA = computeAcc . embedAcc

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

    -- Conversions for closed scalar functions and expressions. This just
    -- applies scalar simplifications.
    --
    cvtF :: PreFun acc aenv t -> PreFun acc aenv t
    cvtF = simplify

    cvtE :: PreExp acc aenv' t -> PreExp acc aenv' t
    cvtE = simplify

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
         ->       acc aenv as
         -> Embed acc aenv bs
    fuse op (embedAcc -> Embed env cc) = Embed env (op env cc)

    fuse2 :: (Arrays as, Arrays bs)
          => (forall aenv'. Extend acc aenv aenv' -> Cunctation acc aenv' as -> Cunctation acc aenv' bs -> Cunctation acc aenv' cs)
          ->       acc aenv as
          ->       acc aenv bs
          -> Embed acc aenv cs
    fuse2 op a1 a0
      | Embed env1 cc1  <- embedAcc a1
      , Embed env0 cc0  <- embedAcc (sink env1 a0)
      , env             <- env1 `append` env0
      = Embed env (op env (sink env0 cc1) cc0)

    embed :: (Arrays as, Arrays bs)
          => (forall aenv'. Extend acc aenv aenv' -> acc aenv' as -> PreOpenAcc acc aenv' bs)
          ->       acc aenv as
          -> Embed acc aenv bs
    embed op (embedAcc -> Embed env cc)
      = Embed (env `PushEnv` inject (op env (inject (compute' cc)))) (Done ZeroIdx)

    embed2 :: forall aenv as bs cs. (Arrays as, Arrays bs, Arrays cs)
           => (forall aenv'. Extend acc aenv aenv' -> acc aenv' as -> acc aenv' bs -> PreOpenAcc acc aenv' cs)
           ->       acc aenv as
           ->       acc aenv bs
           -> Embed acc aenv cs
    embed2 op (embedAcc -> Embed env1 cc1) (embedAcc . sink env1 -> Embed env0 cc0)
      | env     <- env1 `append` env0
      , acc1    <- inject . compute' $ sink env0 cc1
      , acc0    <- inject . compute' $ cc0
      = Embed (env `PushEnv` inject (op env acc1 acc0)) (Done ZeroIdx)

    -- Move additional bindings for producer outside of sequence, so
    -- that producers may fuse with their arguments, resulting in
    -- actual sequencing.
    collectD :: PreOpenSeq acc aenv () arrs
             -> Embed acc aenv arrs
    collectD s | ExtendSeq env s' <- embedSeq embedAcc s
               = Embed (env `PushEnv` inject (Collect s')) (Done ZeroIdx)

data DelayedProducer acc aenv senv a where
  DoneP  :: Idx senv a -> DelayedProducer acc aenv senv a
  YieldP :: Arrays a 
         => SeqPrelude aenv senv env envReg
         -> acc env a
         -> Maybe (acc envReg (Regular a))
         -> DelayedProducer acc aenv senv a

-- Fuse after vectorization but before ordinary fusion.
fuseSeq :: PreOpenSeq OpenAcc aenv () arrs -> PreOpenSeq OpenAcc aenv () arrs
-- fuseSeq s = s -- Uncomment to disable sequence fusion.
fuseSeq s | Just REFL <- matchOpenSeq s (fuseOpenSeq s) = s
          | otherwise                                   = fuseSeq (fuseOpenSeq s)

fuseOpenSeq :: forall aenv senv arrs. PreOpenSeq OpenAcc aenv senv arrs -> PreOpenSeq OpenAcc aenv senv arrs
fuseOpenSeq s =
  case s of
    Producer p s0
      | uses ZeroIdx s0 == 1
      , Just s0' <- strSeq k $ subSeq (ZeroIdx, delayedP p) (fuseOpenSeq s0)
      -> s0'
      | otherwise
      -> Producer p (fuseOpenSeq s0)
    Consumer c -> Consumer c
    Reify f x -> Reify f x
  where
    k :: Idx (senv, a) b -> Maybe (Idx senv b)
    k ZeroIdx     = Nothing
    k (SuccIdx x) = Just x

    delayedP :: Producer OpenAcc aenv senv a -> DelayedProducer OpenAcc aenv (senv, a) a
    delayedP p =
      case p of
        StreamIn arrs         -> shiftDelayedP $ YieldP (SeqPrelude (NilAtup `SnocAtup` ArrList arrs) (ExtEmpty `ExtPush` ZeroIdx) ExtEmpty) (OpenAcc (Avar ZeroIdx)) Nothing
        ToSeq _ slix prox (OpenAcc (Use arr)) ->
          shiftDelayedP $ YieldP (SeqPrelude (NilAtup `SnocAtup` SliceArr slix prox arr) (ExtEmpty `ExtPush` ZeroIdx) ExtEmpty) (OpenAcc (Avar ZeroIdx)) (Just (OpenAcc (Avar ZeroIdx)))
        GeneralMapSeq p0 a a' -> shiftDelayedP $ YieldP p0 a a'
        MapSeq f f' x         -> shiftDelayedP $ YieldP (SeqPrelude NilAtup ExtEmpty (ExtEmpty `ExtPush` x)) (dF1 f) (dF1 <$> f')
        ZipWithSeq f f' x y   -> shiftDelayedP $ YieldP (SeqPrelude NilAtup ExtEmpty (ExtEmpty `ExtPush` x `ExtPush` y)) (dF2 f) (dF2 <$> f')
        _                     -> DoneP ZeroIdx

    dF1 :: PreOpenAfun OpenAcc aenv (a -> b) -> OpenAcc (aenv, a) b
    dF1 (Alam (Abody acc)) = acc

    dF2 :: PreOpenAfun OpenAcc aenv (a -> b -> c) -> OpenAcc ((aenv, a), b) c
    dF2 (Alam (Alam (Abody acc))) = acc

shiftDelayedP :: DelayedProducer OpenAcc aenv senv a -> DelayedProducer OpenAcc aenv (senv, b) a
shiftDelayedP (DoneP x) = DoneP (SuccIdx x)
shiftDelayedP (YieldP pre a a') = YieldP (shiftPre pre) a a'

shiftPre :: SeqPrelude aenv senv env envReg -> SeqPrelude aenv (senv, a) env envReg
shiftPre (SeqPrelude arrs ex ex') = SeqPrelude arrs ex (shiftEx ex')

shiftEx :: ExtReg a a' x b b' -> ExtReg a a' (x, t) b b'
shiftEx ExtEmpty = ExtEmpty
shiftEx (ExtPush ex x) = shiftEx ex `ExtPush` SuccIdx x

strSeq :: forall aenv senv senv' arrs. senv :?> senv' -> PreOpenSeq OpenAcc aenv senv arrs -> Maybe (PreOpenSeq OpenAcc aenv senv' arrs)
strSeq k s =
  case s of
    Producer p s0 -> Producer <$> strP p <*> strSeq (under' k) s0
    Consumer c -> Consumer <$> strC c
    Reify f x -> Reify f <$> k x
  where
    strP :: Producer OpenAcc aenv senv a -> Maybe (Producer OpenAcc aenv senv' a)
    strP p =
      case p of
        StreamIn arrs -> Just (StreamIn arrs)
        ToSeq mf slix prox a -> Just (ToSeq mf slix prox a)
        GeneralMapSeq p a a'
          | PreStr pre' ka ka' <- strPrelude p k
          -> GeneralMapSeq pre' <$> strengthen ka a <*>
               case a' of
                 Nothing -> Just Nothing
                 Just x  -> Just <$> strengthen ka' x
          | otherwise
          -> Nothing
        MapSeq f f' x -> MapSeq f f' <$> k x
        ZipWithSeq f f' x y -> ZipWithSeq f f' <$> k x <*> k y
        ScanSeq f z x -> ScanSeq f z <$> k x

    strC :: Consumer OpenAcc aenv senv c -> Maybe (Consumer OpenAcc aenv senv' c)
    strC c =
      case c of
        FoldSeqFlatten mf f a x -> FoldSeqFlatten mf f a <$> k x
        FoldSeqRegular p f a
          | PreStr pre' _ ka' <- strPrelude p k
          -> FoldSeqRegular pre' <$> strengthen ka' f <*> pure a
          | otherwise
          -> Nothing
        Stuple tup ->
          let f :: Atuple (Consumer OpenAcc aenv senv) t -> Maybe (Atuple (Consumer OpenAcc aenv senv') t)
              f NilAtup = Just NilAtup
              f (SnocAtup tup c0) = SnocAtup <$> f tup <*> strC c0
          in Stuple <$> f tup

subAcc :: forall aenv t arrs. (Arrays t, Arrays arrs)
       => (Idx aenv t, OpenAcc aenv t)
       -> OpenAcc aenv arrs
       -> OpenAcc aenv arrs
subAcc (x0, a0) a = 
  if count True x0 a == 1 -- Inline if only used one place.
    then rebuildA k' a
    else OpenAcc (a0 `alet` rebuildA k a) -- Otherwise, let-bind in front.
  where
    
    k' :: forall a'. Arrays a' => Idx aenv a' -> PreOpenAcc OpenAcc aenv a'
    k' x | Just REFL <- matchIdx x x0
         , OpenAcc a0' <- a0
         = a0'
         | otherwise
         = Avar x
    
    k :: forall a'. Arrays a' => Idx aenv a' -> PreOpenAcc OpenAcc (aenv, t) a'
    k x | Just REFL <- matchIdx x x0
        = Avar ZeroIdx
        | otherwise
        = Avar (SuccIdx x)

    count :: UsesOfAcc OpenAcc
    count ok idx (OpenAcc pacc) = usesOfPreAcc ok count idx pacc
    
    alet bnd body
      | OpenAcc (Avar ZeroIdx) <- body
      , OpenAcc x              <- bnd
      = x

      | OpenAcc (Aprj tupix (OpenAcc (Avar ix))) <- bnd
      , OpenAcc (Alet bnd' body')           <- body
      , Just REFL                           <- matchAcc (weaken SuccIdx bnd) bnd'
      = Alet (OpenAcc $ Aprj tupix (OpenAcc (Avar ix))) (inlineA body' (Avar ZeroIdx))

      | otherwise
      = Alet bnd body


subSeq :: forall aenv senv t arrs.
          (Idx senv t, DelayedProducer OpenAcc aenv senv t)
       -> PreOpenSeq OpenAcc aenv senv arrs
       -> PreOpenSeq OpenAcc aenv senv arrs
subSeq (x0, p0) s =
  case s of
    Producer p s0
      | usesP x0 p == 1 -> Producer (subP p) (subSeq (SuccIdx x0, shiftDelayedP p0) s0)
      | otherwise       -> Producer p        (subSeq (SuccIdx x0, shiftDelayedP p0) s0)
    Consumer c          -> Consumer (subC c)
    Reify f x  -> Reify f x
  where
    subP :: Producer OpenAcc aenv senv b -> Producer OpenAcc aenv senv b
    subP p =
      case p of
        StreamIn arrs -> StreamIn arrs
        ToSeq mf slix prox a -> ToSeq mf slix prox a
        GeneralMapSeq pre0 a0 a0' -> subGeneral pre0 a0 a0'
        MapSeq f f' x -> subGeneral (SeqPrelude NilAtup ExtEmpty (ExtEmpty `ExtPush` x)) (dF1 f) (dF1 <$> f')
        ZipWithSeq f f' x y -> subGeneral (SeqPrelude NilAtup ExtEmpty (ExtEmpty `ExtPush` x `ExtPush` y)) (dF2 f) (dF2 <$> f')
        ScanSeq f z x -> ScanSeq f z x

    subGeneral :: forall a env envReg. Arrays a => SeqPrelude aenv senv env envReg -> OpenAcc env a -> Maybe (OpenAcc envReg (Regular a)) -> Producer OpenAcc aenv senv a
    subGeneral pre0 a0 a0' =
      case p0 of
        YieldP pre1 (a1::OpenAcc aenv' t) a1'
          | IsC <- isArraysFlat (undefined :: a)
          , IsC <- isArraysFlat (undefined :: t)
          , PreAppend pre w0 w1 w0' w1' <- appendPrelude pre0 pre1
          , a1'w <- weaken w1' <$> a1'
          , Just x0'  <- preLookup pre x0
          , Just x0'r <- preLookupReg pre x0
          -> GeneralMapSeq pre (subAcc (x0', weaken w1 a1) (weaken w0 a0)) (subAcc <$> ((,) x0'r <$> a1'w) <*> (weaken w0' <$> a0'))
        _ -> GeneralMapSeq pre0 a0 a0'

    dF1 :: PreOpenAfun OpenAcc aenv (a -> b) -> OpenAcc (aenv, a) b
    dF1 (Alam (Abody acc)) = acc

    dF2 :: PreOpenAfun OpenAcc aenv (a -> b -> c) -> OpenAcc ((aenv, a), b) c
    dF2 (Alam (Alam (Abody acc))) = acc

    subC :: Consumer OpenAcc aenv senv b -> Consumer OpenAcc aenv senv b
    subC c
      | usesC x0 c == 1
      = case c of
          FoldSeqFlatten (Just (Alam f')) _ a x -> subGeneralC (SeqPrelude NilAtup ExtEmpty (ExtEmpty `ExtPush` x)) f' a
          FoldSeqFlatten Nothing f a x -> FoldSeqFlatten Nothing f a x
          FoldSeqRegular pre0 f a -> subGeneralC pre0 f a
          Stuple tup ->
            let f :: Atuple (Consumer OpenAcc aenv senv) tup -> Atuple (Consumer OpenAcc aenv senv) tup
                f NilAtup = NilAtup
                f (SnocAtup t c0) = f t `SnocAtup` subC c0
            in Stuple $ f tup
      | otherwise
      = c

    subGeneralC :: Arrays a => SeqPrelude aenv senv env envReg -> OpenAfun envReg (a -> a) -> OpenAcc aenv a -> Consumer OpenAcc aenv senv a
    subGeneralC pre0 f a =
      case p0 of
        YieldP pre1 (_::OpenAcc aenv' t) a1'
          | IsC <- isArraysFlat (undefined :: t)
          , PreAppend pre _ _ w0' w1' <- appendPrelude pre0 pre1
          , Just a1'w <- weaken w1' <$> a1'
          , Just x0'r <- preLookupReg pre x0
          , Alam (Abody bdy) <- weaken w0' f
          -> FoldSeqRegular pre (Alam $ Abody $ subAcc (SuccIdx x0'r, weaken SuccIdx a1'w) bdy) a
        _ -> FoldSeqRegular pre0 f a

data ExtAppend a a' x b1 b2 b1' b2' where
  ExtAppend :: ExtReg a a' x b b'
         -> b1 :> b
         -> b2 :> b
         -> b1' :> b'
         -> b2' :> b'
         -> ExtAppend a a' x b1 b2 b1' b2'

data ArrAppend arrs1 arrs2 where
  ArrAppend :: Atuple Aconst arrs
            -> arrs1 :> arrs
            -> arrs2 :> arrs
            -> ArrAppend arrs1 arrs2

strPrelude :: SeqPrelude aenv senv env envReg -> senv :?> senv' -> PreStr aenv senv' env envReg
strPrelude (SeqPrelude arrs ex ex') k
  | ExtStr ex'' a b <- strExt ex' k
  = PreStr (SeqPrelude arrs ex ex'') a b

strExt :: ExtReg a a' x b b' -> x :?> x' -> ExtStr a a' x' b b'
strExt ExtEmpty _ = ExtStr ExtEmpty Just Just
strExt (ExtPush ex x) k =
  case k x of
    Nothing
      | ExtStr ex' a b <- strExt ex k
      -> ExtStr ex'
           (\ x -> case x of ZeroIdx -> Nothing; SuccIdx x0 -> a x0)
           (\ x -> case x of ZeroIdx -> Nothing; SuccIdx x0 -> b x0)
    Just x'
      | ExtStr ex' a b <- strExt ex k
      -> ExtStr (ex' `ExtPush` x') (under' a) (under' b)

data PreAppend aenv senv env1 env2 envReg1 envReg2 where
  PreAppend :: SeqPrelude aenv senv env3 envReg3
      -> env1 :> env3
      -> env2 :> env3
      -> envReg1 :> envReg3
      -> envReg2 :> envReg3
      -> PreAppend aenv senv env1 env2 envReg1 envReg2

preLookup :: SeqPrelude aenv senv env envReg -> senv :?> env
preLookup (SeqPrelude _ _ ex) = extLookup ex

preLookupReg :: SeqPrelude aenv senv env envReg -> Idx senv t -> Maybe (Idx envReg (Regular t))
preLookupReg (SeqPrelude _ _ ex) = extLookupReg ex

extLookup :: forall a a' x b b'. ExtReg a a' x b b' -> x :?> b
extLookup ExtEmpty _          = Nothing
extLookup (ExtPush ext y) x
  | Just REFL <- matchIdx x y = Just ZeroIdx
  | otherwise                 = SuccIdx <$> extLookup ext x

extLookupReg :: forall a a' x b b' t. ExtReg a a' x b b' -> Idx x t -> Maybe (Idx b' (Regular t))
extLookupReg ExtEmpty _          = Nothing
extLookupReg (ExtPush ext y) x
  | Just REFL <- matchIdx x y = Just ZeroIdx
  | otherwise                 = SuccIdx <$> extLookupReg ext x

appendExt :: ExtReg a a' x b1 b1'
          -> ExtReg a a' x b2 b2'
          -> ExtAppend a a' x b1 b2 b1' b2'
appendExt ex ExtEmpty = ExtAppend ex id (extShift ex) id (extShiftReg ex)
appendExt ex1 (ExtPush ex2 x)
  | ExtAppend ex w1 w2 w1' w2' <- appendExt ex1 ex2
  = case (extLookup ex1 x, extLookupReg ex1 x) of
      (Nothing, Nothing) -> ExtAppend (ExtPush ex x) (SuccIdx . w1) (under w2) (SuccIdx . w1') (under w2')
      (Just y, Just y')  -> ExtAppend ex 
                 w1 
                 (\ x -> case x of ZeroIdx -> w1 y; SuccIdx x0 -> w2 x0) 
                 w1' 
                 (\ x -> case x of ZeroIdx -> w1' y'; SuccIdx x0 -> w2' x0)

appendArrs :: Atuple Aconst arrs1 -> Atuple Aconst arrs2 -> ArrAppend arrs1 arrs2
appendArrs arrs1 NilAtup = ArrAppend arrs1 id (error "empty function :: Idx () t -> Idx arrs1 t")
appendArrs arrs1 (SnocAtup arrs2 a)
  | ArrAppend arrs wa1 wa2 <- appendArrs arrs1 arrs2
  = case arrLookup arrs1 a of
      Nothing -> ArrAppend (arrs `SnocAtup` a) (SuccIdx . wa1) (under wa2)
      Just y  -> ArrAppend arrs wa1 (\ x -> case x of ZeroIdx -> wa1 y; SuccIdx x0 -> wa2 x0)

arrLookup :: Atuple Aconst arrs -> Aconst a -> Maybe (Idx arrs a)
arrLookup NilAtup _ = Nothing
arrLookup (SnocAtup tup a) a0
  | Just REFL <- matchAconst a a0 = Just ZeroIdx
  | otherwise                     = SuccIdx <$> arrLookup tup a0

appendPrelude :: SeqPrelude aenv senv env1 envReg1
              -> SeqPrelude aenv senv env2 envReg2
              -> PreAppend aenv senv env1 env2 envReg1 envReg2
appendPrelude (SeqPrelude arrs1 ex1 ex1') (SeqPrelude arrs2 ex2 ex2')
  | ArrAppend arrs wa1 wa2 <- appendArrs arrs1 arrs2
  , ExtAppend ex w1 w2 w1' w2' <- appendExt (weakenArrs wa1 ex1) (weakenArrs wa2 ex2)
  , ExtWeak ex1'' s1 s1' <- extWeak ex1' w1 w1'
  , ExtWeak ex2'' s2 s2' <- extWeak ex2' w2 w2'
  , ExtAppend ex' q1 q2 q1' q2' <- appendExt ex1'' ex2''
  = PreAppend (SeqPrelude arrs ex ex') (q1 . s1) (q2 . s2) (q1' . s1') (q2' . s2')

data ExtWeak a a' x b b' where
  ExtWeak :: ExtReg a a' x q q'
             -> b  :> q
             -> b' :> q'
             -> ExtWeak a a' x b b'

data PreStr aenv senv env envReg where
  PreStr :: SeqPrelude aenv senv env' envReg'
         -> env    :?> env'
         -> envReg :?> envReg'
         -> PreStr aenv senv env envReg

data PreWeak aenv senv env envReg where
  PreWeak :: SeqPrelude aenv senv env' envReg'
          -> env    :> env'
          -> envReg :> envReg'
          -> PreWeak aenv senv env envReg

weakenPrelude :: SeqPrelude aenv senv env envReg -> aenv :> aenv' -> PreWeak aenv' senv env envReg
weakenPrelude (SeqPrelude arrs ex ex') k
  | ExtWeak ex0  a b <- extWeak ex  k k
  , ExtWeak ex0' c d <- extWeak ex' a b
  = PreWeak (SeqPrelude arrs ex0 ex0') c d

data ExtStr a a' x b b' where
  ExtStr :: ExtReg a a' x q q'
         -> b  :?> q
         -> b' :?> q'
         -> ExtStr a a' x b b'

extWeak :: ExtReg a a' x b b'
    -> a  :> c
    -> a' :> c'
    -> ExtWeak c c' x b b'
extWeak ExtEmpty k k' = ExtWeak ExtEmpty k k'
extWeak (ExtPush ex x) k k'
  | ExtWeak ex0 k0 k0' <- extWeak ex k k'
  = ExtWeak (ex0 `ExtPush` x) (under k0) (under k0')

weakenArrs :: x :> x' -> ExtReg a a' x b b' -> ExtReg a a' x' b b'
weakenArrs _ ExtEmpty = ExtEmpty
weakenArrs k (ExtPush ex0 x) = weakenArrs k ex0 `ExtPush` k x

under :: x :> y -> (x, a) :> (y, a)
under _ ZeroIdx = ZeroIdx
under k (SuccIdx x) = SuccIdx (k x)

under' :: x :?> y -> (x, a) :?> (y, a)
under' _ ZeroIdx = Just ZeroIdx
under' k (SuccIdx x) = SuccIdx <$> k x

uses :: Idx senv a -> PreOpenSeq acc aenv senv arrs -> Int
uses idx s =
  case s of
    Producer p s0 -> usesP idx p + uses (SuccIdx idx) s0
    Consumer c    -> usesC idx c
    Reify _ x     -> usesX idx x

usesX :: Idx senv a -> Idx senv b -> Int
usesX x y | idxToInt x == idxToInt y = 1
          | otherwise                = 0

usesPre :: forall aenv senv env envReg a. Idx senv a -> SeqPrelude aenv senv env envReg -> Int
usesPre idx (SeqPrelude _ _ ex) =
  let f :: ExtReg c c' senv b b' -> Int
      f ExtEmpty = 0
      f (ExtPush ex0 x) = f ex0 + usesX idx x
  in f ex `max` 1

usesP :: Idx senv a -> Producer acc aenv senv b -> Int
usesP idx p =
  case p of
    StreamIn{} -> 0
    ToSeq{}    -> 0
    GeneralMapSeq p _ _ -> usesPre idx p
    MapSeq _ _ x -> usesX idx x
    ZipWithSeq _ _ x y -> usesX idx x `max` usesX idx y
    ScanSeq _ _ x -> usesX idx x

usesC :: forall acc aenv senv a b. Idx senv a -> Consumer acc aenv senv b -> Int
usesC idx c =
  case c of
    FoldSeqFlatten _ _ _ x -> usesX idx x
    FoldSeqRegular p _ _ -> usesPre idx p
    Stuple tup ->
      let f :: Atuple (Consumer acc aenv senv) t -> Int
          f NilAtup = 0
          f (SnocAtup tup c0) = f tup + usesC idx c0
      in f tup

-- Move additional bindings for producer outside of sequence, so
-- that producers may fuse with their arguments, resulting in
-- actual sequencing.
embedSeq :: forall acc aenv arrs. Kit acc
         => EmbedAcc acc
         -> PreOpenSeq acc aenv () arrs
         -> ExtendSeq       acc aenv () arrs
embedSeq embedAcc s
  = travS s BaseEnv
  where
    travS :: forall senv aenv' arrs'.
             PreOpenSeq acc aenv senv arrs'
          -> Extend acc aenv aenv'
          -> ExtendSeq acc aenv senv arrs'
    travS s env =
      case s of
        Producer p s
          | ExtendSeq env' s' <- travS s env
          , ExtendProducer env'' p' <- travP p env'
          -> ExtendSeq (env' `append` env'') (Producer p' (sinkSeq env'' s'))
        Consumer c
          | c' <- travC c env
          -> ExtendSeq env (Consumer c')
        Reify f ix
          -> ExtendSeq env (Reify (cvtAF `fmap` (sink env `fmap` f)) ix)

    sinkExtend :: Extend acc env env' -> env :> env'
    sinkExtend BaseEnv       = id
    sinkExtend (PushEnv e _) = SuccIdx . sinkExtend e


    travP :: forall arrs' aenv' senv.
             Producer acc aenv senv arrs'
          -> Extend acc aenv aenv'
          -> ExtendProducer acc aenv' senv arrs'
    travP (ToSeq f slix sh a) env
      | Use _ <- extract a  -- Enable "use lazy" optimization.
      = ExtendProducer BaseEnv (ToSeq (cvtAF `fmap` (sink env `fmap` f)) slix sh (sink env a))
      | Embed env' cc <- embedAcc (sink env a)
      = ExtendProducer env' (ToSeq (cvtAF `fmap` (sink env' `fmap` (sink env `fmap` f))) slix sh (inject (compute' cc)))
    travP (StreamIn arrs) _          = ExtendProducer BaseEnv (StreamIn arrs)
    travP (GeneralMapSeq pre a a') env
      | IsC <- isArraysFlat (undefined :: arrs')
      , PreWeak pre' k k' <- weakenPrelude pre (sinkExtend env)
      = ExtendProducer BaseEnv (GeneralMapSeq pre' (cvtA (weaken k a)) (cvtA `fmap` (weaken k' `fmap` a')))
      | otherwise = error "should not happen"
    travP (MapSeq f f' x) env        = ExtendProducer BaseEnv (MapSeq (cvtAF (sink env f)) (cvtAF `fmap` (sink env `fmap` f')) x)
    travP (ZipWithSeq f f' x y) env  = ExtendProducer BaseEnv (ZipWithSeq (cvtAF (sink env f)) (cvtAF `fmap` (sink env `fmap` f')) x y)
    travP (ScanSeq f e x) env        = ExtendProducer BaseEnv (ScanSeq (cvtF (sink env f)) (cvtE (sink env e)) x)

    travC :: forall arrs' aenv' senv.
             Consumer acc aenv senv arrs'
          -> Extend acc aenv aenv'
          -> Consumer acc aenv' senv arrs'
    travC (FoldSeqRegular pre f a) env
      | IsC <- isArraysFlat (undefined :: arrs')
      , PreWeak pre' _ k' <- weakenPrelude pre (sinkExtend env)
      = FoldSeqRegular pre' (cvtAF (weaken k' f)) (cvtA (sink env a))
      | otherwise = error "should not happen"
    travC (FoldSeqFlatten f' f a x) env = FoldSeqFlatten (cvtAF `fmap` (sink env `fmap` f')) (cvtAF (sink env f)) (cvtA (sink env a)) x
    travC (Stuple t) env = Stuple (cvtCT t)
      where
        cvtCT :: Atuple (Consumer acc aenv senv) t -> Atuple (Consumer acc aenv' senv) t
        cvtCT NilAtup        = NilAtup
        cvtCT (SnocAtup t c) = SnocAtup (cvtCT t) (travC c env)

    cvtE :: PreExp acc aenv' t -> PreExp acc aenv' t
    cvtE = simplify

    cvtF :: PreFun acc aenv' t -> PreFun acc aenv' t
    cvtF = simplify

    cvtA :: Arrays a => acc aenv' a -> acc aenv' a
    cvtA = computeAcc . embedAcc

    cvtAF :: PreOpenAfun acc aenv' f -> PreOpenAfun acc aenv' f
    cvtAF (Alam  f) = Alam  (cvtAF f)
    cvtAF (Abody a) = Abody (cvtA a)


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
--   append :: Extend env env1 -> Extend env env2 -> Extend env ???
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
data Embed acc aenv a where
  Embed :: Extend     acc aenv aenv'
        -> Cunctation acc      aenv' a
        -> Embed      acc aenv       a


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
  -- environment, ensuring that the array is manifest and making the term
  -- non-recursive in 'acc'. Also note that the return type is a general
  -- instance of Arrays and not restricted to a single Array.
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


instance Kit acc => Simplify (Cunctation acc aenv a) where
  simplify (Done v)        = Done v
  simplify (Yield sh f)    = Yield (simplify sh) (simplify f)
  simplify (Step sh p f v) = Step (simplify sh) (simplify p) (simplify f) v


-- Convert a real AST node into the internal representation
--
done :: (Arrays a, Kit acc) => PreOpenAcc acc aenv a -> Embed acc aenv a
done pacc
  | Avar v <- pacc      = Embed BaseEnv                         (Done v)
  | otherwise           = Embed (BaseEnv `PushEnv` inject pacc) (Done ZeroIdx)


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
      | ArraysRarray <- accType cc      -> Yield (arrayShape v) (indexArray v)
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
      | ArraysRarray <- accType cc      -> Just $ Step (arrayShape v) identity identity v
      | otherwise                       -> error "step: impossible case"


-- Get the shape of a delayed array
--
shape :: Kit acc => Cunctation acc aenv (Array sh e) -> PreExp acc aenv sh
shape cc
  | Just (Step sh _ _ _) <- step cc     = sh
  | Yield sh _           <- yield cc    = sh


-- Reified type of a delayed array representation.
--
accType :: forall acc aenv a. Arrays a => Cunctation acc aenv a -> ArraysR (ArrRepr a)
accType _ = arrays (undefined :: a)


-- Environment manipulation
-- ========================

-- prjExtend :: Kit acc => Extend acc env env' -> Idx env' t -> PreOpenAcc acc env' t
-- prjExtend (PushEnv _   v) ZeroIdx       = weakenA rebuildAcc SuccIdx v
-- prjExtend (PushEnv env _) (SuccIdx idx) = weakenA rebuildAcc SuccIdx $ prjExtend env idx
-- prjExtend _               _             = $internalError "prjExtend" "inconsistent valuation"

-- Rearrange type arguments to fit with Sink type class.
newtype SinkSeq acc senv aenv a = SinkSeq { unSinkSeq :: PreOpenSeq acc aenv senv a }

-- sink for sequences.
sinkSeq :: Kit acc => Extend acc aenv aenv' -> PreOpenSeq acc aenv senv a -> PreOpenSeq acc aenv' senv a
sinkSeq env s = unSinkSeq $ sink env (SinkSeq s)

instance Kit acc => Sink (SinkSeq acc senv) where
  weaken :: forall aenv aenv' arrs. aenv :> aenv' -> SinkSeq acc senv aenv arrs -> SinkSeq acc senv aenv' arrs
  weaken k (SinkSeq s) = SinkSeq $
    case s of
      Producer p s' -> Producer   (weakenP p) (weakenL s')
      Consumer c    -> Consumer   (weakenC c)
      Reify f ix    -> Reify      (weaken k `fmap` f) ix

    where
      weakenL :: forall senv' arrs'. PreOpenSeq acc aenv senv' arrs' -> PreOpenSeq acc aenv' senv' arrs'
      weakenL s' = unSinkSeq (weaken k (SinkSeq s'))

      weakenP :: forall a. Producer acc aenv senv a -> Producer acc aenv' senv a
      weakenP p =
        case p of
          StreamIn arrs        -> StreamIn arrs
          ToSeq f slix sh a    -> ToSeq (weaken k `fmap` f) slix sh (weaken k a)
          GeneralMapSeq pre a a'
            | IsC <- isArraysFlat (undefined :: a)
            , PreWeak pre' k1 k1' <- weakenPrelude pre k
            -> GeneralMapSeq pre' (weaken k1 a) (weaken k1' `fmap` a')
            | otherwise -> error "should not happen"
          MapSeq f f' x        -> MapSeq (weaken k f) (weaken k `fmap` f') x
          ZipWithSeq f f' x y  -> ZipWithSeq (weaken k f) (weaken k `fmap` f') x y
          ScanSeq f a x        -> ScanSeq (weaken k f) (weaken k a) x

      weakenC :: forall a. Consumer acc aenv senv a -> Consumer acc aenv' senv a
      weakenC c =
        case c of
          FoldSeqRegular pre f a
            | IsC <- isArraysFlat (undefined :: a)
            , PreWeak pre' _ k1' <- weakenPrelude pre k
            -> FoldSeqRegular pre' (weaken k1' f) (weaken k a)
            | otherwise -> error "should not happen"
          FoldSeqFlatten f' f a x -> FoldSeqFlatten (weaken k `fmap` f') (weaken k f) (weaken k a) x
          Stuple t                ->
            let wk :: Atuple (Consumer acc aenv senv) t -> Atuple (Consumer acc aenv' senv) t
                wk NilAtup        = NilAtup
                wk (SnocAtup t c) = SnocAtup (wk t) (weakenC c)
            in Stuple (wk t)


instance Kit acc => Sink (Cunctation acc) where
  weaken k cc = case cc of
    Done v              -> Done (weaken k v)
    Step sh p f v       -> Step (weaken k sh) (weaken k p) (weaken k f) (weaken k v)
    Yield sh f          -> Yield (weaken k sh) (weaken k f)


-- Array fusion of a de Bruijn computation AST
-- ===========================================

-- Array computations
-- ------------------

-- Recast the internal representation of delayed arrays into a real AST node.
-- Use the most specific version of a combinator whenever possible.
--
compute :: (Kit acc, Arrays arrs) => Embed acc aenv arrs -> PreOpenAcc acc aenv arrs
compute (Embed env cc) = bind env (compute' cc)

compute' :: (Kit acc, Arrays arrs) => Cunctation acc aenv arrs -> PreOpenAcc acc aenv arrs
compute' cc = case simplify cc of
  Done v                                        -> Avar v
  Yield IndexNil (Lam (Body b))                 -> Unit (simplify (Let IndexNil b))
  Yield sh f                                    -> Generate sh f
  Step sh p f v
    | Just REFL <- match sh (arrayShape v)
    , Just REFL <- isIdentity p
    , Just REFL <- isIdentity f                 -> Avar v
    | Just REFL <- match sh (arrayShape v)
    , Just REFL <- isIdentity p                 -> Map f (avarIn v)
    | Just REFL <- isIdentity f                 -> Backpermute sh p (avarIn v)
    | otherwise                                 -> Transform sh p f (avarIn v)


-- Evaluate a delayed computation and tie the recursive knot
--
computeAcc :: (Kit acc, Arrays arrs) => Embed acc aenv arrs -> acc aenv arrs
computeAcc = inject . compute

unitD :: (Kit acc, Elt e)
      => PreExp acc aenv e
      -> Embed acc aenv (Scalar e)
unitD e = Embed BaseEnv (Yield IndexNil (Lam (Body (weakenE SuccIdx e))))

-- Representation of a generator as a delayed array
--
generateD :: (Shape sh, Elt e)
          => PreExp acc aenv sh
          -> PreFun acc aenv (sh -> e)
          -> Embed  acc aenv (Array sh e)
generateD sh f
  = Stats.ruleFired "generateD"
  $ Embed BaseEnv (Yield sh f)


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
-- For delayed arrays this is implemented as an index space transformation. For
-- manifest arrays this can be done with the standard Reshape operation in
-- constant time without executing any array operations. This does not affect
-- the fusion process since the term is already manifest.
--
-- TLM: there was a runtime check to ensure the old and new shapes contained the
--      same number of elements: this has been lost for the delayed cases!
--
reshapeD
    :: (Kit acc, Shape sh, Shape sl, Elt e)
    => Embed  acc aenv (Array sh e)
    -> PreExp acc aenv sl
    -> Embed  acc aenv (Array sl e)
reshapeD (Embed env cc) (sink env -> sl)
  | Done v      <- cc
  = Embed (env `PushEnv` inject (Reshape sl (avarIn v))) (Done ZeroIdx)

  | otherwise
  = Stats.ruleFired "reshapeD"
  $ Embed env (backpermuteD sl (reindex (shape cc) sl) cc)


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
    combine :: forall acc aenv a b c e. (Kit acc, Elt a, Elt b, Elt c)
            => PreFun acc aenv (a -> b -> c)
            -> PreFun acc aenv (e -> a)
            -> PreFun acc aenv (e -> b)
            -> PreFun acc aenv (e -> c)
    combine c ixa ixb
      | Lam (Lam (Body c'))     <- weakenE SuccIdx c   :: PreOpenFun acc ((),e) aenv (a -> b -> c)
      , Lam (Body ixa')         <- ixa                          -- else the skolem 'e' will escape
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
-- let-binding:
-- ------------
--
-- Ultimately, we might not want to eliminate the binding. If so, evaluate it
-- and add it to a _clean_ Extend environment for the body. If not, the Extend
-- list effectively _flattens_ all bindings, so any terms required for the bound
-- term get lifted out to the same scope as the body. This increases their
-- lifetime and hence raises the maximum memory used. If we don't do this, we
-- get terms such as:
--
--   let a0  = <terms for binding> in
--   let bnd = <bound term> in
--   <body term>
--
-- rather than the following, where the scope of a0 is clearly only availably
-- when evaluating the bound term, as it should be:
--
--   let bnd =
--     let a0 = <terms for binding>
--     in <bound term>
--   in <body term>
--
aletD :: (Kit acc, Arrays arrs, Arrays brrs)
      => EmbedAcc acc
      -> ElimAcc  acc
      ->          acc aenv        arrs
      ->          acc (aenv,arrs) brrs
      -> Embed    acc aenv        brrs
aletD embedAcc elimAcc (embedAcc -> Embed env1 cc1) acc0

  -- let-floating
  -- ------------
  --
  -- Immediately inline the variable referring to the bound expression into the
  -- body, instead of adding to the environments and creating an indirection
  -- that must be later eliminated by shrinking.
  --
  | Done v1             <- cc1
  , Embed env0 cc0      <- embedAcc $ rebuildA (subAtop (Avar v1) . sink1 env1) acc0
  = Stats.ruleFired "aletD/float"
  $ Embed (env1 `append` env0) cc0

  -- Ensure we only call 'embedAcc' once on the body expression
  --
  | otherwise
  = aletD' embedAcc elimAcc (Embed env1 cc1) (embedAcc acc0)


aletD' :: forall acc aenv arrs brrs. (Kit acc, Arrays arrs, Arrays brrs)
       => EmbedAcc acc
       -> ElimAcc  acc
       -> Embed    acc aenv         arrs
       -> Embed    acc (aenv, arrs) brrs
       -> Embed    acc aenv         brrs
aletD' embedAcc elimAcc (Embed env1 cc1) (Embed env0 cc0)

  -- let-binding
  -- -----------
  --
  -- Check whether we can eliminate the let-binding. Note that we must inspect
  -- the entire term, not just the Cunctation that would be produced by
  -- embedAcc. If we don't we can be left with dead terms that don't get
  -- eliminated. This problem occurred in the canny program.
  --
  | acc1                <- compute (Embed env1 cc1)
  , False               <- elimAcc (inject acc1) acc0
  = Stats.ruleFired "aletD/bind"
  $ Embed (BaseEnv `PushEnv` inject acc1 `append` env0) cc0

  -- let-elimination
  -- ---------------
  --
  -- Handle the remaining cases in a separate function. It turns out that this
  -- is important so we aren't excessively sinking/delaying terms.
  --
  | acc0'               <- sink1 env1 acc0
  = Stats.ruleFired "aletD/eliminate"
  $ case cc1 of
      Step{}    -> eliminate env1 cc1 acc0'
      Yield{}   -> eliminate env1 cc1 acc0'

  where
    acc0 :: acc (aenv, arrs) brrs
    acc0 = computeAcc (Embed env0 cc0)

    -- The second part of let-elimination. Splitting into two steps exposes the
    -- extra type variables, and ensures we don't do extra work manipulating the
    -- body when not necessary (which can lead to a complexity blowup).
    --
    eliminate :: forall aenv aenv' sh e brrs. (Kit acc, Shape sh, Elt e, Arrays brrs)
              => Extend     acc aenv aenv'
              -> Cunctation acc      aenv' (Array sh e)
              ->            acc     (aenv', Array sh e) brrs
              -> Embed      acc aenv                    brrs
    eliminate env1 cc1 body
      | Done v1           <- cc1 = elim (arrayShape v1) (indexArray v1)
      | Step sh1 p1 f1 v1 <- cc1 = elim sh1 (f1 `compose` indexArray v1 `compose` p1)
      | Yield sh1 f1      <- cc1 = elim sh1 f1
      where
        bnd :: PreOpenAcc acc aenv' (Array sh e)
        bnd = compute' cc1

        elim :: PreExp acc aenv' sh -> PreFun acc aenv' (sh -> e) -> Embed acc aenv brrs
        elim sh1 f1
          | sh1'                <- weaken SuccIdx sh1
          , f1'                 <- weaken SuccIdx f1
          , Embed env0' cc0'    <- embedAcc $ rebuildA (subAtop bnd) $ kmap (replaceA sh1' f1' ZeroIdx) body
          = Embed (env1 `append` env0') cc0'

    -- As part of let-elimination, we need to replace uses of array variables in
    -- scalar expressions with an equivalent expression that generates the
    -- result directly
    --
    -- TODO: when we inline bindings we ought to let bind at the first
    --       occurrence and use a variable at all subsequent locations. At the
    --       moment we are just hoping CSE in the simplifier phase does good
    --       things, but that is limited in what it looks for.
    --
    replaceE :: forall env aenv sh e t. (Kit acc, Shape sh, Elt e)
             => PreOpenExp acc env aenv sh -> PreOpenFun acc env aenv (sh -> e) -> Idx aenv (Array sh e)
             -> PreOpenExp acc env aenv t
             -> PreOpenExp acc env aenv t
    replaceE sh' f' avar exp =
      case exp of
        Let x y                         -> Let (cvtE x) (replaceE (weakenE SuccIdx sh') (weakenE SuccIdx f') avar y)
        Var i                           -> Var i
        Foreign ff f e                  -> Foreign ff f (cvtE e)
        Const c                         -> Const c
        Tuple t                         -> Tuple (cvtT t)
        Prj ix e                        -> Prj ix (cvtE e)
        IndexNil                        -> IndexNil
        IndexCons sl sz                 -> IndexCons (cvtE sl) (cvtE sz)
        IndexHead sh                    -> IndexHead (cvtE sh)
        IndexTail sz                    -> IndexTail (cvtE sz)
        IndexTrans sz                   -> IndexTrans (cvtE sz)
        IndexAny                        -> IndexAny
        IndexSlice x ix sh              -> IndexSlice x (cvtE ix) (cvtE sh)
        IndexFull x ix sl               -> IndexFull x (cvtE ix) (cvtE sl)
        ToIndex sh ix                   -> ToIndex (cvtE sh) (cvtE ix)
        FromIndex sh i                  -> FromIndex (cvtE sh) (cvtE i)
        Cond p t e                      -> Cond (cvtE p) (cvtE t) (cvtE e)
        PrimConst c                     -> PrimConst c
        PrimApp g x                     -> PrimApp g (cvtE x)
        ShapeSize sh                    -> ShapeSize (cvtE sh)
        Intersect sh sl                 -> Intersect (cvtE sh) (cvtE sl)
        Union s t                       -> Union (cvtE s) (cvtE t)
        While p f x                     -> While (replaceF sh' f' avar p) (replaceF sh' f' avar f) (cvtE x)

        Shape a
          | Just REFL <- match a a'     -> Stats.substitution "replaceE/shape" sh'
          | otherwise                   -> exp

        Index a sh
          | Just REFL    <- match a a'
          , Lam (Body b) <- f'          -> Stats.substitution "replaceE/!" . cvtE $ Let sh b
          | otherwise                   -> Index a (cvtE sh)

        LinearIndex a i
          | Just REFL    <- match a a'
          , Lam (Body b) <- f'          -> Stats.substitution "replaceE/!!" . cvtE $ Let (Let i (FromIndex (weakenE SuccIdx sh') (Var ZeroIdx))) b
          | otherwise                   -> LinearIndex a (cvtE i)

      where
        a' :: acc aenv (Array sh e)
        a' = avarIn avar

        cvtE :: PreOpenExp acc env aenv s -> PreOpenExp acc env aenv s
        cvtE = replaceE sh' f' avar

        cvtT :: Tuple (PreOpenExp acc env aenv) s -> Tuple (PreOpenExp acc env aenv) s
        cvtT NilTup        = NilTup
        cvtT (SnocTup t e) = cvtT t `SnocTup` cvtE e

    replaceF :: forall env aenv sh e t. (Kit acc, Shape sh, Elt e)
             => PreOpenExp acc env aenv sh -> PreOpenFun acc env aenv (sh -> e) -> Idx aenv (Array sh e)
             -> PreOpenFun acc env aenv t
             -> PreOpenFun acc env aenv t
    replaceF sh' f' avar fun =
      case fun of
        Body e          -> Body (replaceE sh' f' avar e)
        Lam f           -> Lam  (replaceF (weakenE SuccIdx sh') (weakenE SuccIdx f') avar f)

    replaceA :: forall aenv sh e a. (Kit acc, Shape sh, Elt e)
             => PreExp acc aenv sh -> PreFun acc aenv (sh -> e) -> Idx aenv (Array sh e)
             -> PreOpenAcc acc aenv a
             -> PreOpenAcc acc aenv a
    replaceA sh' f' avar pacc =
      case pacc of
        Avar v
          | Just REFL <- match v avar   -> Avar avar
          | otherwise                   -> Avar v

        Alet bnd body                   ->
          let sh'' = weaken SuccIdx sh'
              f''  = weaken SuccIdx f'
          in
          Alet (cvtA bnd) (kmap (replaceA sh'' f'' (SuccIdx avar)) body)

        Use arrs                -> Use arrs
        Unit e                  -> Unit (cvtE e)
        Acond p at ae           -> Acond (cvtE p) (cvtA at) (cvtA ae)
        Aprj ix tup             -> Aprj ix (cvtA tup)
        Atuple tup              -> Atuple (cvtAT tup)
        Awhile p f a            -> Awhile (cvtAF p) (cvtAF f) (cvtA a)
        Apply f a               -> Apply (cvtAF f) (cvtA a)
        Aforeign ff f a         -> Aforeign ff f (cvtA a)       -- no sharing between f and a
        Generate sh f           -> Generate (cvtE sh) (cvtF f)
        Map f a                 -> Map (cvtF f) (cvtA a)
        ZipWith f a b           -> ZipWith (cvtF f) (cvtA a) (cvtA b)
        Backpermute sh p a      -> Backpermute (cvtE sh) (cvtF p) (cvtA a)
        Transform sh p f a      -> Transform (cvtE sh) (cvtF p) (cvtF f) (cvtA a)
        Slice slix a sl         -> Slice slix (cvtA a) (cvtE sl)
        Replicate slix sh a     -> Replicate slix (cvtE sh) (cvtA a)
        Reshape sl a            -> Reshape (cvtE sl) (cvtA a)
        Fold f z a              -> Fold (cvtF f) (cvtE z) (cvtA a)
        Fold1 f a               -> Fold1 (cvtF f) (cvtA a)
        FoldSeg f z a s         -> FoldSeg (cvtF f) (cvtE z) (cvtA a) (cvtA s)
        Fold1Seg f a s          -> Fold1Seg (cvtF f) (cvtA a) (cvtA s)
        Scanl f z a             -> Scanl (cvtF f) (cvtE z) (cvtA a)
        Scanl1 f a              -> Scanl1 (cvtF f) (cvtA a)
        Scanl' f z a            -> Scanl' (cvtF f) (cvtE z) (cvtA a)
        Scanr f z a             -> Scanr (cvtF f) (cvtE z) (cvtA a)
        Scanr1 f a              -> Scanr1 (cvtF f) (cvtA a)
        Scanr' f z a            -> Scanr' (cvtF f) (cvtE z) (cvtA a)
        Permute f d p a         -> Permute (cvtF f) (cvtA d) (cvtF p) (cvtA a)
        Stencil f x a           -> Stencil (cvtF f) x (cvtA a)
        Stencil2 f x a y b      -> Stencil2 (cvtF f) x (cvtA a) y (cvtA b)
        Collect seq             -> Collect (cvtSeq seq)

      where
        cvtA :: acc aenv s -> acc aenv s
        cvtA = kmap (replaceA sh' f' avar)

        cvtSeq :: PreOpenSeq acc aenv senv s -> PreOpenSeq acc aenv senv s
        cvtSeq s =
          case s of
            Producer p s' ->
              Producer
                (case p of
                   StreamIn arrs        -> StreamIn arrs
                   ToSeq f slix sh a    -> ToSeq (cvtAF `fmap` f) slix sh (cvtA a)
                   GeneralMapSeq pre a a' ->
                     GeneralMapSeq pre
                       (kmap (replaceA (weaken (seqPreludeShift pre) sh') (weaken (seqPreludeShift pre) f') (weaken (seqPreludeShift pre) avar)) a)
                       (kmap (replaceA (weaken (seqPreludeShiftReg pre) sh') (weaken (seqPreludeShiftReg pre) f') (weaken (seqPreludeShiftReg pre) avar)) <$> a')
                   MapSeq f f' x        -> MapSeq (cvtAF f) (cvtAF `fmap` f') x
                   ZipWithSeq f f' x y  -> ZipWithSeq (cvtAF f) (cvtAF `fmap` f') x y
                   ScanSeq f e x        -> ScanSeq (cvtF f) (cvtE e) x)
                (cvtSeq s')
            Consumer c ->
              Consumer (cvtC c)
            Reify f ix -> Reify (cvtAF `fmap` f) ix

        cvtC :: Consumer acc aenv senv s -> Consumer acc aenv senv s
        cvtC c =
          case c of
            FoldSeqRegular pre (Alam (Abody acc)) a ->
              FoldSeqRegular pre
                       (Alam $ Abody $ kmap (replaceA (weaken (SuccIdx . seqPreludeShiftReg pre) sh') (weaken (SuccIdx . seqPreludeShiftReg pre) f') (weaken (SuccIdx . seqPreludeShiftReg pre) avar)) acc)
                       (cvtA a)
            FoldSeqFlatten f' f a x -> FoldSeqFlatten (cvtAF `fmap` f') (cvtAF f) (cvtA a) x
            Stuple t                -> Stuple (cvtCT t)

        cvtCT :: Atuple (Consumer acc aenv senv) t -> Atuple (Consumer acc aenv senv) t
        cvtCT NilAtup        = NilAtup
        cvtCT (SnocAtup t c) = SnocAtup (cvtCT t) (cvtC c)

        cvtAF :: PreOpenAfun acc aenv s -> PreOpenAfun acc aenv s
        cvtAF = cvt sh' f' avar
          where
            cvt :: forall aenv a.
                   PreExp acc aenv sh -> PreFun acc aenv (sh -> e) -> Idx aenv (Array sh e)
                -> PreOpenAfun acc aenv a
                -> PreOpenAfun acc aenv a
            cvt sh'' f'' avar' (Abody a) = Abody $ kmap (replaceA sh'' f'' avar') a
            cvt sh'' f'' avar' (Alam af) = Alam $ cvt (weaken SuccIdx sh'')
                                                      (weaken SuccIdx f'')
                                                      (SuccIdx avar')
                                                      af

        cvtE :: PreExp acc aenv s -> PreExp acc aenv s
        cvtE = replaceE sh' f' avar

        cvtF :: PreFun acc aenv s -> PreFun acc aenv s
        cvtF = replaceF sh' f' avar

        cvtAT :: Atuple (acc aenv) s -> Atuple (acc aenv) s
        cvtAT NilAtup          = NilAtup
        cvtAT (SnocAtup tup a) = cvtAT tup `SnocAtup` cvtA a


-- The apply operator, or (>->) in the surface language. This eliminates
-- redundant application to an identity function, instead lifting the argument
-- to a let-binding. This case arises in the use of pipe to avoid fusion and
-- force its argument to be evaluated, e.g.:
--
-- > compute :: Acc a -> Acc a
-- > compute = id >-> id
--
applyD :: (Kit acc, Arrays as, Arrays bs)
       => PreOpenAfun acc aenv (as -> bs)
       ->             acc aenv as
       -> Embed       acc aenv bs
applyD afun x
  | Alam (Abody body)   <- afun
  , Avar ZeroIdx        <- extract body
  = Stats.ruleFired "applyD/identity"
  $ done $ extract x

  | otherwise
  = done $ Apply afun x


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
       => EmbedAcc acc
       -> PreExp   acc aenv Bool
       ->          acc aenv arrs
       ->          acc aenv arrs
       -> Embed    acc aenv arrs
acondD embedAcc p t e
  | Const True  <- p        = Stats.knownBranch "True"      $ embedAcc t
  | Const False <- p        = Stats.knownBranch "False"     $ embedAcc e
  | Just REFL <- match t e  = Stats.knownBranch "redundant" $ embedAcc e
  | otherwise               = done $ Acond p (computeAcc (embedAcc t))
                                             (computeAcc (embedAcc e))


-- Array tuple projection. Whenever possible we want to peek underneath the
-- tuple structure and continue the fusion process.
--
aprjD :: forall acc aenv arrs a. (Kit acc, IsAtuple arrs, Arrays arrs, Arrays a)
      => EmbedAcc acc
      -> TupleIdx (TupleRepr arrs) a
      ->       acc aenv arrs
      -> Embed acc aenv a
aprjD embedAcc ix a
  | Atuple tup <- extract a = Stats.ruleFired "aprj/Atuple" . embedAcc $ aprjAT ix tup
  | otherwise               = done $ Aprj ix (cvtA a)
  where
    cvtA :: acc aenv arrs -> acc aenv arrs
    cvtA = computeAcc . embedAcc

    aprjAT :: TupleIdx atup a -> Atuple (acc aenv) atup -> acc aenv a
    aprjAT ZeroTupIdx      (SnocAtup _ a) = a
    aprjAT (SuccTupIdx ix) (SnocAtup t _) = aprjAT ix t

-- A sequence with additional bindings
data ExtendSeq acc aenv senv arrs where
  ExtendSeq :: forall acc aenv aenv' senv arrs.
                Extend acc aenv aenv'
             -> PreOpenSeq acc aenv' senv arrs
             -> ExtendSeq acc aenv senv arrs

-- A producer with additional bindings
data ExtendProducer acc aenv senv arrs where
  ExtendProducer :: forall acc aenv aenv' senv arrs.
                    Extend acc aenv aenv'
                 -> Producer acc aenv' senv arrs
                 -> ExtendProducer acc aenv senv arrs

-- Scalar expressions
-- ------------------

isIdentity :: PreFun acc aenv (a -> b) -> Maybe (a :=: b)
isIdentity f
  | Lam (Body (Var ZeroIdx)) <- f       = Just REFL
  | otherwise                           = Nothing

identity :: Elt a => PreOpenFun acc env aenv (a -> a)
identity = Lam (Body (Var ZeroIdx))

toIndex :: (Kit acc, Shape sh) => PreOpenExp acc env aenv sh -> PreOpenFun acc env aenv (sh -> Int)
toIndex sh = Lam (Body (ToIndex (weakenE SuccIdx sh) (Var ZeroIdx)))

fromIndex :: (Kit acc, Shape sh) => PreOpenExp acc env aenv sh -> PreOpenFun acc env aenv (Int -> sh)
fromIndex sh = Lam (Body (FromIndex (weakenE SuccIdx sh) (Var ZeroIdx)))

reindex :: (Kit acc, Shape sh, Shape sh')
        => PreOpenExp acc env aenv sh'
        -> PreOpenExp acc env aenv sh
        -> PreOpenFun acc env aenv (sh -> sh')
reindex sh' sh
  | Just REFL <- match sh sh'   = identity
  | otherwise                   = fromIndex sh' `compose` toIndex sh

extend :: (Kit acc, Shape sh, Shape sl, Elt slix)
       => SliceIndex (EltRepr slix) (EltRepr sl) co (EltRepr sh)
       -> PreExp acc aenv slix
       -> PreFun acc aenv (sh -> sl)
extend sliceIndex slix = Lam (Body (IndexSlice sliceIndex (weakenE SuccIdx slix) (Var ZeroIdx)))

restrict :: (Kit acc, Shape sh, Shape sl, Elt slix)
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

simpleExp :: PreOpenExp acc env aenv t -> Bool
simpleExp e =
  case e of
    Let x y            -> simpleExp x && simpleExp y
    Var _              -> True
    Const _            -> True
    Tuple t            -> simpleTuple t
    Prj _ e            -> simpleExp e
    IndexNil           -> True
    IndexCons sl sz    -> simpleExp sl && simpleExp sz
    IndexHead sh       -> simpleExp sh
    IndexTail sz       -> simpleExp sz
    IndexTrans sz      -> simpleExp sz
    IndexAny           -> True
    IndexSlice _ ix sh -> simpleExp ix && simpleExp sh
    IndexFull _ ix sl  -> simpleExp ix && simpleExp sl
    ToIndex sh ix      -> simpleExp sh && simpleExp ix
    FromIndex sh i     -> simpleExp sh && simpleExp i
    PrimConst _        -> True
    PrimApp _ x        -> simpleExp x
    ShapeSize sh       -> simpleExp sh
    Intersect sh sl    -> simpleExp sh && simpleExp sl
    Union s t          -> simpleExp s && simpleExp t
    Shape _            -> True
    Index _ IndexNil   -> True
    _                  -> False

simpleTuple :: Tuple (PreOpenExp acc env aenv) t -> Bool
simpleTuple NilTup        = True
simpleTuple (SnocTup t e) = simpleTuple t && simpleExp e
