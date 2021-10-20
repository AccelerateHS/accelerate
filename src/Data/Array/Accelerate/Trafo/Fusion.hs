{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternGuards        #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE ViewPatterns         #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo.Fusion
-- Copyright   : [2012..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
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

  convertAcc,  convertAccWith,
  convertAfun, convertAfunWith,

) where

import Data.BitSet
import Data.Array.Accelerate.Annotations
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.AST.LeftHandSide
import Data.Array.Accelerate.AST.Environment
import Data.Array.Accelerate.AST.Var
import Data.Array.Accelerate.AST.Idx
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Trafo.Config
import Data.Array.Accelerate.Trafo.Var
import Data.Array.Accelerate.Trafo.Delayed
import Data.Array.Accelerate.Trafo.Environment
import Data.Array.Accelerate.Trafo.Shrink
import Data.Array.Accelerate.Trafo.Simplify
import Data.Array.Accelerate.Trafo.Substitution
import Data.Array.Accelerate.Representation.Array                   ( Array, ArrayR(..), ArraysR )
import Data.Array.Accelerate.Representation.Shape                   ( ShapeR(..), shapeType )
import Data.Array.Accelerate.Representation.Slice
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.Debug.Internal.Flags                   ( array_fusion )
import qualified Data.Array.Accelerate.Debug.Internal.Stats         as Stats
#ifdef ACCELERATE_DEBUG
import System.IO.Unsafe -- for debugging
#endif

import Data.Function
import Lens.Micro                                                 ( over, mapped, _2 )
import Prelude                                                      hiding ( exp, until )


-- Delayed Array Fusion
-- ====================

-- | Apply the fusion transformation to a closed de Bruijn AST
--
convertAcc :: HasCallStack => Acc arrs -> DelayedAcc arrs
convertAcc = convertAccWith defaultOptions

convertAccWith :: HasCallStack => Config -> Acc arrs -> DelayedAcc arrs
convertAccWith config = withSimplStats . convertOpenAcc config

-- | Apply the fusion transformation to a function of array arguments
--
convertAfun :: HasCallStack => Afun f -> DelayedAfun f
convertAfun = convertAfunWith defaultOptions

convertAfunWith :: HasCallStack => Config -> Afun f -> DelayedAfun f
convertAfunWith config = withSimplStats . convertOpenAfun config

-- -- | Apply the fusion transformation to the array computations embedded
-- --   in a sequence computation.
--
-- convertSeq :: Bool -> Seq a -> DelayedSeq a
-- convertSeq fuseAcc (embedSeq (embedOpenAcc fuseAcc) -> ExtendSeq aenv s)
--   = withSimplStats (DelayedSeq (cvtE aenv) (convertOpenSeq fuseAcc s))
--   where
--     cvtE :: Extend OpenAcc aenv aenv' -> Extend DelayedOpenAcc aenv aenv'
--     cvtE BaseEnv                                          = BaseEnv
--     cvtE (PushEnv env a) | a' <- convertOpenAcc fuseAcc a = PushEnv (cvtE env) a'

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
convertOpenAcc
    :: HasCallStack
    => Config
    -> OpenAcc aenv arrs
    -> DelayedOpenAcc aenv arrs
convertOpenAcc config = manifest config . computeAcc . embedOpenAcc config


-- Convert array computations into an embeddable delayed representation.
-- Reapply the embedding function from the first pass and unpack the
-- representation. It is safe to match on BaseEnv because the first pass
-- will put producers adjacent to the term consuming it.
--
delayed
    :: HasCallStack
    => Config
    -> OpenAcc aenv (Array sh e)
    -> DelayedOpenAcc aenv (Array sh e)
delayed config (embedOpenAcc config -> Embed env cc)
  | BaseEnv <- env
  = simplify cc & \case
      Left (Done _ v)                                             -> avarsIn Manifest v
      Right d                                                     -> d & \case
        Yield ann aR sh f                                         -> Delayed ann aR sh f                              (f `compose` fromIndex ann (arrayRshape aR) sh)
        Step  ann aR sh p f v
          | Just Refl <- matchOpenExp sh (arrayShape ann v)
          , Just Refl <- isIdentity p                             -> Delayed ann aR sh (f `compose` indexArray ann v) (f `compose` linearIndex ann v)
          | f'        <- f `compose` indexArray ann v `compose` p -> Delayed ann aR sh f'                             (f' `compose` fromIndex ann (arrayRshape aR) sh)
  --
  | otherwise
  = manifest config (computeAcc (Embed env cc))


-- Convert array programs as manifest terms.
--
manifest
    :: HasCallStack
    => Config
    -> OpenAcc aenv a
    -> DelayedOpenAcc aenv a
manifest config (OpenAcc pacc) =
  let fusionError = internalError "unexpected fusible materials"
  in
  Manifest $ case pacc of
    -- Non-fusible terms
    -- -----------------
    Avar ix                     -> Avar ix
    Use ann aR a                -> Use ann aR a
    Unit ann t e                -> Unit ann t e
    Alet ann lhs bnd body       -> alet ann lhs (manifest config bnd) (manifest config body)
    Acond ann p t e             -> Acond ann p (manifest config t) (manifest config e)
    Awhile ann p f a            -> Awhile ann (cvtAF p) (cvtAF f) (manifest config a)
    Apair ann a1 a2             -> Apair ann (manifest config a1) (manifest config a2)
    Anil ann                    -> Anil ann
    Atrace ann msg a1 a2        -> Atrace ann msg (manifest config a1) (manifest config a2)
    Apply ann repr f a          -> apply ann repr (cvtAF f) (manifest config a)
    Aforeign ann repr ff f a    -> Aforeign ann repr ff (cvtAF f) (manifest config a)

    -- Producers
    -- ---------
    --
    -- Some producers might still exist as a manifest array. Typically this
    -- is because they are the last stage of the computation, or the result
    -- of a let-binding to be used multiple times. The input array here
    -- should be a evaluated array term, else something went wrong.
    --
    Map ann t f a               -> Map ann t f (delayed config a)
    Generate ann repr sh f      -> Generate ann repr sh f
    Transform ann repr sh p f a -> Transform ann repr sh p f (delayed config a)
    Backpermute ann shR sh p a  -> Backpermute ann shR sh p (delayed config a)
    Reshape ann slr sl a        -> Reshape ann slr sl (manifest config a)

    Replicate{}                 -> fusionError
    Slice{}                     -> fusionError
    ZipWith{}                   -> fusionError

    -- Consumers
    -- ---------
    --
    -- Embed producers directly into the representation. For delayed terms
    -- with local bindings, these will have been floated up above the
    -- consumer already
    --
    Fold ann f z a              -> Fold     ann f z (delayed config a)
    FoldSeg ann i f z a s       -> FoldSeg  ann i f z (delayed config a) (delayed config s)
    Scan  ann d f z a           -> Scan     ann d f z (delayed config a)
    Scan' ann d f z a           -> Scan'    ann d f z (delayed config a)
    Permute ann f d p a         -> Permute  ann f (manifest config d) p (delayed config a)
    Stencil ann s t f x a       -> Stencil  ann s t f x (delayed config a)
    Stencil2 ann s1 s2 t f x a y b
                                -> Stencil2 ann s1 s2 t f x (delayed config a) y (delayed config b)
    -- Collect s                -> Collect  (cvtS s)

    where
      -- Flatten needless let-binds, which can be introduced by the
      -- conversion to the internal embeddable representation.
      --
      alet :: HasCallStack
           => Ann
           -> ALeftHandSide a aenv aenv'
           -> DelayedOpenAcc aenv a
           -> DelayedOpenAcc aenv' b
           -> PreOpenAcc DelayedOpenAcc aenv b
      alet ann lhs bnd body
        | Just bodyVars  <- extractDelayedArrayVars body
        , Just Refl      <- bindingIsTrivial lhs bodyVars
        , Manifest x     <- bnd
        = modifyAnn (<> ann) x
        --
        | otherwise
        = Alet ann lhs bnd body

      -- Eliminate redundant application to an identity function. This
      -- arises in the use of pipe to avoid fusion and force its argument
      -- to be evaluated, i.e.:
      --
      -- > compute :: Acc a -> Acc a
      -- > compute = id >-> id
      --
      apply :: HasCallStack
            => Ann
            -> ArraysR b
            -> PreOpenAfun DelayedOpenAcc aenv (a -> b)
            ->             DelayedOpenAcc aenv a
            -> PreOpenAcc  DelayedOpenAcc aenv b
      apply ann repr afun x
        | Alam lhs (Abody body)   <- afun
        , Just bodyVars           <- extractDelayedArrayVars body
        , Just Refl               <- bindingIsTrivial lhs bodyVars
        , Manifest x'             <- x
        = Stats.ruleFired "applyD/identity" $ modifyAnn (<> ann) x'
        --
        | otherwise
        = Apply ann repr afun x

      cvtAF :: HasCallStack => OpenAfun aenv f -> PreOpenAfun DelayedOpenAcc aenv f
      cvtAF (Alam lhs f) = Alam lhs (cvtAF f)
      cvtAF (Abody b)    = Abody (manifest config b)

      -- cvtS :: PreOpenSeq OpenAcc aenv senv s -> PreOpenSeq DelayedOpenAcc aenv senv s
      -- cvtS = convertOpenSeq config

convertOpenAfun :: HasCallStack => Config -> OpenAfun aenv f -> DelayedOpenAfun aenv f
convertOpenAfun c (Alam lhs f) = Alam lhs (convertOpenAfun c f)
convertOpenAfun c (Abody b) = Abody (convertOpenAcc  c b)

{--
convertOpenSeq :: Config -> PreOpenSeq OpenAcc aenv senv a -> PreOpenSeq DelayedOpenAcc aenv senv a
convertOpenSeq config s =
  case s of
    Consumer c          -> Consumer (cvtC c)
    Reify ix            -> Reify ix
    Producer p s'       -> Producer p' (convertOpenSeq config s')
      where
        p' = case p of
               StreamIn arrs     -> StreamIn arrs
               ToSeq slix sh a   -> ToSeq slix sh (delayed config a)
               MapSeq f x        -> MapSeq (cvtAF f) x
               ChunkedMapSeq f x -> ChunkedMapSeq (cvtAF f) x
               ZipWithSeq f x y  -> ZipWithSeq (cvtAF f) x y
               ScanSeq f e x     -> ScanSeq (cvtF f) (cvtE e) x
  where
    cvtC :: Consumer OpenAcc aenv senv a -> Consumer DelayedOpenAcc aenv senv a
    cvtC c =
      case c of
        FoldSeq f e x        -> FoldSeq (cvtF f) (cvtE e) x
        FoldSeqFlatten f a x -> FoldSeqFlatten (cvtAF f) (manifest config a) x
        Stuple t             -> Stuple (cvtCT t)

    cvtCT :: Atuple (Consumer OpenAcc aenv senv) t -> Atuple (Consumer DelayedOpenAcc aenv senv) t
    cvtCT NilAtup        = NilAtup
    cvtCT (SnocAtup t c) = SnocAtup (cvtCT t) (cvtC c)

    cvtAF :: OpenAfun aenv f -> PreOpenAfun DelayedOpenAcc aenv f
    cvtAF (Alam f)  = Alam  (cvtAF f)
    cvtAF (Abody b) = Abody (manifest config b)

    cvtE :: OpenExp env aenv t -> DelayedOpenExp env aenv t
    cvtE = convertOpenExp config

    cvtF :: OpenFun env aenv f -> DelayedOpenFun env aenv f
    cvtF (Lam f)  = Lam (cvtF f)
    cvtF (Body b) = Body (cvtE b)
--}


type EmbedAcc acc = forall aenv arrs. acc aenv arrs -> Embed acc aenv arrs
type ElimAcc  acc = forall aenv s t. acc aenv s -> acc (aenv,s) t -> Bool


-- | Apply the fusion transformation to the AST to combine and simplify terms.
-- This converts terms into the internal delayed array representation and merges
-- adjacent producer/producer terms. Using the reduced internal form limits the
-- number of combinations that need to be considered.
--
embedOpenAcc :: HasCallStack => Config -> OpenAcc aenv arrs -> Embed OpenAcc aenv arrs
embedOpenAcc config (OpenAcc pacc) =
  embedPreOpenAcc config matchOpenAcc (embedOpenAcc config) elimOpenAcc pacc
  where
    -- When does the cost of re-computation outweigh that of memory access? For
    -- the moment only do the substitution on a single use of the bound array
    -- into the use site, but it is likely advantageous to be far more
    -- aggressive here.
    --
    -- SEE: [Sharing vs. Fusion]
    --
    elimOpenAcc :: ElimAcc OpenAcc
    elimOpenAcc _bnd body
      | count False ZeroIdx body <= lIMIT = True
      | otherwise                         = False
      where
        lIMIT = 1

        count :: UsesOfAcc OpenAcc
        count no ix (OpenAcc pacc) = usesOfPreAcc no count ix pacc

    matchOpenAcc :: MatchAcc OpenAcc
    matchOpenAcc (OpenAcc pacc1) (OpenAcc pacc2) =
      matchPreOpenAcc matchOpenAcc pacc1 pacc2


embedPreOpenAcc
    :: HasCallStack
    => Config
    -> MatchAcc   OpenAcc
    -> EmbedAcc   OpenAcc
    -> ElimAcc    OpenAcc
    -> PreOpenAcc OpenAcc aenv arrs
    -> Embed      OpenAcc aenv arrs
embedPreOpenAcc config matchAcc embedAcc elimAcc pacc
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
    -- FIXME: The annotation about the let binding site is thrown away here. Is
    --        there some way we can still use this? Do we lose any information
    --        this way?
    --
    Alet _ lhs bnd body            -> aletD embedAcc elimAcc lhs bnd body
    Anil ann                       -> done $ Anil ann
    Acond ann p at ae              -> acondD ann matchAcc embedAcc (cvtE p) at ae
    Apply ann aR f a               -> done $ Apply ann aR (cvtAF f) (cvtA a)
    Awhile ann p f a               -> done $ Awhile ann (cvtAF p) (cvtAF f) (cvtA a)
    Apair ann a1 a2                -> done $ Apair ann (cvtA a1) (cvtA a2)
    Atrace ann msg a1 a2           -> done $ Atrace ann msg (cvtA a1) (cvtA a2)
    Aforeign ann aR ff f a         -> done $ Aforeign ann aR ff (cvtAF f) (cvtA a)
    -- Collect s                   -> collectD s

    -- Array injection
    Avar v                         -> done $ Avar v
    Use ann aR a                   -> done $ Use ann aR a
    Unit ann t e                   -> done $ Unit ann t (cvtE e)

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
    Generate ann aR sh f           -> generateD ann aR (cvtE sh) (cvtF f)
    Map ann t f a                  -> mapD ann t (cvtF f) (embedAcc a)
    ZipWith ann t f a b            -> fuse2 (into (zipWithD ann t) (cvtF f)) a b
    Transform ann aR sh p f a      -> transformD ann aR (cvtE sh) (cvtF p) (cvtF f) (embedAcc a)
    Backpermute ann slr sl p a     -> fuse (into2 (backpermuteD ann slr) (cvtE sl) (cvtF p)) a
    Slice ann slix a sl            -> fuse (into  (sliceD ann slix)      (cvtE sl)) a
    Replicate ann slix sh a        -> fuse (into  (replicateD ann slix)  (cvtE sh)) a
    Reshape ann slr sl a           -> reshapeD ann slr (embedAcc a) (cvtE sl)

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
    Fold ann f z a                 -> embed  aR (into2M (Fold ann)             (cvtF f) (cvtE <$> z)) a
    FoldSeg ann i f z a s          -> embed2 aR (into2M (FoldSeg ann i)        (cvtF f) (cvtE <$> z)) a s
    Scan  ann d f z a              -> embed  aR (into2M (Scan  ann d)          (cvtF f) (cvtE <$> z)) a
    Scan' ann d f z a              -> embed  aR (into2  (Scan' ann d)          (cvtF f) (cvtE z)) a
    Permute ann f d p a            -> embed2 aR (into2  (permute ann)          (cvtF f) (cvtF p)) d a
    Stencil ann s t f x a          -> embed  aR (into2  (stencil1 ann s t)     (cvtF f) (cvtB x)) a
    Stencil2 ann s1 s2 t f x a y b -> embed2 aR (into3  (stencil2 ann s1 s2 t) (cvtF f) (cvtB x) (cvtB y)) a b

  where
    aR = arraysR pacc

    -- If fusion is not enabled, force terms to the manifest representation
    --
    unembed :: HasCallStack => Embed OpenAcc aenv arrs -> Embed OpenAcc aenv arrs
    unembed x
      | array_fusion `member` options config = x
      | Embed env cc <- x
      , pacc         <- compute cc
      = case avarsOut extractOpenAcc pacc of
          Just vars -> Embed env $ Done (extractAnn pacc) vars
          _
            | DeclareVars lhs _ value <- declareVars (annR (OpenAcc pacc) $ arraysR pacc) (arraysR pacc)
              -> Embed (PushEnv env lhs $ OpenAcc pacc) $ Done (extractAnn pacc) $ value weakenId

    cvtA :: HasCallStack => OpenAcc aenv' a -> OpenAcc aenv' a
    cvtA = computeAcc . embedAcc

    cvtAF :: HasCallStack => PreOpenAfun OpenAcc aenv' f -> PreOpenAfun OpenAcc aenv' f
    cvtAF (Alam lhs f) = Alam lhs (cvtAF f)
    cvtAF (Abody a)    = Abody (cvtA a)

    -- Helpers to shuffle the order of arguments to a constructor
    --
    permute ann f p d a     = Permute ann f d p a

    -- NOTE: [Stencil fusion]
    --
    -- We allow stencils to delay their argument arrays with no special
    -- considerations. This means that the delayed function will be evaluated
    -- _at every element_ of the stencil pattern. We should do some analysis of
    -- when this duplication is beneficial (keeping in mind that the stencil
    -- implementations themselves may share neighbouring elements).
    --
    stencil1 ann s t f x a          = Stencil  ann s     t f x a
    stencil2 ann s1 s2 t f x y a b  = Stencil2 ann s1 s2 t f x a y b

    -- Conversions for closed scalar functions and expressions. This just
    -- applies scalar simplifications.
    --
    cvtF :: HasCallStack => Fun aenv' t -> Fun aenv' t
    cvtF = simplifyFun

    cvtE :: HasCallStack => Exp aenv' t -> Exp aenv' t
    cvtE = simplifyExp

    cvtB :: HasCallStack => Boundary aenv' t -> Boundary aenv' t
    cvtB Clamp        = Clamp
    cvtB Mirror       = Mirror
    cvtB Wrap         = Wrap
    cvtB (Constant c) = Constant c
    cvtB (Function f) = Function (cvtF f)

    -- Helpers to embed and fuse delayed terms
    --
    into :: (HasCallStack, Sink f)
         => (f env' a -> b)
         -> f env a
         -> Extend ArrayR OpenAcc env env'
         -> b
    into op a env = op (sinkA env a)

    into2 :: (HasCallStack, Sink f1, Sink f2)
          => (f1 env' a -> f2 env' b -> c)
          -> f1 env a
          -> f2 env b
          -> Extend ArrayR OpenAcc env env'
          -> c
    into2 op a b env = op (sinkA env a) (sinkA env b)

    into2M :: (HasCallStack, Sink f1, Sink f2)
           => (f1 env' a -> Maybe (f2 env' b) -> c)
           -> f1 env a
           -> Maybe (f2 env b)
           -> Extend ArrayR acc env env'
           -> c
    into2M op a b env = op (sinkA env a) (sinkA env <$> b)

    into3 :: (HasCallStack, Sink f1, Sink f2, Sink f3)
          => (f1 env' a -> f2 env' b -> f3 env' c -> d)
          -> f1 env a
          -> f2 env b
          -> f3 env c
          -> Extend ArrayR OpenAcc env env'
          -> d
    into3 op a b c env = op (sinkA env a) (sinkA env b) (sinkA env c)

    -- Operations which can be fused into consumers. Move all of the local
    -- bindings out of the way so that the fusible function operates
    -- directly on the delayed representation. See also: [Representing
    -- delayed arrays]
    --
    fuse :: HasCallStack
         => (forall r aenv'. Extend ArrayR OpenAcc aenv aenv' -> Cunctation r aenv' as -> Cunctation D aenv' bs)
         ->       OpenAcc aenv as
         -> Embed OpenAcc aenv bs
    fuse op (embedAcc -> Embed env cc) = Embed env (op env cc)

    fuse2 :: HasCallStack
          => (forall r s aenv'. Extend ArrayR OpenAcc aenv aenv' -> Cunctation r aenv' as -> Cunctation s aenv' bs -> Cunctation D aenv' cs)
          ->       OpenAcc aenv as
          ->       OpenAcc aenv bs
          -> Embed OpenAcc aenv cs
    fuse2 op a1 a0
      | Embed env1 cc1  <- embedAcc a1
      , Embed env0 cc0  <- embedAcc (sinkA env1 a0)
      , env             <- env1 `append` env0
      = Embed env (op env (sinkA env0 cc1) cc0)

    -- Consumer operations which will be evaluated.
    --
    -- NOTE: [Fusion and the lowest common use site]
    --
    -- The AST given to us by sharing recovery will place let bindings at
    -- the lowest common use site for that shared term. For example:
    --
    --   fold f z (let a0 = ..
    --                 a1 = ..
    --              in zipWith g a0 a1)
    --
    -- In order to enable producer/consumer fusion for the above example,
    -- it is necessary to float the let bindings above the `fold`
    -- operation; SEE: [Sharing vs. Fusion] for more information.
    --
    -- Furthermore, we used to maintain an invariant that all (manifest)
    -- arguments were supplied as array variables, for example:
    --
    --   fold1 f (let a0 = ..               let a0 = ..
    --             in stencil g a0)   ==>       a1 = stencil g a0
    --                                          a2 = fold1 f a1
    --
    -- However, if the argument term will be evaluated (i.e. can not be
    -- fused into the producer) then it is better that we do _not_ float
    -- those terms, and instead leave them under the consumer. This helps
    -- to syntactically constrain the "liveness" of terms: if the argument
    -- to an operation is not an array variable, we can see directly that
    -- this will be the last use-site of that array. In particular, this is
    -- useful for the 'permute' operation to know when it can in-place
    -- update the array of default values.
    --
    embed :: HasCallStack
          => ArraysR bs
          -> (forall aenv'. Extend ArrayR OpenAcc aenv aenv' -> OpenAcc aenv' as -> PreOpenAcc OpenAcc aenv' bs)
          ->       OpenAcc aenv as
          -> Embed OpenAcc aenv bs
    embed reprBs op acc@(embedAcc -> Embed env cc)
      | Done{} <- cc
      , result <- OpenAcc $ op BaseEnv (computeAcc (Embed env cc))
      , DeclareVars lhs _ value <- declareVars (annR result reprBs) reprBs
      = Embed (PushEnv BaseEnv lhs result) $ Done (extractAnn acc) $ value weakenId
      | otherwise
      , result <- OpenAcc $ op env     (OpenAcc (compute cc))
      -- Next line is duplicated for both branches, as the type variable for the environment is instantiated differently
      , DeclareVars lhs _ value <- declareVars (annR result reprBs) reprBs
      = Embed (PushEnv env     lhs result) $ Done (extractAnn acc) $ value weakenId

    embed2 :: HasCallStack
           => ArraysR cs
           -> (forall aenv'. Extend ArrayR OpenAcc aenv aenv' -> OpenAcc aenv' as -> OpenAcc aenv' bs -> PreOpenAcc OpenAcc aenv' cs)
           ->       OpenAcc aenv as
           ->       OpenAcc aenv bs
           -> Embed OpenAcc aenv cs
    embed2 reprCs op acc@(embedAcc -> Embed env1 cc1) a0
      | Done{}          <- cc1
      , a1              <- computeAcc (Embed env1 cc1)
      = embed reprCs (\env0 -> op env0 (sinkA env0 a1)) a0
      --
      | Embed env0 cc0  <- embedAcc (sinkA env1 a0)
      , env             <- env1 `append` env0
      = case cc0 of
          Done{}
            | result <- OpenAcc $ op env1 (OpenAcc (compute cc1)) (computeAcc (Embed env0 cc0))
            , DeclareVars lhs _ value <- declareVars (annR result reprCs) reprCs
              -> Embed (PushEnv env1 lhs result)
                       $ Done (extractAnn acc)
                       $ value weakenId
          _
            -- Next line is duplicated for both branches, as the type
            -- variable for the environment is instantiated differently
            | result <- OpenAcc $ op env  (OpenAcc (compute (sinkA env0 cc1))) (OpenAcc (compute cc0))
            , DeclareVars lhs _ value <- declareVars (annR result reprCs) reprCs
              -> Embed (PushEnv env  lhs result)
                       $ Done (extractAnn acc)
                       $ value weakenId

    -- trav1 :: (Arrays as, Arrays bs)
    --       => (forall aenv'. Embed acc aenv' as -> Embed acc aenv' as)
    --       -> (forall aenv'. Extend ArrayR acc aenv aenv' -> acc aenv' as -> PreOpenAcc acc aenv' bs)
    --       ->       acc aenv as
    --       -> Embed acc aenv bs
    -- trav1 f op (f . embedAcc -> Embed env cc)
    --   = Embed (env `pushArrayEnv` inject (op env (inject (compute cc)))) doneZeroIdx

    -- trav2 :: (Arrays as, Arrays bs, Arrays cs)
    --       => (forall aenv'. Embed acc aenv' as -> Embed acc aenv' as)
    --       -> (forall aenv'. Embed acc aenv' bs -> Embed acc aenv' bs)
    --       -> (forall aenv'. Extend ArrayR acc aenv aenv' -> acc aenv' as -> acc aenv' bs -> PreOpenAcc acc aenv' cs)
    --       ->       acc aenv as
    --       ->       acc aenv bs
    --       -> Embed acc aenv cs
    -- trav2 f1 f0 op (f1 . embedAcc -> Embed env1 cc1) (f0 . embedAcc . sinkA env1 -> Embed env0 cc0)
    --   | env     <- env1 `append` env0
    --   , acc1    <- inject . compute $ sinkA env0 cc1
    --   , acc0    <- inject . compute $ cc0
    --   = Embed (env `pushArrayEnv` inject (op env acc1 acc0)) doneZeroIdx

    -- force :: Arrays as => Embed acc aenv' as -> Embed acc aenv' as
    -- force (Embed env cc)
    --   | Done{} <- cc = Embed env                                  cc
    --   | otherwise    = Embed (env `pushArrayEnv` inject (compute cc)) doneZeroIdx

    -- -- Move additional bindings for producers outside of the sequence, so that
    -- -- producers may fuse with their arguments resulting in actual sequencing
    -- collectD :: PreOpenSeq acc aenv () arrs
    --          -> Embed acc aenv arrs
    -- collectD (embedSeq embedAcc -> ExtendSeq env s')
    --   = Embed (env `pushArrayEnv` inject (Collect s')) doneZeroIdx


{--
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
        Reify ix
          -> ExtendSeq env (Reify ix)

    travP :: forall arrs' aenv' senv.
             Producer acc aenv senv arrs'
          -> Extend acc aenv aenv'
          -> ExtendProducer acc aenv' senv arrs'
    travP (ToSeq slix sh a) env
      | Embed env' cc <- embedAcc (sink env a)
      = ExtendProducer env' (ToSeq slix sh (inject (compute cc)))
    travP (StreamIn arrs) _          = ExtendProducer BaseEnv (StreamIn arrs)
    travP (MapSeq f x) env           = ExtendProducer BaseEnv (MapSeq (cvtAF (sink env f)) x)
    travP (ChunkedMapSeq f x) env    = ExtendProducer BaseEnv (ChunkedMapSeq (cvtAF (sink env f)) x)
    travP (ZipWithSeq f x y) env     = ExtendProducer BaseEnv (ZipWithSeq (cvtAF (sink env f)) x y)
    travP (ScanSeq f e x) env        = ExtendProducer BaseEnv (ScanSeq (cvtF (sink env f)) (cvtE (sink env e)) x)

    travC :: forall arrs' aenv' senv.
             Consumer acc aenv senv arrs'
          -> Extend acc aenv aenv'
          -> Consumer acc aenv' senv arrs'
    travC (FoldSeq f e x) env = FoldSeq (cvtF (sink env f)) (cvtE (sink env e)) x
    travC (FoldSeqFlatten f a x) env = FoldSeqFlatten (cvtAF (sink env f)) (cvtA (sink env a)) x
    travC (Stuple t) env = Stuple (cvtCT t)
      where
        cvtCT :: Atuple (Consumer acc aenv senv) t -> Atuple (Consumer acc aenv' senv) t
        cvtCT NilAtup        = NilAtup
        cvtCT (SnocAtup t c) = SnocAtup (cvtCT t) (travC c env)

    cvtE :: Elt t => Exp aenv' t -> Exp aenv' t
    cvtE = simplifyExp

    cvtF :: Fun aenv' t -> Fun aenv' t
    cvtF = simplifyFun

    cvtA :: Arrays a => acc aenv' a -> acc aenv' a
    cvtA = computeAcc . embedAcc

    cvtAF :: PreOpenAfun acc aenv' f -> PreOpenAfun acc aenv' f
    cvtAF (Alam  f) = Alam  (cvtAF f)
    cvtAF (Abody a) = Abody (cvtA a)


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
--}


-- Internal representation
-- =======================

-- NOTE: [Representing delayed arrays]
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
  Embed :: Extend ArrayR acc aenv aenv'
        -> Cunctation r           aenv' a
        -> Embed         acc aenv       a

instance HasArraysR acc => HasArraysR (Embed acc) where
  arraysR (Embed _ c) = arraysR c


-- Delayed arrays are represented as functions
data D

-- Manifest arrays are real arrays backed by unboxed vectors
data M

-- Cunctation (n): the action or an instance of delaying; a tardy action.
--
-- This describes the ways in which the fusion transformation represents
-- intermediate arrays. The fusion process operates by recasting producer array
-- computations in terms of a set of scalar functions used to construct an
-- element at each index, and fusing successive producers by combining these
-- scalar functions.
--
-- TODO: For the delayed representation, can we just combine annotations as we
--       go? How does this interact with things like loop unrolling?
--
data Cunctation r aenv a where

  -- The base case is just a real (manifest) array term. No fusion happens here.
  -- Note that the array is referenced by an index into the extended
  -- environment, ensuring that the array is manifest and making the term
  -- non-recursive in 'acc'.
  --
  -- TODO: In a lot of situations we throw away this annotation when fusing
  --       manifest arrays into other operations. Double check that this is
  --       correct.
  --
  Done  :: Ann
        -> ArrayVars    aenv arrs
        -> Cunctation M aenv arrs

  -- We can represent an array by its shape and a function to compute an element
  -- at each index.
  --
  Yield :: Ann
        -> ArrayR (Array sh e)
        -> Exp          aenv sh
        -> Fun          aenv (sh -> e)
        -> Cunctation D aenv (Array sh e)

  -- A more restrictive form than 'Yield' may afford greater opportunities for
  -- optimisation by a backend. This more structured form applies an index and
  -- value transform to an input array. Note that the transform is applied to an
  -- array stored as an environment index, so that the term is non-recursive and
  -- it is always possible to embed into a collective operation.
  --
  Step  :: Ann
        -> ArrayR (Array sh' b)
        -> Exp          aenv sh'
        -> Fun          aenv (sh' -> sh)
        -> Fun          aenv (a   -> b)
        -> ArrayVar     aenv (Array sh  a)
        -> Cunctation D aenv (Array sh' b)

instance HasArraysR (Cunctation r) where
  arraysR (Done _ v)          = varsType v
  arraysR (Yield _ aR _ _)    = TupRsingle aR
  arraysR (Step _ aR _ _ _ _) = TupRsingle aR

instance Sink (Cunctation r) where
  weaken k = \case
    Done ann v            -> Done  ann (weakenVars k v)
    Step ann aR sh p f v  -> Step  ann aR (weaken k sh) (weaken k p) (weaken k f) (weaken k v)
    Yield ann aR sh f     -> Yield ann aR (weaken k sh) (weaken k f)

simplify
    :: HasCallStack
    => Cunctation r aenv a
    -> Either (Cunctation M aenv a) (Cunctation D aenv a)
simplify = \case
  Done ann v
    -> Left  $ Done ann v
  Yield ann aR (simplifyExp -> sh) (simplifyFun -> f)
    -> Right $ Yield ann aR sh f
  Step ann aR (simplifyExp -> sh) (simplifyFun -> p) (simplifyFun -> f) v
    | Just Refl <- matchOpenExp sh (arrayShape ann v)
    , Just Refl <- isIdentity p
    , Just Refl <- isIdentity f
    -> Left  $ (Done ann (TupRsingle v))
    | otherwise
    -> Right $ Step ann aR sh p f v


-- Convert a real AST node into the internal representation
--
done :: HasCallStack => PreOpenAcc OpenAcc aenv a -> Embed OpenAcc aenv a
done pacc
  | Just vars <- avarsOut extractOpenAcc pacc
  = Embed BaseEnv (Done (extractAnn pacc) vars)
  | DeclareVars lhs _ value <- declareVars (annR (OpenAcc pacc) $ arraysR pacc) (arraysR pacc)
  = Embed (PushEnv BaseEnv lhs $ OpenAcc pacc) $ Done (extractAnn pacc) $ value weakenId

doneZeroIdx :: Ann -> ArrayR (Array sh e) -> Cunctation M (aenv, Array sh e) (Array sh e)
doneZeroIdx ann aR = Done ann (TupRsingle (Var ann aR ZeroIdx))


-- Recast a cunctation into a mapping from indices to elements
--
yield :: HasCallStack
      => Cunctation r aenv (Array sh e)
      -> Cunctation D aenv (Array sh e)
yield cc =
  case cc of
    Yield{}                              -> cc
    Step ann tR sh p f v                 -> Yield ann tR sh (f `compose` indexArray ann v `compose` p)
    Done ann (TupRsingle v@(Var _ tR _)) -> Yield ann tR (arrayShape ann v) (indexArray ann v)

-- Recast a cunctation into a delayed representation
--
delaying
    :: HasCallStack
    => Cunctation r aenv (Array sh e)
    -> Cunctation D aenv (Array sh e)
delaying cc =
  case cc of
    Yield{} -> cc
    Step{}  -> cc
    Done ann u
      | TupRsingle v  <- u
      , Var _ aR _    <- v
      , ArrayR shR tR <- aR
     -> Step ann aR (arrayShape ann v) (identity ann (shapeType shR)) (identity ann tR) v

-- Get the shape of a delayed array
--
shape :: HasCallStack => Cunctation r aenv (Array sh e) -> Exp aenv sh
shape cc =
  case delaying cc of
    Step  _ _ sh _ _ _ -> sh
    Yield _ _ sh _     -> sh


-- prjExtend :: Kit acc => Extend acc env env' -> Idx env' t -> PreOpenAcc acc env' t
-- prjExtend (PushEnv _   v) ZeroIdx       = weakenA rebuildAcc SuccIdx v
-- prjExtend (PushEnv env _) (SuccIdx idx) = weakenA rebuildAcc SuccIdx $ prjExtend env idx
-- prjExtend _               _             = $internalError "prjExtend" "inconsistent valuation"

{--
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
      Reify ix      -> Reify      ix

    where
      weakenL :: forall senv' arrs'. PreOpenSeq acc aenv senv' arrs' -> PreOpenSeq acc aenv' senv' arrs'
      weakenL s' = unSinkSeq (weaken k (SinkSeq s'))

      weakenP :: forall a. Producer acc aenv senv a -> Producer acc aenv' senv a
      weakenP p =
        case p of
          StreamIn arrs        -> StreamIn arrs
          ToSeq slix sh a      -> ToSeq slix sh (weaken k a)
          MapSeq f x           -> MapSeq (weaken k f) x
          ChunkedMapSeq f x    -> ChunkedMapSeq (weaken k f) x
          ZipWithSeq f x y     -> ZipWithSeq (weaken k f) x y
          ScanSeq f a x        -> ScanSeq (weaken k f) (weaken k a) x

      weakenC :: forall a. Consumer acc aenv senv a -> Consumer acc aenv' senv a
      weakenC c =
        case c of
          FoldSeq f a x        -> FoldSeq (weaken k f) (weaken k a) x
          FoldSeqFlatten f a x -> FoldSeqFlatten (weaken k f) (weaken k a) x
          Stuple t             ->
            let wk :: Atuple (Consumer acc aenv senv) t -> Atuple (Consumer acc aenv' senv) t
                wk NilAtup        = NilAtup
                wk (SnocAtup t c) = wk t `SnocAtup` weakenC c
            in
            Stuple (wk t)
--}

-- Array fusion of a de Bruijn computation AST
-- ===========================================

-- Array computations
-- ------------------

-- Evaluate a delayed computation and tie the recursive knot
--
-- We do a bit of extra work to (try to) maintain that terms should be left
-- at their lowest common use site. SEE: [Fusion and the lowest common use site]
--
computeAcc
    :: HasCallStack
    => Embed OpenAcc aenv arrs
    -> OpenAcc       aenv arrs
computeAcc (Embed      BaseEnv              cc) = OpenAcc (compute cc)
computeAcc (Embed env@(PushEnv bot lhs top) cc) =
  simplify cc & \case
    Left (Done ann v) -> bindA env ann (avarsIn OpenAcc v)
    Right d           -> d & \case
      Yield ann repr sh f
        -> bindA env ann (OpenAcc (Generate ann repr sh f))

      Step ann repr sh p f v@(Var _ _ ix)
        | Just Refl <- matchOpenExp sh (arrayShape ann v)
        , Just Refl <- isIdentity p
        -> case ix of
             ZeroIdx
               | LeftHandSideSingle _ ArrayR{} <- lhs
               , Just (OpenAccFun g) <- strengthen noTop (OpenAccFun f)
                    -> bindA bot ann (OpenAcc (Map ann (arrayRtype repr) g top))
             _      -> bindA env ann (OpenAcc (Map ann (arrayRtype repr) f (avarIn OpenAcc v)))

        | Just Refl <- isIdentity f
        -> case ix of
             ZeroIdx
               | LeftHandSideSingle _ ArrayR{} <- lhs
               , Just (OpenAccFun q)  <- strengthen noTop (OpenAccFun p)
               , Just (OpenAccExp sz) <- strengthen noTop (OpenAccExp sh)
                    -> bindA bot ann (OpenAcc (Backpermute ann (arrayRshape repr) sz q top))
             _      -> bindA env ann (OpenAcc (Backpermute ann (arrayRshape repr) sh p (avarIn OpenAcc v)))

        | otherwise
        -> case ix of
             ZeroIdx
               | LeftHandSideSingle _ ArrayR{} <- lhs
               , Just (OpenAccFun g)  <- strengthen noTop (OpenAccFun f)
               , Just (OpenAccFun q)  <- strengthen noTop (OpenAccFun p)
               , Just (OpenAccExp sz) <- strengthen noTop (OpenAccExp sh)
                    -> bindA bot ann (OpenAcc (Transform ann repr sz q g top))
             _      -> bindA env ann (OpenAcc (Transform ann repr sh p f (avarIn OpenAcc v)))

  where
    -- TODO: This annotation comes from the cunctation, that's probably not
    --       correct
    bindA :: HasCallStack
          => Extend ArrayR OpenAcc aenv aenv'
          -> Ann
          -> OpenAcc aenv' a
          -> OpenAcc aenv  a
    bindA BaseEnv             _   b = b
    bindA (PushEnv env lhs a) ann b
      -- If the freshly bound value is directly, returned, we don't have to bind it in a
      -- let. We can do this if the left hand side does not contain wildcards (other than
      -- wildcards for unit / nil) and if the value contains the same variables.
      | Just vars <- extractOpenArrayVars b
      , Just Refl <- bindingIsTrivial lhs vars = bindA env ann a
      | otherwise                              = bindA env ann (OpenAcc (Alet ann lhs a b))

    noTop :: (aenv, a) :?> aenv
    noTop ZeroIdx      = Nothing
    noTop (SuccIdx ix) = Just ix



-- Convert the internal representation of delayed arrays into a real AST
-- node. Use the most specific version of a combinator whenever possible.
--
compute
    :: HasCallStack
    => Cunctation r       aenv arrs
    -> PreOpenAcc OpenAcc aenv arrs
compute cc = simplify cc & \case
  Left (Done ann v) -> v & \case
    TupRunit                               -> Anil ann
    -- TODO: Is @ann'@ ever not the same as @ann@?
    TupRsingle (Var ann' repr@ArrayR{} ix) -> Avar $ Var ann' repr ix
    TupRpair v1 v2                         -> Apair ann (avarsIn OpenAcc v1) (avarsIn OpenAcc v2)
  Right d -> d & \case
    Yield ann aR sh f             -> Generate ann aR sh f
    Step ann (ArrayR shR tR) sh p f v
      | Just Refl <- matchOpenExp sh (arrayShape ann v)
      , Just Refl <- isIdentity p -> Map ann tR f (avarIn OpenAcc v)
      | Just Refl <- isIdentity f -> Backpermute ann shR sh p (avarIn OpenAcc v)
      | otherwise                 -> Transform ann (ArrayR shR tR) sh p f (avarIn OpenAcc v)


-- TODO: For all of these transformations, quadruple check that we pull the
--       annotations from the correct places


-- Representation of a generator as a delayed array
--
generateD
    :: HasCallStack
    => Ann
    -> ArrayR (Array sh e)
    -> Exp aenv sh
    -> Fun aenv (sh -> e)
    -> Embed OpenAcc aenv (Array sh e)
generateD ann repr sh f
  = Stats.ruleFired "generateD"
  $ Embed BaseEnv (Yield ann repr sh f)


-- Fuse a unary function into a delayed array. Also looks for unzips which can
-- be executed in constant time; SEE [unzipD]
--
mapD :: HasCallStack
     => Ann
     -> TypeR b
     -> Fun           aenv (a -> b)
     -> Embed OpenAcc aenv (Array sh a)
     -> Embed OpenAcc aenv (Array sh b)
mapD a1 tR f (unzipD a1 tR f -> Just a) = a
mapD a1 tR f (Embed env cc)
  = Stats.ruleFired "mapD"
  $ Embed env (go (delaying cc))
  where
    go (Step  a2 (ArrayR shR _) sh ix g v) = Step  (a1 <> a2) (ArrayR shR tR) sh ix (sinkA env f `compose` g) v
    go (Yield a2 (ArrayR shR _) sh g)      = Yield (a1 <> a2) (ArrayR shR tR) sh    (sinkA env f `compose` g)


-- If we are unzipping a manifest array then force the term to be computed;
-- a backend will be able to execute this in constant time.
--
unzipD
    :: HasCallStack
    => Ann
    -> TypeR b
    -> Fun                  aenv (a -> b)
    -> Embed OpenAcc        aenv (Array sh a)
    -> Maybe (Embed OpenAcc aenv (Array sh b))
unzipD ann tR f (Embed env cc@(Done _ v))
  | Lam lhs (Body a) <- f
  , Just vars        <- extractExpVars a
  , ArrayR shR _     <- arrayR cc
  , f'               <- Lam lhs $ Body $ expVars vars
  = Just $ Embed (env `pushArrayEnv` OpenAcc (Map ann tR f' $ avarsIn OpenAcc v)) $ doneZeroIdx ann $ ArrayR shR tR
unzipD _ _ _ _
  = Nothing

-- Fuse an index space transformation function that specifies where elements in
-- the destination array read there data from in the source array.
--
backpermuteD
    :: HasCallStack
    => Ann
    -> ShapeR sh'
    -> Exp          aenv sh'
    -> Fun          aenv (sh' -> sh)
    -> Cunctation r aenv (Array sh  e)
    -> Cunctation D aenv (Array sh' e)
backpermuteD ann shR' sh' p = Stats.ruleFired "backpermuteD" . go . delaying
  where
    go (Step  ann' (ArrayR _ tR) _ q f v) = Step  (ann <> ann') (ArrayR shR' tR) sh' (q `compose` p) f v
    go (Yield ann' (ArrayR _ tR) _ g)     = Yield (ann <> ann') (ArrayR shR' tR) sh' (g `compose` p)


-- Transform as a combined map and backwards permutation
--
transformD
    :: HasCallStack
    => Ann
    -> ArrayR (Array sh' b)
    -> Exp           aenv sh'
    -> Fun           aenv (sh' -> sh)
    -> Fun           aenv (a   -> b)
    -> Embed OpenAcc aenv (Array sh  a)
    -> Embed OpenAcc aenv (Array sh' b)
transformD ann (ArrayR shR' tR) sh' p f
  = Stats.ruleFired "transformD"
  . fuse (into2 (backpermuteD ann shR') sh' p)
  . mapD ann tR f
  where
    fuse :: HasCallStack
         => (forall r aenv'. Extend ArrayR OpenAcc aenv aenv' -> Cunctation r aenv' as -> Cunctation D aenv' bs)
         -> Embed OpenAcc aenv as
         -> Embed OpenAcc aenv bs
    fuse op (Embed env cc) = Embed env (op env cc)

    into2 :: (HasCallStack, Sink f1, Sink f2)
          => (f1 env' a -> f2 env' b -> c)
          -> f1 env a
          -> f2 env b
          -> Extend ArrayR OpenAcc env env'
          -> c
    into2 op a b env = op (sinkA env a) (sinkA env b)


-- Replicate as a backwards permutation
--
-- TODO: If we have a pattern such as `replicate sh (map f xs)` then in some
--       cases it might be beneficial to not fuse these terms, if `f` is
--       expensive and/or `sh` is large.
--
replicateD
    :: HasCallStack
    => Ann
    -> SliceIndex slix sl co sh
    -> Exp          aenv slix
    -> Cunctation r aenv (Array sl e)
    -> Cunctation D aenv (Array sh e)
replicateD ann sliceIndex slix cc
  = Stats.ruleFired "replicateD"
  $ backpermuteD ann (sliceDomainR sliceIndex) (IndexFull ann sliceIndex slix (shape cc)) (extend ann sliceIndex slix) cc


-- Dimensional slice as a backwards permutation
--
sliceD
    :: HasCallStack
    => Ann
    -> SliceIndex slix sl co sh
    -> Exp          aenv slix
    -> Cunctation r aenv (Array sh e)
    -> Cunctation D aenv (Array sl e)
sliceD ann sliceIndex slix cc
  = Stats.ruleFired "sliceD"
  $ backpermuteD ann (sliceShapeR sliceIndex) (IndexSlice ann sliceIndex slix (shape cc)) (restrict ann sliceIndex slix) cc


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
    :: HasCallStack
    => Ann
    -> ShapeR sl
    -> Embed OpenAcc aenv (Array sh e)
    -> Exp           aenv sl
    -> Embed OpenAcc aenv (Array sl e)
reshapeD ann slr (Embed env cc) (sinkA env -> sl)
  | Done _ v <- cc
  = Embed (env `pushArrayEnv` OpenAcc (Reshape ann slr sl (avarsIn OpenAcc v))) $ doneZeroIdx ann repr

  | otherwise
  = Stats.ruleFired "reshapeD"
  $ Embed env (backpermuteD ann slr sl (reindex ann (arrayRshape $ arrayR cc) (shape cc) slr sl) cc)

  where
    ArrayR _ tR = arrayR cc
    repr        = ArrayR slr tR


-- Combine two arrays element-wise with a binary function to produce a delayed
-- array.
--
zipWithD
    :: HasCallStack
    => Ann
    -> TypeR c
    -> Fun          aenv (a -> b -> c)
    -> Cunctation r aenv (Array sh a)
    -> Cunctation s aenv (Array sh b)
    -> Cunctation D aenv (Array sh c)
zipWithD a1 tR f cc1 cc0
  -- Two stepper functions identically accessing the same array can be kept in
  -- stepping form. This might yield a simpler final term.
  --
  | Step a2 (ArrayR shR _) sh1 p1 f1 v1 <- delaying cc1
  , Step a3 _              sh0 p0 f0 v0 <- delaying cc0
  , Just Refl                           <- matchVar v1 v0
  , Just Refl                           <- matchOpenFun p1 p0
  = Stats.ruleFired "zipWithD/step"
  $ Step (a1 <> a2 <> a3) (ArrayR shR tR) (intersect (a1 <> a2 <> a3) shR sh1 sh0) p0 (combine f f1 f0) v0

  -- Otherwise transform both delayed terms into (index -> value) mappings and
  -- combine the two indexing functions that way.
  --
  | Yield a2 (ArrayR shR _) sh1 f1 <- yield cc1
  , Yield a3 _              sh0 f0 <- yield cc0
  = Stats.ruleFired "zipWithD"
  $ Yield (a1 <> a2 <> a3) (ArrayR shR tR) (intersect (a1 <> a2 <> a3) shR sh1 sh0) (combine f f1 f0)

  | otherwise
  = error "work is stressing me out, I should take a break"
  where
    combine :: forall aenv a b c e. HasCallStack
            => Fun aenv (a -> b -> c)
            -> Fun aenv (e -> a)
            -> Fun aenv (e -> b)
            -> Fun aenv (e -> c)
    combine c ixa ixb
      | Lam lhs1 (Body ixa') <- ixa
      , Lam lhs2 (Body ixb') <- ixb
      -- The two LeftHandSides may differ in the use of wildcards. If they do not match, we must
      -- combine them as done in `combineLHS`. As this will probably not occur often and requires
      -- additional weakening, we do a quick check whether the left hand sides are equal.
      --
      = case matchELeftHandSide lhs1 lhs2 of
          Just Refl
            | Lam lhsA (Lam lhsB (Body c')) <- weakenE (weakenWithLHS lhs1) c
              -> Lam lhs1 $ Body $ Let a1 lhsA ixa'  $ Let a1 lhsB (weakenE (weakenWithLHS lhsA)       ixb') c'
          Nothing
            | CombinedLHS lhs k1 k2         <- combineLHS lhs1 lhs2
            , Lam lhsA (Lam lhsB (Body c')) <- weakenE (weakenWithLHS lhs) c
            , ixa''                         <- weakenE k1 ixa'
              -> Lam lhs  $ Body $ Let a1 lhsA ixa'' $ Let a1 lhsB (weakenE (weakenWithLHS lhsA .> k2) ixb') c'
          _
              -> error "how's your break?"
      --
      | otherwise
      = error "work is stressing me out, I should get back to it"


data CombinedLHS s t env1' env2' env where
  CombinedLHS :: LeftHandSide s t env env'
              -> env1' :> env'
              -> env2' :> env'
              -> CombinedLHS s t env1' env2' env

combineLHS
    :: HasCallStack
    => LeftHandSide s t env env1'
    -> LeftHandSide s t env env2'
    -> CombinedLHS  s t env1' env2' env
combineLHS = go weakenId weakenId
  where
    go :: env1 :> env -> env2 :> env -> LeftHandSide s t env1 env1' -> LeftHandSide s t env2 env2' -> CombinedLHS s t env1' env2' env
    go k1 k2 (LeftHandSideWildcard tR)  (LeftHandSideWildcard _)  = CombinedLHS (LeftHandSideWildcard tR)          k1        k2
    go k1 k2 (LeftHandSideSingle a1 tR) (LeftHandSideSingle a2 _) = CombinedLHS (LeftHandSideSingle (a1 <> a2) tR) (sink k1) (sink k2)
    go k1 k2 (LeftHandSidePair l1 h1)  (LeftHandSidePair l2 h2)
      | CombinedLHS l k1'  k2'  <- go k1  k2  l1 l2
      , CombinedLHS h k1'' k2'' <- go k1' k2' h1 h2               = CombinedLHS (LeftHandSidePair l h)             k1''      k2''
    go k1 k2 (LeftHandSideWildcard _)  lhs
      | Exists lhs' <- rebuildLHS lhs                             = CombinedLHS lhs'        (weakenWithLHS lhs' .> k1) (sinkWithLHS lhs lhs' k2)
    go k1 k2 lhs                       (LeftHandSideWildcard _)
      | Exists lhs' <- rebuildLHS lhs                             = CombinedLHS lhs'        (sinkWithLHS lhs lhs' k1)  (weakenWithLHS lhs' .> k2)
    go _ _ _ _
      = internalError "unexpected LHS combination"



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
--   reverse xs = let len  = unindex1 (shape xs)
--                    pf i = len - i - 1
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
-- be beneficial if we can estimate that the cost of re-computation is less than
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
aletD :: HasCallStack
      => EmbedAcc OpenAcc
      -> ElimAcc  OpenAcc
      -> ALeftHandSide arrs aenv aenv'
      ->       OpenAcc aenv  arrs
      ->       OpenAcc aenv' brrs
      -> Embed OpenAcc aenv  brrs
aletD embedAcc elimAcc lhs (embedAcc -> Embed env1 cc1) acc0

  -- let-floating
  -- ------------
  --
  -- Immediately inline the variable referring to the bound expression into the
  -- body, instead of adding to the environments and creating an indirection
  -- that must be later eliminated by shrinking.
  --
  | LeftHandSideSingle _ _                     <- lhs
  , Done _ (TupRsingle v1@(Var  _ ArrayR{} _)) <- cc1
  , Embed env0 cc0                             <- embedAcc $ rebuildA (subAtop (Avar v1) . sink1 env1) acc0
  = Stats.ruleFired "aletD/float"
  $ Embed (env1 `append` env0) cc0

  -- Ensure we only call 'embedAcc' once on the body expression
  --
  | otherwise
  = aletD' embedAcc elimAcc lhs (Embed env1 cc1) (embedAcc acc0)


aletD' :: forall aenv aenv' arrs brrs. HasCallStack
       => EmbedAcc OpenAcc
       -> ElimAcc OpenAcc
       -> ALeftHandSide arrs aenv aenv'
       -> Embed OpenAcc aenv  arrs
       -> Embed OpenAcc aenv' brrs
       -> Embed OpenAcc aenv  brrs
aletD' embedAcc elimAcc (LeftHandSideSingle _ ArrayR{}) (Embed env1 cc1) (Embed env0 cc0)

  -- let-binding
  -- -----------
  --
  -- Check whether we can eliminate the let-binding. Note that we must inspect
  -- the entire term, not just the Cunctation that would be produced by
  -- embedAcc. If we don't we can be left with dead terms that don't get
  -- eliminated. This problem occurred in the canny program.
  --
  | acc1    <- computeAcc (Embed env1 cc1)
  , False   <- elimAcc acc1 acc0
  = Stats.ruleFired "aletD/bind"
  $ Embed (BaseEnv `pushArrayEnv` acc1 `append` env0) cc0

  -- let-elimination
  -- ---------------
  --
  -- Handle the remaining cases in a separate function. It turns out that this
  -- is important so we aren't excessively sinking/delaying terms.
  --
  | acc0'   <- sink1 env1 acc0
  = Stats.ruleFired "aletD/eliminate"
  $ case delaying cc1 of
      Step{}  -> eliminate env1 cc1 acc0'
      Yield{} -> eliminate env1 cc1 acc0'

  where
    acc0 :: OpenAcc aenv' brrs
    acc0 = computeAcc (Embed env0 cc0)

    kmap :: forall aenv a b. (PreOpenAcc OpenAcc aenv a -> PreOpenAcc OpenAcc aenv b)
         -> OpenAcc aenv a
         -> OpenAcc aenv b
    kmap f (OpenAcc pacc) = OpenAcc (f pacc)

    -- The second part of let-elimination. Splitting into two steps exposes the
    -- extra type variables, and ensures we don't do extra work manipulating the
    -- body when not necessary (which can lead to a complexity blowup).
    --
    eliminate
        :: forall r aenv aenv' sh e brrs. HasCallStack
        => Extend ArrayR OpenAcc aenv aenv'
        -> Cunctation r aenv' (Array sh e)
        -> OpenAcc (aenv', Array sh e) brrs
        -> Embed OpenAcc aenv brrs
    eliminate env1 cc1 body
      | Done  ann v1               <- cc1
      , TupRsingle v1'@(Var _ r _) <- v1  = elim ann r (arrayShape ann v1') (indexArray ann v1')
      | Step  ann r sh1 p1 f1 v1   <- cc1 = elim ann r sh1 (f1 `compose` indexArray ann v1 `compose` p1)
      | Yield ann r sh1 f1         <- cc1 = elim ann r sh1 f1
      where
        bnd :: PreOpenAcc OpenAcc aenv' (Array sh e)
        bnd = compute cc1

        elim :: HasCallStack
             => Ann
             -> ArrayR (Array sh e)
             -> Exp aenv' sh
             -> Fun aenv' (sh -> e)
             -> Embed OpenAcc aenv brrs
        elim ann r sh1 f1
          | sh1'              <- weaken (weakenSucc' weakenId) sh1
          , f1'               <- weaken (weakenSucc' weakenId) f1
          , Embed env0' cc0'  <- embedAcc $ rebuildA (subAtop bnd) $ kmap (replaceA sh1' f1' $ Var ann r ZeroIdx) body
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
    replaceE :: forall env aenv sh e t. HasCallStack
             => OpenExp env aenv sh
             -> OpenFun env aenv (sh -> e)
             -> ArrayVar aenv (Array sh e)
             -> OpenExp env aenv t
             -> OpenExp env aenv t
    replaceE sh' f' avar@(Var _ (ArrayR shR _) _) exp =
      case exp of
        Let ann lhs x y                  -> let k = weakenWithLHS lhs
                                            in  Let ann lhs (cvtE x) (replaceE (weakenE k sh') (weakenE k f') avar y)
        Evar var                         -> Evar var
        Foreign ann tR ff f e            -> Foreign ann tR ff f (cvtE e)
        Const ann tR c                   -> Const ann tR c
        Undef ann tR                     -> Undef ann tR
        Nil ann                          -> Nil ann
        Pair ann e1 e2                   -> Pair ann (cvtE e1) (cvtE e2)
        VecPack ann vR e                 -> VecPack ann vR (cvtE e)
        VecUnpack ann vR e               -> VecUnpack ann vR (cvtE e)
        IndexSlice ann x ix sh           -> IndexSlice ann x (cvtE ix) (cvtE sh)
        IndexFull ann x ix sl            -> IndexFull ann x (cvtE ix) (cvtE sl)
        ToIndex ann shR' sh ix           -> ToIndex ann shR' (cvtE sh) (cvtE ix)
        FromIndex ann shR' sh i          -> FromIndex ann shR' (cvtE sh) (cvtE i)
        Case ann e rhs def               -> Case ann (cvtE e) (over (mapped . _2) cvtE rhs) (fmap cvtE def)
        Cond ann p t e                   -> Cond ann (cvtE p) (cvtE t) (cvtE e)
        PrimConst ann c                  -> PrimConst ann c
        PrimApp ann g x                  -> PrimApp ann g (cvtE x)
        ShapeSize ann shR' sh            -> ShapeSize ann shR' (cvtE sh)
        While ann p f x                  -> While ann (replaceF sh' f' avar p) (replaceF sh' f' avar f) (cvtE x)
        Coerce ann t1 t2 e               -> Coerce ann t1 t2 (cvtE e)

        Shape ann a
          | Just Refl <- matchVar a avar -> Stats.substitution "replaceE/shape" $ modifyAnn (<> ann) sh'
          | otherwise                    -> exp

        Index ann a sh
          | Just Refl        <- matchVar a avar
          , Lam lhs (Body b) <- f'       -> Stats.substitution "replaceE/!" . cvtE $ Let ann lhs sh b
          | otherwise                    -> Index ann a (cvtE sh)

        LinearIndex ann a i
          | Just Refl        <- matchVar a avar
          , Lam lhs (Body b) <- f'
                                         -> Stats.substitution "replaceE/!!" . cvtE
                                          $ Let ann
                                                lhs
                                                (Let ann
                                                     (LeftHandSideSingle ann scalarTypeInt) i
                                                     $ FromIndex ann shR (weakenE (weakenSucc' weakenId) sh')
                                                     $ Evar
                                                     $ Var ann scalarTypeInt ZeroIdx)
                                                b
          | otherwise                    -> LinearIndex ann a (cvtE i)

      where
        cvtE :: OpenExp env aenv s -> OpenExp env aenv s
        cvtE = replaceE sh' f' avar

    replaceF :: forall env aenv sh e t. HasCallStack
             => OpenExp env aenv sh
             -> OpenFun env aenv (sh -> e)
             -> ArrayVar aenv (Array sh e)
             -> OpenFun env aenv t
             -> OpenFun env aenv t
    replaceF sh' f' avar fun =
      case fun of
        Body e          -> Body (replaceE sh' f' avar e)
        Lam lhs f       -> let k = weakenWithLHS lhs
                           in  Lam lhs (replaceF (weakenE k sh') (weakenE k f') avar f)

    replaceA :: forall aenv sh e a. HasCallStack
             => Exp aenv sh
             -> Fun aenv (sh -> e)
             -> ArrayVar aenv (Array sh e)
             -> PreOpenAcc OpenAcc aenv a
             -> PreOpenAcc OpenAcc aenv a
    replaceA sh' f' avar pacc =
      case pacc of
        Avar v
          | Just Refl <- matchVar v avar -> Avar avar
          | otherwise                    -> Avar v

        Alet ann lhs bnd (body :: OpenAcc aenv1 a) ->
          let w :: aenv :> aenv1
              w    = weakenWithLHS lhs
              sh'' = weaken w sh'
              f''  = weaken w f'
          in
          Alet ann lhs (cvtA bnd) (kmap (replaceA sh'' f'' (weaken w avar)) body)

        Use ann repr arrs           -> Use ann repr arrs
        Unit ann tR e               -> Unit ann tR (cvtE e)
        Acond ann p at ae           -> Acond ann (cvtE p) (cvtA at) (cvtA ae)
        Anil ann                    -> Anil ann
        Atrace ann msg a b          -> Atrace ann msg (cvtA a) (cvtA b)
        Apair ann a1 a2             -> Apair ann (cvtA a1) (cvtA a2)
        Awhile ann p f a            -> Awhile ann (cvtAF p) (cvtAF f) (cvtA a)
        Apply ann repr f a          -> Apply ann repr (cvtAF f) (cvtA a)
        Aforeign ann repr ff f a    -> Aforeign ann repr ff f (cvtA a)       -- no sharing between f and a
        Generate ann repr sh f      -> Generate ann repr (cvtE sh) (cvtF f)
        Map ann tR f a              -> Map ann tR (cvtF f) (cvtA a)
        ZipWith ann tR f a b        -> ZipWith ann tR (cvtF f) (cvtA a) (cvtA b)
        Backpermute ann shR sh p a  -> Backpermute ann shR (cvtE sh) (cvtF p) (cvtA a)
        Transform ann repr sh p f a -> Transform ann repr (cvtE sh) (cvtF p) (cvtF f) (cvtA a)
        Slice ann slix a sl         -> Slice ann slix (cvtA a) (cvtE sl)
        Replicate ann slix sh a     -> Replicate ann slix (cvtE sh) (cvtA a)
        Reshape ann shR sl a        -> Reshape ann shR (cvtE sl) (cvtA a)
        Fold ann f z a              -> Fold ann (cvtF f) (cvtE <$> z) (cvtA a)
        FoldSeg ann i f z a s       -> FoldSeg ann i (cvtF f) (cvtE <$> z) (cvtA a) (cvtA s)
        Scan  ann d f z a           -> Scan  ann d (cvtF f) (cvtE <$> z) (cvtA a)
        Scan' ann d f z a           -> Scan' ann d (cvtF f) (cvtE z) (cvtA a)
        Permute ann f d p a         -> Permute ann (cvtF f) (cvtA d) (cvtF p) (cvtA a)
        Stencil ann s t f x a       -> Stencil ann s t (cvtF f) (cvtB x) (cvtA a)
        Stencil2 ann s1 s2 t f x a y b
                                    -> Stencil2 ann s1 s2 t (cvtF f) (cvtB x) (cvtA a) (cvtB y) (cvtA b)
        -- Collect seq              -> Collect (cvtSeq seq)

      where
        cvtA :: OpenAcc aenv s -> OpenAcc aenv s
        cvtA = kmap (replaceA sh' f' avar)

        cvtE :: Exp aenv s -> Exp aenv s
        cvtE = replaceE sh' f' avar

        cvtF :: Fun aenv s -> Fun aenv s
        cvtF = replaceF sh' f' avar

        cvtB :: Boundary aenv s -> Boundary aenv s
        cvtB Clamp        = Clamp
        cvtB Mirror       = Mirror
        cvtB Wrap         = Wrap
        cvtB (Constant c) = Constant c
        cvtB (Function f) = Function (cvtF f)

        cvtAF :: HasCallStack => PreOpenAfun OpenAcc aenv s -> PreOpenAfun OpenAcc aenv s
        cvtAF = cvt sh' f' avar
          where
            cvt :: forall aenv a.
                   Exp aenv sh -> Fun aenv (sh -> e) -> ArrayVar aenv (Array sh e)
                -> PreOpenAfun OpenAcc aenv a
                -> PreOpenAfun OpenAcc aenv a
            cvt sh'' f'' avar' (Abody a) = Abody $ kmap (replaceA sh'' f'' avar') a
            cvt sh'' f'' avar' (Alam lhs (af :: PreOpenAfun OpenAcc aenv1 b)) =
              Alam lhs $ cvt (weaken w sh'')
                (weaken w f'')
                (weaken w avar')
                af
              where
                w :: aenv :> aenv1
                w = weakenWithLHS lhs

-- Do not fuse bindings of multiple variables
aletD' _ _ lhs (Embed env1 cc1) (Embed env0 cc0)
  = Stats.ruleFired "aletD/bind"
  $ Embed (PushEnv BaseEnv lhs (computeAcc (Embed env1 cc1)) `append` env0) cc0

{--
        cvtSeq :: PreOpenSeq acc aenv senv s -> PreOpenSeq acc aenv senv s
        cvtSeq s =
          case s of
            Producer p s' ->
              Producer
                (case p of
                   StreamIn arrs        -> StreamIn arrs
                   ToSeq slix sh a      -> ToSeq slix sh (cvtA a)
                   MapSeq f x           -> MapSeq (cvtAF f) x
                   ChunkedMapSeq f x    -> ChunkedMapSeq (cvtAF f) x
                   ZipWithSeq f x y     -> ZipWithSeq (cvtAF f) x y
                   ScanSeq f e x        -> ScanSeq (cvtF f) (cvtE e) x)
                (cvtSeq s')
            Consumer c ->
              Consumer (cvtC c)
            Reify ix -> Reify ix

        cvtC :: Consumer acc aenv senv s -> Consumer acc aenv senv s
        cvtC c =
          case c of
            FoldSeq f e x        -> FoldSeq (cvtF f) (cvtE e) x
            FoldSeqFlatten f a x -> FoldSeqFlatten (cvtAF f) (cvtA a) x
            Stuple t             -> Stuple (cvtCT t)

        cvtCT :: Atuple (Consumer acc aenv senv) t -> Atuple (Consumer acc aenv senv) t
        cvtCT NilAtup        = NilAtup
        cvtCT (SnocAtup t c) = cvtCT t `SnocAtup` cvtC c
--}


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
acondD :: HasCallStack
       => Ann
       -> MatchAcc OpenAcc
       -> EmbedAcc OpenAcc
       -> Exp              aenv PrimBool
       ->          OpenAcc aenv arrs
       ->          OpenAcc aenv arrs
       -> Embed    OpenAcc aenv arrs
acondD ann matchAcc embedAcc p t e
  | Const _ _ 1 <- p            = Stats.knownBranch "True"      . embedAcc $ modifyAnn (<> ann) t
  | Const _ _ 0 <- p            = Stats.knownBranch "False"     . embedAcc $ modifyAnn (<> ann) e
  | Just Refl   <- matchAcc t e = Stats.knownBranch "redundant" $ embedAcc $ modifyAnn (<> ann) e
  | otherwise                   = done $ Acond ann p (computeAcc (embedAcc t))
                                                     (computeAcc (embedAcc e))


-- Scalar expressions
-- ------------------

identity :: Ann -> TypeR a -> OpenFun env aenv (a -> a)
identity ann t
  | DeclareVars lhs _ value <- declareVars (annRfromTup ann t) t
  = Lam lhs $ Body $ expVars $ value weakenId

toIndex :: Ann -> ShapeR sh -> OpenExp env aenv sh -> OpenFun env aenv (sh -> Int)
toIndex ann shR sh
  | repr <- shapeType shR
  , DeclareVars lhs k value <- declareVars (annR sh repr) repr
  = Lam lhs $ Body $ ToIndex ann shR (weakenE k sh) $ expVars $ value weakenId

fromIndex :: Ann -> ShapeR sh -> OpenExp env aenv sh -> OpenFun env aenv (Int -> sh)
fromIndex ann shR sh
  = Lam (LeftHandSideSingle ann scalarTypeInt)
  $ Body
  $ FromIndex ann shR (weakenE (weakenSucc' weakenId) sh)
  $ Evar
  $ Var ann scalarTypeInt ZeroIdx

intersect :: Ann -> ShapeR sh -> OpenExp env aenv sh -> OpenExp env aenv sh -> OpenExp env aenv sh
intersect ann = mkShapeBinary f ann
  where
    f :: OpenExp env' aenv Int -> OpenExp env' aenv Int -> OpenExp env' aenv Int
    f a b = PrimApp ann (PrimMin singleType) $ Pair ann a b

-- union :: ShapeR sh -> OpenExp env aenv sh -> OpenExp env aenv sh -> OpenExp env aenv sh
-- union = mkShapeBinary f
--   where
--     f a b = PrimApp (PrimMax singleType) $ Pair a b

mkShapeBinary
    :: (forall env'. OpenExp env' aenv Int -> OpenExp env' aenv Int -> OpenExp env' aenv Int)
    -> Ann
    -> ShapeR sh
    -> OpenExp env aenv sh
    -> OpenExp env aenv sh
    -> OpenExp env aenv sh
mkShapeBinary _ ann ShapeRz _ _ = Nil ann
mkShapeBinary f ann (ShapeRsnoc shR) (Pair a1 as a) (Pair a2 bs b) = Pair (a1 <> a2) (mkShapeBinary f ann shR as bs) (f a b)
mkShapeBinary f ann shR (Let ann' lhs bnd a) b = Let ann' lhs bnd $ mkShapeBinary f ann shR a (weakenE (weakenWithLHS lhs) b)
mkShapeBinary f ann shR a (Let ann' lhs bnd b) = Let ann' lhs bnd $ mkShapeBinary f ann shR (weakenE (weakenWithLHS lhs) a) b
mkShapeBinary f ann shR a b@Pair{} -- `a` is not Pair
  | repr <- shapeType shR
  , DeclareVars lhs k value <- declareVars (annRfromTup ann repr) repr
  = Let ann lhs a $ mkShapeBinary f ann shR (expVars $ value weakenId) (weakenE k b)
mkShapeBinary f ann shR a b -- `b` is not a Pair
  | repr <- shapeType shR
  , DeclareVars lhs k value <- declareVars (annRfromTup ann repr) repr
  = Let ann lhs b $ mkShapeBinary f ann shR (weakenE k a) (expVars $ value weakenId)

reindex :: Ann
        -> ShapeR sh'
        -> OpenExp env aenv sh'
        -> ShapeR sh
        -> OpenExp env aenv sh
        -> OpenFun env aenv (sh -> sh')
reindex ann shR' sh' shR sh
  | Just Refl <- matchOpenExp sh sh' = identity ann (shapeType shR')
  | otherwise                        = fromIndex ann shR' sh' `compose` toIndex ann shR sh

extend :: Ann
       -> SliceIndex slix sl co sh
       -> Exp aenv slix
       -> Fun aenv (sh -> sl)
extend ann sliceIndex slix
  | repr <- shapeType $ sliceDomainR sliceIndex
  , DeclareVars lhs k value <- declareVars (annRfromTup ann repr) repr
  = Lam lhs $ Body $ IndexSlice ann sliceIndex (weakenE k slix) $ expVars $ value weakenId

restrict :: Ann
         -> SliceIndex slix sl co sh
         -> Exp aenv slix
         -> Fun aenv (sl -> sh)
restrict ann sliceIndex slix
  | repr <- shapeType $ sliceShapeR sliceIndex
  , DeclareVars lhs k value <- declareVars (annRfromTup ann repr) repr
  = Lam lhs $ Body $ IndexFull ann sliceIndex (weakenE k slix) $ expVars $ value weakenId

arrayShape :: Ann -> ArrayVar aenv (Array sh e) -> Exp aenv sh
arrayShape ann = simplifyExp . Shape ann

indexArray :: Ann -> ArrayVar aenv (Array sh e) -> Fun aenv (sh -> e)
indexArray ann v@(Var _ (ArrayR shR _) _)
  | repr <- shapeType shR
  , DeclareVars lhs _ value <- declareVars (annRfromTup ann repr) repr
  = Lam lhs $ Body $ Index ann v $ expVars $ value weakenId

linearIndex :: Ann -> ArrayVar aenv (Array sh e) -> Fun aenv (Int -> e)
linearIndex ann v = Lam (LeftHandSideSingle ann scalarTypeInt) $ Body $ LinearIndex ann v $ Evar $ Var ann scalarTypeInt ZeroIdx


extractOpenAcc :: ExtractAcc OpenAcc
extractOpenAcc (OpenAcc pacc) = Just pacc

extractDelayedOpenAcc :: ExtractAcc DelayedOpenAcc
extractDelayedOpenAcc (Manifest pacc) = Just pacc
extractDelayedOpenAcc _               = Nothing

extractOpenArrayVars
    :: OpenAcc aenv a
    -> Maybe (ArrayVars aenv a)
extractOpenArrayVars (OpenAcc pacc) =
  avarsOut extractOpenAcc pacc

extractDelayedArrayVars
    :: DelayedOpenAcc aenv a
    -> Maybe (ArrayVars aenv a)
extractDelayedArrayVars acc
  | Just pacc <- extractDelayedOpenAcc acc = avarsOut extractDelayedOpenAcc pacc
  | otherwise                              = Nothing

