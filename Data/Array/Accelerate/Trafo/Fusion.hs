{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards        #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
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
  convertAcc, convertAfun, convertStreamSeq, fuseSeq

) where

-- standard library
import Prelude                                          hiding ( exp, until )
import Control.Applicative                              ( pure, (<$>), (<*>) )
import Data.Constraint                                  ( Dict(..) )
import Data.Function                                    ( on )
import Data.Maybe                                       ( fromMaybe )
import Data.Monoid                                      ( Monoid(..), (<>) )
import Data.Typeable                                    ( Typeable )

-- friends
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Trafo.Base
import Data.Array.Accelerate.Trafo.Shrink
import Data.Array.Accelerate.Trafo.Simplify
import Data.Array.Accelerate.Trafo.Substitution
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Representation       ( SliceIndex(..) )
import Data.Array.Accelerate.Array.Sugar                ( Array, Arrays(..), ArraysR(..), ArrRepr
                                                        , Elt, EltRepr, Shape, Slice, Tuple(..), Atuple(..)
                                                        , IsAtuple, TupleRepr, Scalar, ArraysFlavour(..) )
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
convertStreamSeq :: Elt index => Bool -> StreamSeq index OpenAcc a -> DelayedSeq index a
convertStreamSeq fuseAcc (StreamSeq binds (fuseSeq -> seq))
  = let s = embedSeq (embedOpenAcc fuseAcc) seq
    in withSimplStats (StreamSeq (fuseBinds binds) (convertOpenSeq fuseAcc s))
  where
    fuseBinds :: Extend OpenAcc aenv aenv' -> Extend DelayedOpenAcc aenv aenv'
    fuseBinds BaseEnv = BaseEnv
    fuseBinds (PushEnv env a) = fuseBinds env `PushEnv` manifest fuseAcc (computeAcc (embedOpenAcc fuseAcc a))

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
convertOpenAcc fuseAcc = manifest fuseAcc . computeAcc . embedOpenAcc fuseAcc . computeAcc . embedOpenAcc fuseAcc

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
    Subarray sh ix arr      -> Subarray (cvtE sh) (cvtE ix) arr
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
    Collect s cs            -> Collect (cvtS s) (cvtS <$> cs)

    where
      -- Flatten needless let-binds, which can be introduced by the conversion to
      -- the internal embeddable representation.
      --
      alet bnd body
        | Manifest (Avar ZeroIdx) <- body
        , Manifest x              <- bnd
        = x

        | otherwise
        = Alet bnd body

      cvtAT :: Atuple (OpenAcc aenv) a -> Atuple (DelayedOpenAcc aenv) a
      cvtAT NilAtup        = NilAtup
      cvtAT (SnocAtup t a) = cvtAT t `SnocAtup` manifest fuseAcc a

      cvtAF :: OpenAfun aenv f -> PreOpenAfun DelayedOpenAcc aenv f
      cvtAF (Alam f)  = Alam  (cvtAF f)
      cvtAF (Abody b) = Abody (manifest fuseAcc b)

      cvtS :: PreOpenSeq index OpenAcc aenv s -> PreOpenSeq index DelayedOpenAcc aenv s
      cvtS = convertOpenSeq fuseAcc

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
    IndexSlice x ix sh      -> IndexSlice x ix (cvtE sh)
    IndexFull x ix sl       -> IndexFull x (cvtE ix) (cvtE sl)
    ToIndex sh ix           -> ToIndex (cvtE sh) (cvtE ix)
    FromIndex sh ix         -> FromIndex (cvtE sh) (cvtE ix)
    ToSlice x sh ix         -> ToSlice x (cvtE sh) (cvtE ix)
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

convertOpenSeq :: Bool -> PreOpenSeq index OpenAcc aenv a -> PreOpenSeq index DelayedOpenAcc aenv a
convertOpenSeq fuseAcc s =
  case s of
    Consumer c          -> Consumer (cvtC c)
    Reify a             -> Reify (manifest fuseAcc a)
    Producer p s'       -> Producer (cvtP p) (convertOpenSeq fuseAcc s')
  where
    cvtC :: Consumer index OpenAcc aenv a -> Consumer index DelayedOpenAcc aenv a
    cvtC c =
      case c of
        Conclude a d -> Conclude (manifest fuseAcc a) (manifest fuseAcc d)
        Stuple t     -> Stuple (cvtCT t)

    cvtP :: Producer index OpenAcc aenv a -> Producer index DelayedOpenAcc aenv a
    cvtP p =
      case p of
        Pull s -> Pull s
        ProduceAccum l f acc -> ProduceAccum (cvtE <$> l) (cvtAF f) (manifest fuseAcc acc)

    cvtCT :: Atuple (PreOpenSeq index OpenAcc aenv) t -> Atuple (PreOpenSeq index DelayedOpenAcc aenv) t
    cvtCT NilAtup        = NilAtup
    cvtCT (SnocAtup t c) = SnocAtup (cvtCT t) (convertOpenSeq fuseAcc c)

    cvtAF :: OpenAfun aenv f -> PreOpenAfun DelayedOpenAcc aenv f
    cvtAF (Alam f)  = Alam  (cvtAF f)
    cvtAF (Abody b) = Abody (manifest fuseAcc b)

    cvtE :: OpenExp env aenv t -> DelayedOpenExp env aenv t
    cvtE = convertOpenExp fuseAcc


-- Term elimination
-- ----------------

-- Given a bound term and body in which it occurs, we need to decide whether
-- that term should be embedded in the body. More generallly, if the term is of
-- a product type, we need to what components of the product should be embedded.
--
type ElimAcc  acc = forall aenv aenv' s t. Arrays s => Cunctation acc aenv' s -> acc (aenv,s) t -> Elim acc aenv' s

-- Component-wise elimination.
--
data Elim acc aenv a where
  -- The term should not be eliminated.
  ElimBind  :: PreOpenAcc acc aenv a
            -> Elim acc aenv a

  -- The term is dead so should be eliminated
  ElimDead  :: Elim acc aenv a

  -- The term should be eliminated.
  ElimEmbed :: Elim acc aenv a

  -- Components of the result of the term should be eliminated but other
  -- components shouldn't. The subproduct captures those that should be bound.
  -- The process of doing this elimination can also result in bindings that need
  -- to be floated out.
  --
  ElimTuple :: (IsAtupleRepr t', IsAtuple a)
            => Extend acc aenv aenv'
            -> Subproduct (acc aenv') t' (TupleRepr a)
            -> Elim acc aenv a

-- | Apply the fusion transformation to the AST to combine and simplify terms.
-- This converts terms into the internal delayed array representation and merges
-- adjacent producer/producer terms. Using the reduced internal form limits the
-- number of combinations that need to be considered.
--
type EmbedAcc acc = forall aenv arrs. Arrays arrs => acc aenv arrs -> Embed acc aenv arrs


embedOpenAcc :: Arrays arrs => Bool -> OpenAcc aenv arrs -> Embed OpenAcc aenv arrs
embedOpenAcc fuseAcc (OpenAcc pacc) =
  embedPreAcc fuseAcc (embedOpenAcc fuseAcc) elimOpenAcc pacc
  where
    elimOpenAcc :: Arrays s => Cunctation OpenAcc aenv' s -> OpenAcc (aenv,s) t -> Elim OpenAcc aenv' s
    elimOpenAcc bnd body
      = elimA bnd (count ZeroIdx body)
      where
        -- Ensure we only calculate the usage of the bound variable once.
        --
        count :: UsesOfAcc OpenAcc
        count idx (OpenAcc pacc) = usesOfPreAcc count idx pacc

        -- Given how it is used in the body term, decide whether all or some
        -- components can be eliminated.
        --
        -- Note that we must inspect the entire term, not just the Cunctation
        -- that would be produced by embedAcc. If we don't we can be left with
        -- dead terms that don't get eliminated. This problem occurred in the
        -- canny program. RCE: I'm not actually sure this is true anymore.
        --
        elimA :: Arrays s' => Cunctation OpenAcc aenv' s' -> Use s' -> Elim OpenAcc aenv' s'
        elimA bnd u =
          case bnd of
            -- The bound term is dead in the body, so don't traverse it.
            --
            _   | allUse (on (&&) (==0)) u
                -> Stats.ruleFired "elimDead" ElimDead

            -- The bound term can't be fused anyway, so bind it.
            --
            Done _   -> Stats.ruleFired "elimBind" (ElimBind (compute' bnd))

            -- Unit (()) terms can always be eliminated.
            --
            Ctuple NilAtup -> Stats.ruleFired "elimUnit" ElimEmbed

            -- The bound term can be split into several tuple components, decide
            -- whether we can eliminate each one independently.
            --
            Ctuple t | UseTuple u' <- u
                     -> elimTuple t u'

            -- The bound term is indivisble, but fusible.
            --
            _        -> elimBase (compute' bnd) u

        -- When does the cost of re-computation outweigh that of memory access?
        -- For the moment only do the substitution if the bound array is
        -- constructed in a few special ways or if there is only a single use of
        -- it in the body. However, it is likely advantageous to be far more
        -- aggressive here. SEE: [Sharing vs. Fusion]
        --
        elimBase :: PreOpenAcc OpenAcc aenv' s' -> Use s'-> Elim OpenAcc aenv' s'
        elimBase bnd u
          -- The definition of 'unzip' applied to manifest data, which is
          -- defined in the prelude as a map projecting out the appropriate
          -- element. This should always be eliminated
          --
          | Map f a                 <- bnd
          , Avar _                  <- extract a
          , Lam (Body (Prj _ _))    <- f
          = Stats.ruleFired "unzipD" ElimEmbed

          | Avar _ <- bnd
          = Stats.ruleFired "elimAvar" ElimEmbed

          -- Similarly, "simple" scalar expression wrapped in unit arrays should
          -- also be eliminated in all cases.
          --
          | Unit e <- bnd
          , simpleExp e
          = Stats.ruleFired "simpleScalar" ElimEmbed

          -- Eliminate when there is a single use of the bound array in the use
          -- site.
          --
          | allUse (const (<= lIMIT)) u
          = ElimEmbed

          | otherwise
          = ElimBind bnd
          where
            lIMIT = 1

        -- Different components of a tuple can be eliminated independently.
        --
        -- In decidiing what components of a tuple can be eliminated, we have to
        -- be careful how we treat let bindings. This for example is simple to
        -- embed.
        --
        --   let a = (generate sh f, generate sh' g)
        --   in zipWith h (fst a) (snd a)
        --
        -- Because each component of 'a' only occurs once we can transform this
        -- into
        --
        --   zipWith (generate sh f, gemerate sh' g)
        --
        -- What about this, however?
        --
        --   let a = (generate sh f, scanl g 0 arr)
        --   in zipWith h (fst a) (snd a)
        --
        -- In this case we embed the fst component of 'a' but leave the second
        -- component bound.
        --
        --   let a = scanl g 0 arr
        --   in zipWith h (generate sh f) a
        --
        -- It gets more complex with
        --
        --   let a = let b = scanr j 1 arr
        --           in (map f b, scanl g 0 b)
        --   in zipWith h (fst a) (snd a)
        --
        -- This becomes
        --
        --   let b = scanr j 1 arr in
        --   let a = scanl g 0 b
        --   in zipWith h (map f b) a
        --
        -- Here we are floating b out, possibly extending its lifetime. However,
        -- by doing this we are able to fuse the first component of 'a'. In
        -- general we consider the benefit of fusion outweighs the cost of let
        -- floating.
        --
        -- Similarly,
        --
        --   let a = ( let b = scanr j 1 arr
        --             in map f b
        --           , scanl g 0 arr)
        --   in zipWith h (fst a) (snd a)
        --
        -- becomes
        --
        --   let a = scanl g 0 arr in
        --   let b = scanr j 1 arr
        --   in zipWith h (map f b) a
        --
        elimTuple :: IsAtuple t
                  => Atuple (Embed OpenAcc aenv') (TupleRepr t)
                  -> Atuple Use (TupleRepr t)
                  -> Elim OpenAcc aenv' t
        elimTuple = elim ElimTuple id True
          where
            elim :: forall aenv aenv' t a. IsAtuple a
                 => (forall aenv'' t'. IsAtupleRepr t' => Extend OpenAcc aenv' aenv'' -> Subproduct (OpenAcc aenv'') t' t -> Elim OpenAcc aenv a)
                 -> aenv :> aenv'
                 -> Bool
                 -> Atuple (Embed OpenAcc aenv) t
                 -> Atuple Use t
                 -> Elim OpenAcc aenv a
            elim _ _ True NilAtup NilAtup
              = ElimDead
            elim k _ False NilAtup NilAtup
              = k BaseEnv NilSub
            elim k v allDead (SnocAtup t (weaken v -> Embed env' a)) (SnocAtup ut u)
              = case elimA a u of
                  ElimDead     -> elim (\env'' sp -> k env'' (OutSub sp (sink env'' (inject (compute (Embed env' a)))))) v allDead t ut
                  ElimBind a'  -> elim (\env'' sp -> k env'' (InSub sp (AllSub (sink env'' (OpenAcc (bind env' a')))))) v False t ut
                  ElimEmbed    -> elim (\env'' sp -> k (env' `append` env'') (OutSub sp (sink env'' (inject (compute' a))))) (sink env' . v) False t ut
                  ElimTuple env1 t' ->
                    let env'' = env' `append` env1
                    in elim (\env''' sp -> k (env'' `append` env''') (InSub sp (TupleSub  (sinkSub env''' t')))) (sink env'' . v) False t ut

        sinkSub  :: Sink acc => Extend acc aenv aenv' -> Subproduct (acc aenv) t' t -> Subproduct (acc aenv') t' t
        sinkSub _   NilSub = NilSub
        sinkSub env (OutSub t' a) = OutSub (sinkSub env t') (sink env a)
        sinkSub env (InSub t' (AllSub a)) = InSub (sinkSub env t') (AllSub (sink env a))
        sinkSub env (InSub t' (TupleSub t)) = InSub (sinkSub env t') (TupleSub (sinkSub env t))

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
    Atuple tup          -> atupleD embedAcc tup
    Aforeign ff f a     -> done $ Aforeign ff (cvtAF f) (cvtA a)
    Collect s cs        -> collectD s cs

    -- Array injection
    Avar v              -> done $ Avar v
    Use arrs            -> done $ Use arrs
    Subarray sh ix arr  -> done $ Subarray (cvtE sh) (cvtE ix) arr
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
    Fold f z a          -> embed     (into2 Fold          (cvtF f) (cvtE z)) a
    Fold1 f a           -> embed     (into  Fold1         (cvtF f)) a
    FoldSeg f z a s     -> embed2    (into2 FoldSeg       (cvtF f) (cvtE z)) a s
    Fold1Seg f a s      -> embed2    (into  Fold1Seg      (cvtF f)) a s
    Scanl f z a         -> embed     (into2 Scanl         (cvtF f) (cvtE z)) a
    Scanl1 f a          -> embed     (into  Scanl1        (cvtF f)) a
    Scanl' f z a        -> embed     (into2 Scanl'        (cvtF f) (cvtE z)) a
    Scanr f z a         -> embed     (into2 Scanr         (cvtF f) (cvtE z)) a
    Scanr1 f a          -> embed     (into  Scanr1        (cvtF f)) a
    Scanr' f z a        -> embed     (into2 Scanr'        (cvtF f) (cvtE z)) a
    Permute f d p a     -> permuteD  (into2 permute       (cvtF f) (cvtF p)) d a
    Stencil f x a       -> stencilD  (into (stencil x)    (cvtF f)) a
    Stencil2 f x a y b  -> stencil2D (into (stencil2 x y) (cvtF f)) a b

  where
    -- If fusion is not enabled, force terms to the manifest representation
    --
    unembed :: Embed acc aenv arrs -> Embed acc aenv arrs
    unembed x
      | fuseAcc         = x
      | otherwise       = done (compute x)

    cvtA :: Arrays a => acc aenv' a -> acc aenv' a
    cvtA = computeAcc . embedAcc

    cvtAF :: PreOpenAfun acc aenv' f -> PreOpenAfun acc aenv' f
    cvtAF (Alam  f) = Alam  (cvtAF f)
    cvtAF (Abody a) = Abody (cvtA a)

    -- Helpers to shuffle the order of arguments to a constructor
    --
    permute f p d a     = Permute f d p a
    stencil x f a       = Stencil f x a
    stencil2 x y f a b  = Stencil2 f x a y b

    -- Helper functions for operations which require their argument arrays
    -- to be manifest. Without this we can get sequences like:
    --
    -- > stencil s (map f a)
    --
    -- rather than:
    --
    -- > let a' = map f a
    -- > in  stencil s a'
    --
    -- This way we retain the invariant that every time we expect
    -- a manifest array it will be in the form of an environment index.
    --
    stencilD :: (Arrays as, Arrays bs)
             => (forall aenv'. Extend acc aenv aenv' -> acc aenv' as -> PreOpenAcc acc aenv' bs)
             -> acc aenv as
             -> Embed acc aenv bs
    stencilD = trav1 bind

    stencil2D :: (Arrays as, Arrays bs, Arrays cs)
              => (forall aenv'. Extend acc aenv aenv' -> acc aenv' as -> acc aenv' bs -> PreOpenAcc acc aenv' cs)
              ->       acc aenv as
              ->       acc aenv bs
              -> Embed acc aenv cs
    stencil2D = trav2 bind bind

    permuteD :: (Arrays as, Arrays bs, Arrays cs)
              => (forall aenv'. Extend acc aenv aenv' -> acc aenv' as -> acc aenv' bs -> PreOpenAcc acc aenv' cs)
              ->       acc aenv as
              ->       acc aenv bs
              -> Embed acc aenv cs
    permuteD = trav2 bind id

    -- Conversions for closed scalar functions and expressions. This just
    -- applies scalar simplifications.
    --
    cvtF :: PreFun acc aenv t -> PreFun acc aenv t
    cvtF = simplify

    cvtE :: Elt t =>PreExp acc aenv' t -> PreExp acc aenv' t
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
    embed = trav1 id

    embed2 :: forall aenv as bs cs. (Arrays as, Arrays bs, Arrays cs)
           => (forall aenv'. Extend acc aenv aenv' -> acc aenv' as -> acc aenv' bs -> PreOpenAcc acc aenv' cs)
           ->       acc aenv as
           ->       acc aenv bs
           -> Embed acc aenv cs
    embed2 = trav2 id id

    trav1 :: (Arrays as, Arrays bs)
          => (forall aenv'. Embed acc aenv' as -> Embed acc aenv' as)
          -> (forall aenv'. Extend acc aenv aenv' -> acc aenv' as -> PreOpenAcc acc aenv' bs)
          ->       acc aenv as
          -> Embed acc aenv bs
    trav1 f op (f . embedAcc -> Embed env cc)
      = Embed (env `PushEnv` inject (op env (inject (compute' cc)))) (Done ZeroIdx)

    trav2 :: forall aenv as bs cs. (Arrays as, Arrays bs, Arrays cs)
          => (forall aenv'. Embed acc aenv' as -> Embed acc aenv' as)
          -> (forall aenv'. Embed acc aenv' bs -> Embed acc aenv' bs)
          -> (forall aenv'. Extend acc aenv aenv' -> acc aenv' as -> acc aenv' bs -> PreOpenAcc acc aenv' cs)
          ->       acc aenv as
          ->       acc aenv bs
          -> Embed acc aenv cs
    trav2 f1 f0 op (f1 . embedAcc -> Embed env1 cc1) (f0 . embedAcc . sink env1 -> Embed env0 cc0)
      | env     <- env1 `append` env0
      , acc1    <- inject . compute' $ sink env0 cc1
      , acc0    <- inject . compute' $ cc0
      = Embed (env `PushEnv` inject (op env acc1 acc0)) (Done ZeroIdx)

    bind :: Arrays as => Embed acc aenv' as -> Embed acc aenv' as
    bind (Embed env cc)
      | Done{} <- cc = Embed env                                  cc
      | otherwise    = Embed (env `PushEnv` inject (compute' cc)) (Done ZeroIdx)

    -- TODO: Sequence invariant code motion
    collectD :: PreOpenNaturalSeq acc aenv arrs
             -> Maybe (PreOpenChunkedSeq acc aenv arrs)
             -> Embed acc aenv arrs
    collectD s Nothing
      = Embed (BaseEnv `PushEnv` inject (Collect (embedSeq embedAcc s) Nothing)) (Done ZeroIdx)
    collectD s (Just cs)
      = Embed (BaseEnv `PushEnv` inject (Collect (embedSeq embedAcc s) (Just (embedSeq embedAcc cs)))) (Done ZeroIdx)

-- 2 steps
--
-- 1. Annotate computation with strongest environment and usage count of each producer
--
-- 2. For every producer, push down if unique, leave alone if not.
--

abstract :: aenv ::> aenv' -> aenv :> aenv'
abstract WeakBase       ix           = ix
abstract WeakEmpty      _            = $internalError "abstract" "Impossible index"
abstract (WeakIn _)     ZeroIdx      = ZeroIdx
abstract (WeakIn wenv)  (SuccIdx ix) = SuccIdx $ abstract wenv ix
abstract (WeakOut wenv) ix           = SuccIdx $ abstract wenv ix

data Count aenv where
  CountBase :: Count aenv
  CountPush :: Count aenv -> Int -> Count (aenv, a)

instance Monoid (Count aenv) where
  mempty = CountBase

  CountBase `mappend` CountBase = CountBase
  CountBase `mappend` CountPush c i = CountPush (CountBase <> c) i
  CountPush c i `mappend` CountBase = CountPush (c <> CountBase) i
  CountPush c i `mappend` CountPush c' i' = CountPush (c <> c') (i + i')


-- Proof that a is in aenv and aenv' is aenv with a removed.
data In a aenv aenv' where
  Here  :: In a (aenv,a) aenv
  There :: In a aenv aenv' -> In a (aenv,t) (aenv',t)

renameIn :: In a aenv aenv' -> aenv :> (aenv',a)
renameIn Here      ix           = ix
renameIn (There _) ZeroIdx      = SuccIdx ZeroIdx
renameIn (There i) (SuccIdx ix) = swap $ SuccIdx (renameIn i ix)
  where
    swap :: ((aenv,a),b) :> ((aenv,b),a)
    swap ZeroIdx                = SuccIdx ZeroIdx
    swap (SuccIdx ZeroIdx)      = ZeroIdx
    swap (SuccIdx (SuccIdx ix)) = SuccIdx (SuccIdx ix)

data DependentSeq index aenv arrs where
  Dproducer  :: Arrays a
             => aenv' ::> aenv                  -- Strongest possible environment
             -> Dproducer index aenv' a
             -> Int                             -- Usage count
             -> DependentSeq index (aenv,a) arrs
             -> DependentSeq index aenv     arrs

  Dconsumers :: (Elt index, Arrays arrs)
             => Dconsumer index aenv arrs
             -> DependentSeq index aenv arrs

  Dreify     :: Arrays a
             => aenv' ::> aenv
             -> OpenAcc aenv' a
             -> DependentSeq index aenv [a]

data Dproducer index aenv a where
  Dpull :: Source a
        -> Dproducer index aenv a

  DproduceAccum :: (Arrays a, Arrays s, Elt index)
                => Stream index s aenv a
                -> Dproducer index aenv a

data Stream index s aenv a where
  Stream :: Maybe (Exp aenv Int)
         -> OpenAfun aenv (Scalar index -> s -> (a,s))
         -> OpenAcc aenv s
         -> Stream index s aenv a

instance Sink (Dproducer index) where
  weaken _ (Dpull src) = Dpull src
  weaken v (DproduceAccum s) = DproduceAccum (weaken v s)

instance Sink (Stream index s) where
  weaken v (Stream l f s) = Stream (weaken v <$> l) (weaken v f) (weaken v s)

data Dconsumer index aenv arrs where
  Dtuple   :: (IsAtuple arrs, Arrays arrs)
           => Atuple (DependentSeq index aenv) (TupleRepr arrs)
           -> Dconsumer index aenv arrs

  Ddone    :: Arrays a
           => aenv' ::> aenv
           -> OpenAcc aenv' a
           -> aenv'' ::> aenv
           -> OpenAcc aenv'' a
           -> Dconsumer index aenv a

fuseSeq :: Elt index
        => PreOpenSeq index OpenAcc aenv arrs
        -> PreOpenSeq index OpenAcc aenv arrs
fuseSeq = deannotate . fuseDependent . makeDependent

deannotate :: DependentSeq index aenv arrs
           -> PreOpenSeq index OpenAcc aenv arrs
deannotate (Dproducer _ (Dpull src) _ ds)
  = Producer (Pull src) (deannotate ds)
deannotate (Dproducer wenv (DproduceAccum (Stream l f s)) _ ds)
  = Producer (weaken (abstract wenv) (ProduceAccum l f s)) (deannotate ds)
deannotate (Dconsumers cons)
  = Consumer $ dcons cons
  where
    dcons :: forall aenv index a. Elt index => Dconsumer index aenv a -> Consumer index OpenAcc aenv a
    dcons (Ddone env a envd d) = Conclude (weaken (abstract env) a) (weaken (abstract envd) d)
    dcons (Dtuple t) = Stuple (dtup t)
      where
        dtup :: Atuple (DependentSeq index aenv) arrs
             -> Atuple (PreOpenSeq index OpenAcc aenv) arrs
        dtup NilAtup = NilAtup
        dtup (t `SnocAtup` c) = dtup t `SnocAtup` deannotate c
deannotate (Dreify wenv a) = weaken (abstract wenv) $ Reify a

makeDependent :: Elt index
              => PreOpenSeq index OpenAcc aenv arrs
              -> DependentSeq index aenv arrs
makeDependent = fst . makeD
  where
    makeD :: Elt index
          => PreOpenSeq index OpenAcc aenv arrs
          -> (DependentSeq index aenv arrs, Count aenv)
    makeD (Producer p s)
      | Stronger env <- dependenciesProducer dependenciesOpenAcc p
      , (s', CountPush counts c) <- makeD s
      , p' <- makeP (weaken (inverse env) p)
      = (Dproducer env p' c s', counts <> count env)
    makeD (Consumer c) | (c' , counts) <- makeC c
      = (c', counts)
    makeD (Reify a)
      | Stronger env <- dependenciesOpenAcc a
      = (Dreify env (weaken (inverse env) a), count env)

    makeP :: Producer index OpenAcc aenv arrs -> Dproducer index aenv arrs
    makeP (Pull src) = Dpull src
    makeP (ProduceAccum l f a) = DproduceAccum (Stream l f a)
    makeP _ = $internalError "makeDependent" "AST is at incorrect stage for fusion"

    makeC :: Elt index
          => Consumer index OpenAcc aenv arrs
          -> (DependentSeq index aenv arrs, Count aenv)
    makeC (Iterate l f a)
      | Stronger env  <- dependenciesAfun dependenciesOpenAcc f
      , Stronger envd <- dependenciesOpenAcc a
      , p  <- DproduceAccum $ weaken (inverse env) (Stream l (streamify f) a)
      , a' <- weaken (inverse envd) a
      = (Dproducer env p 1 (Dconsumers (Ddone (WeakIn WeakEmpty) v0 (WeakOut envd) a'))
        , count env)
    makeC (Stuple t)
      | (t', counts) <- makeT t
      = (Dconsumers (Dtuple t'), counts)
      where
        makeT :: Elt index
              => Atuple (PreOpenSeq index OpenAcc aenv) arrs
              -> (Atuple (DependentSeq index aenv) arrs, Count aenv)
        makeT NilAtup = (NilAtup, CountBase)
        makeT (t `SnocAtup` c)
          | (t', counts) <- makeT t
          , (c', counts') <- makeD c
          = (t' `SnocAtup` c', counts <> counts')

    streamify :: OpenAfun aenv (index -> a -> a)
              -> OpenAfun aenv (index -> a -> (a,a))
    streamify (Alam (Alam (Abody a))) = Alam . Alam . Abody . OpenAcc $Alet a $ tuple v0 v0

    count :: aenv' ::> aenv -> Count aenv
    count WeakEmpty = CountBase
    count WeakBase = CountBase
    count (WeakIn v) = CountPush (count v) 1
    count (WeakOut v) = CountPush (count v) 0

    inverse :: aenv ::> aenv' -> aenv' :> aenv
    inverse WeakBase      ix           = ix
    inverse (WeakIn _)    ZeroIdx      = ZeroIdx
    inverse (WeakIn rix)  (SuccIdx ix) = SuccIdx (inverse rix ix)
    inverse (WeakOut rix) (SuccIdx ix) = inverse rix ix
    inverse _ _ = error "Reduced environment is not consistent with term"

fuseDependent :: DependentSeq index aenv arrs
              -> DependentSeq index aenv arrs
fuseDependent (Dproducer wenv p count ds)
  | count == 1
  , DproduceAccum s <- p
  = pushDown wenv s Here (fuseDependent ds)
  | otherwise
  = Dproducer wenv p count (fuseDependent ds)
fuseDependent (Dconsumers c)
  = Dconsumers c
fuseDependent (Dreify env a)
  = Dreify env a

pushDown :: forall index aenv out out' a s arrs. (Arrays a, Arrays s, Elt index)
         => out' ::> out
         -> Stream index s out' a
         -> In a aenv out
         -> DependentSeq index aenv arrs
         -> DependentSeq index out  arrs
pushDown wenv p inenv (Dproducer wenv' p' count ds)
  = case invariant inenv wenv' of
      Left wenv'' -> Dproducer wenv'' p' count (pushDown (WeakOut wenv) p (There inenv) ds)
      Right (StrongerIn wenvIn inenv')
        | StrongerUnion wenv1 wenv2 wenv3 <- wenv `strongerUnion` wenvIn
        , Insert wenv4 inenv'' <- insert inenv' wenv3
        , true_p <- weaken (abstract wenv2) p
        , true_p' <- weaken (abstract wenv4) p'
        -> Dproducer wenv1 (bindSInP inenv'' true_p true_p') count (pushDown (WeakOut wenv) p (There inenv) ds)
pushDown wenv p inenv (Dconsumers c)
   = pushC c
   where
     pushC :: forall arrs.
              Dconsumer index aenv arrs
           -> DependentSeq index out arrs
     pushC (Ddone wenv' a wenvd d)
       | Left wenvd' <- invariant inenv wenvd
       = case invariant inenv wenv' of
           Left wenv''
             -> Dconsumers (Ddone wenv'' a wenvd' d)
           Right (StrongerIn wenvIn inenv')
             -> Dproducer wenv (DproduceAccum p) 1 . Dconsumers
             $  Ddone (WeakIn wenvIn) (weaken (renameIn inenv') a) (WeakOut wenvd') d
     pushC (Dtuple t)
       = Dconsumers $ Dtuple (pushT t)
       where
         pushT :: Atuple (DependentSeq index aenv) t
               -> Atuple (DependentSeq index out) t
         pushT NilAtup = NilAtup
         pushT (t `SnocAtup` c) = pushT t `SnocAtup` pushDown wenv p inenv c

pushDown wenv p inenv (Dreify wenv' a)
  = Dproducer wenv (DproduceAccum p) 1
  $ case invariant inenv wenv' of
      Left wenv''
        -> Dreify (WeakOut wenv'') a
      Right (StrongerIn wenvIn inenv')
        -> Dreify (WeakIn wenvIn) (weaken (renameIn inenv') a)

strongerUnion :: aenv' ::> aenv -> aenv'' ::> aenv -> StrongerUnion aenv aenv' aenv''
strongerUnion WeakEmpty wenv = StrongerUnion wenv WeakEmpty WeakBase
strongerUnion wenv WeakEmpty = StrongerUnion wenv WeakBase WeakEmpty
strongerUnion WeakBase wenv = StrongerUnion WeakBase WeakBase wenv
strongerUnion wenv WeakBase = StrongerUnion WeakBase wenv WeakBase
strongerUnion (WeakIn wenv) (WeakIn wenv')
  | StrongerUnion wenv0 wenv1 wenv2 <- wenv `strongerUnion` wenv'
  = StrongerUnion (WeakIn wenv0) (WeakIn wenv1) (WeakIn wenv2)
strongerUnion (WeakIn wenv) (WeakOut wenv')
  | StrongerUnion wenv0 wenv1 wenv2 <- wenv `strongerUnion` wenv'
  = StrongerUnion (WeakIn wenv0) (WeakIn wenv1) (WeakOut wenv2)
strongerUnion (WeakOut wenv) (WeakIn wenv')
  | StrongerUnion wenv0 wenv1 wenv2 <- wenv `strongerUnion` wenv'
  = StrongerUnion (WeakIn wenv0) (WeakOut wenv1) (WeakIn wenv2)
strongerUnion (WeakOut wenv) (WeakOut wenv')
  | StrongerUnion wenv0 wenv1 wenv2 <- wenv `strongerUnion` wenv'
  = StrongerUnion (WeakOut wenv0) wenv1 wenv2


data Insert a aenv out where
  Insert :: aenv ::> aenv' -> In a aenv' out -> Insert a aenv out

insert :: In a aenv out -> out ::> out' -> Insert a aenv out'
insert inenv WeakBase  = Insert WeakBase inenv
insert Here WeakEmpty = Insert (WeakIn WeakEmpty) Here
insert _ WeakEmpty = error "Inaccessible"
insert inenv (WeakOut wenv)
  | Insert wenv' inenv' <- insert inenv wenv
  = Insert (WeakOut wenv') (There inenv')
insert Here (WeakIn wenv)
  = Insert (WeakIn (WeakIn wenv)) Here
insert (There inenv) (WeakIn wenv)
  | Insert wenv' inenv' <- insert inenv wenv
  = Insert (WeakIn wenv') (There inenv')

data StrongerUnion aenv aenv' aenv'' where
  StrongerUnion :: aenv''' ::> aenv
                -> aenv'   ::> aenv'''
                -> aenv''  ::> aenv'''
                -> StrongerUnion aenv aenv' aenv''

bindSInP :: (Arrays s, Arrays a)
         => In a aenv out
         -> Stream index s out a
         -> Dproducer index aenv arrs
         -> Dproducer index out  arrs
bindSInP inenv src (DproduceAccum target)
  = DproduceAccum $ fuseStreams src (weaken (renameIn inenv) target)
bindSInP _ _ (Dpull target)
  = Dpull target

fuseStreams :: (Elt index, Arrays s, Arrays s', Arrays a, Arrays arrs)
            => Stream index s      aenv     a
            -> Stream index s'     (aenv,a) arrs
            -> Stream index (s,s') aenv     arrs
fuseStreams (Stream l fun init) (Stream l' fun' init')
  = Stream (mergeLimits l (discardTop <$> l')) (pair fun fun') (tuple init (discardTop init'))
  where
    pair  :: (Elt index, Arrays a, Arrays b, Arrays b', Arrays arrs)
          => OpenAfun aenv (Scalar index -> b -> (a, b))
          -> OpenAfun (aenv,a) (Scalar index -> b' -> (arrs, b'))
          -> OpenAfun aenv (Scalar index -> (b,b') -> (arrs, (b, b')))
    pair f f'
      = Alam $ Alam $ Abody $
        alet (app2 (weaken (SuccIdx . SuccIdx) f) v1 (fstA v0))
      $ alet (app3 (weaken (SuccIdx . SuccIdx . SuccIdx) (Alam f')) (fstA v0) v2 (sndA v1))
      $ tuple (fstA v0) (tuple (sndA v1) (sndA v0))

    mergeLimits :: Maybe (Exp aenv Int) -> Maybe (Exp aenv Int) -> Maybe (Exp aenv Int)
    mergeLimits Nothing Nothing = Nothing
    mergeLimits Nothing (Just l) = Just l
    mergeLimits (Just l) Nothing = Just l
    mergeLimits (Just l) (Just l') = Just (min l l')
      where
        min :: Exp aenv Int -> Exp aenv Int -> Exp aenv Int
        min a b = PrimApp (PrimMin scalarType) (Tuple (NilTup `SnocTup` a `SnocTup` b))

    discardTop :: Rebuildable acc => acc (aenv,a) b -> acc aenv b
    discardTop acc = fromMaybe ($internalError "fuseStreams" "Unexpected reference to sequence variable")
                               (strengthen f acc)
      where
        f :: (aenv, a) :?> aenv
        f ZeroIdx = Nothing
        f (SuccIdx ix) = Just ix

v0 :: (Kit acc, Arrays a) => acc (aenv,a) a
v0 = inject (Avar ZeroIdx)

v1 :: Arrays a => OpenAcc ((aenv,a),b) a
v1 = OpenAcc (Avar (SuccIdx ZeroIdx))

v2 :: Arrays a => OpenAcc (((aenv,a),b),c) a
v2 = OpenAcc (Avar (SuccIdx (SuccIdx ZeroIdx)))

under :: aenv :> aenv' -> (aenv,a) :> (aenv',a)
under _ ZeroIdx      = ZeroIdx
under v (SuccIdx ix) = SuccIdx (v ix)

fstA :: (Arrays a, Arrays b)
    => OpenAcc aenv (a, b)
    -> OpenAcc aenv a
fstA = OpenAcc . Aprj (SuccTupIdx ZeroTupIdx)

sndA :: (Arrays a, Arrays b)
    => OpenAcc aenv (a, b)
    -> OpenAcc aenv b
sndA = OpenAcc . Aprj ZeroTupIdx

alet :: (Arrays a, Arrays b)
     => OpenAcc aenv a
     -> OpenAcc (aenv, a) b
     -> OpenAcc aenv b
alet a = OpenAcc . Alet a

app2 :: OpenAfun aenv (a -> b -> c)
     -> OpenAcc aenv a
     -> OpenAcc aenv b
     -> OpenAcc aenv c
app2 (Alam (Alam (Abody f))) a b = alet a $ alet (weaken SuccIdx b) f

app3 :: OpenAfun aenv (a -> b -> c -> d)
     -> OpenAcc aenv a
     -> OpenAcc aenv b
     -> OpenAcc aenv c
     -> OpenAcc aenv d
app3 (Alam (Alam (Alam (Abody f)))) a b c = alet a $ alet (weaken SuccIdx b) $ alet (weaken (SuccIdx . SuccIdx) c) f

tuple :: (Arrays a, Arrays b) => OpenAcc aenv a -> OpenAcc aenv b -> OpenAcc aenv (a,b)
tuple a b = OpenAcc (Atuple (NilAtup `SnocAtup` a `SnocAtup` b))

data StrongerIn a aenv out where
  StrongerIn :: out' ::> out -> In a aenv out' -> StrongerIn a aenv out

invariant :: In a aenv out -> aenv' ::> aenv -> Either (aenv' ::> out) (StrongerIn a aenv' out)
invariant _             WeakEmpty      = Left WeakEmpty
invariant Here          WeakBase       = Right (StrongerIn WeakBase Here)
invariant Here          (WeakIn wenv)  = Right (StrongerIn wenv Here)
invariant Here          (WeakOut wenv) = Left wenv
invariant (There inenv) WeakBase       = Right (StrongerIn WeakBase (There inenv))
invariant (There inenv) (WeakOut wenv) = case invariant inenv wenv of
                                           Left wenv' -> Left (WeakOut wenv')
                                           Right (StrongerIn wenv' inenv') -> Right (StrongerIn (WeakOut wenv') inenv')
invariant (There inenv) (WeakIn wenv)  = case invariant inenv wenv of
                                           Left wenv' -> Left (WeakIn wenv')
                                           Right (StrongerIn wenv' inenv') -> Right (StrongerIn (WeakIn wenv') (There inenv'))


-- Assume sequence operations themselves are already fused and just fuse array
-- operations.
--
embedSeq :: forall index acc aenv arrs. Kit acc
         => EmbedAcc acc
         -> PreOpenSeq index acc aenv arrs
         -> PreOpenSeq index acc aenv arrs
embedSeq embedAcc
  = travS
  where
    travS :: PreOpenSeq index acc aenv' arrs'
          -> PreOpenSeq index acc aenv' arrs'
    travS s =
      case s of
        Producer p s
          -> Producer (travP p) (travS s)
        Consumer c
          -> Consumer (travC c)
        Reify a
          -> Reify (cvtA a)

    travP :: Producer index acc aenv' arrs'
          -> Producer index acc aenv' arrs'
    travP (Pull s) = Pull s
    travP (ProduceAccum l f a) = ProduceAccum (cvtE <$> l) (cvtAF f) (cvtA a)

    travC :: Consumer index acc aenv' arrs'
          -> Consumer index acc aenv' arrs'
    travC (Conclude d a) = Conclude (cvtA d) (cvtA a)
    travC (Stuple t) = Stuple (cvtCT t)
      where
        cvtCT :: Atuple (PreOpenSeq index acc aenv') t -> Atuple (PreOpenSeq index acc aenv') t
        cvtCT NilAtup        = NilAtup
        cvtCT (SnocAtup t c) = SnocAtup (cvtCT t) (travS c)

    cvtE :: Elt t => PreExp acc aenv' t -> PreExp acc aenv' t
    cvtE = simplify

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

  -- A tuple of cunctations. In some cases, knowing that a tuple of arrays can
  -- be represented by the cunctations of its individual components affords
  -- more opportunities for optimisation.
  --
  Ctuple :: (IsAtuple a, Arrays a)
         => Atuple (Embed acc aenv) (TupleRepr a)
         -> Cunctation acc aenv a


instance Kit acc => Simplify (Cunctation acc aenv a) where
  simplify (Done v)        = Done v
  simplify (Yield sh f)    = Yield (simplify sh) (simplify f)
  simplify (Step sh p f v) = Step (simplify sh) (simplify p) (simplify f) v
  simplify (Ctuple t)      = Ctuple t


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
    _                                   -> error "yield: impossible case"


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
    _                                   -> error "step: impossible case"


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


instance Kit acc => Sink (Cunctation acc) where
  weaken k cc = case cc of
    Done v              -> Done (weaken k v)
    Step sh p f v       -> Step (weaken k sh) (weaken k p) (weaken k f) (weaken k v)
    Yield sh f          -> Yield (weaken k sh) (weaken k f)
    Ctuple t            -> Ctuple (wkET k t)
      where
        wkET :: aenv :> aenv' -> Atuple (Embed acc aenv) t -> Atuple (Embed acc aenv') t
        wkET _ NilAtup        = NilAtup
        wkET k (SnocAtup t c) = wkET k t `SnocAtup` weaken k c

instance Kit acc => Sink (Embed acc) where
  weaken k (Embed env cc) = wkE (\v env' -> Embed env' (weaken v cc)) k env
    where
      wkE :: (forall out'. aenv' :> out' -> Extend acc out out' -> Embed acc out a)
          -> aenv :> out
          -> Extend acc aenv aenv'
          -> Embed acc out a
      wkE f v BaseEnv = f v BaseEnv
      wkE f v (PushEnv env a) = wkE (\v' env' -> f (under v') (env' `PushEnv` weaken v' a)) v env

-- Tuple manipulation
-- ==================

-- Note: [Tuple manipulation]
--
-- As a part of the embedding stage, we need to be able to transform tuples and
-- other product types, splitting out those components that should be embedded
-- from those that should remain as part of the product. Unfortunately, due to
-- the way product types are represented in Accelerate, with a non injective
-- relationship between surface types and representation types, this causes
-- problems. Supposing we have a tuple like so
--
--   (a,b,c)
--
-- then suppose we want to pull b out of it, leaving us with (a,c). However,
-- the only way we can inspect the structure of a product is via its
-- representation type. That means we take
--
-- ((((),a),b),c)
--
-- and product (((),a),c). But what is the surface type corresponding to this
-- representation type?
--
-- FreeProd is a product type that gives a surface type for any product
-- representation type. That is, for all t, FreeProd (ProdRepr t) is a valid
-- product type. Additionally, for all t', ProdRepr (FreeProd t') ~ t'. This
-- gives us what we need in order to transform product types.
--

-- The free product. A surface product type for any given product representation
-- tyoe.
--
data FreeProd t where
  NilFreeProd  :: FreeProd ()
  SnocFreeProd :: Arrays s => FreeProd t -> s -> FreeProd (t,s)
  deriving ( Typeable )

instance IsProduct Arrays (FreeProd ()) where
  type ProdRepr (FreeProd ()) = ()
  fromProd _ _ = ()
  toProd _ _ = NilFreeProd
  prod _ _ = ProdRunit

instance (IsProduct Arrays (FreeProd t), Arrays s) => IsProduct Arrays (FreeProd (t,s)) where
  type ProdRepr (FreeProd (t,s)) = (ProdRepr (FreeProd t), s)
  fromProd cst (SnocFreeProd t s) = (fromProd cst t, s)
  toProd cst (t,s) = SnocFreeProd (toProd cst t) s
  prod cst _ = ProdRsnoc (prod cst (undefined :: FreeProd t))

type instance ArrRepr (FreeProd (t,a)) = (ArrRepr (FreeProd t), ArrRepr a)
type instance ArrRepr (FreeProd ())    = ((),())

instance (IsAtuple (FreeProd t), Typeable t, Arrays (FreeProd t), Arrays a) => Arrays (FreeProd (t,a)) where
  arrays  _ = arrays (undefined :: FreeProd t) `ArraysRpair` arrays (undefined :: a)
  flavour _ = ArraysFtuple
  --
  toArr (t,a) = SnocFreeProd (toArr t) (toArr a)
  fromArr (SnocFreeProd t a) = (fromArr t, fromArr a)

instance Arrays (FreeProd ()) where
  arrays  _ = ArraysRpair ArraysRunit ArraysRunit
  flavour _ = ArraysFtuple
  --
  toArr   _ = NilFreeProd
  fromArr _ = ((),())

-- Unofortunately, the properties that hold for all array tuple representations
-- GHCs typechecker cannot infer.
--
type IsAtupleRepr t = (Arrays (FreeProd t), Typeable t, IsAtuple (FreeProd t), t ~ ProdRepr (FreeProd t))

-- Subproduct captures that a product representation t' can be extracted from
-- product representation t.
--
data Subproduct k t' t where
  NilSub :: Subproduct k () ()
  InSub  :: (Arrays a, Arrays a') => Subproduct k t' t -> Subcomponent k a' a -> Subproduct k (t',a') (t,a)
  OutSub :: Arrays a => Subproduct k t' t -> k a -> Subproduct k t' (t,a)

-- Similar to above, this captures that a component of a tuple a contains a
-- "smaller" component a'.
--
data Subcomponent k a' a where
  AllSub   :: k a -> Subcomponent k a a
  TupleSub :: (IsAtupleRepr t', IsAtuple a)
           => Subproduct k t' (TupleRepr a)
           -> Subcomponent k (FreeProd t') a

-- Given a sub-product, we can generate a term that embeds all components not
-- in it and references those that are.
--
fromSubproduct :: Kit acc
               => Subproduct (acc aenv) t' t
               -> Atuple (acc (aenv, FreeProd t')) t
fromSubproduct sp | Dict <- witness sp
                  = fromSubproduct (inject . flip Aprj v0) sp
  where
    -- We have to do some trickery with a continuation in order to deal with
    -- nested tuples.
    fromSubproduct :: (Kit acc, IsAtuple (FreeProd t''), Arrays (FreeProd t''))
                    => (forall a. Arrays a => TupleIdx t' a -> acc (aenv, FreeProd t'') a)
                    -> Subproduct (acc aenv) t' t
                    -> Atuple (acc (aenv, FreeProd t'')) t
    fromSubproduct _ NilSub             = NilAtup
    fromSubproduct k (InSub sp (AllSub _))
      = SnocAtup (fromSubproduct (k . SuccTupIdx) sp) (k ZeroTupIdx)
    fromSubproduct k (InSub sp (TupleSub ts))
      | at <- fromSubproduct (inject . flip Aprj (k ZeroTupIdx)) ts
      = SnocAtup (fromSubproduct (k . SuccTupIdx) sp) (inject $ Atuple at)
    fromSubproduct k (OutSub sp a)
      = SnocAtup (fromSubproduct k sp) (weaken SuccIdx a)

    -- We need to peek under the subproduct structure to get a witness that the
    -- sub products is a valid product representation.
    --
    witness :: Subproduct k t' t -> Dict (IsAtupleRepr t')
    witness NilSub = Dict
    witness (InSub (witness -> Dict) _) = Dict
    witness (OutSub (witness -> Dict) _) = Dict


subproduct :: Kit acc => Subproduct (acc aenv) t' t -> Atuple (acc aenv) t'
subproduct NilSub = NilAtup
subproduct (InSub t (AllSub a)) = subproduct t `SnocAtup` a
subproduct (InSub t (TupleSub t')) = subproduct t `SnocAtup` inject (Atuple (subproduct t'))
subproduct (OutSub t _) = subproduct t


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
  Ctuple t                                      -> Atuple (cvtCT t)
  where
    cvtCT :: Kit acc => Atuple (Embed acc aenv) t -> Atuple (acc aenv) t
    cvtCT NilAtup = NilAtup
    cvtCT (SnocAtup t c) = cvtCT t `SnocAtup` inject (compute c)


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
    :: (Kit acc, Shape sh, Shape sl, Slice slix, Elt e)
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
    :: (Kit acc, Shape sh, Shape sl, Slice slix, Elt e)
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

  -- dead-code elimination
  -- ---------------------
  --
  -- If the binding is not used at all then get rid of it entirely. While the
  -- let-elimination below deals with most dead code cases, it only works if the
  -- bound term is not manifest.
  --
  | Just acc0' <- strengthen noTop acc0
  = Stats.ruleFired "aletD/dead" embedAcc acc0'

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

  where
    noTop :: (aenv,a) :?> aenv
    noTop ZeroIdx = Nothing
    noTop (SuccIdx ix) = Just ix


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
  -- Check whether we can eliminate the let-binding.
  --
  -- If no component of the binding can be eliminated, avoid needlessly
  -- traversing the body.
  --
  | noEliminations elim
  , acc1                <- compute (Embed env1 cc1)
  = Stats.ruleFired "aletD/bindAll"
  $ Embed (BaseEnv `PushEnv` inject acc1 `append` env0) cc0

  -- let-elimination
  -- ---------------
  --
  -- Handle the remaining cases in a separate function. It turns out that this
  -- is important so we aren't excessively sinking/delaying terms.
  --
  | otherwise
  = Stats.ruleFired "aletD/eliminateSome"
  $ eliminate elim env1 cc1 acc0

  where
    acc0 :: acc (aenv, arrs) brrs
    acc0 = computeAcc (Embed env0 cc0)

    elim = elimAcc cc1 acc0

    noEliminations :: forall aenv a. Elim acc aenv a -> Bool
    noEliminations (ElimBind _)    = True
    noEliminations ElimDead        = False
    noEliminations ElimEmbed       = False
    noEliminations (ElimTuple _ t) = tup t
      where
        tup :: Subproduct k t' t -> Bool
        tup NilSub = True
        tup (InSub t (AllSub _)) = tup t
        tup (InSub t (TupleSub t')) = tup t && tup t'
        tup (OutSub _ _) = False

    -- The second part of let-elimination. Splitting into two steps exposes the
    -- extra type variables, and ensures we don't do extra work manipulating the
    -- body when not necessary (which can lead to a complexity blowup).
    --
    eliminate :: forall aenv aenv' t brrs. (Kit acc, Arrays brrs, Arrays t)
              => Elim acc aenv' t
              -> Extend     acc aenv aenv'
              -> Cunctation acc      aenv' t
              ->            acc      (aenv, t) brrs
              -> Embed      acc aenv           brrs
    eliminate elim env1 cc1 body
      | ElimTuple env1' sp <- elim
      , t'' <- fromSubproduct sp
      , body' <- rebuildA (subAtop (Atuple t'')) (weaken (under SuccIdx) (sink1 (env1 `append` env1') body))
      , Embed env0' cc0' <- embedAcc . inject $ Alet (inject (Atuple (subproduct sp))) body'
      = Embed (env1 `append` env1' `append` env0') cc0'
      | ElimDead <- elim
      = embedAcc $ rebuildA (subAtop (compute (Embed env1 cc1))) body
      | ElimEmbed <- elim
      , Embed env0' cc0' <- embedAcc $ rebuildA (subAtop bnd) $ kmap (replaceA (weaken SuccIdx cc1) ZeroIdx) (sink1 env1 body)
      = Embed (env1 `append` env0') cc0'
      where
        bnd :: PreOpenAcc acc aenv' t
        bnd = compute' cc1

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
        IndexSlice x ix sh              -> IndexSlice x ix (cvtE sh)
        IndexFull x ix sl               -> IndexFull x (cvtE ix) (cvtE sl)
        ToIndex sh ix                   -> ToIndex (cvtE sh) (cvtE ix)
        FromIndex sh i                  -> FromIndex (cvtE sh) (cvtE i)
        ToSlice x sh ix                 -> ToSlice x (cvtE sh) (cvtE ix)
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

    replaceA :: forall aenv t a. (Kit acc, Arrays t)
             => Cunctation acc aenv t -> Idx aenv t
             -> PreOpenAcc acc aenv a
             -> PreOpenAcc acc aenv a
    replaceA cunc avar pacc =
      case pacc of
        Avar v
          | Just REFL <- match v avar   -> Avar avar
          | otherwise                   -> Avar v

        Alet bnd body                   ->
          let cunc' = weaken SuccIdx cunc
          in
          Alet (cvtA bnd) (kmap (replaceA cunc' (SuccIdx avar)) body)

        Use arrs                -> Use arrs
        Subarray sh ix arr      -> Subarray (cvtE sh) (cvtE ix) arr
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
        Collect s cs            -> Collect (replaceSeq cunc avar s) (replaceSeq cunc avar <$> cs)

      where
        cvtA :: acc aenv s -> acc aenv s
        cvtA = kmap (replaceA cunc avar)

        cvtAF :: PreOpenAfun acc aenv s -> PreOpenAfun acc aenv s
        cvtAF = cvt cunc avar
          where
            cvt :: forall aenv a.
                   Cunctation acc aenv t -> Idx aenv t
                -> PreOpenAfun acc aenv a
                -> PreOpenAfun acc aenv a
            cvt cunc avar' (Abody a) = Abody $ kmap (replaceA cunc avar') a
            cvt cunc avar' (Alam af) = Alam $ cvt (weaken SuccIdx cunc)
                                                  (SuccIdx avar')
                                                  af

        cvtE :: PreExp acc aenv s -> PreExp acc aenv s
        cvtE = assumeArray cunc (\sh f ix -> replaceE sh f ix . reduceAccessExp ix) (const id) avar

        cvtF :: PreFun acc aenv s -> PreFun acc aenv s
        cvtF = assumeArray cunc (\sh f ix -> replaceF sh f ix . reduceAccessFun ix) (const id) avar

        cvtAT :: Atuple (acc aenv) s -> Atuple (acc aenv) s
        cvtAT NilAtup          = NilAtup
        cvtAT (SnocAtup tup a) = cvtAT tup `SnocAtup` cvtA a

    replaceSeq :: forall index aenv t t'. (Kit acc, Arrays t')
               => Cunctation acc aenv t' -> Idx aenv t'
               -> PreOpenSeq index acc aenv t -> PreOpenSeq index acc aenv t
    replaceSeq cunc avar s =
      case s of
        Producer p s' ->
          Producer
            (case p of
               Pull s -> Pull s
               ProduceAccum l f a -> ProduceAccum (cvtE <$> l) (cvtAF f) (cvtA a))
            (replaceSeq (weaken SuccIdx cunc) (weaken SuccIdx avar) s')
        Consumer c ->
          Consumer (cvtC c)
        Reify a -> Reify (cvtA a)

      where
        cvtC :: Consumer index acc aenv s -> Consumer index acc aenv s
        cvtC c =
          case c of
            Conclude a d -> Conclude (cvtA a) (cvtA d)
            Stuple t     -> Stuple (cvtCT t)

        cvtCT :: Atuple (PreOpenSeq index acc aenv) s -> Atuple (PreOpenSeq index acc aenv) s
        cvtCT NilAtup        = NilAtup
        cvtCT (SnocAtup t c) = SnocAtup (cvtCT t) (replaceSeq cunc avar c)

        cvtA :: acc aenv s -> acc aenv s
        cvtA = kmap (replaceA cunc avar)

        cvtAF :: PreOpenAfun acc aenv s -> PreOpenAfun acc aenv s
        cvtAF = cvt cunc avar
          where
            cvt :: forall aenv a.
                   Cunctation acc aenv t' -> Idx aenv t'
                -> PreOpenAfun acc aenv a
                -> PreOpenAfun acc aenv a
            cvt cunc' avar' (Abody a) = Abody $ kmap (replaceA cunc' avar') a
            cvt cunc' avar' (Alam af) = Alam $ cvt (weaken SuccIdx cunc')
                                                   (SuccIdx avar')
                                                   af

        cvtE :: PreExp acc aenv s -> PreExp acc aenv s
        cvtE = assumeArray cunc (\sh f ix -> replaceE sh f ix . reduceAccessExp ix) (const id) avar

    assumeArray :: forall aenv t a. Arrays t
                => Cunctation acc aenv t
                -> (forall sh e. (t ~ Array sh e, Shape sh, Elt e) => PreExp acc aenv sh -> PreFun acc aenv (sh -> e) -> a)
                -> a
                -> a
    assumeArray (Done v1) k _
      | ArraysFarray <- flavour (undefined :: t)
      = k (arrayShape v1) (indexArray v1)
    assumeArray (Step sh1 p1 f1 v1) k _
      = k sh1 (f1 `compose` indexArray v1 `compose` p1)
    assumeArray (Yield sh1 f1) k _
      = k sh1 f1
    assumeArray _ _ a
      = a

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
  | Embed env cc <- embedAcc a
  = case cc of
      Ctuple t | Embed env' t' <- aprjAT ix t
               -> Stats.ruleFired "aprj/Atuple" $ Embed (env `append` env') t'
      _        -> done . Aprj ix . computeAcc $ Embed env cc
  where
    aprjAT :: forall acc aenv atup. TupleIdx atup a -> Atuple (acc aenv) atup -> acc aenv a
    aprjAT ZeroTupIdx      (SnocAtup _ a) = a
    aprjAT (SuccTupIdx ix) (SnocAtup t _) = aprjAT ix t

-- Array tuple construction. Ideally we do not want tuple construction to act as
-- a barrier to fusion. For example,
--
--   let t = (generate ..., generate ...)
--   in zipWith f (fst t) (snd t)
--
-- should get fused. In general however, it is dangerous to always fuse code of
-- this form. Suppose we have this,
--
--   let t = (let a = k in generate ..., generate ...)
--   in zipWith f (fst t) (snd t)
--
-- In this case, we cannot perform fusion without floating k out of its scope,
-- causing it to be resident in memory for longer than previously.
--
-- As a result of this we are conservative in our fusion through tuples and only
-- perform fusion when k has zero space-cost. We consider tuple projection and
-- variables to have zero space cost, as well as tuple construction and let
-- bindings when their subterms also have no cost.
--
atupleD :: forall acc aenv a. (Kit acc, Arrays a, IsAtuple a)
        => EmbedAcc acc
        -> Atuple (acc aenv) (TupleRepr a)
        -> Embed acc aenv a
atupleD embedAcc t = Embed BaseEnv (Ctuple (cvtET t))
  where
    cvtET :: Atuple (acc aenv)       t
          -> Atuple (Embed acc aenv) t
    cvtET NilAtup = NilAtup
    cvtET (SnocAtup t a) = SnocAtup (cvtET t) (embedAcc a)

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

extend :: (Kit acc, Shape sh, Shape sl, Slice slix)
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
    IndexSlice _ _ sh  -> simpleExp sh
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
