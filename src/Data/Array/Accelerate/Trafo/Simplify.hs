{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternGuards        #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns         #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo.Simplify
-- Copyright   : [2012..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Trafo.Simplify (

  Simplify(..),

) where

-- standard library
import Control.Applicative                              hiding ( Const )
import Control.Lens                                     hiding ( Const, ix )
import Data.Maybe
import Data.Monoid
import Data.Typeable
import Text.Printf
import Prelude                                          hiding ( exp, iterate )

-- friends
import Data.Array.Accelerate.AST                        hiding ( prj )
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Trafo.Algebra
import Data.Array.Accelerate.Trafo.Base
import Data.Array.Accelerate.Trafo.Shrink
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Representation       ( Array, shapeToList )
import qualified Data.Array.Accelerate.Debug.Stats      as Stats
import qualified Data.Array.Accelerate.Debug.Flags      as Debug
import qualified Data.Array.Accelerate.Debug.Trace      as Debug


class Simplify f where
  simplify :: f -> f

instance Kit acc => Simplify (PreFun acc aenv f) where
  simplify = simplifyFun

instance Kit acc => Simplify (PreExp acc aenv e) where
  simplify = simplifyExp


-- Scalar optimisations
-- ====================

{--
-- Common subexpression elimination finds computations that are performed at
-- least twice on a given execution path and eliminates the second and later
-- occurrences, replacing them with uses of saved values. This implements a
-- simplified version of that idea, where we look for the expressions of the
-- form:
--
--   let x = e1 in e2
--
-- and replace all occurrences of e1 in e2 with x. This is not full redundancy
-- elimination, but good enough to catch some cases, and in particular those
-- likely to be introduced by scalar composition of terms in the fusion process.
--
-- While it may seem that common subexpression elimination is always worthwhile,
-- as it reduces the number of arithmetic operations performed, this is not
-- necessarily advantageous. The simplest case in which it may not be desirable
-- is if it causes a register to be occupied for a long time in order to hold
-- the shared expression's value, which hence reduces the number of registers
-- available for other uses. Even worse is if the value has to be spilled to
-- memory because there are insufficient registers available. We sidestep this
-- tricky and target-dependent issue by, for now, simply ignoring it.
--
localCSE :: (Kit acc, Elt a)
         => Gamma      acc env env aenv
         -> PreOpenExp acc env     aenv a
         -> PreOpenExp acc (env,a) aenv b
         -> Maybe (PreOpenExp acc env aenv b)
localCSE env bnd body
  | Just ix <- lookupExp env bnd = Stats.ruleFired "CSE" . Just $ inline body (Var ix)
  | otherwise                    = Nothing
--}
{--
-- Common subexpression elimination, which attempts to match the given
-- expression against something already bound in the environment. This can occur
-- due to simplification, in which case we replace the entire subterm with x.
--
-- > let x = e in .. e ..
--
globalCSE :: (Kit acc, Elt t)
          => Gamma      acc env env aenv
          -> PreOpenExp acc env     aenv t
          -> Maybe (PreOpenExp acc env aenv t)
globalCSE env exp
  | Just ix <- lookupExp env exp = Stats.ruleFired "CSE" . Just $ Var ix
  | otherwise                    = Nothing
--}

{--
-- Compared to regular Haskell, the scalar expression language of Accelerate is
-- rather limited in order to meet the restrictions of what can be efficiently
-- implemented on specialised hardware, such as GPUs. For example, to avoid
-- excessive SIMD divergence, we do not support any form of recursion or
-- iteration in scalar expressions. This harmonises well with the stratified
-- design of the Accelerate language: collective array operations comprise many
-- scalar computations that are executed in parallel, so for simplicity of
-- scheduling these operations we would like some assurance that each scalar
-- computation takes approximately the same time to execute as all others.
--
-- However, some computations are naturally expressed in terms of iteration. For
-- some problems, we can instead use generative techniques to implement the
-- program by defining a single step of a recurrence relation as an Accelerate
-- collective operation and using standard Haskell to unroll the loop a _fixed_
-- number of times.
--
-- However, this is outrageously slow because the intermediate values are
-- written to memory at the end of every iteration. Luckily the fusion process
-- will eliminate this intermediate memory traffic by combining the 'n'
-- collective operations into a single operation with 'n' instances of the loop
-- body. However, doing this we uncover an embarrassing secret: C compilers do
-- not compile C code, they compile _idiomatic_ C code.
--
-- This process recovers the iteration structure that was lost in the process of
-- fusing the collective operations. This allows a backend to generate explicit
-- loops in its target language.
--
recoverLoops
    :: (Kit acc, Elt b)
    => Gamma      acc env env aenv
    -> PreOpenExp acc env     aenv a
    -> PreOpenExp acc (env,a) aenv b
    -> Maybe (PreOpenExp acc env aenv b)
recoverLoops _ bnd e3
  -- To introduce scaler loops, we look for expressions of the form:
  --
  --   let x =
  --     let y = e1 in e2
  --   in e3
  --
  -- and if e2 and e3 are congruent, replace with:
  --
  --   iterate[2] (\y -> e2) e1
  --
  | Let e1 e2           <- bnd
  , Just Refl           <- matchEnvTop e2 e3
  , Just Refl           <- match e2 e3
  = Stats.ruleFired "loop recovery/intro" . Just
  $ Iterate (constant 2) e2 e1

  -- To merge expressions into a loop body, look for the pattern:
  --
  --   let x = iterate[n] f e1
  --   in e3
  --
  -- and if e3 matches the loop body, replace the let binding with the bare
  -- iteration with the trip count increased by one.
  --
  | Iterate n f e1      <- bnd
  , Just Refl           <- match f e3
  = Stats.ruleFired "loop recovery/merge" . Just
  $ Iterate (constant 1 `plus` n) f e1

  | otherwise
  = Nothing

  where
    plus :: PreOpenExp acc env aenv Int -> PreOpenExp acc env aenv Int -> PreOpenExp acc env aenv Int
    plus x y = PrimApp (PrimAdd numType) $ Tuple $ NilTup `SnocTup` x `SnocTup` y

    constant :: Int -> PreOpenExp acc env aenv Int
    constant i = Const ((),i)

    matchEnvTop :: (Elt s, Elt t)
                => PreOpenExp acc (env,s) aenv f
                -> PreOpenExp acc (env,t) aenv g
                -> Maybe (s :=: t)
    matchEnvTop _ _ = gcast Refl
--}


-- Walk a scalar expression applying simplifications to terms bottom-up.
--
-- TODO: Look for particular patterns of expressions that can be replaced by
--       something equivalent and simpler. In particular, indexing operations
--       introduced by the fusion transformation. This would benefit from a
--       rewrite rule schema.
--
-- TODO: We currently pass around an environment Gamma, but we do not use it.
--       It might be helpful to do some inlining if this enables other optimizations.
--       Eg, for `let x = -y in -x`, the inlining would allow us to shorten it to `y`.
--       If we do not want to do inlining, we should remove the environment here.
simplifyOpenExp
    :: forall acc env aenv e. (Kit acc)
    => Gamma acc env env aenv
    -> PreOpenExp acc env aenv e
    -> (Bool, PreOpenExp acc env aenv e)
simplifyOpenExp env = first getAny . cvtE
  where
    cvtE :: PreOpenExp acc env aenv t -> (Any, PreOpenExp acc env aenv t)
    cvtE exp = case exp of
      Let lhs@(LeftHandSideSingle _) bnd body
        -- Just reduct <- recoverLoops env (snd bnd') (snd body') -> yes . snd $ cvtE reduct
        -- Just reduct <- localCSE     env (snd bnd') (snd body') -> yes . snd $ cvtE reduct
        | otherwise -> Let lhs <$> bnd' <*> body'
        where
          bnd'  = cvtE bnd
          env'  = env `pushExp` snd bnd'
          body' = cvtE' (incExp env') body
      Let lhs bnd body -> Let lhs <$> bnd' <*> body'
        where
          bnd'  = cvtE bnd
          env'  = lhsExpr lhs env
          body' = cvtE' env' body

      Evar var                  -> pure $ Evar var
      Const tp c                -> pure $ Const tp c
      Undef tp                  -> pure $ Undef tp
      Nil                       -> pure Nil
      Pair e1 e2                -> Pair <$> cvtE e1 <*> cvtE e2
      IndexSlice x ix sh        -> IndexSlice x <$> cvtE ix <*> cvtE sh
      IndexFull x ix sl         -> IndexFull x <$> cvtE ix <*> cvtE sl
      ToIndex shr sh ix         -> toIndex shr (cvtE sh) (cvtE ix)
      FromIndex shr sh ix       -> fromIndex shr (cvtE sh) (cvtE ix)
      Cond p t e                -> cond (cvtE p) (cvtE t) (cvtE e)
      PrimConst c               -> pure $ PrimConst c
      PrimApp f x               -> (u<>v, fx)
        where
          (u, x') = cvtE x
          (v, fx) = evalPrimApp env f x'
      Index a sh                -> Index a <$> cvtE sh
      LinearIndex a i           -> LinearIndex a <$> cvtE i
      Shape a                   -> shape a
      ShapeSize shr sh          -> shapeSize shr (cvtE sh)
      Foreign ff f e            -> Foreign ff <$> first Any (simplifyOpenFun EmptyExp f) <*> cvtE e
      While p f x               -> While <$> cvtF env p <*> cvtF env f <*> cvtE x
      Coerce t1 t2 e            -> Coerce t1 t2 <$> cvtE e

    cvtE' :: Gamma acc env' env' aenv -> PreOpenExp acc env' aenv e' -> (Any, PreOpenExp acc env' aenv e')
    cvtE' env' = first Any . simplifyOpenExp env'

    cvtF :: Gamma acc env' env' aenv -> PreOpenFun acc env' aenv f -> (Any, PreOpenFun acc env' aenv f)
    cvtF env' = first Any . simplifyOpenFun env'

    -- Simplify conditional expressions, in particular by eliminating branches
    -- when the predicate is a known constant.
    --
    cond :: forall t.
            (Any, PreOpenExp acc env aenv Bool)
         -> (Any, PreOpenExp acc env aenv t)
         -> (Any, PreOpenExp acc env aenv t)
         -> (Any, PreOpenExp acc env aenv t)
    cond p@(_,p') t@(_,t') e@(_,e')
      | Const _ True  <- p'        = Stats.knownBranch "True"      (yes t')
      | Const _ False <- p'        = Stats.knownBranch "False"     (yes e')
      | Just Refl <- match t' e' = Stats.knownBranch "redundant" (yes e')
      | otherwise                = Cond <$> p <*> t <*> e


    -- Shape manipulations
    --
    shape :: forall sh t. acc aenv (Array sh t) -> (Any, PreOpenExp acc env aenv sh)
    shape a
      | ArrayR ShapeRz _ <- arrayRepr a
        = Stats.ruleFired "shape/Z" $ yes Nil
    shape a
        = pure $ Shape a

    shapeSize :: forall sh. ShapeR sh -> (Any, PreOpenExp acc env aenv sh) -> (Any, PreOpenExp acc env aenv Int)
    shapeSize shr (_, extractConstTuple -> Just c) = Stats.ruleFired "shapeSize/const" $ yes (Const scalarTypeInt (product (shapeToList shr c)))
    shapeSize shr sh           = ShapeSize shr <$> sh

    toIndex :: forall sh. ShapeR sh -> (Any, PreOpenExp acc env aenv sh) -> (Any, PreOpenExp acc env aenv sh) -> (Any, PreOpenExp acc env aenv Int)
    toIndex _ (_,sh) (_,FromIndex _ sh' ix)
      | Just Refl <- match sh sh' = Stats.ruleFired "toIndex/fromIndex" $ yes ix
    toIndex shr sh ix             = ToIndex shr <$> sh <*> ix

    fromIndex :: forall sh. ShapeR sh -> (Any, PreOpenExp acc env aenv sh) -> (Any, PreOpenExp acc env aenv Int) -> (Any, PreOpenExp acc env aenv sh)
    fromIndex _ (_,sh) (_,ToIndex _ sh' ix)
      | Just Refl <- match sh sh' = Stats.ruleFired "fromIndex/toIndex" $ yes ix
    fromIndex shr sh ix           = FromIndex shr <$> sh <*> ix

    first :: (a -> a') -> (a,b) -> (a',b)
    first f (x,y) = (f x, y)

    yes :: x -> (Any, x)
    yes x = (Any True, x)

extractConstTuple :: PreOpenExp acc env aenv t -> Maybe t
extractConstTuple Nil          = Just ()
extractConstTuple (Pair e1 e2) = (,) <$> extractConstTuple e1 <*> extractConstTuple e2
extractConstTuple (Const _ c)  = Just c
extractConstTuple _            = Nothing

-- Simplification for open functions
--
simplifyOpenFun
    :: Kit acc
    => Gamma acc env env aenv
    -> PreOpenFun acc env aenv f
    -> (Bool, PreOpenFun acc env aenv f)
simplifyOpenFun env (Body e)    = Body    <$> simplifyOpenExp env  e
simplifyOpenFun env (Lam lhs f) = Lam lhs <$> simplifyOpenFun env' f
  where
    env' = lhsExpr lhs env

lhsExpr :: Kit acc => ELeftHandSide t env env' -> Gamma acc env env aenv -> Gamma acc env' env' aenv
lhsExpr (LeftHandSideWildcard _) env = env
lhsExpr (LeftHandSideSingle  tp) env = incExp env `pushExp` Evar (Var tp ZeroIdx)
lhsExpr (LeftHandSidePair l1 l2) env = lhsExpr l2 $ lhsExpr l1 env

-- Simplify closed expressions and functions. The process is applied
-- repeatedly until no more changes are made.
--
simplifyExp :: Kit acc => PreExp acc aenv t -> PreExp acc aenv t
simplifyExp = iterate summariseOpenExp (simplifyOpenExp EmptyExp)

simplifyFun :: Kit acc => PreFun acc aenv f -> PreFun acc aenv f
simplifyFun = iterate summariseOpenFun (simplifyOpenFun EmptyExp)


-- NOTE: [Simplifier iterations]
--
-- Run the simplification pass _before_ the shrinking step. There are cases
-- where it is better to run shrinking first, and then simplification would
-- complete in a single step, but the converse is also true. However, as
-- shrinking can remove some structure of the let bindings, which might be
-- useful for the transformations (e.g. loop recovery) we want to maintain this
-- information for at least the first pass.
--
-- We always apply the simplification step once. Following this, we iterate
-- shrinking and simplification until the expression no longer changes. Both
-- shrink and simplify return a boolean indicating whether any work was done; we
-- stop as soon as either returns false.
--
-- With internal checks on, we also issue a warning if the iteration limit is
-- reached, but it was still possible to make changes to the expression.
--
{-# SPECIALISE iterate :: (Exp aenv t -> Stats) -> (Exp aenv t -> (Bool, Exp aenv t)) -> Exp aenv t -> Exp aenv t #-}
{-# SPECIALISE iterate :: (Fun aenv t -> Stats) -> (Fun aenv t -> (Bool, Fun aenv t)) -> Fun aenv t -> Fun aenv t #-}

iterate
    :: forall f a. (Match f, Shrink (f a))
    => (f a -> Stats)
    -> (f a -> (Bool, f a))
    -> f a
    -> f a
iterate summarise f = fix 1 . setup
  where
    -- The maximum number of simplifier iterations. To be conservative and avoid
    -- excessive run times, we (should) set this value very low.
    --
    -- TODO: make this tunable via debug flags.
    --
    lIMIT       = 25

    simplify'   = Stats.simplifierDone . f
    setup x     = Debug.trace Debug.dump_simpl_iterations (msg 0 "init" x)
                $ snd (trace 1 "simplify" (simplify' x))

    fix :: Int -> f a -> f a
    fix i x0
      | i > lIMIT       = $internalWarning "simplify" "iteration limit reached" (not (x0 ==^ f x0)) x0
      | not shrunk      = x1
      | not simplified  = x2
      | otherwise       = fix (i+1) x2
      where
        (shrunk,     x1) = trace i "shrink"   $ shrink' x0
        (simplified, x2) = trace i "simplify" $ simplify' x1

    -- debugging support
    --
    u ==^ (_,v)         = isJust (match u v)

    trace i s v@(changed,x)
      | changed         = Debug.trace Debug.dump_simpl_iterations (msg i s x) v
      | otherwise       = v

    msg :: Int -> String -> f a -> String
    msg i s x = printf "simpl-iters/%-8s [%d]: %s" s i (ppr x)

    ppr :: f a -> String
    ppr = show . summarise


-- Debugging support
-- -----------------

data Stats = Stats
  { _terms    :: {-# UNPACK #-} !Int
  , _types    :: {-# UNPACK #-} !Int
  , _binders  :: {-# UNPACK #-} !Int
  , _vars     :: {-# UNPACK #-} !Int
  , _ops      :: {-# UNPACK #-} !Int
  }

instance Show Stats where
  show (Stats a b c d e) =
    printf "terms = %d, types = %d, lets = %d, vars = %d, primops = %d" a b c d e

infixl 6 +++
(+++) :: Stats -> Stats -> Stats
Stats a1 b1 c1 d1 e1 +++ Stats a2 b2 c2 d2 e2 = Stats (a1+a2) (b1+b2) (c1+c2) (d1+d2) (e1+e2)
{-# INLINE (+++) #-}

terms, types, binders, vars, ops :: Lens' Stats Int
terms   = lens _terms   (\Stats{..} v -> Stats { _terms   = v, ..})
types   = lens _types   (\Stats{..} v -> Stats { _types   = v, ..})
binders = lens _binders (\Stats{..} v -> Stats { _binders = v, ..})
vars    = lens _vars    (\Stats{..} v -> Stats { _vars    = v, ..})
ops     = lens _ops     (\Stats{..} v -> Stats { _ops     = v, ..})
{-# INLINE terms   #-}
{-# INLINE types   #-}
{-# INLINE binders #-}
{-# INLINE vars    #-}
{-# INLINE ops     #-}

summariseOpenFun :: PreOpenFun acc env aenv f -> Stats
summariseOpenFun (Body e)  = summariseOpenExp e & terms +~ 1
summariseOpenFun (Lam _ f) = summariseOpenFun f & terms +~ 1 & binders +~ 1

summariseOpenExp :: PreOpenExp acc env aenv t -> Stats
summariseOpenExp = (terms +~ 1) . goE
  where
    zero = Stats 0 0 0 0 0

    travE :: PreOpenExp acc env aenv t -> Stats
    travE = summariseOpenExp

    travF :: PreOpenFun acc env aenv t -> Stats
    travF = summariseOpenFun

    travA :: acc aenv a -> Stats
    travA _ = zero & vars +~ 1  -- assume an array index, else we should have failed elsewhere

    travC :: PrimConst c -> Stats
    travC (PrimMinBound t) = travBoundedType t & terms +~ 1
    travC (PrimMaxBound t) = travBoundedType t & terms +~ 1
    travC (PrimPi t)       = travFloatingType t & terms +~ 1

    travNonNumType :: NonNumType t -> Stats
    travNonNumType _ = zero & types +~ 1

    travIntegralType :: IntegralType t -> Stats
    travIntegralType _ = zero & types +~ 1

    travFloatingType :: FloatingType t -> Stats
    travFloatingType _ = zero & types +~ 1

    travNumType :: NumType t -> Stats
    travNumType (IntegralNumType t) = travIntegralType t & types +~ 1
    travNumType (FloatingNumType t) = travFloatingType t & types +~ 1

    travBoundedType :: BoundedType t -> Stats
    travBoundedType (IntegralBoundedType t) = travIntegralType t & types +~ 1
    travBoundedType (NonNumBoundedType t)   = travNonNumType t & types +~ 1

    -- travScalarType :: ScalarType t -> Stats
    -- travScalarType (SingleScalarType t) = travSingleType t & types +~ 1
    -- travScalarType (VectorScalarType t) = travVectorType t & types +~ 1

    travSingleType :: SingleType t -> Stats
    travSingleType (NumSingleType t)    = travNumType t & types +~ 1
    travSingleType (NonNumSingleType t) = travNonNumType t & types +~ 1

    -- travVectorType :: VectorType t -> Stats
    -- travVectorType (Vector2Type t)  = travSingleType t & types +~ 1
    -- travVectorType (Vector3Type t)  = travSingleType t & types +~ 1
    -- travVectorType (Vector4Type t)  = travSingleType t & types +~ 1
    -- travVectorType (Vector8Type t)  = travSingleType t & types +~ 1
    -- travVectorType (Vector16Type t) = travSingleType t & types +~ 1

    -- The scrutinee has already been counted
    goE :: PreOpenExp acc env aenv t -> Stats
    goE exp =
      case exp of
        Let _ bnd body        -> travE bnd +++ travE body & binders +~ 1
        Evar{}                -> zero & vars +~ 1
        Foreign _ _ x         -> travE x & terms +~ 1   -- +1 for asm, ignore fallback impls.
        Const{}               -> zero
        Undef _               -> zero
        Nil                   -> zero & terms +~ 1
        Pair e1 e2            -> travE e1 +++ travE e2 & terms +~ 1
        IndexSlice _ slix sh  -> travE slix +++ travE sh & terms +~ 1 -- +1 for sliceIndex
        IndexFull _ slix sl   -> travE slix +++ travE sl & terms +~ 1 -- +1 for sliceIndex
        ToIndex _ sh ix       -> travE sh +++ travE ix
        FromIndex _ sh ix     -> travE sh +++ travE ix
        Cond p t e            -> travE p +++ travE t +++ travE e
        While p f x           -> travF p +++ travF f +++ travE x
        PrimConst c           -> travC c
        Index a ix            -> travA a +++ travE ix
        LinearIndex a ix      -> travA a +++ travE ix
        Shape a               -> travA a
        ShapeSize _ sh        -> travE sh
        PrimApp f x           -> travPrimFun f +++ travE x
        Coerce _ _ e          -> travE e

    travPrimFun :: PrimFun f -> Stats
    travPrimFun = (ops +~ 1) . goF
      where
        goF :: PrimFun f -> Stats
        goF fun =
          case fun of
            PrimAdd                t -> travNumType t
            PrimSub                t -> travNumType t
            PrimMul                t -> travNumType t
            PrimNeg                t -> travNumType t
            PrimAbs                t -> travNumType t
            PrimSig                t -> travNumType t
            PrimQuot               t -> travIntegralType t
            PrimRem                t -> travIntegralType t
            PrimQuotRem            t -> travIntegralType t
            PrimIDiv               t -> travIntegralType t
            PrimMod                t -> travIntegralType t
            PrimDivMod             t -> travIntegralType t
            PrimBAnd               t -> travIntegralType t
            PrimBOr                t -> travIntegralType t
            PrimBXor               t -> travIntegralType t
            PrimBNot               t -> travIntegralType t
            PrimBShiftL            t -> travIntegralType t
            PrimBShiftR            t -> travIntegralType t
            PrimBRotateL           t -> travIntegralType t
            PrimBRotateR           t -> travIntegralType t
            PrimPopCount           t -> travIntegralType t
            PrimCountLeadingZeros  t -> travIntegralType t
            PrimCountTrailingZeros t -> travIntegralType t
            PrimFDiv               t -> travFloatingType t
            PrimRecip              t -> travFloatingType t
            PrimSin                t -> travFloatingType t
            PrimCos                t -> travFloatingType t
            PrimTan                t -> travFloatingType t
            PrimAsin               t -> travFloatingType t
            PrimAcos               t -> travFloatingType t
            PrimAtan               t -> travFloatingType t
            PrimSinh               t -> travFloatingType t
            PrimCosh               t -> travFloatingType t
            PrimTanh               t -> travFloatingType t
            PrimAsinh              t -> travFloatingType t
            PrimAcosh              t -> travFloatingType t
            PrimAtanh              t -> travFloatingType t
            PrimExpFloating        t -> travFloatingType t
            PrimSqrt               t -> travFloatingType t
            PrimLog                t -> travFloatingType t
            PrimFPow               t -> travFloatingType t
            PrimLogBase            t -> travFloatingType t
            PrimTruncate         f i -> travFloatingType f +++ travIntegralType i
            PrimRound            f i -> travFloatingType f +++ travIntegralType i
            PrimFloor            f i -> travFloatingType f +++ travIntegralType i
            PrimCeiling          f i -> travFloatingType f +++ travIntegralType i
            PrimIsNaN              t -> travFloatingType t
            PrimIsInfinite         t -> travFloatingType t
            PrimAtan2              t -> travFloatingType t
            PrimLt                 t -> travSingleType t
            PrimGt                 t -> travSingleType t
            PrimLtEq               t -> travSingleType t
            PrimGtEq               t -> travSingleType t
            PrimEq                 t -> travSingleType t
            PrimNEq                t -> travSingleType t
            PrimMax                t -> travSingleType t
            PrimMin                t -> travSingleType t
            PrimLAnd                 -> zero
            PrimLOr                  -> zero
            PrimLNot                 -> zero
            PrimOrd                  -> zero
            PrimChr                  -> zero
            PrimBoolToInt            -> zero
            PrimFromIntegral     i n -> travIntegralType i +++ travNumType n
            PrimToFloating       n f -> travNumType n +++ travFloatingType f

