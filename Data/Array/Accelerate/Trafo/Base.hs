{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE PatternGuards        #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds      #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo.Base
-- Copyright   : [2012] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Trafo.Base (

  -- Toolkit
  Kit(..), Match(..), (:=:)(REFL),
  avarIn, kmap, fromOpenAfun, compose, subApply,

  -- Delayed Arrays
  DelayedAcc,  DelayedOpenAcc(..),
  DelayedAfun, DelayedOpenAfun,
  DelayedExp, DelayedFun, DelayedOpenExp, DelayedOpenFun,

  -- Environments
  Gamma(..), incExp, prjExp, lookupExp,
  Extend(..), join, bind, sink, sink1

) where

-- standard library
import Prelude                                          hiding ( until )
import Data.Hashable
import Control.Applicative                              hiding ( Const )
import Text.PrettyPrint

-- friends
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Array.Sugar                ( Array, Arrays, Shape, Elt )
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Trafo.Substitution
import Data.Array.Accelerate.Pretty.Print

import qualified Data.Array.Accelerate.Debug    as Stats

#include "accelerate.h"


-- Toolkit
-- =======

-- The bat utility belt of operations required to manipulate terms parameterised
-- by the recursive closure.
--
class RebuildableAcc acc => Kit acc where
  inject        :: PreOpenAcc acc env aenv a -> acc env aenv a
  extract       :: acc env aenv a -> PreOpenAcc acc env aenv a
  fromOpenAcc   :: OpenAcc env aenv a -> acc env aenv a
  --
  matchAcc      :: MatchAcc acc
  hashAcc       :: HashAcc acc
  prettyAcc     :: PrettyAcc acc

instance Kit OpenAcc where
  inject                 = OpenAcc
  extract (OpenAcc pacc) = pacc
  fromOpenAcc            = id

  matchAcc      = matchOpenAcc
  hashAcc       = hashOpenAcc
  prettyAcc     = prettyOpenAcc

avarIn :: (Kit acc, Arrays arrs) => Idx aenv arrs -> acc env aenv arrs
avarIn = inject  . Avar

kmap :: Kit acc => (PreOpenAcc acc env aenv a -> PreOpenAcc acc env aenv b) -> acc env aenv a -> acc env aenv b
kmap f = inject . f . extract

fromOpenAfun :: Kit acc => OpenAfun env aenv f -> PreOpenAfun acc env aenv f
fromOpenAfun (Abody b) = Abody (fromOpenAcc b)
fromOpenAfun (Alam  f) = Alam (fromOpenAfun f)

infixr `compose`

-- | Composition of unary functions.
--
compose :: (RebuildableAcc acc, Elt c)
        => PreOpenFun acc env aenv (b -> c)
        -> PreOpenFun acc env aenv (a -> b)
        -> PreOpenFun acc env aenv (a -> c)
compose (Lam (Body f)) (Lam (Body g)) = Stats.substitution "compose" . Lam . Body $ substitute f g
compose _              _              = error "compose: impossible evaluation"

-- | Apply an array level function via substitution
--
subApply :: (RebuildableAcc acc, Arrays a)
         => PreOpenAfun acc env aenv (a -> b)
         -> acc             env aenv a
         -> PreOpenAcc  acc env aenv b
subApply (Alam (Abody f)) a = Alet a f
subApply _                _ = error "subApply: inconsistent evaluation"

-- A class for testing the equality of terms homogeneously, returning a witness
-- to the existentially quantified terms in the positive case.
--
class Match f where
  match :: f s -> f t -> Maybe (s :=: t)

instance Match (Idx env) where
  match = matchIdx

instance Kit acc => Match (PreOpenExp acc env aenv) where
  match = matchPreOpenExp matchAcc hashAcc

instance Kit acc => Match (PreOpenFun acc env aenv) where
  match = matchPreOpenFun matchAcc hashAcc

instance Kit acc => Match (PreOpenAcc acc env aenv) where
  match = matchPreOpenAcc matchAcc hashAcc

instance Kit acc => Match (acc env aenv) where      -- overlapping, undecidable, incoherent
  match = matchAcc

-- Delayed Arrays
-- ==============

-- The type of delayed arrays. This representation is used to annotate the AST
-- in the recursive knot to distinguish standard AST terms from operand arrays
-- that should be embedded into their consumers.
--
type DelayedAcc         = DelayedOpenAcc () ()
type DelayedAfun        = PreOpenAfun DelayedOpenAcc () ()

type DelayedExp         = DelayedOpenExp () ()
type DelayedFun         = DelayedOpenFun () ()
type DelayedOpenAfun    = PreOpenAfun DelayedOpenAcc
type DelayedOpenExp     = PreOpenExp DelayedOpenAcc
type DelayedOpenFun     = PreOpenFun DelayedOpenAcc

data DelayedOpenAcc env aenv a where
  Manifest              :: PreOpenAcc DelayedOpenAcc env aenv a -> DelayedOpenAcc env aenv a

  Delayed               :: (Shape sh, Elt e) =>
    { extentD           :: PreOpenExp DelayedOpenAcc env aenv sh
    , indexD            :: PreOpenFun DelayedOpenAcc env aenv (sh  -> e)
    , linearIndexD      :: PreOpenFun DelayedOpenAcc env aenv (Int -> e)
    }                   -> DelayedOpenAcc env aenv (Array sh e)

instance Rebuildable DelayedOpenAcc where
  type AccClo DelayedOpenAcc = DelayedOpenAcc

  rebuild = rebuildDelayed

instance Kit DelayedOpenAcc where
  inject        = Manifest
  extract       = error "DelayedAcc.extract"
  fromOpenAcc   = error "DelayedAcc.fromOpenAcc"
  --
  matchAcc      = matchDelayed
  hashAcc       = hashDelayed
  prettyAcc     = prettyDelayed


hashDelayed :: HashAcc DelayedOpenAcc
hashDelayed (Manifest pacc)     = hash "Manifest"       `hashWithSalt` hashPreOpenAcc hashAcc pacc
hashDelayed Delayed{..}         = hash "Delayed"        `hashE` extentD `hashF` indexD `hashF` linearIndexD
  where
    hashE salt = hashWithSalt salt . hashPreOpenExp hashAcc
    hashF salt = hashWithSalt salt . hashPreOpenFun hashAcc

matchDelayed :: MatchAcc DelayedOpenAcc
matchDelayed (Manifest pacc1) (Manifest pacc2)
  = matchPreOpenAcc matchAcc hashAcc pacc1 pacc2

matchDelayed (Delayed sh1 ix1 lx1) (Delayed sh2 ix2 lx2)
  | Just REFL   <- matchPreOpenExp matchAcc hashAcc sh1 sh2
  , Just REFL   <- matchPreOpenFun matchAcc hashAcc ix1 ix2
  , Just REFL   <- matchPreOpenFun matchAcc hashAcc lx1 lx2
  = Just REFL

matchDelayed _ _
  = Nothing

rebuildDelayed :: RebuildAcc DelayedOpenAcc
rebuildDelayed v av acc = case acc of
  Manifest pacc -> Manifest <$> (rebuildPreOpenAcc rebuildDelayed v av pacc)
  Delayed{..}   -> Delayed <$> (rebuildPreOpenExp rebuildDelayed v av extentD)
                           <*> (rebuildFun rebuildDelayed v av indexD)
                           <*> (rebuildFun rebuildDelayed v av linearIndexD)


-- Note: If we detect that the delayed array is simply accessing an array
-- variable, then just print the variable name. That is:
--
-- > let a0 = <...> in map f (Delayed (shape a0) (\x0 -> a0!x0))
--
-- becomes
--
-- > let a0 = <...> in map f a0
--
prettyDelayed :: PrettyAcc DelayedOpenAcc
prettyDelayed lvl alvl wrap acc = case acc of
  Manifest pacc         -> prettyPreAcc prettyDelayed lvl alvl wrap pacc
  Delayed sh f _
    | Shape a           <- sh
    , Just REFL         <- match f (Lam (Body (Index (weakenE SuccIdx a) (Var ZeroIdx))))
    -> prettyDelayed lvl alvl wrap a

    | otherwise
    -> wrap $ hang (text "Delayed") 2
            $ sep [ prettyPreExp prettyDelayed lvl alvl parens sh
                  , parens (prettyPreFun prettyDelayed lvl alvl f)
                  ]


-- Environments
-- ============

-- An environment that holds let-bound scalar expressions. The second
-- environment variable env' is used to project out the corresponding
-- index when looking up in the environment congruent expressions.
--
data Gamma acc env env' aenv where
  EmptyExp :: Gamma      acc env env'      aenv

  PushExp  :: Gamma      acc env env'      aenv
           -> PreOpenExp acc env           aenv t
           -> Gamma      acc env (env', t) aenv

incExp :: (Kit acc) => Gamma acc env env' aenv -> Gamma acc (env, s) env' aenv
incExp EmptyExp        = EmptyExp
incExp (PushExp env e) = incExp env `PushExp` weakenE SuccIdx e

prjExp :: Idx env' t -> Gamma acc env env' aenv -> PreOpenExp acc env aenv t
prjExp ZeroIdx      (PushExp _   v) = v
prjExp (SuccIdx ix) (PushExp env _) = prjExp ix env
prjExp _            _               = INTERNAL_ERROR(error) "prjExp" "inconsistent valuation"

lookupExp :: Kit acc => Gamma acc env env' aenv -> PreOpenExp acc env aenv t -> Maybe (Idx env' t)
lookupExp EmptyExp        _       = Nothing
lookupExp (PushExp env e) x
  | Just REFL <- match e x = Just ZeroIdx
  | otherwise              = SuccIdx `fmap` lookupExp env x

-- As part of various transformation we often need to lift out array valued
-- inputs to be let-bound at a higher point.
--
-- The Extend type is a heterogeneous snoc-list of array terms that witnesses
-- how the array environment is extended by binding these additional terms.
--
data Extend acc env aenv aenv' where
  BaseEnv :: Extend acc env aenv aenv

  PushEnv :: Arrays a
          => Extend acc env aenv aenv' -> PreOpenAcc acc env aenv' a -> Extend acc env aenv (aenv', a)

-- Append two environment witnesses
--
join :: Extend acc env aenv aenv' -> Extend acc env aenv' aenv'' -> Extend acc env aenv aenv''
join x BaseEnv        = x
join x (PushEnv as a) = x `join` as `PushEnv` a

-- Bring into scope all of the array terms in the Extend environment list. This
-- converts a term in the inner environment (aenv') into the outer (aenv).
--
bind :: (Kit acc, Arrays a)
     => Extend acc env aenv aenv'
     -> PreOpenAcc acc env aenv' a
     -> PreOpenAcc acc env aenv  a
bind BaseEnv         = id
bind (PushEnv env a) = bind env . Alet (inject a) . inject

-- Sink a term from one array environment into another, where additional
-- bindings have come into scope according to the witness and no old things have
-- vanished.
--
-- NB: For this function, it is not required that the scalar environment of the 'Extend'
-- matches that of the term. As it only weakens with respect to the array environment.
--
sink :: Sink f => Extend acc env aenv aenv' -> f env' aenv t -> f env' aenv' t
sink env = weakenA (k env)
  where
    k :: Extend acc env aenv aenv' -> Idx aenv t -> Idx aenv' t
    k BaseEnv       = Stats.substitution "sink" id
    k (PushEnv e _) = SuccIdx . k e

sink1 :: Sink f => Extend acc env aenv aenv' -> f env' (aenv,s) t -> f env' (aenv',s) t
sink1 env = weakenA (k env)
  where
    k :: Extend acc env aenv aenv' -> Idx (aenv,s) t -> Idx (aenv',s) t
    k BaseEnv       = Stats.substitution "sink1" id
    k (PushEnv e _) = split . k e
    --
    split :: Idx (aenv,s) t -> Idx ((aenv,u),s) t
    split ZeroIdx      = ZeroIdx
    split (SuccIdx ix) = SuccIdx (SuccIdx ix)

