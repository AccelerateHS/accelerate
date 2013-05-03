{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE PatternGuards        #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
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
  avarIn, kmap,

  -- Delayed Arrays
  DelayedAcc,  DelayedOpenAcc(..),
  DelayedAfun, DelayedOpenAfun,
  DelayedExp, DelayedFun, DelayedOpenExp, DelayedOpenFun,

  -- Environments
  Gamma(..), incExp, prjExp, lookupExp,

) where

-- standard library
import Prelude                                          hiding ( until )
import Data.Hashable
import Text.PrettyPrint

-- friends
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Array.Sugar                ( Array, Arrays, Shape, Elt )
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Trafo.Substitution
import Data.Array.Accelerate.Pretty.Print

#include "accelerate.h"


-- Toolkit
-- =======

-- The bat utility belt of operations required to manipulate terms parameterised
-- by the recursive closure.
--
class Kit acc where
  inject        :: PreOpenAcc acc aenv a -> acc aenv a
  extract       :: acc aenv a -> PreOpenAcc acc aenv a
  --
  rebuildAcc    :: RebuildAcc acc
  matchAcc      :: MatchAcc acc
  hashAcc       :: HashAcc acc
  prettyAcc     :: PrettyAcc acc

instance Kit OpenAcc where
  inject                 = OpenAcc
  extract (OpenAcc pacc) = pacc

  rebuildAcc    = rebuildOpenAcc
  matchAcc      = matchOpenAcc
  hashAcc       = hashOpenAcc
  prettyAcc     = prettyOpenAcc

avarIn :: (Kit acc, Arrays arrs) => Idx aenv arrs -> acc aenv arrs
avarIn = inject  . Avar

kmap :: Kit acc => (PreOpenAcc acc aenv a -> PreOpenAcc acc aenv b) -> acc aenv a -> acc aenv b
kmap f = inject . f . extract


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

instance Kit acc => Match (PreOpenAcc acc aenv) where
  match = matchPreOpenAcc matchAcc hashAcc

instance Kit acc => Match (acc aenv) where      -- overlapping, undecidable, incoherent
  match = matchAcc


-- Delayed Arrays
-- ==============

-- The type of delayed arrays. This representation is used to annotate the AST
-- in the recursive knot to distinguish standard AST terms from operand arrays
-- that should be embedded into their consumers.
--
type DelayedAcc         = DelayedOpenAcc ()
type DelayedAfun        = PreOpenAfun DelayedOpenAcc ()

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
  inject        = Manifest
  extract       = error "DelayedAcc.extract"
  --
  rebuildAcc    = rebuildDelayed
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
rebuildDelayed v acc = case acc of
  Manifest pacc -> Manifest (rebuildA rebuildDelayed v pacc)
  Delayed{..}   -> Delayed (rebuildEA rebuildDelayed v extentD)
                           (rebuildFA rebuildDelayed v indexD)
                           (rebuildFA rebuildDelayed v linearIndexD)


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
prettyDelayed alvl wrap acc = case acc of
  Manifest pacc         -> prettyPreAcc prettyDelayed alvl wrap pacc
  Delayed sh f _
    | Shape a           <- sh
    , Just REFL         <- match f (Lam (Body (Index a (Var ZeroIdx))))
    -> prettyDelayed alvl wrap a

    | otherwise
    -> wrap $ hang (text "Delayed") 2
            $ sep [ prettyPreExp prettyDelayed 0 alvl parens sh
                  , parens (prettyPreFun prettyDelayed alvl f)
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

incExp :: Gamma acc env env' aenv -> Gamma acc (env, s) env' aenv
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

