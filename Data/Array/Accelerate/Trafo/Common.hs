{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE PatternGuards        #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo.Common
-- Copyright   : [2012] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Trafo.Common (

  -- Toolkit
  Kit(..), Match(..), (:=:)(REFL),
  avarIn,

  -- Environments
  Gamma(..), incExp, prjExp, lookupExp,
--  Delta(..), incAcc, prjAcc, lookupAcc,

) where

-- standard library
import Prelude                                          hiding ( until )

-- friends
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Array.Sugar                ( Arrays )
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Trafo.Substitution
import Data.Array.Accelerate.Pretty.Print

#include "accelerate.h"


-- Toolkit
-- -------

-- The bat utility belt of operations required to manipulate terms parameterised
-- by the recursive closure.
--
class Kit acc where
  termOut       :: PreOpenAcc acc aenv a -> acc aenv a
  rebuildAcc    :: RebuildAcc acc
  matchAcc      :: MatchAcc acc
  hashAcc       :: HashAcc acc
  prettyAcc     :: PrettyAcc acc

instance Kit OpenAcc where
  termOut       = OpenAcc
  rebuildAcc    = rebuildOpenAcc
  matchAcc      = matchOpenAcc
  hashAcc       = hashOpenAcc
  prettyAcc     = prettyOpenAcc

avarIn :: (Kit acc, Arrays arrs) => Idx aenv arrs -> acc aenv arrs
avarIn = termOut . Avar


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


-- Environments
-- ------------

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

