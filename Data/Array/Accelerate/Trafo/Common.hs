{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
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

  -- Controlling optimisations
  until,

  -- Environments
  Gamma(..), incExp, prjExp, lookupExp,
  Delta(..), incAcc, prjAcc, lookupAcc,

) where

-- standard library
import Prelude                                          hiding ( until )

-- friends
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Trafo.Substitution

#include "accelerate.h"


-- Repeatedly evaluate a transformation until no changes are made, or an
-- iteration limit (10) is reached.
--
until :: forall f done. (f -> f -> Maybe done) -> (f -> f) -> f -> f
until stop go = fix 0
  where
    fix :: Int -> f -> f
    fix !i !x | i < lIMIT, Nothing <- stop x x'   = fix (i+1) x'
              | otherwise                         = x'
              where
                !lIMIT = 10
                !x'    = go x


-- Environments
-- ------------

-- An environment that holds let-bound scalar expressions. The second
-- environment variable env' is used to project out the corresponding
-- index when looking up in the environment congruent expressions.
--
data Gamma env env' aenv where
  EmptyExp :: Gamma   env env'      aenv

  PushExp  :: Gamma   env env'      aenv
           -> OpenExp env           aenv t
           -> Gamma   env (env', t) aenv

incExp :: Gamma env env' aenv -> Gamma (env, s) env' aenv
incExp EmptyExp        = EmptyExp
incExp (PushExp env e) = incExp env `PushExp` weakenE e

prjExp :: Idx env' t -> Gamma env env' aenv -> OpenExp env aenv t
prjExp ZeroIdx      (PushExp _   v) = v
prjExp (SuccIdx ix) (PushExp env _) = prjExp ix env
prjExp _            _               = INTERNAL_ERROR(error) "prjExp" "inconsistent valuation"

lookupExp :: Gamma env env' aenv -> OpenExp env aenv t -> Maybe (Idx env' t)
lookupExp EmptyExp        _       = Nothing
lookupExp (PushExp env e) x
  | Just REFL <- matchOpenExp e x = Just ZeroIdx
  | otherwise                     = SuccIdx `fmap` lookupExp env x


-- An environment which holds let-bound array expressions.
--
data Delta aenv aenv' where
  EmptyAcc :: Delta   aenv aenv'

  PushAcc  :: Delta   aenv aenv'
           -> OpenAcc aenv            a
           -> Delta   aenv (aenv', a)

incAcc :: Delta aenv aenv' -> Delta (aenv,s) aenv'
incAcc EmptyAcc         = EmptyAcc
incAcc (PushAcc aenv a) = incAcc aenv `PushAcc` weakenA a

prjAcc :: Idx aenv' t -> Delta aenv aenv' -> OpenAcc aenv t
prjAcc ZeroIdx      (PushAcc _   v) = v
prjAcc (SuccIdx ix) (PushAcc env _) = prjAcc ix env
prjAcc _            _               = INTERNAL_ERROR(error) "prjAcc" "inconsistent valuation"

lookupAcc :: Delta aenv aenv' -> OpenAcc aenv a -> Maybe (Idx aenv' a)
lookupAcc EmptyAcc         _      = Nothing
lookupAcc (PushAcc aenv a) x
  | Just REFL <- matchOpenAcc a x = Just ZeroIdx
  | otherwise                     = SuccIdx `fmap` lookupAcc aenv x


