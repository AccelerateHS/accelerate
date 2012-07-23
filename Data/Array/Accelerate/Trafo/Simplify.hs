{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo.Simplify
-- Copyright   : [2012] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Trafo.Simplify (

  -- simplify scalar expressions
  simplifyExp,
  simplifyFun,

) where

-- standard library
import Prelude                                          hiding ( exp )
import Data.Typeable

-- friends
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Trafo.Substitution
import Data.Array.Accelerate.Array.Sugar                ( Elt )
import Data.Array.Accelerate.Tuple                      hiding ( Tuple )
import qualified Data.Array.Accelerate.Tuple            as Tuple


-- An environment that holds let-bound scalar expressions. The second
-- environment variable env' is used to project out the corresponding
-- index when looking up in the environment congruent expressions.
--
data Gamma env env' aenv where
  EmptyEnv :: Gamma env () aenv

  PushEnv  :: Typeable t
           => Gamma   env env'      aenv
           -> OpenExp env           aenv t
           -> Gamma   env (env', t) aenv

incEnv :: Gamma env env' aenv -> Gamma (env, s) env' aenv
incEnv EmptyEnv        = EmptyEnv
incEnv (PushEnv env e) = incEnv env `PushEnv` weakenE e

lookupEnv :: Typeable t
          => Gamma   env env' aenv
          -> OpenExp env      aenv t
          -> Maybe  (Idx env' t)
lookupEnv EmptyEnv        _             = Nothing
lookupEnv (PushEnv env e) x
  | Just REFL <- matchOpenExp e x       = Just ZeroIdx
  | otherwise                           = SuccIdx `fmap` lookupEnv env x


-- Simplify scalar let bindings. Currently this takes the form of a pretty weedy
-- CSE optimisation, where we look for expressions of the form:
--
-- > let x = e1 in e2
--
-- and replace all occurrences of e1 in e1 with x. This doesn't do full CSE, but
-- is enough to catch some cases, particularly redundant operations such as
-- array indexing introduced by the fusion pass.
--
simplifyLet
    :: (Elt a, Elt b)
    => Gamma   env env aenv
    -> OpenExp env     aenv a
    -> OpenExp (env,a) aenv b
    -> OpenExp env     aenv b
simplifyLet env bnd body
  | Just ix <- lookupEnv env bnd        = inline body (Var ix)
  | otherwise                           = Let bnd body


-- Simplify conditional expressions. If the branches are equal, we can avoid the
-- conditional altogether.
--
-- TODO: implement constant folding, then attempt to evaluate the predicate and
--       insert only the appropriate branch (module Algebra).
--
simplifyCond
    :: Elt t
    => Gamma env env aenv
    -> OpenExp env aenv Bool
    -> OpenExp env aenv t       -- then branch
    -> OpenExp env aenv t       -- else branch
    -> OpenExp env aenv t
simplifyCond _env p t e
  | Just REFL <- matchOpenExp t e       = t
  | otherwise                           = Cond p t e


-- Walk over the scalar expression, applying simplifications
--
simplifyExp :: Exp aenv t -> Exp aenv t
simplifyExp = simplifyOpenExp EmptyEnv

simplifyFun :: Fun aenv t -> Fun aenv t
simplifyFun = simplifyOpenFun EmptyEnv


simplifyOpenExp
    :: forall env aenv t.
       Gamma   env env aenv
    -> OpenExp env     aenv t
    -> OpenExp env     aenv t
simplifyOpenExp env = cvt
  where
    cvtA :: OpenAcc aenv a -> OpenAcc aenv a
    cvtA = id

    cvt :: OpenExp env aenv e -> OpenExp env aenv e
    cvt exp = case exp of
      Let bnd body      -> let bnd'  = cvt bnd
                               env'  = incEnv env `PushEnv` weakenE bnd'
                               body' = simplifyOpenExp env' body
                           in
                           simplifyLet env bnd' body'
      --
      Var ix            -> Var ix
      Const c           -> Const c
      Tuple tup         -> Tuple (simplifyTuple env tup)
      Prj tup ix        -> Prj tup (cvt ix)
      IndexNil          -> IndexNil
      IndexCons sh sz   -> IndexCons (cvt sh) (cvt sz)
      IndexHead sh      -> IndexHead (cvt sh)
      IndexTail sh      -> IndexTail (cvt sh)
      IndexAny          -> IndexAny
      ToIndex sh ix     -> ToIndex (cvt sh) (cvt ix)
      FromIndex sh ix   -> FromIndex (cvt sh) (cvt ix)
      Cond p t e        -> simplifyCond env (cvt p) (cvt t) (cvt e)
      PrimConst c       -> PrimConst c
      PrimApp f x       -> PrimApp f (cvt x)
      IndexScalar a sh  -> IndexScalar (cvtA a) (cvt sh)
      Shape a           -> Shape (cvtA a)
      ShapeSize sh      -> ShapeSize (cvt sh)
      Intersect s t     -> Intersect (cvt s) (cvt t)


simplifyTuple
    :: Gamma env env aenv
    -> Tuple.Tuple (OpenExp env aenv) t
    -> Tuple.Tuple (OpenExp env aenv) t
simplifyTuple _   NilTup          = NilTup
simplifyTuple env (SnocTup tup e) = simplifyTuple env tup `SnocTup` simplifyOpenExp env e


simplifyOpenFun
    :: Gamma   env env aenv
    -> OpenFun env     aenv t
    -> OpenFun env     aenv t
simplifyOpenFun env (Body e) = Body (simplifyOpenExp env e)
simplifyOpenFun env (Lam  f) = Lam  (simplifyOpenFun (incEnv env `PushEnv` Var ZeroIdx) f)

