{-# LANGUAGE CPP, GADTs, TypeFamilies, ScopedTypeVariables, TypeOperators, RankNTypes #-}
-- |
-- Module      : Data.Array.Accelerate.Subst
-- Copyright   : [2011] Sean Seefried
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- /Substitution for de Bruijn terms/
-- This module defines the 'substOpenExp' and 'substOpenFun' functions
-- for module "Data.Array.Accelerate.AST"
--
module Data.Array.Accelerate.Subst (

  -- * Types
  Cat,
  -- * Functions
  substOpenExp, substOpenFun

)
where

#include "accelerate.h"

-- friends
import Data.Array.Accelerate.Tuple (Tuple(..))
import Data.Array.Accelerate.AST

--
-- | The 'Cursor' data type represents where in an environment we wish to insert
--   a new de Bruijn index. If the original environment is equal to @Cat env benv@
--   then the new index is inserted between the @env@ and the @benv@. @benv@ stands
--   for \"bound environment\". As we recursively descend into terms it increases in size.
--
--
data Cursor env benv t where
  Zero :: Cursor env () t
  Succ :: Cursor env benv t -> Cursor env (benv, s) t

--
-- | Concatenation of two environments.
--
type family Cat env env'
type instance Cat env ()       = env
type instance Cat env (env',s) = (Cat env env', s)

eqCursorIdx :: forall env benv s t.
               Cursor env benv s
            -> Idx (Cat (env, s) benv)  t
            -> Maybe (s :=: t)
eqCursorIdx Zero ZeroIdx      = Just REFL
eqCursorIdx (Succ m) (SuccIdx n) = eqCursorIdx m n
eqCursorIdx _         _          = Nothing

--
-- Takes an Idx and extends the environment to a larger one.
--
extendIdx :: forall eenv env s t. eenv -> Idx (env, s) t -> Idx (Cat eenv env, s) t
extendIdx _ ZeroIdx               = ZeroIdx
extendIdx _ (SuccIdx ZeroIdx)     = SuccIdx ZeroIdx
extendIdx _ (SuccIdx (SuccIdx m)) = SuccIdx (extendIdx (undefined :: eenv) (SuccIdx m))

data s :=: t where
  REFL :: s :=: s

liftIdx :: forall env benv s t. Cursor env benv s -> Idx (Cat env benv) t -> Idx (Cat (env, s) benv) t
liftIdx Zero      n          = SuccIdx n
liftIdx (Succ _)  ZeroIdx    = ZeroIdx
liftIdx (Succ m) (SuccIdx n) = SuccIdx (liftIdx m n)

liftTuple :: forall acc env benv aenv s t.
             Cursor env benv s
          -> Tuple (PreOpenExp acc (Cat env benv)     aenv) t
          -> Tuple (PreOpenExp acc (Cat (env,s) benv) aenv) t
liftTuple _ NilTup = NilTup
liftTuple n (SnocTup tup elt) = SnocTup (liftTuple n tup) (lift n elt)

--
-- The 'lift' function is responsible for incrementing all free variables in an expression.
--
lift :: forall acc aenv env benv s t.
        Cursor env benv s
     -> PreOpenExp acc (Cat env benv) aenv t
     -> PreOpenExp acc (Cat (env, s) benv) aenv t
lift n (Var ix)            = Var (liftIdx n ix)
lift _ (Const c)           = Const c
lift n (Tuple tup)         = Tuple (liftTuple n tup)
lift n (Prj tupIx oe)      = Prj tupIx (lift n oe)
lift _ IndexNil            = IndexNil
lift n (IndexCons hIx tIx) = IndexCons (lift n hIx) (lift n tIx)
lift n (IndexHead e)       = IndexHead (lift n e)
lift n (IndexTail e)       = IndexTail (lift n e)
lift n (Cond c t e)        = Cond (lift n c) (lift n t) (lift n e)
lift _ (PrimConst pconst)  = PrimConst pconst
lift n (PrimApp pfun arg)  = PrimApp pfun (lift n arg)
--
-- All array computations (of type 'acc') appearing in the following three
-- cases are guaranteed not to contain free scalar variables so we
-- don't need to lift.
--
lift n (IndexScalar acc e) = IndexScalar acc (lift n e)
lift _ (Shape acc)         = Shape acc
lift _ (Size acc)          = Size acc


--
-- The "subCursor" family of functions substitutes and open scalar expression into
-- an entity (expression, tuple, function) in place of a particular de Bruijn index
-- equivalent to the value of the provided 'Cursor'.
--
subCursorVarNE :: Cursor env benv s -> Idx (Cat (env, s) benv) t -> Idx (Cat env benv) t
subCursorVarNE Zero ZeroIdx         =
  INTERNAL_ERROR(error) "subCursorVarNE" "subCursorVarNE called on Cursor and Idx that are equal"
subCursorVarNE Zero (SuccIdx n)     = n
subCursorVarNE (Succ _) (ZeroIdx)   = ZeroIdx
subCursorVarNE (Succ m) (SuccIdx n) = SuccIdx (subCursorVarNE m n)

subCursorPreOpenExp:: Cursor env benv s
               -> PreOpenExp acc (Cat env benv) aenv s
               -> PreOpenExp acc (Cat (env, s) benv) aenv t
               -> PreOpenExp acc (Cat env benv) aenv t
subCursorPreOpenExp m t (Var n)               = case eqCursorIdx m n of
                                                  Just REFL -> t
                                                  Nothing   -> Var (subCursorVarNE m n)
subCursorPreOpenExp _ _ (Const c)             = Const c
subCursorPreOpenExp m t (Tuple tup)           = Tuple (subCursorTuple m t tup)
subCursorPreOpenExp m t (Prj tupIdx e)        = Prj tupIdx (subCursorPreOpenExp m t e)
subCursorPreOpenExp _ _ IndexNil              = IndexNil
subCursorPreOpenExp m t (IndexCons sh idx)    = IndexCons (subCursorPreOpenExp m t sh)
                                                      (subCursorPreOpenExp m t idx)
subCursorPreOpenExp m t (IndexHead hexp)      = IndexHead (subCursorPreOpenExp m t hexp)
subCursorPreOpenExp m t  (IndexTail texp)     = IndexTail (subCursorPreOpenExp m t texp)
subCursorPreOpenExp m t (Cond cexp texp eexp) = Cond (subCursorPreOpenExp m t cexp)
                                                 (subCursorPreOpenExp m t texp)
                                                 (subCursorPreOpenExp m t eexp)
subCursorPreOpenExp _ _ (PrimConst cnst)      = PrimConst cnst
subCursorPreOpenExp m t (PrimApp fun arg)     = PrimApp fun (subCursorPreOpenExp m t arg)
--
-- All array computations (of type 'acc') appearing in the following three
-- cases are guaranteed not to contain free scalar variables so we
-- don't need to substitute.
--
subCursorPreOpenExp m t (IndexScalar acc e)   = IndexScalar acc (subCursorPreOpenExp m t e)
subCursorPreOpenExp _ _  (Shape sh)           = Shape sh
subCursorPreOpenExp _ _ (Size acc)            = Size acc


subCursorTuple :: Cursor env benv s
               -> PreOpenExp acc (Cat env benv) aenv s
               -> Tuple (PreOpenExp acc (Cat (env, s) benv) aenv) t
               -> Tuple (PreOpenExp acc (Cat env benv)      aenv) t
subCursorTuple _ _ NilTup          = NilTup
subCursorTuple m t (SnocTup tup e) = SnocTup (subCursorTuple m t tup) (subCursorPreOpenExp m t e)

subCursorPreOpenFun :: Cursor env benv s
                    -> PreOpenExp acc (Cat env benv) aenv s
                    -> PreOpenFun acc (Cat (env, s) benv) aenv t
                    -> PreOpenFun acc (Cat env benv) aenv t
subCursorPreOpenFun m t (Body e)   = Body (subCursorPreOpenExp m t e)
subCursorPreOpenFun m t (Lam body) = Lam  (subCursorPreOpenFun (Succ m) (lift Zero t) body)

--
-- The "extend" family of functions takes an entity (expression, tuple, function) and extends
-- its environment.
--
-- e.g. @PreOpenExp acc ((), Int) aenv Int@ could have its environment extended to
--      @PreOpenExp acc (((), Word), Int) aenv Int@.
--
extendPreOpenExp :: forall acc aenv env env' s t.
          env -> PreOpenExp acc (env', s) aenv t -> PreOpenExp acc (Cat env env', s) aenv t
extendPreOpenExp _ (Var n)               = Var (extendIdx (undefined :: env) n)
extendPreOpenExp _ (Const c)             = Const c
extendPreOpenExp _ (Tuple tup)           = Tuple (extendTup (undefined :: env) tup)
extendPreOpenExp _ (Prj tupIdx e)        = Prj tupIdx (extendPreOpenExp (undefined :: env) e)
extendPreOpenExp _ IndexNil              = IndexNil
extendPreOpenExp _ (IndexCons sh idx)    = IndexCons (extendPreOpenExp (undefined::env) sh)
                                                     (extendPreOpenExp (undefined::env) idx)
extendPreOpenExp _ (IndexHead hexp)      = IndexHead (extendPreOpenExp (undefined::env) hexp)
extendPreOpenExp _ (IndexTail texp)      = IndexTail (extendPreOpenExp (undefined::env) texp)
extendPreOpenExp _ (Cond cexp texp eexp) = Cond (extendPreOpenExp (undefined::env) cexp)
                                                (extendPreOpenExp (undefined::env) texp)
                                                (extendPreOpenExp (undefined::env) eexp)
extendPreOpenExp _ (PrimConst cnst)      = PrimConst cnst
extendPreOpenExp _ (PrimApp fun arg)     = PrimApp fun (extendPreOpenExp (undefined::env) arg)
extendPreOpenExp _ (IndexScalar acc e)   = IndexScalar acc (extendPreOpenExp (undefined::env) e)
extendPreOpenExp _ (Shape sh)            = Shape sh
extendPreOpenExp _ (Size acc)            = Size acc

extendTup :: forall acc aenv env env' s t.
             env
          -> Tuple (PreOpenExp acc (env', s) aenv) t
          -> Tuple (PreOpenExp acc (Cat env env', s) aenv) t
extendTup _ NilTup = NilTup
extendTup _ (SnocTup tup e) = SnocTup (extendTup (undefined::env) tup)
                                      (extendPreOpenExp (undefined::env) e)

extendPreOpenFun :: forall acc aenv env env' s t.
                    env
                 -> PreOpenFun acc (env', s) aenv t
                 -> PreOpenFun acc (Cat env env', s) aenv t
extendPreOpenFun _ (Body e)   = Body (extendPreOpenExp (undefined::env) e)
extendPreOpenFun _ (Lam body) = Lam  (extendPreOpenFun (undefined::env) body)

--
-- | 'substOpenExp' and 'substOpenFun' provide the capability to substitute
--   an open scalar expression into, respectively, an open scalar expression
--   or open scalar function in place of varieble with de Bruijn index equal to zero.
--   (i.e. ZeroIdx).
--
--   (In the following discussion, if an expression @e@ has Haskell type @OpenExp env aenv t@
--   we say that its "environment is @env@" and its "expression type is @t@")

--   @substOpenExp term exp@ substitutes expression @term@ into expression @exp@ in place of
--   any occurrence of value @Var ZeroIdx@.
--
--   (@substOpenFun is similar.)
--
--   Preconditions:
--
--   If the expression type of @term@ is @s@, the environment of @term@ is @env@,
--   then then environment of @exp@ must be @(env', s)@ (where @env'@ must be a prefix of @env@).
--
--   e.g. if the environment of @term@ is @(((), Int), Word8)@ and the expression type of
--        @term@ is @Double@ then the environment of @exp@ can be any of the following:
--        - @((((), Int), Word8), Double)@
--        - @(((), Word8), Double)@
--        - @((), Double)@
--
--   Substitution of multiple expressions can be achieved by repeated calls to
--   @substOpenExp@/@substOpenFun@
--   Say you wanted to substitute @exp0 :: OpenExp ((), Double) () Float@
--   in place of @Var ZeroIdx@ and
--   @exp1 :: OpenExp () () Double@ in place of
--   @Var (SuccIdx ZeroIdx)@ in expression @bodyExp@. This can be done with:
--
--   @substOpenExp exp1 (substOpenExp exp0 bodyExp)@
--
substOpenExp :: forall aenv env env' s t.
                OpenExp (Cat env env') aenv s
             -> OpenExp (env', s)      aenv t
             -> OpenExp (Cat env env') aenv t
substOpenExp t e = subCursorPreOpenExp Zero t (extendPreOpenExp (undefined::env) e)

substOpenFun :: forall aenv env env' s t.
                OpenExp (Cat env env') aenv s
             -> OpenFun (env', s)      aenv t
             -> OpenFun (Cat env env') aenv t
substOpenFun t f = subCursorPreOpenFun Zero t (extendPreOpenFun (undefined::env) f)

--
-- A smattering of test code.
--
{-

import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Pretty

test0 :: OpenExp ((), Double) () (Float, Float)
test0 = Tuple (SnocTup (SnocTup NilTup pie) pie)
  where pie = PrimConst (PrimPi (TypeFloat FloatingDict))

test1 :: OpenExp () () Double
test1 = PrimConst (PrimPi (TypeDouble FloatingDict))

testBody :: OpenExp (((), Double), (Float, Float)) () ((Float, Float), Double)
testBody = Tuple (SnocTup (SnocTup NilTup (Var ZeroIdx)) (Var (SuccIdx ZeroIdx)))

test = do
  print testBody
  let testBody' = substOpenExp test0 testBody
  print testBody'
  print (substOpenExp test1 testBody')
-}


