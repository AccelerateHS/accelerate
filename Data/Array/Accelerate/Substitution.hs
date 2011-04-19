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
-- This module defines the substitution functions for module "Data.Array.Accelerate.AST"
--
module Data.Array.Accelerate.Substitution (

  -- * Types
  Cat,
  -- * Functions
    substOpenExpIntoOpenExp
  , substOpenExpIntoOpenFun
  , substOpenExpIntoTuple
  , substOpenAccIntoOpenAcc
  , substOpenAccIntoOpenAfun
  , substOpenAccIntoOpenExp
  , substOpenAccIntoTuple

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
-- | 'extendIdx' extends the environment in an 'Idx' to a larger one.
--
extendIdx :: forall eenv env s t. eenv -> Idx (env, s) t -> Idx (Cat eenv env, s) t
extendIdx _ ZeroIdx               = ZeroIdx
extendIdx _ (SuccIdx ZeroIdx)     = SuccIdx ZeroIdx
extendIdx _ (SuccIdx (SuccIdx m)) = SuccIdx (extendIdx (undefined :: eenv) (SuccIdx m))

-- Encodes equality between types
data s :=: t where
  REFL :: s :=: s

liftIdx :: forall env benv s t. Cursor env benv s -> Idx (Cat env benv) t -> Idx (Cat (env, s) benv) t
liftIdx Zero      n          = SuccIdx n
liftIdx (Succ _)  ZeroIdx    = ZeroIdx
liftIdx (Succ m) (SuccIdx n) = SuccIdx (liftIdx m n)

--
-- Conventions: In the code below we often represent scalar environments with the type
-- variable @env@ and array environments with the type variable @aenv@.
-- Correspoding "bound" environments (see 'Cursor') are named @benv@ and @baenv@ 
-- respectively.
--

--
-- The @lift@ family of functions increment all free variables in either a /scalar environment/ 
-- or /array environment/ for various data structures. The former are prefixed with @liftEnv@, 
-- the latter with @liftAenv@
--
--
liftEnvTuple :: forall acc env benv aenv s t.
             Cursor env benv s
          -> Tuple (PreOpenExp acc (Cat env benv)     aenv) t
          -> Tuple (PreOpenExp acc (Cat (env,s) benv) aenv) t
liftEnvTuple n tuple = case tuple of
  NilTup          -> NilTup
  SnocTup tup elt -> SnocTup (liftEnvTuple n tup) (liftEnvPreOpenExp n elt)

liftEnvPreOpenExp :: forall acc aenv env benv s t.
        Cursor env benv s
     -> PreOpenExp acc (Cat env benv) aenv t
     -> PreOpenExp acc (Cat (env, s) benv) aenv t
liftEnvPreOpenExp n preOpenExp = case preOpenExp of
  Var ix            -> Var (liftIdx n ix)
  Const c           -> Const c
  Tuple tup         -> Tuple (liftEnvTuple n tup)
  Prj tupIx oe      -> Prj tupIx (liftEnvPreOpenExp n oe)
  IndexNil          -> IndexNil
  IndexCons hIx tIx -> IndexCons (liftEnvPreOpenExp n hIx) (liftEnvPreOpenExp n tIx)
  IndexHead e       -> IndexHead (liftEnvPreOpenExp n e)
  IndexTail e       -> IndexTail (liftEnvPreOpenExp n e)
  Cond c t e        -> Cond (liftEnvPreOpenExp n c) (liftEnvPreOpenExp n t) (liftEnvPreOpenExp n e)
  PrimConst pconst  -> PrimConst pconst
  PrimApp pfun arg  -> PrimApp pfun (liftEnvPreOpenExp n arg)
--
-- All array computations (of type 'acc') appearing in the following three
-- cases are guaranteed not to contain free scalar variables so we
-- don't need to lift.
--
  IndexScalar acc e -> IndexScalar acc (liftEnvPreOpenExp n e)
  Shape acc         -> Shape acc
  Size acc          -> Size acc

liftAenvOpenAcc :: forall aenv baenv s t.
                   Cursor aenv baenv s
                -> OpenAcc (Cat aenv baenv) t
                -> OpenAcc (Cat (aenv, s) baenv) t
liftAenvOpenAcc n (OpenAcc preOpenAcc_) = OpenAcc $ liftPOA preOpenAcc_
  where
    liftOA = liftAenvOpenAcc n
    liftOE = liftAenvOpenExp n
    liftOF = liftAenvOpenFun n
    liftPOA  :: PreOpenAcc OpenAcc (Cat aenv baenv) t 
            ->  PreOpenAcc OpenAcc (Cat (aenv, s) baenv) t
    liftPOA preOpenAcc = case preOpenAcc of
      Let bound body       -> Let (liftOA bound) (liftAenvOpenAcc (Succ n) body)
      Let2 _ _             -> INTERNAL_ERROR(error) "liftAenvOpenAcc" "Let2 not supported"
      Avar ix              -> Avar (liftIdx n ix)
      PairArrays acc1 acc2 -> PairArrays (liftOA acc1) (liftOA acc2)
      -- afun is closed so leave it alone
      Apply afun acc       -> Apply afun (liftOA acc)
      Acond e acc1 acc2    -> Acond (liftOE e) (liftOA acc1) (liftOA acc2)
      Use arr              -> Use arr
      Unit e               -> Unit (liftOE e)
      Reshape e acc        -> Reshape (liftOE e) (liftOA acc)
      Generate e fun       -> Generate (liftOE e) (liftOF fun)
      Replicate slix e acc -> Replicate slix (liftOE e) (liftOA acc)
      Index slix acc e     -> Index slix (liftOA acc) (liftOE e)
      Map fun acc          -> Map (liftOF fun) (liftOA acc)
      ZipWith fun acc1 acc2 -> ZipWith (liftOF fun) (liftOA acc1) (liftOA acc2)
      Fold fun e acc       -> Fold (liftOF fun) (liftOE e) (liftOA acc)
      Fold1 fun acc        -> Fold1 (liftOF fun) (liftOA acc)
      FoldSeg fun e acc1 acc2 -> FoldSeg (liftOF fun) (liftOE e) (liftOA acc1) (liftOA acc2)
      Fold1Seg fun acc1 acc2  -> Fold1Seg (liftOF fun)  (liftOA acc1) (liftOA acc2)
      Scanl fun e acc      -> Scanl (liftOF fun) (liftOE e) (liftOA acc)
      Scanl' fun e acc     -> Scanl' (liftOF fun) (liftOE e) (liftOA acc)
      Scanl1 fun acc       -> Scanl1 (liftOF fun) (liftOA acc)
      Scanr fun e acc      -> Scanr  (liftOF fun) (liftOE e) (liftOA acc)
      Scanr' fun e acc     -> Scanr' (liftOF fun) (liftOE e) (liftOA acc)
      Scanr1 fun acc       -> Scanr1 (liftOF fun) (liftOA acc)
      Permute f1 acc1 f2 acc2 -> Permute (liftOF f1) (liftOA acc1) (liftOF f2) (liftOA acc2)
      Backpermute e f acc  -> Backpermute (liftOE e) (liftOF f) (liftOA acc)
      Stencil fun bdry acc -> Stencil (liftOF fun) bdry (liftOA acc)
      Stencil2 fun bdry1 acc1 bdry2 acc2 -> 
        Stencil2 (liftOF fun) bdry1 (liftOA acc1) bdry2 (liftOA acc2)

liftAenvOpenExp :: forall aenv env baenv s t.
        Cursor aenv baenv                 s
     -> OpenExp env (Cat aenv baenv)      t
     -> OpenExp env (Cat (aenv, s) baenv) t
liftAenvOpenExp n openExp = case openExp of
  Var ix            -> Var ix -- lifting the array environment not the scalar environment.
  Const c           -> Const c
  Tuple tup         -> Tuple (liftAenvTuple n tup)
  Prj tupIx oe      -> Prj tupIx (liftOE oe)
  IndexNil          -> IndexNil
  IndexCons hIx tIx -> IndexCons (liftOE hIx) (liftOE tIx)
  IndexHead e       -> IndexHead (liftOE e)
  IndexTail e       -> IndexTail (liftOE e)
  Cond c t e        -> Cond (liftOE c) (liftOE t) (liftOE e)
  PrimConst pconst  -> PrimConst pconst
  PrimApp pfun arg  -> PrimApp pfun (liftOE arg)
  IndexScalar acc e -> IndexScalar (liftOA acc) (liftOE e)
  Shape acc         -> Shape (liftOA acc)
  Size acc          -> Size  (liftOA acc)
  where
    liftOE = liftAenvOpenExp n 
    liftOA = liftAenvOpenAcc n

liftAenvOpenFun :: forall aenv env baenv s t.
                   Cursor aenv baenv                 s
                -> OpenFun env (Cat aenv baenv)      t
                -> OpenFun env (Cat (aenv, s) baenv) t
liftAenvOpenFun n openFun = case openFun of
  Body e   -> Body (liftAenvOpenExp n e)
  Lam body -> Lam  (liftAenvOpenFun n body) -- lifting the array environment not the scalar environment

liftAenvTuple :: forall aenv env baenv s t.
                 Cursor aenv baenv                         s
              -> Tuple (OpenExp env (Cat aenv baenv))      t
              -> Tuple (OpenExp env (Cat (aenv, s) baenv)) t
liftAenvTuple n tuple = case tuple of
  NilTup        -> NilTup
  SnocTup tup e -> SnocTup (liftAenvTuple n tup) (liftAenvOpenExp n e)

--
-- The @sub@ family of functions substitutes an open scalar expression/array computation
-- into an entity (array computatino, expression, tuple, function) in place of a particular 
-- de Bruijn index equivalent to the value of the provided 'Cursor'.
--
--
-- The convention is that any function which substitutes a value of type 'OpenAcc' is 
-- prefixed with @subOA@ and any function which substitutes a value of type 'PreOpenExp'
-- is prefixed with @subPOE@.
--
-- You may notice that there are more functions prefixed with @subOA@. This is because 
-- one can't substitute open expressions into values of type 'OpenAcc'. All 
-- scalar expressions in such values are closed. (This can be verified by noticing that
-- all constructors requiring scalar expressions in the data type 'PreOpenAcc' take
-- 'PreExp's as arguments.)
--
subCursorVarNE :: Cursor env benv s -> Idx (Cat (env, s) benv) t -> Idx (Cat env benv) t
subCursorVarNE Zero ZeroIdx         =
  INTERNAL_ERROR(error) "subCursorVarNE" "subCursorVarNE called on Cursor and Idx that are equal"
subCursorVarNE Zero (SuccIdx n)     = n
subCursorVarNE (Succ _) (ZeroIdx)   = ZeroIdx
subCursorVarNE (Succ m) (SuccIdx n) = SuccIdx (subCursorVarNE m n)

subOAOpenAcc :: forall aenv baenv s t.
                    Cursor aenv baenv s
                 -> OpenAcc (Cat aenv baenv)      s
                 -> OpenAcc (Cat (aenv, s) baenv) t
                 -> OpenAcc (Cat aenv baenv)      t
subOAOpenAcc m t (OpenAcc pacc) = case pacc of
  Avar n -> case eqCursorIdx m n of
              Just REFL -> t
              Nothing   -> OpenAcc $ Avar (subCursorVarNE m n)
  other -> OpenAcc $ sub other
  where
    sub :: PreOpenAcc OpenAcc (Cat (aenv, s) baenv) t -> PreOpenAcc OpenAcc (Cat aenv baenv) t
    sub other = case other of  
      Let bound body       -> Let (subOA bound) 
                                  (subOAOpenAcc (Succ m) (liftAenvOpenAcc Zero t) body)
      Let2 _ _ -> error "FIXME: Let2 not supported"
      PairArrays acc1 acc2 -> PairArrays (subOA acc1) (subOA acc2)
      Avar _ -> INTERNAL_ERROR(error) "subOAOpenAcc" "Should not get here. Already covered above."
      -- Function must be closed so we can't substitute into it. But we must
      -- subtitute into the OpenAcc. 
      Apply afun acc -> Apply afun (subOA acc)
      Acond e acc1 acc2 -> Acond (subPE e) (subOA acc1) (subOA acc2)
      Use arr -> Use arr
      Unit e  -> Unit (subPE e)
      Reshape e acc -> Reshape (subPE e) (subOA acc)
      Generate e fun -> Generate (subPE e) (subPF fun)
      Replicate slix e acc -> Replicate slix (subPE e) (subOA acc)
      Index slix acc e -> Index slix (subOA acc) (subPE e)
      Map fun acc -> Map (subPF fun) (subOA acc)
      ZipWith fun acc1 acc2 -> ZipWith (subPF fun) (subOA acc1) (subOA acc2)
      Fold fun e acc -> Fold (subPF fun) (subPE e) (subOA acc) 
      Fold1 fun acc -> Fold1 (subPF fun) (subOA acc)
      FoldSeg fun e acc1 acc2 -> FoldSeg (subPF fun) (subPE e) (subOA acc1) (subOA acc2)
      Fold1Seg fun acc1 acc2 -> Fold1Seg (subPF fun) (subOA acc1) (subOA acc2)
      Scanl fun e acc -> Scanl (subPF fun) (subPE e) (subOA acc)
      Scanl' fun e acc -> Scanl' (subPF fun) (subPE e) (subOA acc)
      Scanl1 fun acc -> Scanl1 (subPF fun) (subOA acc)
      Scanr fun e acc -> Scanr (subPF fun) (subPE e) (subOA acc)
      Scanr' fun e acc -> Scanr' (subPF fun) (subPE e) (subOA acc)
      Scanr1 fun acc  -> Scanr1 (subPF fun) (subOA acc)
      Permute f1 acc1 f2 acc2 -> Permute (subPF f1) (subOA acc1) (subPF f2) (subOA acc2)
      Backpermute e f acc -> Backpermute (subPE e) (subPF f) (subOA acc)
      Stencil fun bdry acc -> Stencil (subPF fun) bdry (subOA acc)
      Stencil2 fun bdry1 acc1 bdry2 acc2 -> Stencil2 (subPF fun) bdry1 (subOA acc1) bdry2 (subOA acc2)
    subPE :: Exp (Cat (aenv, s) baenv) t' -> Exp (Cat aenv baenv) t'
    subPE = subOAOpenExp m t
    subPF :: Fun (Cat (aenv, s) baenv) t' -> Fun (Cat aenv baenv) t'
    subPF = subOAOpenFun m t
    subOA :: OpenAcc (Cat (aenv, s) baenv) t' -> OpenAcc (Cat aenv baenv) t'
    subOA = subOAOpenAcc m t

subOAOpenAfun :: forall aenv baenv s t.
                 Cursor aenv baenv s
              -> OpenAcc  (Cat aenv baenv)      s
              -> OpenAfun (Cat (aenv, s) baenv) t
              -> OpenAfun (Cat aenv baenv)      t
subOAOpenAfun m t afun = case afun of 
  Abody acc -> Abody (subOAOpenAcc m t acc)
  Alam body -> Alam $ subOAOpenAfun (Succ m) (liftAenvOpenAcc Zero t) body

-- Substituting an 'OpenAcc' into a 'PreOpenFun OpenAcc'
subOAOpenFun :: forall aenv baenv env s t.
                 Cursor aenv baenv s
              -> OpenAcc         (Cat aenv baenv)      s
              -> OpenFun env (Cat (aenv, s) baenv) t
              -> OpenFun env (Cat aenv baenv)      t
subOAOpenFun m t preOpenFun = case preOpenFun of
  Body e   -> Body (subOAOpenExp m t e)
  Lam body -> Lam (subOAOpenFun m t body)

    
-- Substituting an 'OpenAcc' into a 'PreOpenExp OpenAcc'
subOAOpenExp :: forall aenv baenv env s t.
                   Cursor aenv baenv s
                -> OpenAcc        (Cat aenv baenv)      s
                -> OpenExp env (Cat (aenv, s) baenv) t
                -> OpenExp env (Cat aenv baenv)      t
subOAOpenExp m t preOpenExp = case preOpenExp of
  Var ix            -> Var ix
  Const e           -> Const e
  Tuple tup         -> Tuple $ subOATuple m t tup
  Prj tupIdx e      -> Prj tupIdx (subPOE e)
  IndexNil          -> IndexNil
  IndexCons e1 e2   -> IndexCons (subPOE e1) (subPOE e2)
  IndexHead hexp    -> IndexHead (subPOE hexp)
  IndexTail texp    -> IndexTail (subPOE texp)
  Cond e1 e2 e3     -> Cond (subPOE e1) (subPOE e2) (subPOE e3)
  PrimConst pconst  -> PrimConst pconst
  PrimApp pfun e    -> PrimApp pfun (subPOE e)  
  IndexScalar acc e -> IndexScalar (subOA acc) (subPOE e)
  Shape acc         -> Shape (subOA acc)
  Size  acc         -> Size (subOA acc)
  where
    subPOE :: OpenExp env (Cat (aenv, s) baenv) t' 
           -> OpenExp env (Cat aenv baenv)      t'
    subPOE = subOAOpenExp m t
    subOA :: OpenAcc (Cat (aenv, s) baenv) t'  -> OpenAcc (Cat aenv baenv) t' 
    subOA = subOAOpenAcc m t

subOATuple :: Cursor aenv baenv s
           -> OpenAcc                (Cat aenv baenv)      s
           -> Tuple (PreOpenExp OpenAcc env (Cat (aenv, s) baenv)) t
           -> Tuple (PreOpenExp OpenAcc env (Cat aenv baenv))      t
subOATuple m t tuple = case tuple of
  NilTup       -> NilTup
  SnocTup tup e -> SnocTup (subOATuple m t tup) (subOAOpenExp m t e)

subPOEPreOpenExp :: Cursor env benv s
                 -> PreOpenExp acc (Cat env benv) aenv s
                 -> PreOpenExp acc (Cat (env, s) benv) aenv t
                 -> PreOpenExp acc (Cat env benv) aenv t
subPOEPreOpenExp m t preOpenExp = case preOpenExp of 
  Var n               -> case eqCursorIdx m n of
                           Just REFL -> t
                           Nothing   -> Var (subCursorVarNE m n)
  Const c             -> Const c
  Tuple tup           -> Tuple (subPOETuple m t tup)
  Prj tupIdx e        -> Prj tupIdx (subPOE e)
  IndexNil            -> IndexNil
  IndexCons sh idx    -> IndexCons (subPOE sh)
                                   (subPOE idx)
  IndexHead hexp      -> IndexHead (subPOE hexp)
  IndexTail texp      -> IndexTail (subPOE texp)
  Cond cexp texp eexp -> Cond (subPOE cexp) (subPOE texp) (subPOE eexp)
  PrimConst cnst      -> PrimConst cnst
  PrimApp fun arg     -> PrimApp fun (subPOE arg)
--
-- All array computations (of type 'acc') appearing in the following three
-- cases are guaranteed not to contain free scalar variables so we
-- don't need to substitute. See definition of 'PreOpenAcc' data type.
--
  IndexScalar acc e   -> IndexScalar acc (subPOEPreOpenExp m t e)
  Shape sh            -> Shape sh
  Size acc            -> Size acc
  where
    subPOE = subPOEPreOpenExp m t 

subPOETuple :: Cursor env benv s
            -> PreOpenExp acc (Cat env benv) aenv s
            -> Tuple (PreOpenExp acc (Cat (env, s) benv) aenv) t
            -> Tuple (PreOpenExp acc (Cat env benv)      aenv) t
subPOETuple _ _ NilTup          = NilTup
subPOETuple m t (SnocTup tup e) = SnocTup (subPOETuple m t tup) (subPOEPreOpenExp m t e)

subPOEPreOpenFun :: Cursor env benv s
                    -> PreOpenExp acc (Cat env benv) aenv s
                    -> PreOpenFun acc (Cat (env, s) benv) aenv t
                    -> PreOpenFun acc (Cat env benv) aenv t
subPOEPreOpenFun m t (Body e)   = Body (subPOEPreOpenExp m t e)
subPOEPreOpenFun m t (Lam body) = Lam  (subPOEPreOpenFun (Succ m) (liftEnvPreOpenExp Zero t) body)

extAenvOpenAcc :: forall aenv aenv' s t.
                   aenv 
               -> OpenAcc (aenv', s) t 
               -> OpenAcc (Cat aenv aenv', s) t
extAenvOpenAcc witness (OpenAcc pacc_) = OpenAcc $ extPOA pacc_
  where 
    extPOA :: PreOpenAcc OpenAcc (aenv2, s2) t2 -> PreOpenAcc OpenAcc (Cat aenv aenv2, s2) t2
    extPOA pacc = case pacc of
      Avar idx -> Avar (extendIdx witness idx)
      Let bound body       -> Let (extOA bound) (extOA body)
      Let2 _ _ -> error "FIXME: Let2 not supported"
      PairArrays acc1 acc2 -> PairArrays (extOA acc1) (extOA acc2)
      -- Function must be closed so we can't extstitute into it. But we must
      -- exttitute into the OpenAcc. 
      Apply afun acc -> Apply afun (extOA acc)
      Acond e acc1 acc2 -> Acond (extOE e) (extOA acc1) (extOA acc2)
      Use arr -> Use arr
      Unit e  -> Unit (extOE e)
      Reshape e acc -> Reshape (extOE e) (extOA acc)
      Generate e fun -> Generate (extOE e) (extOF fun)
      Replicate slix e acc -> Replicate slix (extOE e) (extOA acc)
      Index slix acc e -> Index slix (extOA acc) (extOE e)
      Map fun acc -> Map (extOF fun) (extOA acc)
      ZipWith fun acc1 acc2 -> ZipWith (extOF fun) (extOA acc1) (extOA acc2)
      Fold fun e acc -> Fold (extOF fun) (extOE e) (extOA acc) 
      Fold1 fun acc -> Fold1 (extOF fun) (extOA acc)
      FoldSeg fun e acc1 acc2 -> FoldSeg (extOF fun) (extOE e) (extOA acc1) (extOA acc2)
      Fold1Seg fun acc1 acc2 -> Fold1Seg (extOF fun) (extOA acc1) (extOA acc2)
      Scanl fun e acc -> Scanl (extOF fun) (extOE e) (extOA acc)
      Scanl' fun e acc -> Scanl' (extOF fun) (extOE e) (extOA acc)
      Scanl1 fun acc -> Scanl1 (extOF fun) (extOA acc)
      Scanr fun e acc -> Scanr (extOF fun) (extOE e) (extOA acc)
      Scanr' fun e acc -> Scanr' (extOF fun) (extOE e) (extOA acc)
      Scanr1 fun acc  -> Scanr1 (extOF fun) (extOA acc)
      Permute f1 acc1 f2 acc2 -> Permute (extOF f1) (extOA acc1) (extOF f2) (extOA acc2)
      Backpermute e f acc -> Backpermute (extOE e) (extOF f) (extOA acc)
      Stencil fun bdry acc -> Stencil (extOF fun) bdry (extOA acc)
      Stencil2 fun bdry1 acc1 bdry2 acc2 -> Stencil2 (extOF fun) bdry1 (extOA acc1) bdry2 (extOA acc2)
    -- 'aenv' type variable in scope. The rest are fresh (on purpose since we need 
    -- polymorphic recursion)
    extOE :: OpenExp env (aenv2, s2) t2 -> OpenExp env (Cat aenv aenv2, s2) t2
    extOE = extAenvOpenExp witness
    extOF :: OpenFun env (aenv2, s2) t2 -> OpenFun env (Cat aenv aenv2, s2) t2
    extOF = extAenvOpenFun witness
    extOA :: OpenAcc (aenv2, s2) t2 -> OpenAcc (Cat aenv aenv2, s2) t2
    extOA = extAenvOpenAcc witness

--
-- The @ext@ family of functions takes an entity (expression, tuple, function) and extends
-- its scalar environment/array environment.
--
-- e.g. @PreOpenExp acc ((), Int) aenv Int@ could have its environment extended to
--      @PreOpenExp acc (((), Word), Int) aenv Int@.
--
--
-- Functions which extend the scalar environment are prefixed with @extEnv@.
-- Functions which extend the array environment are prefixed with @extAenv@.
--
extEnvPreOpenExp :: forall acc aenv env env' s t.
          env -> PreOpenExp acc (env', s) aenv t -> PreOpenExp acc (Cat env env', s) aenv t
extEnvPreOpenExp witness preOpenExp = case preOpenExp of
  Var n               -> Var (extendIdx witness n)
  Const c             -> Const c
  Tuple tup           -> Tuple (extEnvTuple witness tup)
  Prj tupIdx e        -> Prj tupIdx (extPOE e)
  IndexNil            -> IndexNil
  IndexCons sh idx    -> IndexCons (extPOE sh) (extPOE idx) 
  IndexHead hexp      -> IndexHead (extPOE hexp)
  IndexTail texp      -> IndexTail (extPOE texp)
  Cond cexp texp eexp -> Cond (extPOE cexp) (extPOE texp) (extPOE eexp)
  PrimConst cnst      -> PrimConst cnst
  PrimApp fun arg     -> PrimApp fun (extPOE arg)
  IndexScalar acc e   -> IndexScalar acc (extPOE e)
  Shape sh            -> Shape sh
  Size acc            -> Size acc
  where 
    extPOE :: PreOpenExp acc (env', s) aenv t' -> PreOpenExp acc (Cat env env', s) aenv t'
    extPOE = extEnvPreOpenExp witness

extAenvOpenExp :: forall aenv aenv' env s t.
                  aenv -> OpenExp env (aenv', s) t -> OpenExp env (Cat aenv aenv', s) t
extAenvOpenExp witness preOpenExp = case preOpenExp of
  Var n               -> Var n
  Const c             -> Const c
  Tuple tup           -> Tuple (extAenvTuple witness tup)
  Prj tupIdx e        -> Prj tupIdx (extPOE e)
  IndexNil            -> IndexNil
  IndexCons sh idx    -> IndexCons (extPOE sh) (extPOE idx) 
  IndexHead hexp      -> IndexHead (extPOE hexp)
  IndexTail texp      -> IndexTail (extPOE texp)
  Cond cexp texp eexp -> Cond (extPOE cexp) (extPOE texp) (extPOE eexp)
  PrimConst cnst      -> PrimConst cnst
  PrimApp fun arg     -> PrimApp fun (extPOE arg)
  IndexScalar acc e   -> IndexScalar (extOA acc) (extPOE e)
  Shape sh            -> Shape (extOA sh)
  Size acc            -> Size (extOA acc)
  where 
    extPOE :: OpenExp env (aenv', s) t' -> OpenExp env (Cat aenv aenv', s) t'
    extPOE = extAenvOpenExp witness
    extOA :: OpenAcc (aenv', s) t' -> OpenAcc (Cat aenv aenv', s) t'
    extOA = extAenvOpenAcc witness
  
extAenvOpenFun :: forall aenv aenv' env s t.
                  aenv -> OpenFun env (aenv', s) t -> OpenFun env (Cat aenv aenv', s) t
extAenvOpenFun witness preOpenFun = case preOpenFun of
  Body e   -> Body (extAenvOpenExp witness e)
  Lam body -> Lam  (extAenvOpenFun witness body)

extEnvTuple :: forall acc aenv env env' s t.
             env
          -> Tuple (PreOpenExp acc (env', s) aenv) t
          -> Tuple (PreOpenExp acc (Cat env env', s) aenv) t
extEnvTuple witness tuple = case tuple of 
  NilTup         -> NilTup
  SnocTup tup e  -> SnocTup (extEnvTuple witness tup)
                            (extEnvPreOpenExp witness e)

extAenvTuple :: forall aenv aenv' env s t.
             aenv
          -> Tuple (OpenExp env (aenv', s)) t
          -> Tuple (OpenExp env (Cat aenv aenv', s)) t
extAenvTuple witness tuple = case tuple of 
  NilTup         -> NilTup
  SnocTup tup e  -> SnocTup (extAenvTuple witness tup)
                            (extAenvOpenExp witness e)

extEnvPreOpenFun :: forall acc aenv env env' s t.
                    env
                 -> PreOpenFun acc (env', s) aenv t
                 -> PreOpenFun acc (Cat env env', s) aenv t
extEnvPreOpenFun witness preOpenFun = case preOpenFun of
  Body e   -> Body (extEnvPreOpenExp witness e)
  Lam body -> Lam  (extEnvPreOpenFun witness body)

extAenvOpenAfun :: forall aenv aenv'  s t.
                    aenv
                 -> OpenAfun (aenv', s)  t
                 -> OpenAfun (Cat aenv aenv', s)  t
extAenvOpenAfun witness openAfun = case openAfun of
  Abody e   -> Abody (extAenvOpenAcc witness e)
  Alam body -> Alam (extAenvOpenAfun witness body)

--
-- | The @subst@ family of functions provide the capability to substitute
--   an open scalar expression/open array computation into a variety of entities (
--   open scalar expresions, open array computations, tuples, open scalar functions,
--   open array computation functions) place of varieble with de Bruijn index equal to 
--   zero. (i.e. ZeroIdx).
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
--   a substitution function.
--   Say you wanted to substitute @exp0 :: OpenExp ((), Double) () Float@
--   in place of @Var ZeroIdx@ and
--   @exp1 :: OpenExp () () Double@ in place of
--   @Var (SuccIdx ZeroIdx)@ in expression @bodyExp@. This can be done with:
--
--   @substOpenExpIntoOpenExp exp1 (substOpenExpIntoOpenExp exp0 bodyExp)@
--
substOpenExpIntoOpenExp :: forall aenv env env' s t.
                OpenExp (Cat env env') aenv s
             -> OpenExp (env', s)      aenv t
             -> OpenExp (Cat env env') aenv t
substOpenExpIntoOpenExp t e = subPOEPreOpenExp Zero t (extEnvPreOpenExp (undefined::env) e)

substOpenExpIntoOpenFun :: forall aenv env env' s t.
                OpenExp (Cat env env') aenv s
             -> OpenFun (env', s)      aenv t
             -> OpenFun (Cat env env') aenv t
substOpenExpIntoOpenFun t f = subPOEPreOpenFun Zero t (extEnvPreOpenFun (undefined::env) f)

substOpenExpIntoTuple :: forall aenv env env' s t.
                OpenExp (Cat env env') aenv s
             -> Tuple   (OpenExp (env', s)      aenv) t
             -> Tuple   (OpenExp (Cat env env') aenv) t
substOpenExpIntoTuple t tup = subPOETuple Zero t (extEnvTuple (undefined::env) tup)


substOpenAccIntoOpenAcc :: forall aenv aenv' s t.
                OpenAcc (Cat aenv aenv') s
             -> OpenAcc (aenv', s)       t
             -> OpenAcc (Cat aenv aenv') t
substOpenAccIntoOpenAcc t acc =  subOAOpenAcc Zero t (extAenvOpenAcc (undefined :: aenv) acc)

substOpenAccIntoOpenAfun :: forall aenv aenv' s t.
                OpenAcc (Cat aenv aenv') s
             -> OpenAfun (aenv', s)       t
             -> OpenAfun (Cat aenv aenv') t
substOpenAccIntoOpenAfun t afun = subOAOpenAfun Zero t (extAenvOpenAfun (undefined :: aenv) afun)

substOpenAccIntoOpenExp :: forall env aenv aenv' s t.
                           OpenAcc (Cat aenv aenv')     s
                        -> OpenExp env (aenv', s)       t
                        -> OpenExp env (Cat aenv aenv') t
substOpenAccIntoOpenExp t oe = subOAOpenExp Zero t (extAenvOpenExp (undefined::aenv) oe)

substOpenAccIntoTuple :: forall env aenv aenv' s t.
                           OpenAcc (Cat aenv aenv')             s
                        -> Tuple (OpenExp env (aenv', s))       t
                        -> Tuple (OpenExp env (Cat aenv aenv')) t
substOpenAccIntoTuple t oe = subOATuple Zero t (extAenvTuple (undefined::aenv) oe)



--
-- A smattering of test code.
--
{-

import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Pretty
import Data.Array.Accelerate.Array.Sugar

test0 :: OpenExp ((), Double) () (Float, Float)
test0 = Tuple (SnocTup (SnocTup NilTup pie) pie)
  where pie = PrimConst (PrimPi (TypeFloat FloatingDict))

test1 :: OpenExp () () Double
test1 = PrimConst (PrimPi (TypeDouble FloatingDict))

testBody :: OpenExp (((), Double), (Float, Float)) () ((Float, Float), Double)
testBody = Tuple (SnocTup (SnocTup NilTup (Var ZeroIdx)) (Var (SuccIdx ZeroIdx)))

atest0 :: Acc (Array DIM1 Int)
atest0 = OpenAcc $ Use $ fromList (Z:.1) [1]

-- map (\x0 -> (+) (x0, a0 ! (Z :. 1))) a0
fun :: Fun ((), Array DIM1 Int) (Int -> Int)
fun = Lam (Body (PrimApp (PrimAdd (IntegralNumType (TypeInt IntegralDict)))
                         (Tuple (SnocTup (SnocTup NilTup (Var ZeroIdx)) (arrayIndex)))))
 where arrayIndex = IndexScalar (OpenAcc $ Avar ZeroIdx) (Const ((),1))

atestBody :: OpenAcc ((), Array DIM1 Int) (Array DIM1 Int)
atestBody =  OpenAcc $ Map fun avar
  where avar :: OpenAcc ((), Array DIM1 Int) (Array DIM1 Int)
        avar = OpenAcc $ Avar ZeroIdx
test = do
  print testBody
  let testBody' = substOpenExpIntoOpenExp test0 testBody
  print testBody'
  print (substOpenExpIntoOpenExp test1 testBody')
-}


