{-# LANGUAGE CPP                  #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo.Vectorise
-- Copyright   : [2012..2013] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell, Robert Clifton-Everest
-- License     : BSD3
--
-- Maintainer  : Robert Clifton-Everest <robertce@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Performs Blelloch's flattening transform.
--

module Data.Array.Accelerate.Trafo.Vectorise (

  liftExp

) where

import Prelude                                          hiding ( exp, replicate )
import qualified Prelude                                as P
import Data.Typeable

-- friends
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Trafo.Base
import Data.Array.Accelerate.Trafo.Substitution
import Data.Array.Accelerate.Analysis.Match

import qualified Data.Array.Accelerate.Debug            as Stats

#include "accelerate.h"

type family ArraysOfTupleRepr sh t
type instance ArraysOfTupleRepr sh ()    = ()
type instance ArraysOfTupleRepr sh (t,e) = (ArraysOfTupleRepr sh t, Array sh e)

type family ExpandEnv env env'
type instance ExpandEnv env ()        = env
type instance ExpandEnv env (env', t) = ExpandEnv (env, t) env'

type TupleEnv aenv sh t = ExpandEnv aenv (ArraysOfTupleRepr sh (TupleRepr t))

type ArraysOfTuple sh t = ArraysOfTupleRepr sh (EltRepr t)

data Delta env aenv aenv' where
  Empty       :: Delta () () ()
  PushExpD    :: (Shape sh, Elt e)
              => Delta env aenv aenv'
              -> sh {- dummy -}
              -> Delta (env, e) aenv (aenv', Array sh e)
  PushZExpD   :: (Elt e)
              => Delta env aenv aenv'
              -> Delta (env, e) aenv (aenv', Array Z e)
  PushAccD    :: (Arrays t)
              => Delta env aenv aenv'
              -> Delta env (aenv, t) (aenv', t)

type LiftAcc acc = forall env aenv aenv' t. Delta env aenv aenv'
                   -> acc env aenv t
                   -> acc ()  aenv' t

liftPreOpenAcc :: forall acc env aenv aenv' t. (Kit acc, Arrays t)
               => LiftAcc acc
               -> Delta env aenv aenv'
               -> PreOpenAcc acc env aenv t
               -> PreOpenAcc acc () aenv' t
liftPreOpenAcc liftAcc delta exp
  = case exp of
    Alet a b            -> aletL a b
    Elet e a            -> eletL e a
    Avar ix             -> avarL ix
    Atuple tup          -> Atuple (cvtT tup)
    Aprj tup a          -> Aprj tup (cvtA a)
    Apply f a           -> Apply (cvtAfun f) (cvtA a)
    Aforeign ff afun as -> Aforeign ff afun (cvtA as)
    Acond p t e         -> acondL (cvtE' p) (cvtA t) (cvtA e)
    Use a               -> Use a
    Unit e              -> Alet (cvtE' e) (inject $ Unit topEA)
    Reshape e a         -> Alet (cvtE' e) (inject $ Reshape topEA (weaken (cvtA a)))
    Generate e f        -> generateL (cvtE' e) (cvtF1 f)
    --Transform sh ix f a -> Transform (cvtE sh) (cvtF ix) (cvtF f) (cvtA a)
    Replicate sl slix a -> Alet (cvtE' slix) (inject $ Replicate sl topEA (weaken (cvtA a)))
    Slice sl a slix     -> Alet (cvtE' slix) (inject $ Slice sl (weaken (cvtA a)) topEA)
    Map f a             -> cvtF1 f (cvtA a)
    ZipWith f a1 a2     -> cvtF2 f (cvtA a1) (cvtA a2)
    --Fold f z a          -> Fold (cvtF f) (cvtE z) (cvtA a)
    --Fold1 f a           -> Fold1 (cvtF f) (cvtA a)
    --FoldSeg f z a s     -> FoldSeg (cvtF f) (cvtE z) (cvtA a) (cvtA s)
    --Fold1Seg f a s      -> Fold1Seg (cvtF f) (cvtA a) (cvtA s)
    --Scanl f z a         -> Scanl (cvtF f) (cvtE z) (cvtA a)
    --Scanl' f z a        -> Scanl' (cvtF f) (cvtE z) (cvtA a)
    --Scanl1 f a          -> Scanl1 (cvtF f) (cvtA a)
    --Scanr f z a         -> Scanr (cvtF f) (cvtE z) (cvtA a)
    --Scanr' f z a        -> Scanr' (cvtF f) (cvtE z) (cvtA a)
    --Scanr1 f a          -> Scanr1 (cvtF f) (cvtA a)
    --Permute f1 a1 f2 a2 -> Permute (cvtF f1) (cvtA a1) (cvtF f2) (cvtA a2)
    --Backpermute sh f a  -> Backpermute (cvtE sh) (cvtF f) (cvtA a)
    --Stencil f b a       -> Stencil (cvtF f) b (cvtA a)
    --Stencil2 f b1 a1 b2 a2
    --                    -> Stencil2 (cvtF f) b1 (cvtA a1) b2 (cvtA a2)

  where
    cvtA :: forall t. acc env aenv t -> acc () aenv' t
    cvtA = liftAcc delta

    cvtE :: forall e sh. Shape sh
         => PreOpenExp acc env aenv e
         -> acc ((),sh) aenv' (Array sh e)
    cvtE = inject . liftExp liftAcc delta

    cvtE' :: forall e. Elt e
          => PreOpenExp acc env aenv e
          -> acc () aenv' (Array Z e)
    cvtE' exp = inlineAE rebuildAcc (cvtE exp) (Const ())

    cvtT :: forall t.
            Atuple (acc env aenv) t
         -> Atuple (acc () aenv') t
    cvtT NilAtup        = NilAtup
    cvtT (SnocAtup t a) = SnocAtup (cvtT t) (cvtA a)

    cvtAfun :: forall f.
               PreOpenAfun acc env aenv f
            -> PreOpenAfun acc () aenv' f
    cvtAfun = cvtAfun' delta
      where
        cvtAfun' :: forall env aenv aenv' f.
                    Delta env aenv aenv'
                 -> PreOpenAfun acc env aenv  f
                 -> PreOpenAfun acc ()  aenv' f
        cvtAfun' d (Alam f)  = Alam $ cvtAfun' (PushAccD d) f
        cvtAfun' d (Abody f) = Abody $ liftAcc d f

    cvtF1 :: forall a b sh. Shape sh
          => PreOpenFun acc env aenv  (a -> b)
          -> acc            ()  aenv' (Array sh a)
          -> PreOpenAcc acc ()  aenv' (Array sh b)
    cvtF1 (Lam (Body f)) a = Alet a (inlineAE rebuildAcc f' (Shape (inject $ Avar ZeroIdx)))
      where
        f' = inject $ liftExp liftAcc (PushExpD delta (undefined :: sh)) f

    cvtF2 :: forall a b c sh. Shape sh
          => PreOpenFun acc env aenv  (a -> b -> c)
          -> acc            ()  aenv' (Array sh a)
          -> acc            ()  aenv' (Array sh b)
          -> PreOpenAcc acc ()  aenv' (Array sh c)
    cvtF2 (Lam (Lam (Body f))) a b = (Alet a . inject . Alet b') (inlineAE rebuildAcc f' (Shape (inject $ Avar ZeroIdx)))
      where
        f' :: acc ((),sh) ((aenv', Array sh a), Array sh b) (Array sh c)
        f' = inject $ liftExp liftAcc (PushExpD (PushExpD delta (undefined :: sh)) (undefined :: sh)) f

        b' :: acc () (aenv', Array sh a) (Array sh b)
        b' = rebuildAcc Var (Avar . SuccIdx) b

    aletL :: Arrays bnd => acc env aenv bnd -> acc env (aenv, bnd) t -> PreOpenAcc acc () aenv' t
    aletL bnd body = Alet (liftAcc delta bnd) (liftAcc (PushAccD delta) body)

    eletL :: forall bnd body. (Elt bnd, Arrays body) => PreOpenExp acc env aenv bnd -> acc (env, bnd) aenv body -> PreOpenAcc acc () aenv' body
    eletL bnd body = Alet (inject bnd') (liftAcc (PushZExpD delta) body)
      where
        bnd' :: PreOpenAcc acc () aenv' (Array Z bnd)
        bnd' = extract $ inlineAE rebuildAcc (cvtE bnd) (Const ())

    avarL :: Arrays t
          => Idx aenv t
          -> PreOpenAcc acc () aenv' t
    avarL = Avar . cvtIx delta
      where
        cvtIx :: forall env aenv aenv'. Delta env aenv aenv' -> Idx aenv t -> Idx aenv' t
        cvtIx (PushExpD d _)  ix           = SuccIdx (cvtIx d ix)
        cvtIx (PushZExpD d)   ix           = SuccIdx (cvtIx d ix)
        cvtIx (PushAccD d)    ZeroIdx      = ZeroIdx
        cvtIx (PushAccD d)    (SuccIdx ix) = SuccIdx (cvtIx d ix)
        cvtIx _               _            = INTERNAL_ERROR(error) "liftExp" "Inconsistent valuation"

    acondL :: Arrays t
           => acc () aenv' (Array Z Bool)
           -> acc () aenv' t
           -> acc () aenv' t
           -> PreOpenAcc acc () aenv' t
    acondL p t e = Alet p (inject $ Acond topEA (weaken t) (weaken e))

    generateL :: forall sh e. (Elt e, Shape sh)
              => acc () aenv' (Array Z sh)
              -> (acc () aenv' (Array sh sh) -> PreOpenAcc acc () aenv' (Array sh e))
              -> PreOpenAcc acc () aenv' (Array sh e)
    generateL e f = f (inject $ Alet e gen)
      where
        gen :: acc () (aenv', Array Z sh) (Array sh sh)
        gen = inject $ Generate topEA (Lam (Body (Var ZeroIdx)))

liftExp :: forall acc env aenv aenv' aenv'' sh e e'. (Kit acc, Shape sh)
        => LiftAcc acc
        -> Delta env aenv aenv'
        -> PreOpenExp acc env aenv e
        -> PreOpenAcc acc ((),sh) aenv' (Array sh e)
liftExp liftAcc delta exp
  = case exp of
      Let bnd body              -> letL bnd body
      Var ix                    -> varL delta ix id
      Const c                   -> replicate (Const c)
      Tuple tup                 -> cvtT tup
      Prj ix t                  -> Map (fun1 (Prj ix)) (cvtE t)
      IndexNil                  -> replicate IndexNil
      IndexAny                  -> replicate IndexAny
      IndexCons sh sz           -> ZipWith (fun2 IndexCons) (cvtE sh) (cvtE sz)
      IndexHead sh              -> Map (fun1 IndexHead) (cvtE sh)
      IndexTail sh              -> Map (fun1 IndexTail) (cvtE sh)
      IndexSlice x ix sh        -> ZipWith (fun2 (IndexSlice x)) (cvtE ix) (cvtE sh)
      IndexFull x ix sl         -> ZipWith (fun2 (IndexFull x)) (cvtE ix) (cvtE sl)
      ToIndex sh ix             -> ZipWith (fun2 ToIndex) (cvtE sh) (cvtE ix)
      FromIndex sh ix           -> ZipWith (fun2 FromIndex) (cvtE sh) (cvtE ix)
      Cond p t e                -> condL p t e
      --While p it i              -> Awhile (cvtF1' p) (cvtF1 it) (cvtE i)
      PrimConst c               -> replicate (PrimConst c)
      PrimApp f x               -> Map (Lam (Body (PrimApp f (Var ZeroIdx)))) (cvtE x)
      Index a sh                -> indexL a sh
      LinearIndex a i           -> linearIndexL a i
      Shape a                   -> shapeL a
      ShapeSize sh              -> Map (fun1 ShapeSize) (cvtE sh)
      Intersect s t             -> ZipWith (fun2 Intersect) (cvtE s) (cvtE t)
      Foreign ff f e            -> Map (fun1 (Foreign ff f)) (cvtE e)
  where
    cvtE :: forall sh e. Shape sh
         => PreOpenExp acc env aenv e
         -> acc ((),sh) aenv' (Array sh e)
    cvtE exp' = inject $ liftExp liftAcc delta exp'

    cvtA :: forall sh' e'. acc env aenv (Array sh' e')
         -> acc ((),sh) aenv' (Array sh' e')
    cvtA = rebuildAcc (Var . SuccIdx) Avar . liftAcc delta

    cvtF1 :: PreOpenFun acc env aenv (a -> b)
          -> PreOpenAfun acc ((),sh) aenv' (Array sh a -> Array sh b)
    cvtF1 (Lam (Body f)) = (Alam . Abody) (inject $ liftExp liftAcc (PushExpD delta (undefined::sh)) f)

    -- (a, b) => let a' = a^ in
    --             let b' = b^ in generate (\ix -> (a' ! ix, b' ! ix))
    -- RCE: Ideally we would like to do this by lifting the tuple into a tuple of arrays.
    -- Unfortunately this can't be done because the type system us unable to recognise that the
    -- lifted tuple is an instance of IsTuple.
    cvtT :: (Elt e, IsTuple e)
         => Tuple (PreOpenExp acc env aenv) (TupleRepr e)
         -> PreOpenAcc acc ((),sh) aenv' (Array sh e)
    cvtT t = cvtT' t cvtE gen
      where
        cvtT' :: forall t aenv'.
                 Tuple (PreOpenExp acc env aenv) t
              -> (forall e. PreOpenExp acc env aenv e -> acc ((),sh) aenv' (Array sh e))
              -> PreOpenAcc acc ((),sh) (ExpandEnv aenv' (ArraysOfTupleRepr sh t)) (Array sh e)
              -> PreOpenAcc acc ((),sh) aenv'                                      (Array sh e)
        cvtT' NilTup lift arr        = arr
        cvtT'(SnocTup t' e) lift arr = Alet (lift e) (inject $ cvtT' t' lift' arr)
          where
            lift' :: forall e e'. PreOpenExp acc env aenv e -> acc ((),sh) (aenv', Array sh e') (Array sh e)
            lift' = inject . weakenA rebuildAcc SuccIdx . extract . lift

        gen :: PreOpenAcc acc ((),sh) (TupleEnv aenv' sh e) (Array sh e)
        gen = Generate (Var ZeroIdx) (Lam (Body (Tuple t')))
          where
            t' :: Tuple (PreOpenExp acc (((),sh),sh) (TupleEnv aenv' sh e)) (TupleRepr e)
            t' = weakenTup (ixt (undefined :: aenv') t) (mkTup t)
              where
                mkTup :: forall e c. Tuple c e
                      -> Tuple (PreOpenExp acc (((),sh),sh) (ArraysOfTupleRepr sh e)) e
                mkTup NilTup          = NilTup
                mkTup (SnocTup t'' e) = SnocTup (weakenTup SuccIdx (mkTup t'')) e'
                  where
                    e' :: forall s e'. e ~ (s,e') => PreOpenExp acc (((),sh),sh) (ArraysOfTupleRepr sh e) e'
                    e' = Index (inject (Avar ZeroIdx)) (Var ZeroIdx)

        weakenTup :: forall env aenv aenv' e. aenv :> aenv'
                  -> Tuple (PreOpenExp acc env aenv) e
                  -> Tuple (PreOpenExp acc env aenv') e
        weakenTup v = rebuildTup rebuildAcc Var (Avar . v)

        tix :: forall t c env e. Tuple c t -> Idx env e -> Idx (ExpandEnv env (ArraysOfTupleRepr sh t)) e
        tix NilTup ix        = ix
        tix (SnocTup t (_:: c t')) ix = tix t ix'
          where
            ix' :: Idx (env, Array sh t') e
            ix' = SuccIdx ix

        ixt :: forall t c env e. env {- dummy -}
            -> Tuple c t
            -> Idx (ArraysOfTupleRepr sh t) e
            -> Idx (ExpandEnv env (ArraysOfTupleRepr sh t)) e
        ixt _   (SnocTup NilTup _) ZeroIdx      = ZeroIdx
        ixt _   (SnocTup t      _) ZeroIdx      = tix t (ZeroIdx :: Idx (env, e) e)
        ixt env (SnocTup t      _) (SuccIdx ix) = ixt env' t ix
          where
            env' :: forall s e'. t ~ (s,e') => (env, Array sh e')
            env' = undefined -- dummy argument

    replicate :: Elt e => PreOpenExp acc (((), sh), sh) aenv' e -> PreOpenAcc acc ((),sh) aenv' (Array sh e)
    replicate c = Generate (Var ZeroIdx) (Lam (Body c))

    matchShape :: forall sh1 sh2. (Shape sh1, Shape sh2) => sh1 -> sh2 -> Maybe (sh1 :=: sh2)
    matchShape _ _ = gcast REFL -- TODO: Have a way to reify shapes

    varL :: forall env aenv aenv''. (Elt e, Shape sh)
         => Delta env aenv aenv''
         -> Idx env e
         -> (forall a. Idx aenv'' a -> Idx aenv' a)
         -> PreOpenAcc acc ((),sh) aenv' (Array sh e)
    varL (PushExpD _ (_::sh')) ZeroIdx cvt
      = case matchShape (undefined :: sh) (undefined :: sh') of
          Just REFL -> Avar (cvt ZeroIdx)
          _         -> INTERNAL_ERROR(error) "liftExp" "Unexpected incorrect shape"
    varL (PushZExpD d)   ZeroIdx      cvt = replicate (Index (inject $ Avar (cvt ZeroIdx)) IndexNil)
    varL (PushExpD d _)  (SuccIdx ix) cvt = varL d ix (cvt . SuccIdx)
    varL (PushZExpD d)   (SuccIdx ix) cvt = varL d ix (cvt . SuccIdx)
    varL (PushAccD d)    ix           cvt = varL d ix (cvt . SuccIdx)
    varL _               _            cvt = INTERNAL_ERROR(error) "liftExp" "Inconsistent valuation"

    letL :: forall bnd_t. (Elt e, Elt bnd_t)
         => PreOpenExp acc env          aenv  bnd_t
         -> PreOpenExp acc (env, bnd_t) aenv  e
         -> PreOpenAcc acc ((),sh)      aenv' (Array sh e)
    letL bnd body = Alet bnd' (inject body')
      where
        bnd'  = cvtE bnd

        body' :: PreOpenAcc acc ((),sh) (aenv', Array sh bnd_t) (Array sh e)
        body' = liftExp liftAcc (PushExpD delta (undefined :: sh)) body

    prjL :: forall t. (Elt e, IsTuple t, Elt t)
         => TupleIdx (TupleRepr t) e
         -> PreOpenExp acc env     aenv  t
         -> PreOpenAcc acc ((),sh) aenv' (Array sh e)
    prjL ix t = Map (fun1 (Prj ix)) (cvtE t)

    condL :: Elt e
          => PreOpenExp acc env     aenv  Bool
          -> PreOpenExp acc env     aenv  e
          -> PreOpenExp acc env     aenv  e
          -> PreOpenAcc acc ((),sh) aenv' (Array sh e)
    condL p t e = ZipWith (fun2 decide) (cvtE p) (inject $ ZipWith (fun2 tup) (cvtE t) (cvtE e))
      where
        decide p' ab = Cond p' (Prj (SuccTupIdx ZeroTupIdx) ab) (Prj ZeroTupIdx ab)

    iterateL :: Elt e
             => PreOpenExp acc env      aenv   n
             -> PreOpenExp acc (env, e) aenv   e
             -> PreOpenExp acc env      aenv   e
             -> PreOpenAcc acc ((),sh)  aenv' (Array sh e)
    iterateL n f x = INTERNAL_ERROR(error) "liftExp" "Unexpected Iterate"

    indexL :: forall sh'. (Elt e, Shape sh')
           => acc            env      aenv  (Array sh' e)
           -> PreOpenExp acc env      aenv  sh'
           -> PreOpenAcc acc ((),sh)  aenv' (Array sh e)
    indexL a sh = Alet (cvtE sh) (inject perm)
      where
        a'   = weakenA rebuildAcc SuccIdx (extract $ cvtA a)
        perm = Backpermute (Var ZeroIdx) f (inject a')
        f    = Lam (Body (Index (inject $ Avar ZeroIdx) (Var ZeroIdx)))

    -- linearIndex a i
    --   => let x = i^ in
    --        let a' = a^ in backpermute outShape (\sh -> fromIndex (shape a') (x ! sh)) a'
    --
    -- RCE: This transform could be done with a generate (and explicitly use LinearIndex), as
    -- opposed to a backpermute. The performance difference of the two should be investigated.
    linearIndexL :: forall sh'. (Elt e, Shape sh')
                 => acc            env     aenv  (Array sh' e)
                 -> PreOpenExp acc env     aenv  Int
                 -> PreOpenAcc acc ((),sh) aenv' (Array sh e)
    linearIndexL a i = Alet (cvtE i) (inject $ Alet (inject a') (inject perm))
      where
        a'   = weakenA rebuildAcc SuccIdx (extract $ cvtA a)

        shr = Var ZeroIdx

        -- shape a
        sha = Shape (inject $ Avar ZeroIdx)

        -- backpermute (shape r) (\sh -> fromIndex (shape a') (x ! sh)) a'
        perm = Backpermute shr f (inject $ Avar ZeroIdx)

        -- (\sh -> fromIndex (shape a') (x ! sh))
        f    = Lam (Body (FromIndex sha (Index (inject $ Avar (SuccIdx ZeroIdx)) (Var ZeroIdx))))

    shapeL :: forall e'. (Shape e, Elt e')
           => acc            env     aenv  (Array e e')
           -> PreOpenAcc acc ((),sh) aenv' (Array sh e)
    shapeL a = Alet (cvtA a) (inject $ Generate (Var ZeroIdx) (Lam (Body (Shape (inject (Avar ZeroIdx))))))

    tup :: forall env aenv a b. (Elt a,Elt b)
        => PreOpenExp acc env aenv a
        -> PreOpenExp acc env aenv b
        -> PreOpenExp acc env aenv (a,b)
    tup a b = Tuple (SnocTup (SnocTup NilTup a) b)

    fun1 :: forall env aenv a b. (Elt a, Elt b)
         => (PreOpenExp acc (env,a) aenv a -> PreOpenExp acc (env,a) aenv b)
         -> PreOpenFun acc env aenv (a -> b)
    fun1 f = Lam (Body (f (Var ZeroIdx)))

    fun2 :: forall env aenv a b c. (Elt a, Elt b, Elt c)
         => (PreOpenExp acc ((env,a), b) aenv a -> PreOpenExp acc ((env,a), b) aenv b -> PreOpenExp acc ((env,a), b) aenv c)
         -> PreOpenFun acc env aenv (a -> b -> c)
    fun2 f = Lam (Lam (Body (f (Var (SuccIdx ZeroIdx)) (Var ZeroIdx))))

-- Utility functions
-- ------------------

topEA :: forall acc env aenv t. (Kit acc, Elt t)
      => PreOpenExp acc env (aenv, Array Z t) t
topEA = Index (inject (Avar ZeroIdx)) (Const ())

weaken :: forall acc env aenv t s. Kit acc
       => acc env aenv      t
       -> acc env (aenv, s) t
weaken = rebuildAcc Var (Avar. SuccIdx)
