{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo.Normalise
-- Copyright   : [2012..2014] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Trafo.Normalise (

  untupleAcc, untupleAfun, TupleMap(..)

) where

import Prelude                                          hiding ( exp )
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Trafo.Substitution

import Data.Proxy

-- Convert to Administrative Normal (a-normal) Form, where lets-within-lets of
-- an expression are flattened.
--
--   let x =
--     let y = e1 in e2
--   in e3
--
-- ==>
--
--   let y = e1  in
--   let x = e2
--   in e3
--
-- anormalise :: PreOpenExp acc env aenv t -> PreOpenExp acc env aenv t
-- anormalise = cvt
--   where
--     split1 :: Idx (env, a) t -> Idx ((env, s), a) t
--     split1 ZeroIdx      = ZeroIdx
--     split1 (SuccIdx ix) = SuccIdx (SuccIdx ix)

--     cvtA :: acc aenv a -> acc aenv a
--     cvtA = id

--     cvtT :: Tuple (PreOpenExp acc env aenv) t -> Tuple (PreOpenExp acc env aenv) t
--     cvtT NilTup         = NilTup
--     cvtT (SnocTup t e)  = cvtT t `SnocTup` cvt e

--     cvtF :: PreOpenFun acc env aenv f -> PreOpenFun acc env aenv f
--     cvtF (Body e)       = Body (cvt e)
--     cvtF (Lam f)        = Lam (cvtF f)

--     cvt :: PreOpenExp acc env aenv e -> PreOpenExp acc env aenv e
--     cvt exp =
--       case exp of
--         Let bnd body    ->
--           let bnd'      = cvt bnd
--               body'     = cvt body
--           in
--           case bnd' of
--             Let bnd'' body''    -> Let bnd'' $ Let body'' (weakenE split1 body')
--             _                   -> Let bnd' body'
--         --
--         Var ix                  -> Var ix
--         Const c                 -> Const c
--         Tuple tup               -> Tuple (cvtT tup)
--         Prj tup ix              -> Prj tup (cvt ix)
--         IndexNil                -> IndexNil
--         IndexCons sh sz         -> IndexCons (cvt sh) (cvt sz)
--         IndexHead sh            -> IndexHead (cvt sh)
--         IndexTail sh            -> IndexTail (cvt sh)
--         IndexAny                -> IndexAny
--         IndexSlice x ix sh      -> IndexSlice x (cvt ix) (cvt sh)
--         IndexFull x ix sl       -> IndexFull x (cvt ix) (cvt sl)
--         ToIndex sh ix           -> ToIndex (cvt sh) (cvt ix)
--         FromIndex sh ix         -> FromIndex (cvt sh) (cvt ix)
--         Cond p t e              -> Cond (cvt p) (cvt t) (cvt e)
--         Iterate n f x           -> Iterate n (cvt f) (cvt x)
--         PrimConst c             -> PrimConst c
--         PrimApp f x             -> PrimApp f (cvt x)
--         Index a sh              -> Index (cvtA a) (cvt sh)
--         LinearIndex a i         -> LinearIndex (cvtA a) (cvt i)
--         Shape a                 -> Shape (cvtA a)
--         ShapeSize sh            -> ShapeSize (cvt sh)
--         Intersect s t           -> Intersect (cvt s) (cvt t)
--         Foreign ff f e          -> Foreign ff (cvtF f) (cvt e)


data TupleMap aenv aenv' where
  BaseTupleMap :: TupleMap aenv aenv
  OneTupleMap  :: (IsAtuple t, Arrays t') => TupleMap aenv aenv' -> TupleReduction (TupleRepr t) t' -> TupleMap (aenv, t) (aenv', t')
  SnocTupleMap :: TupleMap aenv aenv' -> TupleMap (aenv,t) (aenv',t)

data Untupled f t where
  Same     :: f t -> Untupled f t
  OneTuple :: (Arrays t, IsAtuple t, Arrays t') => TupleReduction (TupleRepr t) t' -> f t' -> Untupled f t

data TupleReduction t t' where
  TROne  :: TupleReduction ((),t) t
  TRNest :: (Arrays t, IsAtuple t)
         => TupleReduction (TupleRepr t) t' -> TupleReduction ((),t) t'

buildTuple :: Arrays t' => TupleReduction t t' -> OpenAcc aenv t' -> Atuple (OpenAcc aenv) t
buildTuple TROne t'  = NilAtup `SnocAtup` t'
buildTuple (TRNest tr) t' = NilAtup `SnocAtup` OpenAcc (Atuple (buildTuple tr t'))

wrap :: Arrays t => (forall t. Arrays t => f t -> f' t) -> Untupled f t -> Untupled f' t
wrap f (Same t)     = Same (f t)
wrap f (OneTuple tr t) = OneTuple tr (f t)

-- Remove any 1-tuples
--
untupleAcc :: forall aenv aenv' t. Arrays t
           => TupleMap aenv aenv'
           -> OpenAcc aenv  t
           -> Untupled (OpenAcc aenv') t
untupleAcc tmap (OpenAcc pacc) = wrap OpenAcc $ case pacc of
  Alet bnd body             -> alet bnd body
  Avar ix                   -> avar tmap ix id
  Atuple tup                -> atuple tup
  Aprj tup a                -> aprj tup a
  Apply f a                 -> apply f a
  Aforeign ff afun acc      -> Same $ Aforeign ff (untupleAfun BaseTupleMap afun) (same acc)
  Acond p t e               -> Same $ Acond (cvtE p) (same t) (same e)
  Awhile p f a              -> Same $ Awhile (cvtAF p) (cvtAF f) (same a)
  Use a                     -> use a
  Unit e                    -> Same $ Unit (cvtE e)
  Reshape e a               -> Same $ Reshape (cvtE e) (cvtA a)
  Generate e f              -> Same $ Generate (cvtE e) (cvtF f)
  Transform sh ix f a       -> Same $ Transform (cvtE sh) (cvtF ix) (cvtF f) (cvtA a)
  Replicate sl slix a       -> Same $ Replicate sl (cvtE slix) (cvtA a)
  Slice sl a slix           -> Same $ Slice sl (cvtA a) (cvtE slix)
  Map f a                   -> Same $ Map (cvtF f) (cvtA a)
  ZipWith f a1 a2           -> Same $ ZipWith (cvtF f) (cvtA a1) (cvtA a2)
  Fold f z a                -> Same $ Fold (cvtF f) (cvtE z) (cvtA a)
  Fold1 f a                 -> Same $ Fold1 (cvtF f) (cvtA a)
  Scanl f z a               -> Same $ Scanl (cvtF f) (cvtE z) (cvtA a)
  Scanl' f z a              -> Same $ Scanl' (cvtF f) (cvtE z) (cvtA a)
  Scanl1 f a                -> Same $ Scanl1 (cvtF f) (cvtA a)
  Scanr f z a               -> Same $ Scanr (cvtF f) (cvtE z) (cvtA a)
  Scanr' f z a              -> Same $ Scanr' (cvtF f) (cvtE z) (cvtA a)
  Scanr1 f a                -> Same $ Scanr1 (cvtF f) (cvtA a)
  Permute f1 a1 f2 a2       -> Same $ Permute (cvtF f1) (cvtA a1) (cvtF f2) (cvtA a2)
  Backpermute sh f a        -> Same $ Backpermute (cvtE sh) (cvtF f) (cvtA a)
  Stencil f b a             -> Same $ Stencil (cvtF f) b (cvtA a)
  Stencil2 f b1 a1 b2 a2    -> Same $ Stencil2 (cvtF f) b1 (cvtA a1) b2 (cvtA a2)
  Collect s                 -> Same $ Collect (cvtS s)
  FoldSeg f z a s           -> Same $ FoldSeg (cvtF f) (cvtE z) (cvtA a) (cvtA s)
  Fold1Seg f a s            -> Same $ Fold1Seg (cvtF f) (cvtA a) (cvtA s)

  where
    cvtA :: (Shape sh, Elt e)
         => OpenAcc aenv  (Array sh e)
         -> OpenAcc aenv' (Array sh e)
    cvtA (untupleAcc tmap -> Same a) = a
    cvtA _                           = error "Unreachable"

    same :: Arrays a
         => OpenAcc aenv  a
         -> OpenAcc aenv' a
    same a = case untupleAcc tmap a of
               Same a'        -> a'
               OneTuple tr a' -> OpenAcc (Atuple (buildTuple tr a'))

    atuple :: IsAtuple t => Atuple (OpenAcc aenv) (TupleRepr t) -> Untupled (PreOpenAcc OpenAcc aenv') t
    atuple (NilAtup `SnocAtup` t) = case untupleAcc tmap t of
                                      Same (OpenAcc t') -> OneTuple TROne t'
                                      OneTuple tr (OpenAcc t') -> OneTuple (TRNest tr) t'
    atuple t = Same $ Atuple (cvtAT t) -- The tuple is already larger than a one-tuple. Further reduction is pointless.
      where
        cvtAT :: forall t. Atuple (OpenAcc aenv) t -> Atuple (OpenAcc aenv') t
        cvtAT atup = case atup of
          NilAtup      -> NilAtup
          SnocAtup t a -> cvtAT t `SnocAtup` same a

    cvtAF :: forall t. OpenAfun aenv t -> OpenAfun aenv' t
    cvtAF = untupleAfun tmap

    cvtE :: forall t. Exp aenv t -> Exp aenv' t
    cvtE = untupleExp tmap

    cvtF :: forall t. Fun aenv t -> Fun aenv' t
    cvtF = untupleFun tmap

    cvtS :: forall senv t. PreOpenSeq OpenAcc aenv senv t -> PreOpenSeq OpenAcc aenv' senv t
    cvtS seq =
      case seq of
        Producer p s -> Producer (cvtP p) (cvtS s)
        Consumer c   -> Consumer (cvtC c)
        Reify f ix   -> Reify (cvtAF `fmap` f) ix

    cvtP :: forall senv t. Producer OpenAcc aenv senv t -> Producer OpenAcc aenv' senv t
    cvtP p =
      case p of
        StreamIn arrs        -> StreamIn arrs
        ToSeq f sl slix a    -> ToSeq (cvtAF `fmap` f) sl slix (cvtA a)
        MapSeq f f' x        -> MapSeq (cvtAF f) (cvtAF `fmap` f') x
        ZipWithSeq f f' x y  -> ZipWithSeq (cvtAF f) (cvtAF `fmap` f') x y
        ScanSeq f e x        -> ScanSeq (cvtF f) (cvtE e) x

    cvtC :: forall senv t. Consumer OpenAcc aenv senv t -> Consumer OpenAcc aenv' senv t
    cvtC c =
      case c of
        FoldSeq f' f e x        -> FoldSeq (cvtAF `fmap` f') (cvtF f) (cvtE e) x
        FoldSeqFlatten f' f a x -> FoldSeqFlatten (cvtAF `fmap` f') (cvtAF f) (same a) x
        Stuple t                -> Stuple (cvtCT t)

    cvtCT :: forall senv t. Atuple (Consumer OpenAcc aenv senv) t -> Atuple (Consumer OpenAcc aenv' senv) t
    cvtCT NilAtup        = NilAtup
    cvtCT (SnocAtup t c) = SnocAtup (cvtCT t) (cvtC c)

    alet :: (Arrays bnd, Arrays body) => OpenAcc aenv bnd -> OpenAcc (aenv, bnd) body -> Untupled (PreOpenAcc OpenAcc aenv') body
    alet (OpenAcc (Alet (OpenAcc (Aprj tix (OpenAcc (Avar ix)))) body')) body
      = alet (OpenAcc (Aprj tix (OpenAcc (Avar ix)))) (OpenAcc (Alet body' (weaken ixt body)))
      where
        ixt :: forall aenv a b. (aenv, a) :> ((aenv, b), a)
        ixt ZeroIdx      = ZeroIdx
        ixt (SuccIdx ix) = SuccIdx (SuccIdx ix)
    alet bnd body =
      case untupleAcc tmap bnd of
        OneTuple tr bnd' -> Alet bnd' `wrap` untupleAcc (OneTupleMap tmap tr) body
        Same bnd'        -> Alet bnd' `wrap` untupleAcc (SnocTupleMap tmap) body

    avar :: forall aenv0 aenv0' t. Arrays t
         => TupleMap aenv0 aenv0'
         -> Idx aenv0 t
         -> (forall t. Idx aenv0' t -> Idx aenv' t)
         -> Untupled (PreOpenAcc OpenAcc aenv') t
    avar BaseTupleMap       ix           ixt = Same $ Avar (ixt ix)
    avar (SnocTupleMap _)   ZeroIdx      ixt = Same $ Avar (ixt ZeroIdx)
    avar (SnocTupleMap tm)  (SuccIdx ix) ixt = avar tm ix (ixt . SuccIdx)
    avar (OneTupleMap _ tr) ZeroIdx      ixt = OneTuple tr $ Avar (ixt ZeroIdx)
    avar (OneTupleMap tm _) (SuccIdx ix) ixt = avar tm ix (ixt . SuccIdx)

    aprj :: forall t a. (Arrays t, IsAtuple t, Arrays a)
         => TupleIdx (TupleRepr t) a
         -> OpenAcc aenv t
         -> Untupled (PreOpenAcc OpenAcc aenv') a
    aprj idx t =
      case untupleAcc tmap t of
        OneTuple (TRNest tr) (OpenAcc t') | ZeroTupIdx <- idx
                                          -> OneTuple tr t'
        OneTuple TROne       (OpenAcc t') | ZeroTupIdx <- idx
                                          -> Same t'
        Same t'                           -> Same (Aprj idx t')
        _                                 -> error "Arrrrrr"

    apply :: Arrays a => OpenAfun aenv (a -> t) -> OpenAcc aenv a -> Untupled (PreOpenAcc OpenAcc aenv') t
    apply (Alam (Abody t)) a =
      case untupleAcc tmap a of
        OneTuple tr a' -> Alet a' `wrap` untupleAcc (OneTupleMap tmap tr) t
        Same a'        -> Alet a' `wrap` untupleAcc (SnocTupleMap tmap) t
    apply _ _ = error "Ohhhhh it's getting late."

    use :: ArrRepr t -> Untupled (PreOpenAcc OpenAcc aenv') t
    use t | ArraysFtuple        <- flavour (undefined :: t)
          , ProdRsnoc ProdRunit <- prod (Proxy :: Proxy Arrays) (undefined :: t)
          , ((), t')             <- fromProd (Proxy :: Proxy Arrays) (toArr t :: t)
          = OneTuple (TROne) (Use (fromArr t'))
          | otherwise
          = Same $ Use t

untupleAfun :: TupleMap aenv aenv' -> OpenAfun aenv f -> OpenAfun aenv' f
untupleAfun tm (Abody a) =
  case untupleAcc tm a of
    OneTuple tr a' -> Abody (OpenAcc (Atuple (buildTuple tr a')))
    Same a'        -> Abody a'
untupleAfun tm (Alam a)  = Alam $ untupleAfun (SnocTupleMap tm) a

untupleExp :: forall env aenv aenv' t. TupleMap aenv aenv'
           -> OpenExp env aenv  t
           -> OpenExp env aenv' t
untupleExp tmap = cvtE
  where
    cvtE :: forall env t. OpenExp env aenv t -> OpenExp env aenv' t
    cvtE exp =
      case exp of
        Let bnd body            -> Let (cvtE bnd) (cvtE body)
        Var ix                  -> Var ix
        Const c                 -> Const c
        Tuple tup               -> Tuple (cvtT tup)
        Prj tup t               -> Prj tup (cvtE t)
        IndexNil                -> IndexNil
        IndexCons sh sz         -> IndexCons (cvtE sh) (cvtE sz)
        IndexHead sh            -> IndexHead (cvtE sh)
        IndexTail sh            -> IndexTail (cvtE sh)
        IndexTrans sh           -> IndexTrans (cvtE sh)
        IndexAny                -> IndexAny
        IndexSlice x ix sh      -> IndexSlice x (cvtE ix) (cvtE sh)
        IndexFull x ix sl       -> IndexFull x (cvtE ix) (cvtE sl)
        ToIndex sh ix           -> ToIndex (cvtE sh) (cvtE ix)
        FromIndex sh ix         -> FromIndex (cvtE sh) (cvtE ix)
        Cond p t e              -> Cond (cvtE p) (cvtE t) (cvtE e)
        While p f x             -> While (cvtF p) (cvtF f) (cvtE x)
        PrimConst c             -> PrimConst c
        PrimApp f x             -> PrimApp f (cvtE x)
        Index a sh              -> Index (cvtA a) (cvtE sh)
        LinearIndex a i         -> LinearIndex (cvtA a) (cvtE i)
        Shape a                 -> Shape (cvtA a)
        ShapeSize sh            -> ShapeSize (cvtE sh)
        Intersect s t           -> Intersect (cvtE s) (cvtE t)
        Union s t               -> Union (cvtE s) (cvtE t)
        Foreign ff f e          -> Foreign ff (untupleFun BaseTupleMap f) (cvtE e)

    cvtF :: forall env t. OpenFun env aenv t -> OpenFun env aenv' t
    cvtF = untupleFun tmap

    cvtT :: forall env t. Tuple (OpenExp env aenv) t -> Tuple (OpenExp env aenv') t
    cvtT tup = case tup of
      NilTup      -> NilTup
      SnocTup t a -> cvtT t `SnocTup` cvtE a

    cvtA :: (Shape sh, Elt e) => OpenAcc aenv (Array sh e) -> OpenAcc aenv' (Array sh e)
    cvtA (untupleAcc tmap -> Same a) = a
    cvtA _                           = error "Urrrr"

untupleFun :: TupleMap aenv aenv' -> OpenFun env aenv t -> OpenFun env aenv' t
untupleFun tmap f = case f of
  Body e -> Body (untupleExp tmap e)
  Lam f  -> Lam  (untupleFun tmap f)
