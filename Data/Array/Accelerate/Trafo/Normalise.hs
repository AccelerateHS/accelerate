{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
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

  untupleAcc, untupleAfun, ReducedMap(..), untupleSeq

) where

import Prelude                                          hiding ( exp )
import Data.Functor                                     ( (<$>) )
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Trafo.Base
import Data.Array.Accelerate.Trafo.Substitution
import Data.Array.Accelerate.Error

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


data ReducedMap aenv aenv' where
  BaseReducedMap :: ReducedMap aenv aenv
  SnocReducedMap :: Arrays t' => ReducedMap aenv aenv' -> Reduction t t' -> ReducedMap (aenv,t) (aenv',t')

data Reduced f t where
  Reduced :: Arrays t' => Reduction t t' -> f t' -> Reduced f t

data Reduction t t' where
  RId   :: Reduction t t
  RNest :: (Arrays t', IsAtuple t, TupleRepr t ~ ((),t'))
        => Reduction t' t''
        -> Reduction t t''

wrap :: (forall t. f t -> f' t) -> Reduced f t -> Reduced f' t
wrap f (Reduced r t) = Reduced r (f t)

-- Remove any 1-tuples
--
untupleAcc :: forall aenv aenv' t. Arrays t
           => ReducedMap aenv aenv'
           -> OpenAcc aenv  t
           -> Reduced (OpenAcc aenv') t
untupleAcc tmap (OpenAcc pacc) = wrap OpenAcc $ case pacc of
  Alet bnd body             -> alet bnd body
  Avar ix                   -> avar tmap ix id
  Atuple tup                -> atuple tup
  Aprj tup a                -> aprj tup a
  Apply f a                 -> apply f a
  Aforeign ff afun acc      -> noRed $ Aforeign ff (untupleAfun BaseReducedMap afun) (same acc)
  Acond p t e               -> noRed $ Acond (cvtE p) (same t) (same e)
  Awhile p f a              -> noRed $ Awhile (cvtAF p) (cvtAF f) (same a)
  Use a                     -> use a
  Subarray ix sh a          -> noRed $ Subarray (cvtE ix) (cvtE sh) a
  Unit e                    -> noRed $ Unit (cvtE e)
  Reshape e a               -> noRed $ Reshape (cvtE e) (cvtA a)
  Generate e f              -> noRed $ Generate (cvtE e) (cvtF f)
  Transform sh ix f a       -> noRed $ Transform (cvtE sh) (cvtF ix) (cvtF f) (cvtA a)
  Replicate sl slix a       -> noRed $ Replicate sl (cvtE slix) (cvtA a)
  Slice sl a slix           -> noRed $ Slice sl (cvtA a) (cvtE slix)
  Map f a                   -> noRed $ Map (cvtF f) (cvtA a)
  ZipWith f a1 a2           -> noRed $ ZipWith (cvtF f) (cvtA a1) (cvtA a2)
  Fold f z a                -> noRed $ Fold (cvtF f) (cvtE z) (cvtA a)
  Fold1 f a                 -> noRed $ Fold1 (cvtF f) (cvtA a)
  Scanl f z a               -> noRed $ Scanl (cvtF f) (cvtE z) (cvtA a)
  Scanl' f z a              -> noRed $ Scanl' (cvtF f) (cvtE z) (cvtA a)
  Scanl1 f a                -> noRed $ Scanl1 (cvtF f) (cvtA a)
  Scanr f z a               -> noRed $ Scanr (cvtF f) (cvtE z) (cvtA a)
  Scanr' f z a              -> noRed $ Scanr' (cvtF f) (cvtE z) (cvtA a)
  Scanr1 f a                -> noRed $ Scanr1 (cvtF f) (cvtA a)
  Permute f1 a1 f2 a2       -> noRed $ Permute (cvtF f1) (cvtA a1) (cvtF f2) (cvtA a2)
  Backpermute sh f a        -> noRed $ Backpermute (cvtE sh) (cvtF f) (cvtA a)
  Stencil f b a             -> noRed $ Stencil (cvtF f) b (cvtA a)
  Stencil2 f b1 a1 b2 a2    -> noRed $ Stencil2 (cvtF f) b1 (cvtA a1) b2 (cvtA a2)
  Collect s cs              -> noRed $ Collect (untupleSeq tmap s) (untupleSeq tmap <$> cs)
  FoldSeg f z a s           -> noRed $ FoldSeg (cvtF f) (cvtE z) (cvtA a) (cvtA s)
  Fold1Seg f a s            -> noRed $ Fold1Seg (cvtF f) (cvtA a) (cvtA s)

  where
    cvtA :: (Shape sh, Elt e)
         => OpenAcc aenv  (Array sh e)
         -> OpenAcc aenv' (Array sh e)
    cvtA (untupleAcc tmap -> Reduced RId a) = a
    cvtA _                                  = error "Unreachable"

    noRed :: f t
          -> Reduced f t
    noRed = Reduced RId

    same :: Arrays a => OpenAcc aenv a -> OpenAcc aenv' a
    same = unreduce . untupleAcc tmap

    atuple :: IsAtuple t => Atuple (OpenAcc aenv) (TupleRepr t) -> Reduced (PreOpenAcc OpenAcc aenv') t
    atuple (NilAtup `SnocAtup` t) | Reduced r t' <- untupleAcc tmap t
                                  = Reduced (RNest r) (extract t')
    atuple t                      = Reduced RId (Atuple (cvtAT t))
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

    alet :: (Arrays bnd, Arrays body) => OpenAcc aenv bnd -> OpenAcc (aenv, bnd) body -> Reduced (PreOpenAcc OpenAcc aenv') body
    alet (OpenAcc (Avar ix)) body = extract `wrap` untupleAcc tmap  (inlineA body (Avar ix))
    alet (OpenAcc (Alet bnd body')) body
      = alet bnd (OpenAcc (Alet body' (weaken oneBelow body)))
    alet bnd body
      | Reduced r bnd'   <- untupleAcc tmap bnd
      , Reduced r' body' <- untupleAcc (SnocReducedMap tmap r) body
      = Reduced r' $ Alet bnd' body'

    avar :: forall aenv0 aenv0' t. Arrays t
         => ReducedMap aenv0 aenv0'
         -> Idx aenv0 t
         -> (forall t. Idx aenv0' t -> Idx aenv' t)
         -> Reduced (PreOpenAcc OpenAcc aenv') t
    avar BaseReducedMap         ix           ixt = Reduced RId $ Avar (ixt ix)
    avar (SnocReducedMap _ r)   ZeroIdx      ixt = Reduced r $ Avar (ixt ZeroIdx)
    avar (SnocReducedMap tm _)  (SuccIdx ix) ixt = avar tm ix (ixt . SuccIdx)

    aprj :: forall t a. (Arrays t, IsAtuple t, Arrays a)
         => TupleIdx (TupleRepr t) a
         -> OpenAcc aenv t
         -> Reduced (PreOpenAcc OpenAcc aenv') a
    aprj idx t
      | Reduced r t' <- untupleAcc tmap t
      = case r of
          RId                          -> Reduced RId $ Aprj idx t'
          RNest r' | ZeroTupIdx <- idx -> Reduced r' (extract t')
          _                            -> error "Unreachable"

    apply :: Arrays a => OpenAfun aenv (a -> t) -> OpenAcc aenv a -> Reduced (PreOpenAcc OpenAcc aenv') t
    apply f a = Reduced RId (Apply (cvtAF f) (same a))

    use :: ArrRepr t -> Reduced (PreOpenAcc OpenAcc aenv') t
    use = noRed . Use

untupleSeq :: forall index aenv aenv' t.
              ReducedMap aenv aenv'
           -> PreOpenSeq index OpenAcc aenv  t
           -> PreOpenSeq index OpenAcc aenv' t
untupleSeq tmap seq =
  case seq of
    Producer p s -> Producer (cvtP p) (untupleSeq (SnocReducedMap tmap RId) s)
    Consumer c   -> Consumer (cvtC c)
    Reify a      -> Reify (cvtA a)
  where
    stageError :: a
    stageError = $internalError "untupleSeq" "Internal syntax is at the wrong stage"

    cvtP :: forall t. Producer index OpenAcc aenv t -> Producer index OpenAcc aenv' t
    cvtP p =
      case p of
        Pull src           -> Pull src
        ProduceAccum l f a -> ProduceAccum (cvtE <$> l) (cvtAF f) (cvtA a)
        _                  -> stageError

    cvtC :: forall t. Consumer index OpenAcc aenv t -> Consumer index OpenAcc aenv' t
    cvtC c =
      case c of
        Iterate l f a        -> Iterate (cvtE <$> l) (cvtAF f) (cvtA a)
        Stuple t             -> Stuple (cvtCT t)
        _                    -> stageError

    cvtCT :: forall t. Atuple (PreOpenSeq index OpenAcc aenv) t -> Atuple (PreOpenSeq index OpenAcc aenv') t
    cvtCT NilAtup        = NilAtup
    cvtCT (SnocAtup t c) = SnocAtup (cvtCT t) (untupleSeq tmap c)

    cvtA :: Arrays a
         => OpenAcc aenv  a
         -> OpenAcc aenv' a
    cvtA = unreduce . untupleAcc tmap

    cvtAF :: forall t. OpenAfun aenv t -> OpenAfun aenv' t
    cvtAF = untupleAfun tmap

    cvtE :: forall t. Exp aenv t -> Exp aenv' t
    cvtE = untupleExp tmap


untupleExp :: forall env aenv aenv' t. ReducedMap aenv aenv'
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
        ToSlice x slix sh i     -> ToSlice x (cvtE slix) (cvtE sh) (cvtE i)
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
        Foreign ff f e          -> Foreign ff (untupleFun BaseReducedMap f) (cvtE e)

    cvtF :: forall env t. OpenFun env aenv t -> OpenFun env aenv' t
    cvtF = untupleFun tmap

    cvtT :: forall env t. Tuple (OpenExp env aenv) t -> Tuple (OpenExp env aenv') t
    cvtT tup = case tup of
      NilTup      -> NilTup
      SnocTup t a -> cvtT t `SnocTup` cvtE a

    cvtA :: (Shape sh, Elt e) => OpenAcc aenv (Array sh e) -> OpenAcc aenv' (Array sh e)
    cvtA (untupleAcc tmap -> Reduced RId a) = a
    cvtA _                           = error "Urrrr"

untupleFun :: ReducedMap aenv aenv' -> OpenFun env aenv t -> OpenFun env aenv' t
untupleFun tmap f = case f of
  Body e -> Body (untupleExp tmap e)
  Lam f  -> Lam  (untupleFun tmap f)

untupleAfun :: ReducedMap aenv aenv' -> OpenAfun aenv t -> OpenAfun aenv' t
untupleAfun tmap f = case f of
  Abody a -> Abody (unreduce $ untupleAcc tmap a)
  Alam f  -> Alam  (untupleAfun (SnocReducedMap tmap RId) f)



-- Utility functions
-- -------------------

oneBelow :: forall aenv a b. (aenv,a) :> ((aenv,b),a)
oneBelow ZeroIdx      = ZeroIdx
oneBelow (SuccIdx ix) = SuccIdx (SuccIdx ix)

unreduce :: forall a aenv. Arrays a => Reduced (OpenAcc aenv) a -> OpenAcc aenv a
unreduce (Reduced r a) = unr r a
  where
    unr :: forall t t'. Arrays t => Reduction t t' -> OpenAcc aenv t' -> OpenAcc aenv t
    unr r a =
      case r of
        RId      -> a
        RNest r' -> OpenAcc (Atuple (NilAtup `SnocAtup` unr r' a))
