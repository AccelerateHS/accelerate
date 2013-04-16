{-# LANGUAGE GADTs #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo.Normalise
-- Copyright   : [2012] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Trafo.Normalise (

  anormalise

) where

import Prelude                                          hiding ( exp )
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Trafo.Substitution


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
anormalise :: PreOpenExp acc env aenv t -> PreOpenExp acc env aenv t
anormalise = cvt
  where
    split1 :: Idx (env, a) t -> Idx ((env, s), a) t
    split1 ZeroIdx      = ZeroIdx
    split1 (SuccIdx ix) = SuccIdx (SuccIdx ix)

    cvtA :: acc aenv a -> acc aenv a
    cvtA = id

    cvtT :: Tuple (PreOpenExp acc env aenv) t -> Tuple (PreOpenExp acc env aenv) t
    cvtT NilTup         = NilTup
    cvtT (SnocTup t e)  = cvtT t `SnocTup` cvt e

    cvtF :: PreOpenFun acc env aenv f -> PreOpenFun acc env aenv f
    cvtF (Body e)       = Body (cvt e)
    cvtF (Lam f)        = Lam (cvtF f)

    cvt :: PreOpenExp acc env aenv e -> PreOpenExp acc env aenv e
    cvt exp =
      case exp of
        Let bnd body    ->
          let bnd'      = cvt bnd
              body'     = cvt body
          in
          case bnd' of
            Let bnd'' body''    -> Let bnd'' $ Let body'' (weakenE split1 body')
            _                   -> Let bnd' body'
        --
        Var ix                  -> Var ix
        Const c                 -> Const c
        Tuple tup               -> Tuple (cvtT tup)
        Prj tup ix              -> Prj tup (cvt ix)
        IndexNil                -> IndexNil
        IndexCons sh sz         -> IndexCons (cvt sh) (cvt sz)
        IndexHead sh            -> IndexHead (cvt sh)
        IndexTail sh            -> IndexTail (cvt sh)
        IndexAny                -> IndexAny
        IndexSlice x ix sh      -> IndexSlice x (cvt ix) (cvt sh)
        IndexFull x ix sl       -> IndexFull x (cvt ix) (cvt sl)
        ToIndex sh ix           -> ToIndex (cvt sh) (cvt ix)
        FromIndex sh ix         -> FromIndex (cvt sh) (cvt ix)
        Cond p t e              -> Cond (cvt p) (cvt t) (cvt e)
        Iterate n f x           -> Iterate n (cvt f) (cvt x)
        PrimConst c             -> PrimConst c
        PrimApp f x             -> PrimApp f (cvt x)
        Index a sh              -> Index (cvtA a) (cvt sh)
        LinearIndex a i         -> LinearIndex (cvtA a) (cvt i)
        Shape a                 -> Shape (cvtA a)
        ShapeSize sh            -> ShapeSize (cvt sh)
        Intersect s t           -> Intersect (cvt s) (cvt t)
        Foreign ff f e          -> Foreign ff (cvtF f) (cvt e)

