{-# LANGUAGE GADTs #-}
-- |
-- Module      : Data.Array.Accelerate.CUDA.Smart
-- Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--

module Data.Array.Accelerate.CUDA.Smart (convertAcc, Sugar.Acc) where

import Prelude hiding (exp)

import Data.Array.Accelerate.AST
import qualified Data.Array.Accelerate.Smart       as Sugar
import qualified Data.Array.Accelerate.Array.Sugar as Sugar


-- Convert a closed array expression from HOAS to de Bruijn form AST
--
convertAcc :: Sugar.Acc a -> Acc a
convertAcc =  refineAcc . Sugar.convertAcc


-- Inject extra computational steps specific to the CUDA backend implementation.
-- TODO: Implement array fusion
--
refineAcc :: Acc a -> Acc a
refineAcc = refineOpenAcc Empty

refineOpenAcc :: Val aenv -> OpenAcc aenv a -> OpenAcc aenv a
refineOpenAcc aenv (Let  a1 a2)
  = Let  (refineOpenAcc aenv a1)
         (refineOpenAcc (aenv `Push` undefined) a2)

refineOpenAcc aenv (Let2 a1 a2)
  = Let2 (refineOpenAcc aenv a1)
         (refineOpenAcc (aenv `Push` undefined `Push` undefined) a2)

refineOpenAcc aenv (Unit e)              = Unit (refineExp aenv e)
refineOpenAcc aenv (Reshape e a)         = Reshape (refineExp aenv e) (refineOpenAcc aenv a)
refineOpenAcc aenv (Replicate slix e a)  = Replicate slix (refineExp aenv e) (refineOpenAcc aenv a)
refineOpenAcc aenv (Index slix a e)      = Index slix (refineOpenAcc aenv a) (refineExp aenv e)
refineOpenAcc aenv (Map f a)             = Map (refineFun aenv f) (refineOpenAcc aenv a)
refineOpenAcc aenv (ZipWith f a1 a2)     = ZipWith (refineFun aenv f) (refineOpenAcc aenv a1) (refineOpenAcc aenv a2)
refineOpenAcc aenv (Fold f e a)          = Fold (refineFun aenv f) (refineExp aenv e) (refineOpenAcc aenv a)
refineOpenAcc aenv (Scanl f e a)         = Scanl (refineFun aenv f) (refineExp aenv e) (refineOpenAcc aenv a)
refineOpenAcc aenv (Scanr f e a)         = Scanr (refineFun aenv f) (refineExp aenv e) (refineOpenAcc aenv a)
refineOpenAcc aenv (Permute f1 a1 f2 a2) = Permute (refineFun aenv f1) (refineOpenAcc aenv a1) (refineFun aenv f2) (refineOpenAcc aenv a2)
refineOpenAcc aenv (Backpermute e f a)   = Backpermute (refineExp aenv e) (refineFun aenv f) (refineOpenAcc aenv a)
refineOpenAcc aenv (FoldSeg f e a s)     = FoldSeg (refineFun aenv f) (refineExp aenv e) (refineOpenAcc aenv a) offsets
  where
    offsets = Let2 scanop (Avar (SuccIdx ZeroIdx))
    scanop  = Scanl (Sugar.convertFun2 undefined Sugar.mkAdd)
                    (Const (Sugar.fromElem (0::Int)))
                    (refineOpenAcc aenv s)

refineOpenAcc _ acc = acc

-- Functions ??
--
refineFun :: Val aenv -> Fun aenv t -> Fun aenv t
refineFun _ = id


-- Expressions
--
refineExp :: Val aenv -> Exp aenv a -> Exp aenv a
refineExp = refineOpenExp Empty

refineOpenExp :: Val env -> Val aenv -> OpenExp env aenv a -> OpenExp env aenv a
refineOpenExp env aenv (IndexScalar a e) = IndexScalar (refineOpenAcc aenv a) (refineOpenExp env aenv e)
refineOpenExp _   aenv (Shape a)         = Shape (refineOpenAcc aenv a)
refineOpenExp _ _ exp = exp

