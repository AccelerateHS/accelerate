{-# LANGUAGE GADTs, FlexibleInstances, TypeSynonymInstances #-}
-- |
-- Module      : Data.Array.Accelerate.CUDA.AST
-- Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--

module Data.Array.Accelerate.CUDA.AST (

  module Data.Array.Accelerate.AST,
  AccKernel, AccBarrier, AccBinding(..), ExecAcc, ExecAfun, ExecOpenAcc(..)

) where

-- friends
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Pretty
import Data.Array.Accelerate.Array.Sugar                ( Array, Shape, Elt )
import Data.Array.Accelerate.CUDA.State                 ( CIO )

import Foreign.CUDA.Driver.Event                        ( Event )
import Foreign.CUDA.Driver.Stream                       ( Stream )
import qualified Foreign.CUDA.Driver                    as CUDA

-- system
import Text.PrettyPrint


-- A binary object that will be used to execute a kernel
--
type AccKernel a = (String, CIO CUDA.Module)

-- Kernel execution is asynchronous, barriers allow (cross-stream)
-- synchronisation
--
type AccBarrier = (Stream, Event)

-- Array computations that were embedded within scalar expressions, and will be
-- required to execute the kernel; i.e. bound to texture references or similar.
--
data AccBinding aenv where
  ArrayVar :: (Shape sh, Elt e)
           => Idx aenv (Array sh e)
           -> AccBinding aenv

instance Eq (AccBinding aenv) where
  ArrayVar ix1 == ArrayVar ix2 = idxToInt ix1 == idxToInt ix2


-- Interleave compilation & execution state annotations into an open array
-- computation AST
--
data ExecOpenAcc aenv a where
  ExecAcc :: AccKernel a                        -- an executable binary object
          -> AccBarrier                         -- determine when the operation has completed
          -> [AccBinding aenv]                  -- auxiliary arrays from the environment the kernel needs access to
          -> PreOpenAcc ExecOpenAcc aenv a      -- the actual computation
          -> ExecOpenAcc aenv a                 -- the recursive knot

-- An annotated AST suitable for execution in the CUDA environment
--
type ExecAcc  a = ExecOpenAcc () a
type ExecAfun a = PreAfun ExecOpenAcc a

instance Show (ExecOpenAcc aenv a) where
  show = render . prettyExecAcc 0 noParens

instance Show (ExecAfun a) where
  show = render . prettyExecAfun 0


-- Display the annotated AST
--
prettyExecAfun :: Int -> ExecAfun a -> Doc
prettyExecAfun alvl pfun = prettyPreAfun prettyExecAcc alvl pfun

prettyExecAcc :: PrettyAcc ExecOpenAcc
prettyExecAcc alvl wrap (ExecAcc _ _ fv pacc) =
  let base = prettyPreAcc prettyExecAcc alvl wrap pacc
      ann  = braces (freevars fv)
  in case pacc of
       Avar _         -> base
       Let  _ _       -> base
       Let2 _ _       -> base
       Apply _ _      -> base
       PairArrays _ _ -> base
       Acond _ _ _    -> base
       _              -> ann <+> base
  where
    freevars = (text "fv=" <>) . brackets . hcat . punctuate comma
                               . map (\(ArrayVar ix) -> char 'a' <> int (idxToInt ix))

