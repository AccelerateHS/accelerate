{-# LANGUAGE GADTs #-}
-- |
-- Module      : Data.Array.Accelerate.CUDA
-- Copyright   : [2008..2009] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module is the CUDA backend for the embedded array language.
--

module Data.Array.Accelerate.CUDA (

    -- * Generate and execute CUDA code for an array expression
    Arrays, run

  ) where

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Array.Representation
import Data.Array.Accelerate.Array.Sugar                (Array(..))
import qualified Data.Array.Accelerate.Smart            as Sugar

import Data.Array.Accelerate.CUDA.State
import Data.Array.Accelerate.CUDA.Compile
import Data.Array.Accelerate.CUDA.Execute
import Data.Array.Accelerate.CUDA.Array.Data
import Data.Array.Accelerate.CUDA.Array.Device


-- Accelerate: CUDA
-- ~~~~~~~~~~~~~~~~

-- | Compiles and runs a complete embedded array program using the CUDA backend
--
run :: Arrays a => Sugar.Acc a -> IO a
run acc = evalCUDA
        $ execute (Sugar.convertAcc acc) >>= collect


-- Evaluation
-- ~~~~~~~~~~
--

execute :: Arrays a => Acc a -> CIO a
execute acc = prepare acc >> executeAcc acc

-- Traverse the array expression in depth-first order, initiating asynchronous
-- code generation and data transfer.
--
prepare :: OpenAcc aenv a -> CIO ()
prepare (Use (Array sh ad)) =
  let n = size sh
  in do
    mallocArray    ad n
    pokeArrayAsync ad n Nothing

prepare (Let  xs ys)   = prepare xs >> prepare ys
prepare (Let2 xs ys)   = prepare xs >> prepare ys
prepare (Avar _)       = return ()              -- TLM: ??
prepare (Unit _)       = return ()              -- TLM: ??
prepare (Reshape _ xs) = prepare xs             -- TLM: ??
prepare (Index _ xs _) = prepare xs             -- TLM: ??

prepare acc@(Replicate _ _ xs)   = prepare xs >> compile acc
prepare acc@(Map _ xs)           = prepare xs >> compile acc
prepare acc@(ZipWith _ xs ys)    = prepare xs >> prepare ys >> compile acc
prepare acc@(Fold _ _ xs)        = prepare xs >> compile acc
prepare acc@(FoldSeg _ _ xs ys)  = prepare xs >> prepare ys >> compile acc
prepare acc@(Scanl _ _ xs)       = prepare xs >> compile acc
prepare acc@(Scanr _ _ xs)       = prepare xs >> compile acc
prepare acc@(Permute _ xs _ ys)  = prepare xs >> prepare ys >> compile acc
prepare acc@(Backpermute _ _ xs) = prepare xs >> compile acc

