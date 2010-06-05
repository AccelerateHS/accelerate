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

import Control.Exception
import Control.Monad.State
import Data.Maybe                                       (fromMaybe)
import qualified Data.Map                               as M  (empty)
import qualified Data.IntMap                            as IM (empty)

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Array.Representation
import Data.Array.Accelerate.Array.Sugar                (Array(..))
import qualified Data.Array.Accelerate.Smart            as Sugar

import Data.Array.Accelerate.CUDA.State
import Data.Array.Accelerate.CUDA.Compile
import Data.Array.Accelerate.CUDA.Execute
import Data.Array.Accelerate.CUDA.Array.Data
import Data.Array.Accelerate.CUDA.Array.Device

import qualified Foreign.CUDA.Driver                    as CUDA


-- Accelerate: CUDA
-- ~~~~~~~~~~~~~~~~

-- |
-- Compiles and runs a complete embedded array program using the CUDA backend
--
run :: Arrays a => Sugar.Acc a -> IO a
run acc = evalCUDA
        $ executeAcc (Sugar.convertAcc acc) >>= collect


-- Initialisation
-- ~~~~~~~~~~~~~~
--

-- |
-- Evaluate a CUDA array computation under a newly initialised environment,
-- discarding the final state.
--
evalCUDA :: CIO a -> IO a
evalCUDA =  liftM fst . runCUDA

-- TLM: Optionally choose which device to use, or select the "best"?
--
runCUDA :: CIO a -> IO (a, CUDAState)
runCUDA acc =
  bracket (initialise Nothing) finalise $ \(dev,_ctx) -> do
    props <- CUDA.props dev
    runStateT acc (CUDAState 0 props IM.empty M.empty)
    --
    -- TLM 2010-06-05: assert all memory has been released ??

  where
    finalise     = CUDA.destroy . snd   -- TLM 2010-06-05: does this release all memory?
    initialise n = do
      CUDA.initialise []
      dev <- CUDA.device (fromMaybe 0 n)
      ctx <- CUDA.create dev [CUDA.SchedAuto]
      return (dev, ctx)


-- Evaluation
-- ~~~~~~~~~~
--

executeAcc :: Arrays a => Acc a -> CIO a
executeAcc acc = prepare acc >> execute acc

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
prepare (Avar _)       = return ()		-- TLM: ??
prepare (Unit _)       = return ()      	-- TLM: ??
prepare (Reshape _ xs) = prepare xs		-- TLM: ??
prepare (Index _ xs _) = prepare xs     	-- TLM: ??

prepare acc@(Replicate _ _ xs)   = prepare xs >> compile acc
prepare acc@(Map _ xs)           = prepare xs >> compile acc
prepare acc@(ZipWith _ xs ys)    = prepare xs >> prepare ys >> compile acc
prepare acc@(Fold _ _ xs)        = prepare xs >> compile acc
prepare acc@(FoldSeg _ _ xs ys)  = prepare xs >> prepare ys >> compile acc
prepare acc@(Scanl _ _ xs)       = prepare xs >> compile acc
prepare acc@(Scanr _ _ xs)       = prepare xs >> compile acc
prepare acc@(Permute _ xs _ ys)  = prepare xs >> prepare ys >> compile acc
prepare acc@(Backpermute _ _ xs) = prepare xs >> compile acc

