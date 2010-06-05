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
import Control.Applicative
import Control.Monad.State
import Data.Maybe					(fromMaybe)

import qualified Data.Map       			as M  (empty)
import qualified Data.IntMap    			as IM (empty)

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
run acc =
  let ast = Sugar.convertAcc acc
  in  evalCUDA $
        generate ast >> execute ast >>= collect ast


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

  where
    finalise     = CUDA.destroy . snd
    initialise n = do
      CUDA.initialise []
      dev <- CUDA.device (fromMaybe 0 n)
      ctx <- CUDA.create dev [CUDA.SchedAuto]
      return (dev, ctx)


-- Evaluation
-- ~~~~~~~~~~
--

-- Traverse the array expression in depth-first order, initiating asynchronous
-- code generation and data transfer.
--
generate :: OpenAcc aenv a -> CIO ()
generate (Use (Array sh ad)) =
  let n = size sh
  in do
    mallocArray    ad n
    pokeArrayAsync ad n Nothing

generate (Let  xs ys)   = generate xs >> generate ys
generate (Let2 xs ys)   = generate xs >> generate ys
generate (Avar _)       = return ()		-- TLM: ??
generate (Unit _)       = return ()      	-- TLM: ??
generate (Reshape _ xs) = generate xs		-- TLM: ??
generate (Index _ xs _) = generate xs     	-- TLM: ??

generate acc@(Replicate _ _ xs)   = generate xs >> compile acc
generate acc@(Map _ xs)           = generate xs >> compile acc
generate acc@(ZipWith _ xs ys)    = generate xs >> generate ys >> compile acc
generate acc@(Fold _ _ xs)        = generate xs >> compile acc
generate acc@(FoldSeg _ _ xs ys)  = generate xs >> generate ys >> compile acc
generate acc@(Scanl _ _ xs)       = generate xs >> compile acc
generate acc@(Scanr _ _ xs)       = generate xs >> compile acc
generate acc@(Permute _ xs _ ys)  = generate xs >> generate ys >> compile acc
generate acc@(Backpermute _ _ xs) = generate xs >> compile acc


-- |
-- Collect the result of an array computation
--
collect :: OpenAcc aenv a -> a -> CIO a
collect (Replicate _ _ _)   = getArr
collect (Map _ _)           = getArr
collect (ZipWith _ _ _)     = getArr
collect (Fold _ _ _)        = getArr
collect (FoldSeg _ _ _ _)   = getArr
collect (Scanl _ _ _)       = getArr2
collect (Scanr _ _ _)       = getArr2
collect (Permute _ _ _ _)   = getArr
collect (Backpermute _ _ _) = getArr
collect _ = error "Data.Array.Accelerate.CUDA: internal error"


getArr :: Array dim e -> CIO (Array dim e)
getArr arr@(Array sh ad) = peekArray ad (size sh) >> free ad >> return arr

getArr2 :: (Array dim1 e1, Array dim2 e2) -> CIO (Array dim1 e1, Array dim2 e2)
getArr2 (a1,a2) = (,) <$> getArr a1 <*> getArr a2

