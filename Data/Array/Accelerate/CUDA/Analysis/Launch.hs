{-# LANGUAGE GADTs #-}
-- |
-- Module      : Data.Array.Accelerate.CUDA.Analysis.Launch
-- Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--

module Data.Array.Accelerate.CUDA.Analysis.Launch (launchConfig)
  where

import Control.Monad.IO.Class

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Analysis.Type

import Data.Array.Accelerate.CUDA.State
import qualified Foreign.CUDA.Analysis                  as CUDA
import qualified Foreign.CUDA.Driver                    as CUDA


-- |
-- Determine kernel launch parameters for the given array computation (as well
-- as compiled function module). This consists of the thread block size, number
-- of blocks, and dynamically allocated shared memory (bytes), respectively.
--
-- By default, this launches the kernel with the minimum block size that gives
-- maximum occupancy, and the grid size limited to the maximum number of
-- physically resident blocks. Hence, kernels may need to process multiple
-- elements per thread.
--
-- TLM: this could probably be stored in the KernelEntry
--
launchConfig :: OpenAcc aenv a -> Int -> CUDA.Fun -> CIO (Int, Int, Integer)
launchConfig acc n fn = do
  regs <- liftIO $ CUDA.requires fn CUDA.NumRegs
  stat <- liftIO $ CUDA.requires fn CUDA.SharedSizeBytes        -- static memory only
  prop <- getM deviceProps

  let dyn        = sharedMem acc
      (cta, occ) = CUDA.optimalBlockSize prop (const regs) ((stat+) . dyn)
      mbk        = CUDA.multiProcessorCount prop * CUDA.activeThreadBlocks occ

  return (cta, mbk `min` gridSize acc n cta, toInteger (dyn cta))


-- |
-- Determine the number of blocks of the given size necessary to process the
-- given array expression. This should understand things like #elements per
-- thread for the various kernels.
--
gridSize :: OpenAcc aenv a -> Int -> Int -> Int
gridSize acc size cta =
  let between arr n = (n+arr-1) `div` n
  in  1 `max` ((cta - 1 + (size `between` elementsPerThread acc)) `div` cta)

elementsPerThread :: OpenAcc aenv a -> Int
elementsPerThread _ = 1


-- |
-- Analyse the given array expression, returning an estimate of dynamic shared
-- memory usage as a function of thread block size. This can be used by the
-- occupancy calculator to optimise kernel launch shape.
--
sharedMem :: OpenAcc aenv a -> Int -> Int
sharedMem (Fold  _ x _) t = sizeOf (expType x) * t
sharedMem (Scanl _ x _) t = sizeOf (expType x) * t
sharedMem (Scanr _ x _) t = sizeOf (expType x) * t
sharedMem _             _ = 0

