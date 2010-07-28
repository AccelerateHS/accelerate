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

import Data.Int
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Analysis.Type
import Data.Array.Accelerate.CUDA.State
import qualified Foreign.CUDA.Analysis                  as CUDA
import qualified Foreign.CUDA.Driver                    as CUDA
import qualified Foreign.Storable                       as F


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

  let dyn        = sharedMem prop acc
      (cta, occ) = CUDA.optimalBlockSize prop (const regs) ((stat+) . dyn)
      mbk        = CUDA.multiProcessorCount prop * CUDA.activeThreadBlocks occ

  return (cta, mbk `min` gridSize prop acc n cta, toInteger (dyn cta))


-- |
-- Determine the number of blocks of the given size necessary to process the
-- given array expression. This should understand things like #elements per
-- thread for the various kernels.
--
-- foldSeg: 'size' is the number of segments, require one warp per segment
--
gridSize :: CUDA.DeviceProperties -> OpenAcc aenv a -> Int -> Int -> Int
gridSize p (FoldSeg _ _ _ _) size cta = ((size * CUDA.warpSize p) + cta - 1) `div` cta
gridSize _ acc size cta =
  let between arr n = (n+arr-1) `div` n
  in  1 `max` ((cta - 1 + (size `between` elementsPerThread acc)) `div` cta)

elementsPerThread :: OpenAcc aenv a -> Int
elementsPerThread _ = 1


-- |
-- Analyse the given array expression, returning an estimate of dynamic shared
-- memory usage as a function of thread block size. This can be used by the
-- occupancy calculator to optimise kernel launch shape.
--
sharedMem :: CUDA.DeviceProperties -> OpenAcc aenv a -> Int -> Int
sharedMem _ (Fold  _ x _)     blockDim = sizeOf (expType x) * blockDim
sharedMem _ (Scanl _ x _)     blockDim = sizeOf (expType x) * blockDim
sharedMem _ (Scanr _ x _)     blockDim = sizeOf (expType x) * blockDim
sharedMem p (FoldSeg _ x _ _) blockDim =
  let warp = CUDA.warpSize p
  in
  (blockDim `div` warp * 2) * F.sizeOf (undefined :: Int32) +
  (blockDim + warp `div` 2) * sizeOf   (expType x)

sharedMem _ _ _ = 0

