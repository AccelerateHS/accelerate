{-# LANGUAGE CPP, GADTs #-}
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
import Data.Array.Accelerate.Analysis.Shape
import Data.Array.Accelerate.CUDA.State
import qualified Foreign.CUDA.Analysis                  as CUDA
import qualified Foreign.CUDA.Driver                    as CUDA
import qualified Foreign.Storable                       as F

#include "accelerate.h"


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
launchConfig :: OpenAcc aenv a -> Int -> CUDA.Fun -> CIO (Int, Int, Integer)
launchConfig acc n fn = do
  regs <- liftIO $ CUDA.requires fn CUDA.NumRegs
  stat <- liftIO $ CUDA.requires fn CUDA.SharedSizeBytes        -- static memory only
  prop <- getM deviceProps

  let dyn        = sharedMem prop acc
      (cta, occ) = blockSize prop acc regs ((stat+) . dyn)
      mbk        = CUDA.multiProcessorCount prop * CUDA.activeThreadBlocks occ

  return (cta, mbk `min` gridSize prop acc n cta, toInteger (dyn cta))


-- |
-- Determine the optimal thread block size for a given array computation. Fold
-- requires blocks with a power-of-two number of threads.
--
blockSize :: CUDA.DeviceProperties -> OpenAcc aenv a -> Int -> (Int -> Int) -> (Int, CUDA.Occupancy)
blockSize p (Fold _ _ _) r s = CUDA.optimalBlockSizeBy p CUDA.incPow2 (const r) s
blockSize p (Fold1 _ _)  r s = CUDA.optimalBlockSizeBy p CUDA.incPow2 (const r) s
blockSize p _            r s = CUDA.optimalBlockSizeBy p CUDA.incWarp (const r) s


-- |
-- Determine the number of blocks of the given size necessary to process the
-- given array expression. This should understand things like #elements per
-- thread for the various kernels.
--
-- foldSeg: 'size' is the number of segments, require one warp per segment
--
gridSize :: CUDA.DeviceProperties -> OpenAcc aenv a -> Int -> Int -> Int
gridSize p acc@(FoldSeg _ _ _ _) size cta = split acc (size * CUDA.warpSize p) cta
gridSize p acc@(Fold1Seg _ _ _)  size cta = split acc (size * CUDA.warpSize p) cta
gridSize p acc@(Fold _ _ a)      size cta = if accDim a == 1 then split acc size cta else split acc (size * CUDA.warpSize p) cta
gridSize p acc@(Fold1 _ a)       size cta = if accDim a == 1 then split acc size cta else split acc (size * CUDA.warpSize p) cta
gridSize _ acc                   size cta = split acc size cta

split :: OpenAcc aenv a -> Int -> Int -> Int
split acc size cta = (size `between` eltsPerThread acc) `between` cta
  where
    between arr n   = 1 `max` ((n + arr - 1) `div` n)
    eltsPerThread _ = 1


-- |
-- Analyse the given array expression, returning an estimate of dynamic shared
-- memory usage as a function of thread block size. This can be used by the
-- occupancy calculator to optimise kernel launch shape.
--
sharedMem :: CUDA.DeviceProperties -> OpenAcc aenv a -> Int -> Int
-- non-computation forms
sharedMem _ (Let _ _)     _ = INTERNAL_ERROR(error) "sharedMem" ""
sharedMem _ (Let2 _ _)    _ = INTERNAL_ERROR(error) "sharedMem" ""
sharedMem _ (Avar _)      _ = INTERNAL_ERROR(error) "sharedMem" ""
sharedMem _ (Use _)       _ = INTERNAL_ERROR(error) "sharedMem" ""
sharedMem _ (Unit _)      _ = INTERNAL_ERROR(error) "sharedMem" ""
sharedMem _ (Reshape _ _) _ = INTERNAL_ERROR(error) "sharedMem" ""

-- skeleton nodes
sharedMem _ (Generate _ _)      _        = 0
sharedMem _ (Replicate _ _ _)   _        = 0
sharedMem _ (Index _ _ _)       _        = 0
sharedMem _ (Map _ _)           _        = 0
sharedMem _ (ZipWith _ _ _)     _        = 0
sharedMem _ (Permute _ _ _ _)   _        = 0
sharedMem _ (Backpermute _ _ _) _        = 0
sharedMem _ (Fold  _ _ a)       blockDim = sizeOf (accType a) * blockDim
sharedMem _ (Fold1 _ a)         blockDim = sizeOf (accType a) * blockDim
sharedMem _ (Scanl _ x _)       blockDim = sizeOf (expType x) * blockDim
sharedMem _ (Scanr _ x _)       blockDim = sizeOf (expType x) * blockDim
sharedMem _ (Scanl' _ x _)      blockDim = sizeOf (expType x) * blockDim
sharedMem _ (Scanr' _ x _)      blockDim = sizeOf (expType x) * blockDim
sharedMem _ (Scanl1 _ a)        blockDim = sizeOf (accType a) * blockDim
sharedMem _ (Scanr1 _ a)        blockDim = sizeOf (accType a) * blockDim
sharedMem p (FoldSeg _ _ a _)   blockDim =
  (blockDim `div` CUDA.warpSize p * 2) * F.sizeOf (undefined::Int32) + blockDim * sizeOf (accType a)
sharedMem p (Fold1Seg _ a _) blockDim =
  (blockDim `div` CUDA.warpSize p * 2) * F.sizeOf (undefined::Int32) + blockDim * sizeOf (accType a)
sharedMem _ (Stencil _ _ _)      _ = INTERNAL_ERROR(error) "sharedMem" "Stencil not implemented yet"
sharedMem _ (Stencil2 _ _ _ _ _) _ = INTERNAL_ERROR(error) "sharedMem" "Stencil2 not implemented yet"

