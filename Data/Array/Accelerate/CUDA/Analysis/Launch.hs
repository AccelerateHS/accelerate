{-# LANGUAGE GADTs #-}
-- |
-- Module      : Data.Array.Accelerate.CUDA.Analysis.Launch
-- Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
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
import Data.Array.Accelerate.CUDA.State
import Data.Array.Accelerate.Array.Sugar                (Array(..))
import Data.Array.Accelerate.Array.Representation

import qualified Foreign.CUDA.Analysis                  as CUDA
import qualified Foreign.CUDA.Driver                    as CUDA


-- |
-- Determine kernel launch parameters for the given array computation (as well
-- as compiled function module). This consists of the thread block size, number
-- of blocks, and dynamically allocated shared memory (bytes), respectively.
--
-- TLM: this could probably be stored in the KernelEntry
--
launchConfig :: OpenAcc aenv a -> CUDA.Fun -> CIO (Int, Int, Integer)
launchConfig acc fn = do
  regs <- liftIO $ CUDA.requires fn CUDA.NumRegs
  stat <- liftIO $ CUDA.requires fn CUDA.SharedSizeBytes        -- static memory only
  prop <- getM deviceProps

  let dyn = sharedMem acc
      cta = fst $ CUDA.optimalBlockSize prop (const regs) ((stat+) . dyn)

  return (cta, gridSize acc cta, fromIntegral (stat + dyn cta))


-- |
-- Determine the number of blocks of the given size necessary to process the
-- given array expression. This should understand things like #elements per
-- thread for the various kernels.
--
gridSize :: OpenAcc aenv a -> Int -> Int
gridSize acc cta = (cta - 1 + (2 `eltPerThread` arraySize acc)) `div` cta
  where eltPerThread n arr = (n+arr-1) `div` n

arraySize :: OpenAcc aenv a -> Int
arraySize (Use (Array sh _)) = size sh
arraySize (Map _ xs)         = arraySize xs
arraySize (ZipWith _ xs ys)  = arraySize xs `min` arraySize ys   -- TLM: intersect??


-- |
-- Analyse the given array expression, returning an estimate of dynamic shared
-- memory usage as a function of thread block size. This can be used by the
-- occupancy calculator to optimise kernel launch shape.
--
sharedMem :: OpenAcc aenv a -> Int -> Int
sharedMem (Map _ _)       _ = 0
sharedMem (ZipWith _ _ _) _ = 0
--sharedMem (Fold _ x _)    t = 2 * t * sizeOf (toElem $ expType x) --Elem ~ Storable ??
sharedMem (Use _)         _ = undefined

