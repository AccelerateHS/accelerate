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

import qualified Foreign.CUDA.Analysis          as CUDA
import qualified Foreign.CUDA.Driver            as CUDA


-- |
-- Determine kernel launch parameters for the given array computation (as well
-- as compiled function module). This consists of the thread block size, number
-- of blocks, and dynamically allocated shared memory (bytes), respectively.
--
launchConfig :: OpenAcc aenv a -> CUDA.Fun -> CIO (Int, Int, Integer)
launchConfig acc fn = do
  regs <- liftIO $ CUDA.requires fn CUDA.NumRegs
  stat <- liftIO $ CUDA.requires fn CUDA.SharedSizeBytes        -- static memory only
  prop <- getM deviceProps

  let dyn     = sharedMem acc
      (blk,_) = CUDA.optimalBlockSize prop (const regs) ((stat+) . dyn)

  return (blk, gridSize acc blk, fromIntegral (stat + dyn blk))


-- |
-- Determine the number of blocks of the given size necessary to process the
-- given array expression. This should understand things like #elements per
-- thread for the various kernels.
--
gridSize :: OpenAcc acc a -> Int -> Int
gridSize _acc _blk = undefined

{-
arraySize :: (Ix dim, Ix (ElemRepr dim), Elem e) => Array dim e -> Int
arraySize (Array sh _) = size sh
-}


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


