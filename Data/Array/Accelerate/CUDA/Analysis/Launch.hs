{-# LANGUAGE GADTs, PatternGuards #-}
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
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Analysis.Type
import Data.Array.Accelerate.Array.Representation
import Data.Array.Accelerate.Array.Sugar                (Array(..))

import Data.Array.Accelerate.CUDA.State
import qualified Foreign.CUDA.Analysis                  as CUDA
import qualified Foreign.CUDA.Driver                    as CUDA
import Foreign.Storable


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
launchConfig :: OpenAcc aenv a -> CUDA.Fun -> CIO (Int, Int, Integer)
launchConfig acc@(Scan _ _ _) _ = return (128, gridSize acc 128, toInteger (sharedMem acc 128))

launchConfig acc fn = do
  regs <- liftIO $ CUDA.requires fn CUDA.NumRegs
  stat <- liftIO $ CUDA.requires fn CUDA.SharedSizeBytes        -- static memory only
  prop <- getM deviceProps

  let dyn        = sharedMem acc
      (cta, occ) = CUDA.optimalBlockSize prop (const regs) ((stat+) . dyn)
      mbk        = CUDA.multiProcessorCount prop * CUDA.activeThreadBlocks occ

  return (cta, mbk `min` gridSize acc cta, toInteger (dyn cta))


-- |
-- Determine the number of blocks of the given size necessary to process the
-- given array expression. This should understand things like #elements per
-- thread for the various kernels.
--
gridSize :: OpenAcc aenv a -> Int -> Int
gridSize acc cta =
  let between arr n = (n+arr-1) `div` n
  in  1 `max` ((cta - 1 + (arraySize acc `between` elementsPerThread acc)) `div` cta)

arraySize :: OpenAcc aenv a -> Int
arraySize (Use (Array sh _)) = size sh
arraySize (Map _ xs)         = arraySize xs
arraySize (ZipWith _ xs ys)  = arraySize xs `min` arraySize ys   -- TLM: intersect??
arraySize (Fold _ _ xs)      = arraySize xs
arraySize (Scan _ _ xs)      = arraySize xs

elementsPerThread :: OpenAcc aenv a -> Int
elementsPerThread (Scan _ _ _) = 8
elementsPerThread _            = 1


-- |
-- Analyse the given array expression, returning an estimate of dynamic shared
-- memory usage as a function of thread block size. This can be used by the
-- occupancy calculator to optimise kernel launch shape.
--
sharedMem :: forall aenv a. OpenAcc aenv a -> Int -> Int
sharedMem (Map _ _)       _ = 0
sharedMem (ZipWith _ _ _) _ = 0
sharedMem (Fold _ x _)    t = sizeOfElem (expType x) * t
sharedMem (Scan _ x _)    t = sizeOfElem (expType x) * t * 2


sizeOfElem :: TupleType a -> Int
sizeOfElem UnitTuple       = 0
sizeOfElem (PairTuple a b) = sizeOfElem a + sizeOfElem b

sizeOfElem (SingleTuple (NumScalarType (IntegralNumType t)))
  | IntegralDict <- integralDict t = sizeOf $ (undefined :: IntegralType a -> a) t
sizeOfElem (SingleTuple (NumScalarType (FloatingNumType t)))
  | FloatingDict <- floatingDict t = sizeOf $ (undefined :: FloatingType a -> a) t
sizeOfElem (SingleTuple (NonNumScalarType t))
  | NonNumDict   <- nonNumDict t   = sizeOf $ (undefined :: NonNumType a   -> a) t

