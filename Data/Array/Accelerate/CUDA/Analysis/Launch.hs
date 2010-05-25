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

module Data.Array.Accelerate.CUDA.Analysis.Launch
  where

import Data.Array.Accelerate.AST


-- |
-- Analyse the given array expression, returning estimates of register and
-- shared memory usage as a function of thread block size (respectively). This
-- can be used by the occupancy calculator to optimise kernel launch shape.
--
launchResources :: OpenAcc aenv a -> (Int -> Int, Int -> Int)
launchResources _ = (const 8, (*4))

