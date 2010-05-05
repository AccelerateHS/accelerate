-- |
-- Module      : Data.Array.Accelerate.CUDA.Kernel
-- Copyright   : [2008..2009] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Meta-module exporting base kernel generation routines
--

module Data.Array.Accelerate.CUDA.Kernel
  (
    module Data.Array.Accelerate.CUDA.Kernel.Fold,
    module Data.Array.Accelerate.CUDA.Kernel.Map,
    module Data.Array.Accelerate.CUDA.Kernel.ZipWith
  )
  where

import Data.Array.Accelerate.CUDA.Kernel.Fold    (foldGen)
import Data.Array.Accelerate.CUDA.Kernel.Map     (mapGen)
import Data.Array.Accelerate.CUDA.Kernel.ZipWith (zipWithGen)

