{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module      : Data.Array.Accelerate.CUDA.CodeGen
-- Copyright   : [2008..2009] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.CUDA.CodeGen
  (
    module Data.Array.Accelerate.CUDA.CodeGen.Host,
    module Data.Array.Accelerate.CUDA.CodeGen.Device
  )
  where

import Data.Array.Accelerate.CUDA.CodeGen.Host
import Data.Array.Accelerate.CUDA.CodeGen.Device (fold, map, zipWith)

