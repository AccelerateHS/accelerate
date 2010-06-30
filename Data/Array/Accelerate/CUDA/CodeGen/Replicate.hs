-- |
-- Module      : Data.Array.Accelerate.CUDA.CodeGen.Replicate
-- Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--
-- Replicate an array across one or more dimensions
--

module Data.Array.Accelerate.CUDA.CodeGen.Replicate (mkReplicate)
  where

import Language.C
import Data.Array.Accelerate.CUDA.CodeGen.Data


mkReplicate :: [CType] -> [CExpr] -> CUTranslSkel
mkReplicate _ty _slix = undefined

