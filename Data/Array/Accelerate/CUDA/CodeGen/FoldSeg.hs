-- |
-- Module      : Data.Array.Accelerate.CUDA.CodeGen.FoldSeg
-- Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--
-- Segmented reduction
--

module Data.Array.Accelerate.CUDA.CodeGen.FoldSeg (mkFoldSeg)
  where

import Language.C
import Data.Array.Accelerate.CUDA.CodeGen.Util


mkFoldSeg :: String -> [CType] -> [CExpr] -> [CExpr] -> CTranslUnit
mkFoldSeg _name _ty _identity _apply = undefined

