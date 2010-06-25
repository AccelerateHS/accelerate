-- |
-- Module      : Data.Array.Accelerate.CUDA.CodeGen.Index
-- Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--
-- Replicate an array across one or more dimensions
--

module Data.Array.Accelerate.CUDA.CodeGen.Index (mkIndex)
  where

import Language.C
import Data.Array.Accelerate.CUDA.CodeGen.Util


mkIndex :: String -> [CType] -> [CExpr] -> CTranslUnit
mkIndex _name _ty _slix = undefined

