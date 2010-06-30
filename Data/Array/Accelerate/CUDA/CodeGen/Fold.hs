-- |
-- Module      : Data.Array.Accelerate.CUDA.CodeGen.Fold
-- Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--
-- Reduce an array to a single value
--

module Data.Array.Accelerate.CUDA.CodeGen.Fold (mkFold)
  where

import Language.C
import Data.Array.Accelerate.CUDA.CodeGen.Data
import Data.Array.Accelerate.CUDA.CodeGen.Util
import Data.Array.Accelerate.CUDA.CodeGen.Tuple


mkFold :: [CType] -> [CExpr] -> [CExpr] -> CUTranslSkel
mkFold ty identity apply = CUTranslSkel code skel
  where
    skel = "fold.inl"
    code = CTranslUnit
            ( mkTupleTypeAsc 2 ty ++
            [ mkIdentity identity
            , mkApply 2 apply ])
            (mkNodeInfo (initPos "fold.cu") (Name 0))

