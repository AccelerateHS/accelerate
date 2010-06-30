-- |
-- Module      : Data.Array.Accelerate.CUDA.CodeGen.Data
-- Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--
-- Common data types for code generation
--

module Data.Array.Accelerate.CUDA.CodeGen.Data
  (
    CType, CUTranslSkel(..)
  )
  where

import Language.C
import Text.PrettyPrint

type CType        = [CTypeSpec]
data CUTranslSkel = CUTranslSkel CTranslUnit FilePath

instance Pretty CUTranslSkel where
  pretty (CUTranslSkel code skel) =
    vcat [ include "accelerate_cuda_extras.h"
         , pretty code
         , include skel ]


include :: String -> Doc
include hdr = text "#include <" <> text hdr <> text ">"

