-- |
-- Module      : Data.Array.Accelerate.CUDA.CodeGen.Data
-- Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
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
    CType, CMacro, CUTranslSkel(..)
  )
  where

import Language.C
import Text.PrettyPrint

type CType        = [CTypeSpec]
type CMacro       = (Ident, Maybe CExpr)
data CUTranslSkel = CUTranslSkel CTranslUnit [CMacro] FilePath

instance Pretty CUTranslSkel where
  pretty (CUTranslSkel code defs skel) =
    vcat [ include "accelerate_cuda_extras.h"
         , vcat (map macro defs)
         , pretty code
         , include skel
         ]


include :: FilePath -> Doc
include hdr = text "#include <" <> text hdr <> text ">"

macro :: CMacro -> Doc
macro (d,v) = text "#define" <+> text (identToString d)
                             <+> maybe empty (parens . pretty) v

