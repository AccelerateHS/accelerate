{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}

module Parse where

import Data.Generics

import Language.C
import Language.C.System.GCC
import Language.C.System.Preprocess
import Language.C.Data.Ident

deriving instance Show CArrSize
deriving instance Show CAsmOperand
deriving instance Show CAsmStmt
deriving instance Show CAttr
deriving instance Show CBlockItem
deriving instance Show CBuiltin
deriving instance Show CConst
deriving instance Show CDecl
deriving instance Show CDeclSpec
deriving instance Show CDeclr
deriving instance Show CDerivedDeclr
deriving instance Show CDesignator
deriving instance Show CEnum
deriving instance Show CExpr
deriving instance Show CExtDecl
deriving instance Show CFunDef
deriving instance Show CInit
deriving instance Show CStat
deriving instance Show CStrLit
deriving instance Show CStructTag
deriving instance Show CStructUnion
deriving instance Show CTranslUnit
deriving instance Show CTypeQual
deriving instance Show CTypeSpec


checkResult :: (Show a) => String -> (Either a b) -> IO b
checkResult label = either (error . (label++) . show) return

-- NOTES:
--  * The preprocessor is gcc, *not* the standard CUDA compiler (nvcc).
--
--  * The file must end in a .c extension, else it will be ignored by the
--    preprocessor. The file contents are unaltered C-like CUDA
--
--  * Since we need to include the cuda runtime header, the first CExtDecl's
--    will be from that header, and may be safely dropped.
--
parseCUFile :: Preprocessor cpp
            => cpp -> Maybe FilePath -> [String] -> FilePath -> IO (Either ParseError CTranslUnit)
parseCUFile cpp tmp opts file = parseCFile cpp tmp (opts ++ extras) file
  where
    extras = [ "-I", "."
             , "-I", "/usr/local/cuda/include"
             , "-m32", "-malign-double"
             , "-D__CUDA_ARCH__=100"
             , "-DCUDA_FLOAT_MATH_FUNCTIONS"
             , "-DCUDA_NO_SM_11_ATOMIC_INTRINSICS"
             , "-DCUDA_NO_SM_12_ATOMIC_INTRINSICS"
             , "-DCUDA_NO_SM_13_DOUBLE_INTRINSICS"
             , "-D__CUDACC__"
             , "-include", "cuda_runtime.h"
             , "-include", "accelerate_cuda_extras.h"
             ]

