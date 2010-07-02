-- |
-- Module      : Data.Array.Accelerate.CUDA.CodeGen.Scan
-- Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--

module Data.Array.Accelerate.CUDA.CodeGen.Scan (mkScanl, mkScanr)
  where

import Language.C
import Data.Array.Accelerate.CUDA.CodeGen.Data
import Data.Array.Accelerate.CUDA.CodeGen.Util
import Data.Array.Accelerate.CUDA.CodeGen.Tuple


mkScan :: Bool -> [CType] -> [CExpr] -> [CExpr] -> CUTranslSkel
mkScan isBackward ty identity apply =
  CUTranslSkel code skel
  where
    skel | length ty == 1 = "thrust/scan_safe.inl"      -- TODO: use fast scan for primitive types
         | otherwise      = "thrust/scan_safe.inl"

    code = CTranslUnit
            ( mkTupleTypeAsc 2 ty ++
            [ mkIdentity identity
            , mkApply 2 apply
            , mkFlag "reverse" (fromBool isBackward) ])
            (mkNodeInfo (initPos "scan.cu") (Name 0))


mkScanl :: [CType] -> [CExpr] -> [CExpr] -> CUTranslSkel
mkScanl = mkScan False

mkScanr :: [CType] -> [CExpr] -> [CExpr] -> CUTranslSkel
mkScanr = mkScan True

-- TLM 2010-06-30:
--   Test whether the compiler will use this to avoid branching
--
mkFlag :: String -> CExpr -> CExtDecl
mkFlag name val =
  CDeclExt (CDecl
    [CTypeQual (CAttrQual (CAttr (internalIdent "device") [] internalNode)), CStorageSpec (CStatic internalNode), CTypeQual (CConstQual internalNode), CTypeSpec (CIntType internalNode)]
    [(Just (CDeclr (Just (internalIdent name)) [] Nothing [] internalNode),Just (CInitExpr val internalNode),Nothing)]
    internalNode)

