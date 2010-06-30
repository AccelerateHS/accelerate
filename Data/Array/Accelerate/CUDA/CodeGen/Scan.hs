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


mkScan :: Bool -> Bool -> Bool -> Bool -> Bool -> [CType] -> [CExpr] -> [CExpr] -> CUTranslSkel
mkScan isBackward isExclusive isMultiRow isMultiBlock isFullBlock ty identity apply =
  CUTranslSkel code skel
  where
    skel = "scan.inl"
    code = CTranslUnit
            ( mkTupleTypeAsc 2 ty ++
            [ mkTypedef  "T"  False [CTypeDef (internalIdent "TyOut") internalNode]
            , mkTyVector "T4" 4     [CTypeDef (internalIdent "TyOut") internalNode]
            , mkIdentity identity
            , mkApply 2 apply
            , mkFlag "IS_BACKWARD"   (fromBool isBackward)
            , mkFlag "IS_EXCLUSIVE"  (fromBool isExclusive)
            , mkFlag "IS_MULTIROW"   (fromBool isMultiRow)
            , mkFlag "IS_MULTIBLOCK" (fromBool isMultiBlock)
            , mkFlag "IS_FULLBLOCK"  (fromBool isFullBlock) ])
            (mkNodeInfo (initPos "scan.cu") (Name 0))


mkScanl :: [CType] -> [CExpr] -> [CExpr] -> CUTranslSkel
mkScanl = mkScan False True False True False

mkScanr :: [CType] -> [CExpr] -> [CExpr] -> CUTranslSkel
mkScanr = mkScan True True False True False

-- TLM 2010-06-30:
--   These are meant to be #define declarations, but the compiler may yet be
--   smart enough to inline them.
--
mkFlag :: String -> CExpr -> CExtDecl
mkFlag name val =
  CDeclExt (CDecl
    [CTypeQual (CConstQual internalNode),CTypeSpec (CIntType internalNode)]
    [(Just (CDeclr (Just (internalIdent name)) [] Nothing [] internalNode),Just (CInitExpr val internalNode),Nothing)]
    internalNode)

