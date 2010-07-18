-- |
-- Module      : Data.Array.Accelerate.CUDA.CodeGen.Skeleton
-- Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Constructors for array computation skeletons
--

module Data.Array.Accelerate.CUDA.CodeGen.Skeleton
  (
    mkFold, mkFoldSeg, mkMap, mkZipWith, mkScanl, mkScanr,
    mkPermute, mkBackpermute, mkIndex, mkReplicate
  )
  where

import Language.C
import Data.Array.Accelerate.CUDA.CodeGen.Data
import Data.Array.Accelerate.CUDA.CodeGen.Util
import Data.Array.Accelerate.CUDA.CodeGen.Tuple

--------------------------------------------------------------------------------
-- Reduction
--------------------------------------------------------------------------------

mkFold :: [CType] -> [CExpr] -> [CExpr] -> CUTranslSkel
mkFold ty identity apply = CUTranslSkel code skel
  where
    skel = "fold.inl"
    code = CTranslUnit
            ( mkTupleTypeAsc 2 ty ++
            [ mkIdentity identity
            , mkApply 2 apply ])
            (mkNodeInfo (initPos "fold.cu") (Name 0))

mkFoldSeg :: [CType] -> [CExpr] -> [CExpr] -> CUTranslSkel
mkFoldSeg _ty _identity _apply = undefined


--------------------------------------------------------------------------------
-- Map
--------------------------------------------------------------------------------

mkMap :: [CType] -> [CType] -> [CExpr] -> CUTranslSkel
mkMap tyOut tyIn0 apply = CUTranslSkel code skel
  where
    skel = "map.inl"
    code = CTranslUnit
            ( mkTupleType Nothing  tyOut ++
              mkTupleType (Just 0) tyIn0 ++
            [ mkApply 1 apply ])
            (mkNodeInfo (initPos "map.cu") (Name 0))


mkZipWith :: [CType] -> [CType] -> [CType] -> [CExpr] -> CUTranslSkel
mkZipWith tyOut tyIn1 tyIn0 apply = CUTranslSkel code skel
  where
    skel = "zipWith.inl"
    code = CTranslUnit
            ( mkTupleType Nothing  tyOut ++
              mkTupleType (Just 1) tyIn1 ++
              mkTupleType (Just 0) tyIn0 ++
            [ mkApply 2 apply ])
            (mkNodeInfo (initPos "zipWith.cu") (Name 0))


--------------------------------------------------------------------------------
-- Scan
--------------------------------------------------------------------------------

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


--------------------------------------------------------------------------------
-- Permutation
--------------------------------------------------------------------------------

mkPermute :: [CType] -> [CExpr] -> [CExpr] -> CUTranslSkel
mkPermute ty combine index = CUTranslSkel code skel
  where
    skel = "permute.inl"
    code = CTranslUnit
            ( mkTupleTypeAsc 2 ty ++
            [ mkIgnore (-1)
            , mkIndexFun index
            , mkApply 2 combine ])
            (mkNodeInfo (initPos "permute.cu") (Name 0))

-- TLM 2010-07-10:
--   should generate from Sugar.Ix (or Representation.Ix) notion of ignore
--
mkIgnore :: Integer -> CExtDecl
mkIgnore n =
  CDeclExt
    (CDecl [CTypeQual (CConstQual internalNode),CTypeSpec (CTypeDef (internalIdent "Ix") internalNode)]
           [(Just (CDeclr (Just (internalIdent "ignore")) [] Nothing [] internalNode),Just (CInitExpr (CCast (CDecl [CTypeSpec (CTypeDef (internalIdent "Ix") internalNode)] [] internalNode) (CConst (CIntConst (cInteger n) internalNode)) internalNode) internalNode),Nothing)]
           internalNode)

mkBackpermute :: [CType] -> [CExpr] -> CUTranslSkel
mkBackpermute ty index = CUTranslSkel code skel
  where
    skel = "backpermute.inl"
    code = CTranslUnit
            ( mkTupleTypeAsc 1 ty ++
            [ mkIndexFun index ])
            (mkNodeInfo (initPos "backpermute.cu") (Name 0))


--------------------------------------------------------------------------------
-- Multidimensional Index and Replicate
--------------------------------------------------------------------------------

mkIndex :: [CType] -> [CExpr] -> CUTranslSkel
mkIndex _ty _slix = undefined

mkReplicate :: [CType] -> [CExpr] -> CUTranslSkel
mkReplicate _ty _slix = undefined

