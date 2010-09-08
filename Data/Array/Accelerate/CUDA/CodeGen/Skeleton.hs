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
import System.FilePath
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
            (mkNodeInfo (initPos skel) (Name 0))

mkFoldSeg :: [CType] -> [CType] -> [CExpr] -> [CExpr] -> CUTranslSkel
mkFoldSeg ty int identity apply = CUTranslSkel code skel
  where
    skel = "fold_segmented.inl"
    code = CTranslUnit
            ( mkTupleTypeAsc 2 ty ++
            [ mkTypedef "Int" False (head int)
            , mkIdentity identity
            , mkApply 2 apply ])
            (mkNodeInfo (initPos skel) (Name 0))


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
            (mkNodeInfo (initPos skel) (Name 0))


mkZipWith :: [CType] -> Int -> [CType] -> Int -> [CType] -> Int -> [CExpr] -> CUTranslSkel
mkZipWith tyOut dimOut tyIn1 dimIn1 tyIn0 dimIn0 apply = CUTranslSkel code skel
  where
    skel = "zipWith.inl"
    code = CTranslUnit
            ( mkTupleType Nothing  tyOut ++
              mkTupleType (Just 1) tyIn1 ++
              mkTupleType (Just 0) tyIn0 ++
            [ mkApply 2 apply
            , mkDim "DimOut" dimOut
            , mkDim "DimIn1" dimIn1
            , mkDim "DimIn0" dimIn0 ])
            (mkNodeInfo (initPos skel) (Name 0))


--------------------------------------------------------------------------------
-- Scan
--------------------------------------------------------------------------------

mkScan :: Bool -> [CType] -> [CExpr] -> [CExpr] -> CUTranslSkel
mkScan isBackward ty identity apply =
  CUTranslSkel code skel
  where
    skel | length ty == 1 = "thrust" </> "scan_safe.inl"        -- TODO: use fast scan for primitive types
         | otherwise      = "thrust" </> "scan_safe.inl"

    code = CTranslUnit
            ( mkTupleTypeAsc 2 ty ++
            [ mkIdentity identity
            , mkApply 2 apply
            , mkFlag "reverse" (fromBool isBackward) ])
            (mkNodeInfo (initPos (takeFileName skel)) (Name 0))


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

mkPermute :: [CType] -> Int -> Int -> [CExpr] -> [CExpr] -> CUTranslSkel
mkPermute ty dimOut dimIn0 combinefn indexfn = CUTranslSkel code skel
  where
    skel = "permute.inl"
    code = CTranslUnit
            ( mkTupleTypeAsc 2 ty ++
            [ mkDim "DimOut" dimOut
            , mkDim "DimIn0" dimIn0
            , mkProject Forward indexfn
            , mkApply 2 combinefn ])
            (mkNodeInfo (initPos skel) (Name 0))

mkBackpermute :: [CType] -> Int -> Int -> [CExpr] -> CUTranslSkel
mkBackpermute ty dimOut dimIn0 indexFn = CUTranslSkel code skel
  where
    skel = "backpermute.inl"
    code = CTranslUnit
            ( mkTupleTypeAsc 1 ty ++
            [ mkDim "DimOut" dimOut
            , mkDim "DimIn0" dimIn0
            , mkProject Backward indexFn ])
            (mkNodeInfo (initPos skel) (Name 0))


--------------------------------------------------------------------------------
-- Multidimensional Index and Replicate
--------------------------------------------------------------------------------

mkIndex :: [CType] -> Int -> Int -> Int -> [CExpr] -> CUTranslSkel
mkIndex ty dimSl dimCo dimIn0 slix = CUTranslSkel code skel
  where
    skel = "slice.inl"
    code = CTranslUnit
            ( mkTupleTypeAsc 1 ty ++
            [ mkDim "Slice"    dimSl
            , mkDim "CoSlice"  dimCo
            , mkDim "SliceDim" dimIn0
            , mkSliceIndex slix ])
            (mkNodeInfo (initPos skel) (Name 0))


mkReplicate :: [CType] -> Int -> Int -> [CExpr] -> CUTranslSkel
mkReplicate ty dimSl dimOut slix = CUTranslSkel code skel
  where
    skel = "replicate.inl"
    code = CTranslUnit
	    ( mkTupleTypeAsc 1 ty ++
	    [ mkDim "Slice"    dimSl
	    , mkDim "SliceDim" dimOut
	    , mkSliceReplicate slix ])
	    (mkNodeInfo (initPos skel) (Name 0))

