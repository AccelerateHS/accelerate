-- |
-- Module      : Data.Array.Accelerate.CUDA.CodeGen.Permute
-- Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--

module Data.Array.Accelerate.CUDA.CodeGen.Permute (mkPermute, mkBackpermute)
  where

import Language.C
import Data.Array.Accelerate.CUDA.CodeGen.Data
import Data.Array.Accelerate.CUDA.CodeGen.Util
import Data.Array.Accelerate.CUDA.CodeGen.Tuple

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

-- should generate from Sugar.Ix (or Representation.Ix) notion of ignore
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

