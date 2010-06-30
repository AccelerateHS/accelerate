-- |
-- Module      : Data.Array.Accelerate.CUDA.CodeGen.Map
-- Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--
-- Device code for zipWith_k class of functions
--

module Data.Array.Accelerate.CUDA.CodeGen.Map (mkMap, mkZipWith)
  where

import Prelude hiding (map, zipWith)

import Language.C
import Data.Array.Accelerate.CUDA.CodeGen.Data
import Data.Array.Accelerate.CUDA.CodeGen.Util
import Data.Array.Accelerate.CUDA.CodeGen.Tuple


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

