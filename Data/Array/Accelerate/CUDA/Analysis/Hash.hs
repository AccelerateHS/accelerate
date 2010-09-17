{-# LANGUAGE CPP, GADTs #-}
-- |
-- Module      : Data.Array.Accelerate.CUDA.Analysis.Hash
-- Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--

module Data.Array.Accelerate.CUDA.Analysis.Hash (accToKey)
  where

import Data.Char
import Language.C
import Control.Monad.State
import Text.PrettyPrint

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Pretty ()
import Data.Array.Accelerate.Analysis.Type
import Data.Array.Accelerate.CUDA.CodeGen
import Data.Array.Accelerate.Array.Representation
import qualified Data.Array.Accelerate.Array.Sugar as Sugar

#include "accelerate.h"


-- |
-- Generate a unique key for each kernel computation (not extensively tested...)
--
accToKey :: OpenAcc aenv a -> String
accToKey r@(Replicate s e a) = chr 17  : showTy (accType a) ++ showExp e ++ showSI s e a r
accToKey r@(Index s a e)     = chr 29  : showTy (accType a) ++ showExp e ++ showSI s e r a
accToKey (Map f a)           = chr 41  : showTy (accType a) ++ showFun f
accToKey (ZipWith f x y)     = chr 53  : showTy (accType x) ++ showTy (accType y) ++ showFun f
accToKey (Fold f e a)        = chr 67  : showTy (accType a) ++ showFun f ++ showExp e
accToKey (FoldSeg f e _ a)   = chr 79  : showTy (accType a) ++ showFun f ++ showExp e
accToKey (Scanl f e a)       = chr 97  : showTy (accType a) ++ showFun f ++ showExp e
accToKey (Scanr f e a)       = chr 107 : showTy (accType a) ++ showFun f ++ showExp e
accToKey (Permute c _ p a)   = chr 127 : showTy (accType a) ++ showFun c ++ showFun p
accToKey (Backpermute _ p a) = chr 139 : showTy (accType a) ++ showFun p
accToKey x =
  INTERNAL_ERROR(error) "accToKey"
  (unlines ["incomplete patterns for key generation", render . nest 2 $ text (show x)])

showTy :: TupleType a -> String
showTy UnitTuple = []
showTy (SingleTuple ty) = show ty
showTy (PairTuple a b)  = showTy a ++ showTy b

showFun :: OpenFun env aenv a -> String
showFun f = render . hcat . map pretty $ evalState (codeGenFun f) []

showExp :: OpenExp env aenv a -> String
showExp e = render . hcat . map pretty $ evalState (codeGenExp e) []

showSI :: SliceIndex (Sugar.ElemRepr slix) (Sugar.ElemRepr sl) co (Sugar.ElemRepr dim)
       -> Exp aenv slix                         {- dummy -}
       -> OpenAcc aenv (Sugar.Array sl e)       {- dummy -}
       -> OpenAcc aenv (Sugar.Array dim e)      {- dummy -}
       -> String
showSI sl _ _ _ = slice sl 0
  where
    slice :: SliceIndex slix sl co dim -> Int -> String
    slice (SliceNil)            _ = []
    slice (SliceAll   sliceIdx) n = '_'    :  slice sliceIdx n
    slice (SliceFixed sliceIdx) n = show n ++ slice sliceIdx (n+1)


{-
-- hash function from the dragon book pp437; assumes 7 bit characters and needs
-- the (nearly) full range of values guaranteed for `Int' by the Haskell
-- language definition; can handle 8 bit characters provided we have 29 bit for
-- the `Int's without sign
--
quad :: String -> Int
quad (c1:c2:c3:c4:s)  = (( ord c4 * bits21
                         + ord c3 * bits14
                         + ord c2 * bits7
                         + ord c1)
                         `Prelude.mod` bits28)
                        + (quad s `Prelude.mod` bits28)
quad (c1:c2:c3:[]  )  = ord c3 * bits14 + ord c2 * bits7 + ord c1
quad (c1:c2:[]     )  = ord c2 * bits7  + ord c1
quad (c1:[]        )  = ord c1
quad ([]           )  = 0

bits7, bits14, bits21, bits28 :: Int
bits7  = 2^(7 ::Int)
bits14 = 2^(14::Int)
bits21 = 2^(21::Int)
bits28 = 2^(28::Int)
-}

