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

module Data.Array.Accelerate.CUDA.Analysis.Hash (accToKey, accToID)
  where

import Data.Char
import Language.C
import Text.PrettyPrint

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Pretty ()
import Data.Array.Accelerate.Analysis.Type
import Data.Array.Accelerate.CUDA.CodeGen
import Data.Array.Accelerate.Array.Representation
import qualified Data.Array.Accelerate.Array.Sugar as Sugar

#include "accelerate.h"


-- | Generate a unique key for each kernel computation
--
-- The first radical identifies the skeleton type (actually, this is arithmetic
-- sequence A031377 = primes p(3n-2)), followed by the salient features that
-- parameterise skeleton instantiation.
--
accToKey :: OpenAcc aenv a -> String
accToKey r@(Replicate s e a)   = chr (accToID r) : showTy (accType a) ++ showExp e ++ showSI s e a r
accToKey r@(Index s a e)       = chr (accToID r) : showTy (accType a) ++ showExp e ++ showSI s e r a
accToKey r@(Map f a)           = chr (accToID r) : showTy (accType a) ++ showFun f
accToKey r@(ZipWith f x y)     = chr (accToID r) : showTy (accType x) ++ showTy (accType y) ++ showFun f
accToKey r@(Fold f e a)        = chr (accToID r) : showTy (accType a) ++ showFun f ++ showExp e
accToKey r@(FoldSeg f e _ a)   = chr (accToID r) : showTy (accType a) ++ showFun f ++ showExp e
accToKey r@(Scanl' f e a)      = chr (accToID r) : showTy (accType a) ++ showFun f ++ showExp e
accToKey r@(Scanr' f e a)      = chr (accToID r) : showTy (accType a) ++ showFun f ++ showExp e
accToKey r@(Permute c _ p a)   = chr (accToID r) : showTy (accType a) ++ showFun c ++ showFun p
accToKey r@(Backpermute _ p a) = chr (accToID r) : showTy (accType a) ++ showFun p
accToKey x =
  INTERNAL_ERROR(error) "accToKey"
  (unlines ["incomplete patterns for key generation", render (nest 2 doc)])
  where
    acc = show x
    doc | length acc <= 250 = text acc
        | otherwise         = text (take 250 acc) <+> text "... {truncated}"

accToID :: OpenAcc aenv a -> Int
accToID (Replicate _ _ _)   = 17
accToID (Index _ _ _)       = 29
accToID (Map _ _)           = 41
accToID (ZipWith _ _ _)     = 53
accToID (Fold _ _ _)        = 67
accToID (FoldSeg _ _ _ _)   = 79
accToID (Scanl' _ _ _)      = 97
accToID (Scanr' _ _ _)      = 107
accToID (Permute _ _ _ _)   = 127
accToID (Backpermute _ _ _) = 139
accToID _                   =
  INTERNAL_ERROR(error) "accToID" "incomplete patterns for ID generation"


showTy :: TupleType a -> String
showTy UnitTuple = []
showTy (SingleTuple ty) = show ty
showTy (PairTuple a b)  = showTy a ++ showTy b

showFun :: OpenFun env aenv a -> String
showFun f = render . hcat . map pretty . fst $ runCodeGen (codeGenFun f)

showExp :: OpenExp env aenv a -> String
showExp e = render . hcat . map pretty . fst $ runCodeGen (codeGenExp e)

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
quad :: String -> Int32
quad (c1:c2:c3:c4:s)  = (( ord' c4 * bits21
                         + ord' c3 * bits14
                         + ord' c2 * bits7
                         + ord' c1)
                         `mod` bits28)
                        + (quad s `mod` bits28)
quad (c1:c2:c3:[]  )  = ord' c3 * bits14 + ord' c2 * bits7 + ord' c1
quad (c1:c2:[]     )  = ord' c2 * bits7  + ord' c1
quad (c1:[]        )  = ord' c1
quad ([]           )  = 0

ord' :: Char -> Int32
ord' = fromIntegral . ord

bits7, bits14, bits21, bits28 :: Int32
bits7  = 2^(7 ::Int32)
bits14 = 2^(14::Int32)
bits21 = 2^(21::Int32)
bits28 = 2^(28::Int32)
-}
