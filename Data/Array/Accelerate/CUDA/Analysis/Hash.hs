{-# LANGUAGE GADTs #-}
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
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Pretty ()
import Data.Array.Accelerate.Analysis.Type
import Data.Array.Accelerate.CUDA.State


-- |
-- Generate a unique key for each kernel computation (not extensively tested...)
--
accToKey :: OpenAcc aenv a -> Key
accToKey (Unit e)            = chr 2   : showTy (expType e) ++ show e
accToKey (Reshape _ a)       = chr 7   : showTy (accType a)
accToKey (Replicate _ e a)   = chr 17  : showTy (accType a) ++ show e
accToKey (Index _ a e)       = chr 29  : showTy (accType a) ++ show e
accToKey (Map f a)           = chr 41  : showTy (accType a) ++ show f
accToKey (ZipWith f x y)     = chr 53  : showTy (accType x) ++ showTy (accType y) ++ show f
accToKey (Fold f e a)        = chr 67  : showTy (accType a) ++ show f ++ show e
accToKey (FoldSeg f e _ a)   = chr 79  : showTy (accType a) ++ show f ++ show e
accToKey (Scanl f e a)       = chr 97  : showTy (accType a) ++ show f ++ show e
accToKey (Scanr f e a)       = chr 107 : showTy (accType a) ++ show f ++ show e
accToKey (Permute c e p a)   = chr 127 : showTy (accType a) ++ show c ++ show e ++ show p
accToKey (Backpermute _ p a) = chr 139 : showTy (accType a) ++ show p
accToKey _ =
  error "we should never get here"

showTy :: TupleType a -> String
showTy UnitTuple = []
showTy (SingleTuple ty) = show ty
showTy (PairTuple a b)  = showTy a ++ showTy b

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

