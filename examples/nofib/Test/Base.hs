{-# LANGUAGE TypeOperators #-}

module Test.Base where

import Prelude                                          as P
import Data.Array.Accelerate                            as A
import Data.Array.Accelerate.Array.Sugar


-- Miscellaneous
--
indexHead :: sh:.Int -> Int
indexHead (_ :. sz) = sz

indexTail :: sh:.Int -> sh
indexTail (sh :. _) = sh

isEmptyArray :: Shape sh => Array sh e -> Bool
isEmptyArray arr = arraySize (arrayShape arr) P.== 0

mkDim :: Shape sh => Int -> sh
mkDim n = listToShape (P.replicate n 0)

dim0 :: DIM0
dim0 = mkDim 0

dim1 :: DIM1
dim1 = mkDim 1

dim2 :: DIM2
dim2 = mkDim 2

dim3 :: DIM3
dim3 = mkDim 3

dim4 :: DIM4
dim4 = mkDim 4

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = cycle [[]]
splitEvery n xs =
  let (h,t) = splitAt n xs
  in  h : splitEvery n t

splitPlaces :: P.Integral i => [i] -> [a] -> [[a]]
splitPlaces []     _  = []
splitPlaces (i:is) vs =
  let (h,t) = splitAt (P.fromIntegral i) vs
  in  h : splitPlaces is t

