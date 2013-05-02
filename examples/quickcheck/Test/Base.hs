{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}

module Test.Base where

import Prelude                                          as P
import Test.QuickCheck
import Data.Array.Accelerate
import Data.Array.Accelerate.Array.Sugar                as Sugar


-- A class of things that support almost-equality, so that we can disregard
-- small amounts of floating-point round-off error.
--
class Similar a where
  {-# INLINE (~=) #-}
  (~=) :: a -> a -> Bool
  default (~=) :: Eq a => a -> a -> Bool
  (~=) = (==)

infix 4 ~=

instance Similar a => Similar [a] where
  []     ~= []          = True
  (x:xs) ~= (y:ys)      = x ~= y && xs ~= ys
  _      ~= _           = False

instance (Similar a, Similar b) => Similar (a, b) where
  (x1, y1) ~= (x2, y2)  = x1 ~= x2 && y1 ~= y2


instance Similar Int
instance Similar Int8
instance Similar Int16
instance Similar Int32
instance Similar Int64
instance Similar Word
instance Similar Word8
instance Similar Word16
instance Similar Word32
instance Similar Word64

instance Similar Float  where (~=) = absRelTol
instance Similar Double where (~=) = absRelTol

{-# INLINE relTol #-}
relTol :: (Fractional a, Ord a) => a -> a -> a -> Bool
relTol epsilon x y = abs ((x-y) / (x+y+epsilon)) < epsilon

{-# INLINE absRelTol #-}
absRelTol :: (Fractional a, Ord a) => a -> a -> Bool
absRelTol u v
  | abs (u-v) < epsilonAbs = True
  | abs u > abs v          = abs ((u-v) / u) < epsilonRel
  | otherwise              = abs ((v-u) / v) < epsilonRel
  where
    epsilonRel = 0.001
    epsilonAbs = 0.00001

instance (Eq e, Eq sh, Shape sh) => Eq (Array sh e) where
  a1 == a2      =  arrayShape a1 == arrayShape a2
                && toList a1     == toList a2

  a1 /= a2      =  arrayShape a1 /= arrayShape a2
                || toList a1     /= toList a2

instance (Similar e, Eq sh, Shape sh) => Similar (Array sh e) where
  a1 ~= a2      =  arrayShape a1 == arrayShape a2
                && toList a1     ~= toList a2


-- Print expected/received message on inequality
--
infix 4 .==.
(.==.) :: (Similar a, Show a) => a -> a -> Property
(.==.) ans ref = printTestCase message (ref ~= ans)
  where
    message = unlines ["*** Expected:", show ref
                      ,"*** Received:", show ans ]


-- Miscellaneous
--

indexHead :: Shape sh => (sh:.Int) -> Int
indexHead (_ :. sz) = sz

indexTail :: Shape sh => (sh:.Int) -> sh
indexTail (sh :. _) = sh

isEmptyArray :: Shape sh => Array sh e -> Bool
isEmptyArray arr = arraySize (arrayShape arr) == 0

mkDim :: Shape sh => Int -> sh
mkDim n = listToShape (P.replicate n 0)

dim0 :: DIM0
dim0 = mkDim 0

dim1 :: DIM1
dim1 = mkDim 1

dim2 :: DIM2
dim2 = mkDim 2

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = cycle [[]]
splitEvery n xs =
  let (h,t) = splitAt n xs
  in  h : splitEvery n t

splitPlaces :: Integral i => [i] -> [a] -> [[a]]
splitPlaces []     _  = []
splitPlaces (i:is) vs =
  let (h,t) = splitAt (P.fromIntegral i) vs
  in  h : splitPlaces is t

