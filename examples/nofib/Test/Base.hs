{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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

instance Similar DIM1
instance Similar DIM2
instance Similar DIM3

instance Similar Float  where (~=) = absRelTol 0.001
instance Similar Double where (~=) = absRelTol 0.001

{-# INLINE relTol #-}
relTol :: (Fractional a, Ord a) => a -> a -> a -> Bool
relTol epsilon x y = abs ((x-y) / (x+y+epsilon)) < epsilon

{-# INLINE absRelTol #-}
absRelTol :: (RealFloat a, Ord a) => a -> a -> a -> Bool
absRelTol epsilonRel u v
  |  isInfinite u
  && isInfinite v          = True
  |  isNaN u
  && isNaN v               = True
  | abs (u-v) < epsilonAbs = True
  | abs u > abs v          = abs ((u-v) / u) < epsilonRel
  | otherwise              = abs ((v-u) / v) < epsilonRel
  where
    epsilonAbs = 0.00001

instance (Eq e, Eq sh, Shape sh) => Eq (Array sh e) where
  a1 == a2      =  arrayShape a1 == arrayShape a2
                && toList a1     == toList a2

  a1 /= a2      =  arrayShape a1 /= arrayShape a2
                || toList a1     /= toList a2

instance (Similar e, Eq sh, Shape sh) => Similar (Array sh e) where
  a1 ~= a2      =  arrayShape a1 == arrayShape a2
                && toList a1     ~= toList a2


-- | Assert that the specified actual value is equal-ish to the expected value.
-- If we are in verbose mode, the output message will contain the expected and
-- actual values.
--
assertEqual
    :: (Similar a, Show a)
    => Bool     -- ^ Print the test case as well?
    -> a        -- ^ The expected value
    -> a        -- ^ The actual value
    -> Property
assertEqual v expected actual =
  printTestCase message (expected ~= actual)
  where
    message
      | P.not v         = []
      | otherwise       = unlines [ "*** Expected:", show expected
                                  , "*** Received:", show actual ]

infix 1 ~=?, ~?=

-- Short hand for a test case that asserts similarity, with the actual value on
-- the right hand side and the expected value on the left.
--
(~=?) :: (Similar a, Show a) => a -> a -> Property
(~=?) = assertEqual True

-- Short hand for a test case that asserts similarity, with the actual value on
-- the left hand side and the expected value on the right.
--
(~?=) :: (Similar a, Show a) => a -> a -> Property
(~?=) = flip (~=?)



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

