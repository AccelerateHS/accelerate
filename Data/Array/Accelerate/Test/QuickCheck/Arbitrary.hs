{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Test.QuickCheck.Arbitrary
-- Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Test.QuickCheck.Arbitrary
  (
    -- Instances of Arbitrary
    arbitraryIntegralExp, arbitraryIntegralVector,
    arbitraryFloatingExp, arbitraryFloatingVector
  )
  where

import Data.Array.Accelerate
import Data.Array.Accelerate.Smart

import Control.Applicative                      hiding (Const)
import Control.Monad
import Data.List
import Foreign.Storable
import Test.QuickCheck


-- Primitive Types
-- ---------------
instance Arbitrary Int8  where arbitrary  = arbitrarySizedIntegral
instance Arbitrary Int16 where arbitrary  = arbitrarySizedIntegral
instance Arbitrary Int32 where arbitrary  = arbitrarySizedIntegral
instance Arbitrary Int64 where arbitrary  = arbitrarySizedIntegral

instance Arbitrary Word   where arbitrary = arbitrarySizedIntegral
instance Arbitrary Word8  where arbitrary = arbitrarySizedIntegral
instance Arbitrary Word16 where arbitrary = arbitrarySizedIntegral
instance Arbitrary Word32 where arbitrary = arbitrarySizedIntegral
instance Arbitrary Word64 where arbitrary = arbitrarySizedIntegral


-- Arrays
-- ------
instance (Elem e, Arbitrary e) => Arbitrary (Array DIM0 e) where
  arbitrary = fromList () <$> vector 1

instance (Elem e, Arbitrary e) => Arbitrary (Array DIM1 e) where
  arbitrary = do
    n <- suchThat arbitrary (>= 0)
    fromList n <$> vector n

instance (Elem e, Arbitrary e) => Arbitrary (Array DIM2 e) where
  arbitrary = do
    (n, [a,b]) <- clamp <$> vectorOf 2 (suchThat arbitrary (> 0))
    fromList (a,b) <$> vector n

instance (Elem e, Arbitrary e) => Arbitrary (Array DIM3 e) where
  arbitrary = do
    (n, [a,b,c]) <- clamp <$> vectorOf 3 (suchThat arbitrary (> 0))
    fromList (a,b,c) <$> vector n

instance (Elem e, Arbitrary e) => Arbitrary (Array DIM4 e) where
  arbitrary = do
    (n, [a,b,c,d]) <- clamp <$> vectorOf 4 (suchThat arbitrary (> 0))
    fromList (a,b,c,d) <$> vector n

instance (Elem e, Arbitrary e) => Arbitrary (Array DIM5 e) where
  arbitrary = do
    (n, [a,b,c,d,e]) <- clamp <$> vectorOf 5 (suchThat arbitrary (> 0))
    fromList (a,b,c,d,e) <$> vector n

-- make sure not to create an index too large that we get integer overflow when
-- converting it to linear form
--
clamp :: [Int] -> (Int, [Int])
clamp = mapAccumL k 1
  where k a x = let n = Prelude.max 1 (x `mod` (maxBound `div` a))
                in  (n*a, n)


-- Expressions
-- -----------
instance Arbitrary (Acc (Vector Int))    where arbitrary = sized arbitraryIntegralVector
instance Arbitrary (Acc (Vector Int8))   where arbitrary = sized arbitraryIntegralVector
instance Arbitrary (Acc (Vector Int16))  where arbitrary = sized arbitraryIntegralVector
instance Arbitrary (Acc (Vector Int32))  where arbitrary = sized arbitraryIntegralVector
instance Arbitrary (Acc (Vector Int64))  where arbitrary = sized arbitraryIntegralVector

instance Arbitrary (Acc (Vector Word))   where arbitrary = sized arbitraryIntegralVector
instance Arbitrary (Acc (Vector Word8))  where arbitrary = sized arbitraryIntegralVector
instance Arbitrary (Acc (Vector Word16)) where arbitrary = sized arbitraryIntegralVector
instance Arbitrary (Acc (Vector Word32)) where arbitrary = sized arbitraryIntegralVector
instance Arbitrary (Acc (Vector Word64)) where arbitrary = sized arbitraryIntegralVector

instance Arbitrary (Acc (Vector Float))  where arbitrary = sized arbitraryFloatingVector
instance Arbitrary (Acc (Vector Double)) where arbitrary = sized arbitraryFloatingVector


-- ugly ugly duplication...
--

arbitraryIntegralExp :: (IsIntegral e, Elem e, Arbitrary e) => Int -> Gen (Exp e)
arbitraryIntegralExp 0 = liftM Const arbitrary
arbitraryIntegralExp n =
  let m = n `div` 4
  in  oneof [ do vec <- arbitraryIntegralVector m
                 idx <- abs <$> arbitrary
                 let idx' = shape vec ==* 0 ? (0, constant idx `mod` shape vec)
                 return (IndexScalar vec idx')
            , liftM (!constant ()) (Fold <$> associative <*> arbitraryIntegralExp m <*> arbitraryIntegralVector m)
            ]

arbitraryIntegralVector :: (IsNum e, IsIntegral e, Elem e, Arbitrary e) => Int -> Gen (Acc (Vector e))
arbitraryIntegralVector 0 = Use <$> arbitrary
arbitraryIntegralVector n =
  let m = n `div` 4
  in  oneof [ Map     <$> unaryIntegral  <*> arbitraryIntegralVector m
            , ZipWith <$> binaryIntegral <*> arbitraryIntegralVector m <*> arbitraryIntegralVector m
            , liftM FstArray (Scanl <$> associative <*> arbitraryIntegralExp m <*> arbitraryIntegralVector m)
            , liftM FstArray (Scanr <$> associative <*> arbitraryIntegralExp m <*> arbitraryIntegralVector m)
            ]


arbitraryFloatingExp :: (IsFloating e, Elem e, Arbitrary e) => Int -> Gen (Exp e)
arbitraryFloatingExp 0 = liftM Const arbitrary
arbitraryFloatingExp n =
  let m = n `div` 4
  in  oneof [ do vec <- arbitraryFloatingVector m
                 idx <- abs <$> arbitrary
                 let idx' = shape vec ==* 0 ? (0, constant idx `mod` shape vec)
                 return (IndexScalar vec idx')
            , liftM (!constant ()) (Fold <$> associative <*> arbitraryFloatingExp m <*> arbitraryFloatingVector m)
            ]

arbitraryFloatingVector :: (IsNum e, IsFloating e, Elem e, Arbitrary e) => Int -> Gen (Acc (Vector e))
arbitraryFloatingVector 0 = Use <$> arbitrary
arbitraryFloatingVector n =
  let m = n `div` 4
  in  oneof [ Map     <$> unaryFloating  <*> arbitraryFloatingVector m
            , ZipWith <$> binaryFloating <*> arbitraryFloatingVector m <*> arbitraryFloatingVector m
            , liftM FstArray (Scanl <$> associative <*> arbitraryFloatingExp m <*> arbitraryFloatingVector m)
            , liftM FstArray (Scanr <$> associative <*> arbitraryFloatingExp m <*> arbitraryFloatingVector m)
            ]


-- Functions
-- ---------

wordSize :: Int
wordSize = 8 * sizeOf (undefined :: Int)


unaryNum :: (Elem t, IsNum t, Arbitrary t) => Gen (Exp t -> Exp t)
unaryNum = oneof
  [ return mkNeg
  , return mkAbs
  , return mkSig
  ]

binaryNum :: (Elem t, IsNum t) => Gen (Exp t -> Exp t -> Exp t)
binaryNum = oneof
  [ return mkAdd
  , return mkSub
  , return mkMul
  ]

unaryIntegral :: (Elem t, IsIntegral t, Arbitrary t) => Gen (Exp t -> Exp t)
unaryIntegral = sized $ \n -> oneof
  [ return mkBNot
  , flip shift         . constant <$> choose (-wordSize,wordSize)
  , flip rotate        . constant <$> choose (-wordSize,wordSize)
  , flip mkBShiftL     . constant <$> choose (0,wordSize)
  , flip mkBShiftR     . constant <$> choose (0,wordSize)
  , flip mkBRotateL    . constant <$> choose (0,wordSize)
  , flip mkBRotateR    . constant <$> choose (0,wordSize)
  , flip setBit        . constant <$> choose (0,wordSize)
  , flip clearBit      . constant <$> choose (0,wordSize)
  , flip complementBit . constant <$> choose (0,wordSize)
  , unaryNum
  , binaryNum      <*> arbitraryIntegralExp (n `div` 2)
  , binaryIntegral <*> arbitraryIntegralExp (n `div` 2)
  ]

binaryIntegral :: (Elem t, IsIntegral t) => Gen (Exp t -> Exp t -> Exp t)
binaryIntegral = oneof
  [ return mkQuot
  , return mkRem
  , return mkIDiv
  , return mkMod
  , return mkBAnd
  , return mkBOr
  , return mkBXor
  , return mkMax
  , return mkMin
  , binaryNum
  ]

unaryFloating :: (Elem t, IsFloating t, Arbitrary t) => Gen (Exp t -> Exp t)
unaryFloating = sized $ \n -> oneof
  [ return mkSin
  , return mkCos
  , return mkTan
--  , return mkAsin     -- can't be sure inputs will be in the valid range
--  , return mkAcos
  , return mkAtan
  , return mkAsinh
--  , return mkAcosh
--  , return mkAtanh
--  , return mkExpFloating
--  , return mkSqrt
--  , return mkLog
  , unaryNum
  , binaryFloating `ap` arbitraryFloatingExp (n `div` 2)
  ]

binaryFloating :: (Elem t, IsFloating t) => Gen (Exp t -> Exp t -> Exp t)
binaryFloating = oneof
  [ return mkFPow
  , return mkLogBase
  , return mkMax
  , return mkMin
  , binaryNum
  ]

associative :: (Elem t, IsNum t) => Gen (Exp t -> Exp t -> Exp t)
associative = oneof
  [ return mkMax
  , return mkMin
  , return mkAdd
  , return mkMul
  ]

