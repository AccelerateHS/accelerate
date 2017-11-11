{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Base
-- Copyright   : [2009..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Test.NoFib.Base
  where

import Data.Array.Accelerate.Array.Sugar                            ( Arrays, Shape, DIM0, DIM1, DIM2, DIM3, DIM4, rank )
import Data.Array.Accelerate.Smart                                  ( Acc )
import Data.Array.Accelerate.Trafo.Sharing                          ( Afunction, AfunctionR )

import Data.Int
import Data.Word
import Control.Monad

import Hedgehog
import qualified Hedgehog.Gen                                       as Gen
import qualified Hedgehog.Range                                     as Range
import qualified Data.Array.Accelerate.Hedgehog.Gen.Shape           as Gen


type Run  = forall a. Arrays a => Acc a -> a
type RunN = forall f. Afunction f => f -> AfunctionR f

dim0 :: Gen DIM0
dim0 = shape

dim1 :: Gen DIM1
dim1 = shape

dim2 :: Gen DIM2
dim2 = shape

dim3 :: Gen DIM3
dim3 = shape

dim4 :: Gen DIM4
dim4 = shape

shape :: forall sh. (Gen.Shape sh, Shape sh) => Gen sh
shape = Gen.shape (Range.linear 0 (512 `quot` (2 ^ r)))
  where
    r = rank (undefined::sh)

int :: Gen Int
int = Gen.int Range.linearBounded

i8 :: Gen Int8
i8 = Gen.int8 Range.linearBounded

i16 :: Gen Int16
i16 = Gen.int16 Range.linearBounded

i32 :: Gen Int32
i32 = Gen.int32 Range.linearBounded

i64 :: Gen Int64
i64 = Gen.int64 Range.linearBounded

word :: Gen Word
word = Gen.word Range.linearBounded

w8 :: Gen Word8
w8 = Gen.word8 Range.linearBounded

w16 :: Gen Word16
w16 = Gen.word16 Range.linearBounded

w32 :: Gen Word32
w32 = Gen.word32 Range.linearBounded

w64 :: Gen Word64
w64 = Gen.word64 Range.linearBounded

f32 :: Gen Float
f32 = Gen.float (Range.linearFracFrom 0 flt_min flt_max)

f64 :: Gen Double
f64 = Gen.double (Range.linearFracFrom 0 flt_min flt_max)

flt_max :: RealFloat a => a
flt_max = x
  where
    n      = floatDigits x
    b      = floatRadix x
    (_, u) = floatRange x
    x      = encodeFloat (b^n - 1) (u - n)

flt_min :: RealFloat a => a
flt_min = x
  where
    n      = floatDigits x
    b      = floatRadix x
    (l, _) = floatRange x
    x      = encodeFloat (b^n - 1) (l - n - 1)

except :: Gen e -> (e -> Bool) -> Gen e
except gen f  = do
  v <- gen
  when (f v) Gen.discard
  return v

-- indexHead :: sh:.Int -> Int
-- indexHead (_ :. sz) = sz

-- indexTail :: sh:.Int -> sh
-- indexTail (sh :. _) = sh

-- isEmptyArray :: Shape sh => Array sh e -> Bool
-- isEmptyArray arr = size (shape arr) == 0

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = cycle [[]]
splitEvery n xs =
  let (h,t) = splitAt n xs
  in  h : splitEvery n t

splitPlaces :: [Int] -> [a] -> [[a]]
splitPlaces []     _  = []
splitPlaces (i:is) vs =
  let (h,t) = splitAt i vs
  in  h : splitPlaces is t

