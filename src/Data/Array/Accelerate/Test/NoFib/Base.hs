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

import Data.Array.Accelerate.Array.Sugar                            ( Arrays, Array, Shape, Elt, DIM0, DIM1, DIM2, DIM3, Z(..), (:.)(..), fromList, size )
import Data.Array.Accelerate.Smart                                  ( Acc )
import Data.Array.Accelerate.Trafo.Sharing                          ( Afunction, AfunctionR )
import Data.Array.Accelerate.Type

import Control.Monad

import Hedgehog
import qualified Hedgehog.Gen                                       as Gen
import qualified Hedgehog.Range                                     as Range


type Run  = forall a. Arrays a => Acc a -> a
type RunN = forall f. Afunction f => f -> AfunctionR f

dim0 :: Gen DIM0
dim0 = return Z

dim1 :: Gen DIM1
dim1 = (Z :.) <$> Gen.int (Range.linear 0 1024)

dim2 :: Gen DIM2
dim2 = do
  x <- Gen.int (Range.linear 0 128)
  y <- Gen.int (Range.linear 0 48)
  return (Z :. y :. x)

dim3 :: Gen DIM3
dim3 = do
  x <- Gen.int (Range.linear 0 64)
  y <- Gen.int (Range.linear 0 32)
  z <- Gen.int (Range.linear 0 16)
  return (Z :. z :. y :. x)

array :: (Shape sh, Elt e) => sh -> Gen e -> Gen (Array sh e)
array sh gen = fromList sh <$> Gen.list (Range.singleton (size sh)) gen

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

f16 :: Gen Half
f16 = Gen.realFloat (Range.linearFracFrom 0 (-log_flt_max) log_flt_max)

f32 :: Gen Float
f32 = Gen.float (Range.linearFracFrom 0 (-log_flt_max) log_flt_max)

f64 :: Gen Double
f64 = Gen.double (Range.linearFracFrom 0 (-log_flt_max) log_flt_max)

log_flt_max :: RealFloat a => a
log_flt_max = log flt_max

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

