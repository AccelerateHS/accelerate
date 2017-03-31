{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module:      : Data.Array.Accelerate.Examples.Internal.Similar
-- Copyright    : [2014] Trevor L. McDonell
-- License      : BSD3
--
-- Maintainer   : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Examples.Internal.Similar (

  Similar(..)

) where

import Data.Array.Accelerate                            ( Z(..), (:.)(..), Array, Shape, arrayShape, toList )
import Data.Complex
import Data.Int
import Data.Word
import Foreign.C.Types
import Linear.V1
import Linear.V2
import Linear.V3
import Linear.V4
import Prelude                                          as P


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
  (x1, x2) ~= (y1, y2) = x1 ~= y1 && x2 ~= y2

instance (Similar a, Similar b, Similar c) => Similar (a, b, c) where
  (x1, x2, x3) ~= (y1, y2, y3) = x1 ~= y1 && x2 ~= y2 && x3 ~= y3

instance (Similar a, Similar b, Similar c, Similar d) => Similar (a, b, c, d) where
  (x1, x2, x3, x4) ~= (y1, y2, y3, y4) = x1 ~= y1 && x2 ~= y2 && x3 ~= y3 && x4 ~= y4

instance (Similar a, Similar b, Similar c, Similar d, Similar e)
    => Similar (a, b, c, d, e) where
  (x1, x2, x3, x4, x5) ~= (y1, y2, y3, y4, y5) =
    x1 ~= y1 && x2 ~= y2 && x3 ~= y3 && x4 ~= y4 && x5 ~= y5

instance (Similar a, Similar b, Similar c, Similar d, Similar e, Similar f)
    => Similar (a, b, c, d, e, f) where
  (x1, x2, x3, x4, x5, x6) ~= (y1, y2, y3, y4, y5, y6) =
    x1 ~= y1 && x2 ~= y2 && x3 ~= y3 && x4 ~= y4 && x5 ~= y5 && x6 ~= y6

instance (Similar a, Similar b, Similar c, Similar d, Similar e, Similar f, Similar g)
    => Similar (a, b, c, d, e, f, g) where
  (x1, x2, x3, x4, x5, x6, x7) ~= (y1, y2, y3, y4, y5, y6, y7) =
    x1 ~= y1 && x2 ~= y2 && x3 ~= y3 && x4 ~= y4 && x5 ~= y5 && x6 ~= y6 && x7 ~= y7

instance (Similar a, Similar b, Similar c, Similar d, Similar e, Similar f, Similar g, Similar h)
    => Similar (a, b, c, d, e, f, g, h) where
  (x1, x2, x3, x4, x5, x6, x7, x8) ~= (y1, y2, y3, y4, y5, y6, y7, y8) =
    x1 ~= y1 && x2 ~= y2 && x3 ~= y3 && x4 ~= y4 && x5 ~= y5 && x6 ~= y6 && x7 ~= y7 && x8 ~= y8

instance (Similar a, Similar b, Similar c, Similar d, Similar e, Similar f, Similar g, Similar h, Similar i)
    => Similar (a, b, c, d, e, f, g, h, i) where
  (x1, x2, x3, x4, x5, x6, x7, x8, x9) ~= (y1, y2, y3, y4, y5, y6, y7, y8, y9) =
    x1 ~= y1 && x2 ~= y2 && x3 ~= y3 && x4 ~= y4 && x5 ~= y5 && x6 ~= y6 && x7 ~= y7 && x8 ~= y8 && x9 ~= y9

instance Similar a => Similar (V1 a) where
  V1 x ~= V1 y = x ~= y

instance Similar a => Similar (V2 a) where
  V2 x1 x2 ~= V2 y1 y2 = x1 ~= y1 && x2 ~= y2

instance Similar a => Similar (V3 a) where
  V3 x1 x2 x3 ~= V3 y1 y2 y3 = x1 ~= y1 && x2 ~= y2 && x3 ~= y3

instance Similar a => Similar (V4 a) where
  V4 x1 x2 x3 x4 ~= V4 y1 y2 y3 y4 = x1 ~= y1 && x2 ~= y2 && x3 ~= y3 && x4 ~= y4

instance Similar Z
instance (Eq sh, Eq sz) => Similar (sh:.sz)

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
instance Similar Char
instance Similar Bool
instance Similar CShort
instance Similar CUShort
instance Similar CInt
instance Similar CUInt
instance Similar CLong
instance Similar CULong
instance Similar CLLong
instance Similar CULLong
instance Similar CChar
instance Similar CSChar
instance Similar CUChar

instance Similar Float   where (~=) = absRelTol 0.00005 0.005
instance Similar Double  where (~=) = absRelTol 0.00005 0.005
instance Similar CFloat  where (~=) = absRelTol 0.00005 0.005
instance Similar CDouble where (~=) = absRelTol 0.00005 0.005

instance Similar e => Similar (Complex e) where
  (r1 :+ i1) ~= (r2 :+ i2) = r1 ~= r2 && i1 ~= i2


-- {-# INLINE relTol #-}
-- relTol :: (Fractional a, Ord a) => a -> a -> a -> Bool
-- relTol epsilon x y = abs ((x-y) / (x+y+epsilon)) < epsilon

{-# INLINEABLE absRelTol #-}
absRelTol :: RealFloat a => a -> a -> a -> a -> Bool
absRelTol epsilonAbs epsilonRel u v
  |  isInfinite u
  && isInfinite v          = True
  |  isNaN u
  && isNaN v               = True
  | abs (u-v) < epsilonAbs = True
  | abs u > abs v          = abs ((u-v) / u) < epsilonRel
  | otherwise              = abs ((v-u) / v) < epsilonRel

instance (Similar e, Eq sh, Shape sh) => Similar (Array sh e) where
  a1 ~= a2      =  arrayShape a1 == arrayShape a2
                && toList a1     ~= toList a2

