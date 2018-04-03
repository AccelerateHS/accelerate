{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Test.Similar
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Test.Similar
  where

import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Data.Complex
import Data.Array.Accelerate.Type

import Hedgehog
import Hedgehog.Internal.Source                           ( HasCallStack, withFrozenCallStack )

import Prelude                                            hiding ( (!!) )


-- | Fails the test if the two arguments are not equal, allowing for a small
-- amount of floating point inaccuracy.
--
infix 4 ~~~
(~~~) :: (MonadTest m, Similar a, Show a, HasCallStack) => a -> a -> m ()
a ~~~ b = withFrozenCallStack $ Sim a === Sim b

data Sim a = Sim a

instance Similar a => Eq (Sim a) where
  Sim a == Sim b = a ~= b

instance Show a => Show (Sim a) where
  show (Sim a) = show a


-- | A class of things that support almost-equality, so that we can disregard
-- small amounts of floating-point round-off error.
--
class Similar a where
  {-# INLINE (~=) #-}
  (~=) :: a -> a -> Bool
  default (~=) :: Eq a => a -> a -> Bool
  (~=) = (==)

infix 4 ~=

instance Similar ()
instance Similar Z
instance Similar All
instance Similar Int
instance Similar Int8
instance Similar Int16
instance Similar Int32
instance Similar Int64
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

instance                   Similar (Any Z)
instance (Eq sh, Eq sz) => Similar (sh:.sz)
instance (Eq sh)        => Similar (Any (sh:.Int))

instance Similar Half    where (~=) = absRelTol 0.05    0.5
instance Similar Float   where (~=) = absRelTol 0.00005 0.005
instance Similar Double  where (~=) = absRelTol 0.00005 0.005
instance Similar CFloat  where (~=) = absRelTol 0.00005 0.005
instance Similar CDouble where (~=) = absRelTol 0.00005 0.005

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

instance (Similar a, Similar b, Similar c, Similar d, Similar e, Similar f, Similar g, Similar h, Similar i, Similar j)
    => Similar (a, b, c, d, e, f, g, h, i, j) where
  (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) ~= (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10) =
    x1 ~= y1 && x2 ~= y2 && x3 ~= y3 && x4 ~= y4 && x5 ~= y5 && x6 ~= y6 && x7 ~= y7 && x8 ~= y8 && x9 ~= y9 && x10 ~= y10

instance (Similar a, Similar b, Similar c, Similar d, Similar e, Similar f, Similar g, Similar h, Similar i, Similar j, Similar k)
    => Similar (a, b, c, d, e, f, g, h, i, j, k) where
  (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) ~= (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11) =
    x1 ~= y1 && x2 ~= y2 && x3 ~= y3 && x4 ~= y4 && x5 ~= y5 && x6 ~= y6 && x7 ~= y7 && x8 ~= y8 && x9 ~= y9 && x10 ~= y10 && x11 ~= y11

instance (Similar a, Similar b, Similar c, Similar d, Similar e, Similar f, Similar g, Similar h, Similar i, Similar j, Similar k, Similar l)
    => Similar (a, b, c, d, e, f, g, h, i, j, k, l) where
  (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) ~= (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12) =
    x1 ~= y1 && x2 ~= y2 && x3 ~= y3 && x4 ~= y4 && x5 ~= y5 && x6 ~= y6 && x7 ~= y7 && x8 ~= y8 && x9 ~= y9 && x10 ~= y10 && x11 ~= y11 && x12 ~= y12

instance (Similar a, Similar b, Similar c, Similar d, Similar e, Similar f, Similar g, Similar h, Similar i, Similar j, Similar k, Similar l, Similar m)
    => Similar (a, b, c, d, e, f, g, h, i, j, k, l, m) where
  (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13) ~= (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13) =
    x1 ~= y1 && x2 ~= y2 && x3 ~= y3 && x4 ~= y4 && x5 ~= y5 && x6 ~= y6 && x7 ~= y7 && x8 ~= y8 && x9 ~= y9 && x10 ~= y10 && x11 ~= y11 && x12 ~= y12 && x13 ~= y13

instance (Similar a, Similar b, Similar c, Similar d, Similar e, Similar f, Similar g, Similar h, Similar i, Similar j, Similar k, Similar l, Similar m, Similar n)
    => Similar (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
  (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14) ~= (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14) =
    x1 ~= y1 && x2 ~= y2 && x3 ~= y3 && x4 ~= y4 && x5 ~= y5 && x6 ~= y6 && x7 ~= y7 && x8 ~= y8 && x9 ~= y9 && x10 ~= y10 && x11 ~= y11 && x12 ~= y12 && x13 ~= y13 && x14 ~= y14

instance (Similar a, Similar b, Similar c, Similar d, Similar e, Similar f, Similar g, Similar h, Similar i, Similar j, Similar k, Similar l, Similar m, Similar n, Similar o)
    => Similar (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
  (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) ~= (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15) =
    x1 ~= y1 && x2 ~= y2 && x3 ~= y3 && x4 ~= y4 && x5 ~= y5 && x6 ~= y6 && x7 ~= y7 && x8 ~= y8 && x9 ~= y9 && x10 ~= y10 && x11 ~= y11 && x12 ~= y12 && x13 ~= y13 && x14 ~= y14 && x15 ~= y15

instance (Similar a, Similar b, Similar c, Similar d, Similar e, Similar f, Similar g, Similar h, Similar i, Similar j, Similar k, Similar l, Similar m, Similar n, Similar o, Similar p)
    => Similar (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) where
  (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16) ~= (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16) =
    x1 ~= y1 && x2 ~= y2 && x3 ~= y3 && x4 ~= y4 && x5 ~= y5 && x6 ~= y6 && x7 ~= y7 && x8 ~= y8 && x9 ~= y9 && x10 ~= y10 && x11 ~= y11 && x12 ~= y12 && x13 ~= y13 && x14 ~= y14 && x15 ~= y15 && x16 ~= y16

instance Similar e => Similar (Complex e) where
  (r1 :+ i1) ~= (r2 :+ i2) = r1 ~= r2 && i1 ~= i2

instance Similar a => Similar [a] where
  []     ~= []     = True
  (x:xs) ~= (y:ys) = x ~= y && xs ~= ys
  _      ~= _      = False

instance (Similar e, Eq sh, Shape sh) => Similar (Array sh e) where
  a1 ~= a2 = shape a1 == shape a2 && go 0
    where
      n     = size (shape a1)
      go !i
        | i >= n              = True
        | a1 !! i ~= a2 !! i  = go (i+1)
        | otherwise           = False

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

