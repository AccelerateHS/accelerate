{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.Rational
-- Copyright   : [2016..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Classes.Rational (

  Rational(..)

) where

import Data.Array.Accelerate.Data.Ratio
import Data.Array.Accelerate.Data.Bits

import Data.Array.Accelerate.Language
import Data.Array.Accelerate.Pattern
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.Classes.Eq
import Data.Array.Accelerate.Classes.FromIntegral
import Data.Array.Accelerate.Classes.Integral
import Data.Array.Accelerate.Classes.Num
import Data.Array.Accelerate.Classes.Ord
import Data.Array.Accelerate.Classes.RealFloat

import Prelude                                            ( ($) )


-- | Numbers which can be expressed as the quotient of two integers.
--
-- Accelerate does not have an arbitrary precision Integer type, however
-- fixed-length large integers are provide by the @accelerate-bignum@
-- package.
--
class (Num a, Ord a) => Rational a where
  -- | Convert a number to the quotient of two integers
  --
  toRational :: (FromIntegral Int64 b, Integral b) => Exp a -> Exp (Ratio b)

instance Rational Int    where toRational = integralToRational
instance Rational Int8   where toRational = integralToRational
instance Rational Int16  where toRational = integralToRational
instance Rational Int32  where toRational = integralToRational
instance Rational Int64  where toRational = integralToRational
instance Rational Word   where toRational = integralToRational
instance Rational Word8  where toRational = integralToRational
instance Rational Word16 where toRational = integralToRational
instance Rational Word32 where toRational = integralToRational
instance Rational Word64 where toRational = integralToRational

instance Rational Half   where toRational = floatingToRational
instance Rational Float  where toRational = floatingToRational
instance Rational Double where toRational = floatingToRational


integralToRational
    :: (Integral a, Integral b, FromIntegral a Int64, FromIntegral Int64 b)
    => Exp a
    -> Exp (Ratio b)
integralToRational x = fromIntegral (fromIntegral x :: Exp Int64) :% 1

floatingToRational
    :: (RealFloat a, Integral b, FromIntegral Int64 b)
    => Exp a
    -> Exp (Ratio b)
floatingToRational x = fromIntegral u :% fromIntegral v
  where
    (m, e) = decodeFloat x
    (n, d) = elimZeros m (negate e)
    u :% v = cond (e >= 0)       ((m `shiftL` e) :% 1) $
             cond (m .&. 1 == 0) (n :% shiftL 1 d)     $
                                 (m :% shiftL 1 (negate e))

-- Stolen from GHC.Float.ConversionUtils
-- Double mantissa have 53 bits, which fits in an Int64
--
elimZeros :: Exp Int64 -> Exp Int -> (Exp Int64, Exp Int)
elimZeros x y = (u, v)
  where
    T3 _ u v = while (\(T3 p _ _) -> p) elim (T3 moar x y)
    kthxbai  = constant False
    moar     = constant True

    elim :: Exp (Bool, Int64, Int) -> Exp (Bool, Int64, Int)
    elim (T3 _ n e) =
      let t = countTrailingZeros (fromIntegral n :: Exp Word8)
      in
      cond (e <= t) (T3 kthxbai (shiftR n e) 0)     $
      cond (t <  8) (T3 kthxbai (shiftR n t) (e-t)) $
                    (T3 moar    (shiftR n 8) (e-8))

