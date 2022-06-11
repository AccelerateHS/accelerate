{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
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
import Data.Array.Accelerate.Classes.RealFrac

import Data.Kind
import Prelude                                            ( ($) )


-- | Numbers which can be expressed as the quotient of two integers.
--
-- Accelerate does not have an arbitrary precision Integer type, however
-- fixed-length large integers are provide by the @accelerate-bignum@
-- package.
--
class (Num a, Ord a) => Rational a where
  type Embedding a :: Type

  -- | Convert a number to the quotient of two integers
  --
  toRational :: (FromIntegral (Embedding a) b, Integral b) => Exp a -> Exp (Ratio b)

instance Rational Int      where type Embedding Int     = Int;     toRational = integralToRational
instance Rational Int8     where type Embedding Int8    = Int8;    toRational = integralToRational
instance Rational Int16    where type Embedding Int16   = Int16;   toRational = integralToRational
instance Rational Int32    where type Embedding Int32   = Int32;   toRational = integralToRational
instance Rational Int64    where type Embedding Int64   = Int64;   toRational = integralToRational
instance Rational Int128   where type Embedding Int128  = Int128;  toRational = integralToRational
instance Rational Word     where type Embedding Word    = Word;    toRational = integralToRational
instance Rational Word8    where type Embedding Word8   = Word8;   toRational = integralToRational
instance Rational Word16   where type Embedding Word16  = Word16;  toRational = integralToRational
instance Rational Word32   where type Embedding Word32  = Word32;  toRational = integralToRational
instance Rational Word64   where type Embedding Word64  = Word64;  toRational = integralToRational
instance Rational Word128  where type Embedding Word128 = Word128; toRational = integralToRational

instance Rational Half     where type Embedding Half     = Int16;  toRational = floatingToRational
instance Rational Float    where type Embedding Float    = Int32;  toRational = floatingToRational
instance Rational Double   where type Embedding Double   = Int64;  toRational = floatingToRational
instance Rational Float128 where type Embedding Float128 = Int128; toRational = floatingToRational

integralToRational
    :: (Integral a, Integral b, FromIntegral a b)
    => Exp a
    -> Exp (Ratio b)
integralToRational x = fromIntegral x :% 1

floatingToRational
    :: (RealFloat a, Integral b, FromIntegral (Significand a) b, FromIntegral Int (Significand a), FiniteBits (Significand a))
    => Exp a
    -> Exp (Ratio b)
floatingToRational x = fromIntegral u :% fromIntegral v
  where
    T2 m e = decodeFloat x
    T2 n d = elimZeros m ne'
    ne'    = negate e'
    e'     = fromIntegral e
    u :% v = cond (e' >= 0)      ((m `shiftL` e') :% 1) $
             cond (m .&. 1 == 0) (n :% shiftL 1 d)      $
                                 (m :% shiftL 1 ne')

-- Stolen from GHC.Float.ConversionUtils
-- Double mantissa have 53 bits, which fits in an Int64
--
elimZeros
    :: forall e. (Num e, Ord e, FiniteBits e)
    => Exp e
    -> Exp e
    -> Exp (e, e)
elimZeros x y = T2 u v
  where
    T3 _ u v = while (\(T3 p _ _) -> p) elim (T3 moar x y)
    kthxbai  = constant False
    moar     = constant True

    elim :: Exp (Bool, e, e) -> Exp (Bool, e, e)
    elim (T3 _ n e) =
      let t = countTrailingZeros n
      in
      cond (e <= t) (T3 kthxbai (shiftR n e) 0)     $
      cond (t <  8) (T3 kthxbai (shiftR n t) (e-t)) $
                    (T3 moar    (shiftR n 8) (e-8))

