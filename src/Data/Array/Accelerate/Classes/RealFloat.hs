{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.RealFloat
-- Copyright   : [2016..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Classes.RealFloat (

  RealFloat(..),
  defaultProperFraction,

) where

import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Language                               ( (^), cond, while )
import Data.Array.Accelerate.Pattern
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.Data.Bits

import Data.Array.Accelerate.Classes.Eq
import Data.Array.Accelerate.Classes.Floating
import Data.Array.Accelerate.Classes.FromIntegral
import Data.Array.Accelerate.Classes.Integral
import Data.Array.Accelerate.Classes.Num
import Data.Array.Accelerate.Classes.Ord
import Data.Array.Accelerate.Classes.RealFrac

import Data.Text.Lazy.Builder
import Formatting
import Prelude                                                      ( (.), ($), String, error, undefined, unlines, otherwise )
import Text.Printf
import qualified Prelude                                            as P


-- | Efficient, machine-independent access to the components of a floating-point
-- number
--
class (RealFrac a, Floating a) => RealFloat a where
  -- | The radix of the representation (often 2) (constant)
  floatRadix     :: Exp a -> Exp Int -- Integer
  default floatRadix :: P.RealFloat a => Exp a -> Exp Int
  floatRadix _    = P.fromInteger (P.floatRadix (undefined::a))

  -- | The number of digits of 'floatRadix' in the significand (constant)
  floatDigits    :: Exp a -> Exp Int
  default floatDigits :: P.RealFloat a => Exp a -> Exp Int
  floatDigits _   = constant (P.floatDigits (undefined::a))

  -- | The lowest and highest values the exponent may assume (constant)
  floatRange     :: Exp a -> Exp (Int, Int)
  default floatRange :: P.RealFloat a => Exp a -> Exp (Int, Int)
  floatRange _ = constant $ P.floatRange (undefined::a)

  -- | Return the significand and an appropriately scaled exponent. If
  -- @(m,n) = 'decodeFloat' x@ then @x = m*b^^n@, where @b@ is the
  -- floating-point radix ('floatRadix'). Furthermore, either @m@ and @n@ are
  -- both zero, or @b^(d-1) <= 'abs' m < b^d@, where @d = 'floatDigits' x@.
  decodeFloat    :: Exp a -> Exp (Significand a, Int)

  -- | Inverse of 'decodeFloat'
  encodeFloat    :: Exp (Significand a) -> Exp Int -> Exp a
  default encodeFloat :: (FromIntegral Int a, FromIntegral (Significand a) a) => Exp (Significand a) -> Exp Int -> Exp a
  encodeFloat x e = fromIntegral x * (fromIntegral (floatRadix (undefined :: Exp a)) ** fromIntegral e)

  -- | Corresponds to the second component of 'decodeFloat'
  exponent       :: Exp a -> Exp Int
  exponent x      = let T2 m n = decodeFloat x
                     in cond (m == 0) 0 (n + floatDigits x)

  -- | Corresponds to the first component of 'decodeFloat'
  significand    :: Exp a -> Exp a
  significand x   = let T2 m _ = decodeFloat x
                     in encodeFloat m (negate (floatDigits x))

  -- | Multiply a floating point number by an integer power of the radix
  scaleFloat     :: Exp Int -> Exp a -> Exp a
  scaleFloat k x  = cond (k == 0 || isFix) x (encodeFloat m (n + clamp b))
    where
      isFix  = x == 0 || isNaN x || isInfinite x
      T2 m n = decodeFloat x
      T2 l h = floatRange x
      d      = floatDigits x
      b      = h - l + 4*d
      -- n+k may overflow, which would lead to incorrect results, hence we clamp
      -- the scaling parameter. If (n+k) would be larger than h, (n + clamp b k)
      -- must be too, similar for smaller than (l-d).
      clamp bd  = max (-bd) (min bd k)

  -- | 'True' if the argument is an IEEE \"not-a-number\" (NaN) value
  isNaN          :: Exp a -> Exp Bool

  -- | 'True' if the argument is an IEEE infinity or negative-infinity
  isInfinite     :: Exp a -> Exp Bool

  -- | 'True' if the argument is too small to be represented in normalized
  -- format
  isDenormalized :: Exp a -> Exp Bool

  -- | 'True' if the argument is an IEEE negative zero
  isNegativeZero :: Exp a -> Exp Bool

  -- | 'True' if the argument is an IEEE floating point number
  isIEEE         :: Exp a -> Exp Bool
  default isIEEE :: P.RealFloat a => Exp a -> Exp Bool
  isIEEE _        = constant (P.isIEEE (undefined::a))

  -- | A version of arctangent taking two real floating-point arguments.
  -- For real floating @x@ and @y@, @'atan2' y x@ computes the angle (from the
  -- positive x-axis) of the vector from the origin to the point @(x,y)@.
  -- @'atan2' y x@ returns a value in the range [@-pi@, @pi@].
  atan2          :: Exp a -> Exp a -> Exp a


instance RealFrac Half where
  type Significand Half = Int16
  properFraction = defaultProperFraction

instance RealFrac Float where
  type Significand Float = Int32
  properFraction = defaultProperFraction

instance RealFrac Double where
  type Significand Double = Int64
  properFraction = defaultProperFraction

instance RealFrac Float128 where
  type Significand Float128 = Int128
  properFraction = defaultProperFraction

instance RealFloat Half where
  atan2           = mkAtan2
  isNaN           = mkIsNaN
  isInfinite      = mkIsInfinite
  isDenormalized  = ieee754 "isDenormalized" (ieee754_f16_is_denormalized . mkBitcast)
  isNegativeZero  = ieee754 "isNegativeZero" (ieee754_f16_is_negative_zero . mkBitcast)
  decodeFloat     = ieee754 "decodeFloat"    (ieee754_f16_decode . mkBitcast)

instance RealFloat Float where
  atan2           = mkAtan2
  isNaN           = mkIsNaN
  isInfinite      = mkIsInfinite
  isDenormalized  = ieee754 "isDenormalized" (ieee754_f32_is_denormalized . mkBitcast)
  isNegativeZero  = ieee754 "isNegativeZero" (ieee754_f32_is_negative_zero . mkBitcast)
  decodeFloat     = ieee754 "decodeFloat"    (ieee754_f32_decode . mkBitcast)

instance RealFloat Double where
  atan2           = mkAtan2
  isNaN           = mkIsNaN
  isInfinite      = mkIsInfinite
  isDenormalized  = ieee754 "isDenormalized" (ieee754_f64_is_denormalized . mkBitcast)
  isNegativeZero  = ieee754 "isNegativeZero" (ieee754_f64_is_negative_zero . mkBitcast)
  decodeFloat     = ieee754 "decodeFloat"    (ieee754_f64_decode . mkBitcast)

instance RealFloat Float128 where
  atan2           = mkAtan2
  isNaN           = mkIsNaN
  isInfinite      = mkIsInfinite
  isDenormalized  = ieee754 "isDenormalized" (ieee754_f128_is_denormalized . mkBitcast)
  isNegativeZero  = ieee754 "isNegativeZero" (ieee754_f128_is_negative_zero . mkBitcast)
  decodeFloat     = ieee754 "decodeFloat"    (ieee754_f128_decode . mkBitcast)


-- To satisfy superclass constraints
--
instance RealFloat a => P.RealFloat (Exp a) where
  floatRadix     = preludeError "floatRadix"
  floatDigits    = preludeError "floatDigits"
  floatRange     = preludeError "floatRange"
  decodeFloat    = preludeError "decodeFloat"
  encodeFloat    = preludeError "encodeFloat"
  isNaN          = preludeError "isNaN"
  isInfinite     = preludeError "isInfinite"
  isDenormalized = preludeError "isDenormalized"
  isNegativeZero = preludeError "isNegativeZero"
  isIEEE         = preludeError "isIEEE"

preludeError :: String -> a
preludeError x
  = error
  $ unlines [ printf "Prelude.%s applied to EDSL types: use Data.Array.Accelerate.%s instead" x x
            , ""
            , "These Prelude.RealFloat instances are present only to fulfil superclass"
            , "constraints for subsequent classes in the standard Haskell numeric hierarchy."
            ]

ieee754 :: forall a b. HasCallStack => P.RealFloat a => Builder -> (Exp a -> b) -> Exp a -> b
ieee754 name f x
  | P.isIEEE (undefined::a) = f x
  | otherwise               = internalError (builder % ": Not implemented for non-IEEE floating point") name


-- Must test for ±0.0 to avoid returning -0.0 in the second component of the
-- pair. Unfortunately the branching costs a lot of performance.
--
-- Orphaned from RealFrac module
--
-- defaultProperFraction
--     :: (ToFloating b a, RealFrac a, IsIntegral b, Num b, Floating a)
--     => Exp a
--     -> (Exp b, Exp a)
-- defaultProperFraction x =
--   unlift $ Exp
--          $ Cond (x == 0) (tup2 (0, 0))
--                          (tup2 (n, f))
--   where
--     n = truncate x
--     f = x - toFloating n

defaultProperFraction
    :: (RealFloat a, FromIntegral (Significand a) b, Integral b)
    => Exp a
    -> Exp (b, a)
defaultProperFraction x =
  cond (n >= 0)
    (T2 (fromIntegral m * (2 ^ n)) 0.0)
    (T2 (fromIntegral q) (encodeFloat r n))
  where
    T2 m n = decodeFloat x
    (q, r) = quotRem m (2 ^ (negate n))


-- From: ghc/libraries/base/cbits/primFloat.c
-- ------------------------------------------

-- An IEEE754 number is denormalised iff:
--   * exponent is zero
--   * mantissa is non-zero.
--   * (don't care about setting of sign bit.)
--
ieee754_f128_is_denormalized :: Exp Word128 -> Exp Bool
ieee754_f128_is_denormalized x =
  ieee754_f128_mantissa x == 0 &&
  ieee754_f128_exponent x /= 0

ieee754_f64_is_denormalized :: Exp Word64 -> Exp Bool
ieee754_f64_is_denormalized x =
  ieee754_f64_mantissa x == 0 &&
  ieee754_f64_exponent x /= 0

ieee754_f32_is_denormalized :: Exp Word32 -> Exp Bool
ieee754_f32_is_denormalized x =
  ieee754_f32_mantissa x == 0 &&
  ieee754_f32_exponent x /= 0

ieee754_f16_is_denormalized :: Exp Word16 -> Exp Bool
ieee754_f16_is_denormalized x =
  ieee754_f16_mantissa x == 0 &&
  ieee754_f16_exponent x /= 0

-- Negative zero if only the sign bit is set
--
ieee754_f128_is_negative_zero :: Exp Word128 -> Exp Bool
ieee754_f128_is_negative_zero x =
  ieee754_f128_negative x &&
  ieee754_f128_exponent x == 0 &&
  ieee754_f128_mantissa x == 0

ieee754_f64_is_negative_zero :: Exp Word64 -> Exp Bool
ieee754_f64_is_negative_zero x =
  ieee754_f64_negative x &&
  ieee754_f64_exponent x == 0 &&
  ieee754_f64_mantissa x == 0

ieee754_f32_is_negative_zero :: Exp Word32 -> Exp Bool
ieee754_f32_is_negative_zero x =
  ieee754_f32_negative x &&
  ieee754_f32_exponent x == 0 &&
  ieee754_f32_mantissa x == 0

ieee754_f16_is_negative_zero :: Exp Word16 -> Exp Bool
ieee754_f16_is_negative_zero x =
  ieee754_f16_negative x &&
  ieee754_f16_exponent x == 0 &&
  ieee754_f16_mantissa x == 0


-- Assume the host processor stores integers and floating point numbers in the
-- same endianness (true for modern processors).
--
-- To recap, here's the representation of a quadruple precision
-- IEEE floating point number:
--
-- sign         127          sign bit (0==positive, 1==negative)
-- exponent     126-112      exponent (biased by 16383)
-- fraction     111-0        fraction (bits to right of binary part)
--
ieee754_f128_mantissa :: Exp Word128 -> Exp Word128
ieee754_f128_mantissa x = x .&. 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFF

ieee754_f128_exponent :: Exp Word128 -> Exp Word16
ieee754_f128_exponent x = fromIntegral (x `unsafeShiftR` 112) .&. 0x7FFF

ieee754_f128_negative :: Exp Word128 -> Exp Bool
ieee754_f128_negative x = testBit x 127

-- Representation of a double precision IEEE floating point number:
--
-- sign         63           sign bit (0==positive, 1==negative)
-- exponent     62-52        exponent (biased by 1023)
-- fraction     51-0         fraction (bits to right of binary point)
--
ieee754_f64_mantissa :: Exp Word64 -> Exp Word64
ieee754_f64_mantissa x = x .&. 0xFFFFFFFFFFFFF

ieee754_f64_exponent :: Exp Word64 -> Exp Word16
ieee754_f64_exponent x = fromIntegral (x `unsafeShiftR` 52) .&. 0x7FF

ieee754_f64_negative :: Exp Word64 -> Exp Bool
ieee754_f64_negative x = testBit x 63

-- Representation of a single precision IEEE floating point number:
--
-- sign         31           sign bit (0==positive, 1==negative)
-- exponent     30-23        exponent (biased by 127)
-- fraction     22-0         fraction (bits to right of binary point)
--
ieee754_f32_mantissa :: Exp Word32 -> Exp Word32
ieee754_f32_mantissa x = x .&. 0x7FFFFF

ieee754_f32_exponent :: Exp Word32 -> Exp Word8
ieee754_f32_exponent x = fromIntegral (x `unsafeShiftR` 23)

ieee754_f32_negative :: Exp Word32 -> Exp Bool
ieee754_f32_negative x = testBit x 31

-- Representation of a half precision IEEE floating point number:
--
-- sign         15           sign bit (0==positive, 1==negative)
-- exponent     14-10        exponent (biased by 15)
-- fraction     9-0          fraction (bits to right of binary point)
--
ieee754_f16_mantissa :: Exp Word16 -> Exp Word16
ieee754_f16_mantissa x = x .&. 0x3FF

ieee754_f16_exponent :: Exp Word16 -> Exp Word8
ieee754_f16_exponent x = fromIntegral (x `unsafeShiftR` 10) .&. 0x1F

ieee754_f16_negative :: Exp Word16 -> Exp Bool
ieee754_f16_negative x = testBit x 15


-- reverse engineered following the below

ieee754_f16_decode :: Exp Word16 -> Exp (Int16, Int)
ieee754_f16_decode i =
  let
      _HHIGHBIT                       = 0x0400
      _HMSBIT                         = 0x8000
      _HMINEXP                        = ((_HALF_MIN_EXP) - (_HALF_MANT_DIG) - 1)
      _HALF_MANT_DIG                  = floatDigits (undefined::Exp Half)
      T2 _HALF_MIN_EXP _HALF_MAX_EXP  = floatRange  (undefined::Exp Half)

      high1 = fromIntegral i
      high2 = high1 .&. (_HHIGHBIT - 1)

      exp1  = ((fromIntegral high1 `unsafeShiftR` 10) .&. 0x1F) + _HMINEXP
      exp2  = exp1 + 1

      T2 high3 exp3
            = cond (exp1 /= _HMINEXP)
                   -- don't add hidden bit to denorms
                   (T2 (high2 .|. _HHIGHBIT) exp1)
                   -- a denorm, normalise the mantissa
                   (while (\(T2 h _) -> (h .&. _HHIGHBIT) /= 0 )
                          (\(T2 h e) -> T2 (h `unsafeShiftL` 1) (e-1))
                          (T2 high2 exp2))

      high4 = cond (fromIntegral i < (0 :: Exp Int16)) (-high3) high3
  in
  cond (high1 .&. complement _HMSBIT == 0)
       (T2 0 0)
       (T2 high4 exp3)


-- From: ghc/rts/StgPrimFloat.c
-- ----------------------------

ieee754_f32_decode :: Exp Word32 -> Exp (Int32, Int)
ieee754_f32_decode i =
  let
      _FHIGHBIT                     = 0x00800000
      _FMSBIT                       = 0x80000000
      _FMINEXP                      = ((_FLT_MIN_EXP) - (_FLT_MANT_DIG) - 1)
      _FLT_MANT_DIG                 = floatDigits (undefined::Exp Float)
      T2 _FLT_MIN_EXP _FLT_MAX_EXP  = floatRange  (undefined::Exp Float)

      high1 = fromIntegral i
      high2 = high1 .&. (_FHIGHBIT - 1)

      exp1  = ((fromIntegral high1 `unsafeShiftR` 23) .&. 0xFF) + _FMINEXP
      exp2  = exp1 + 1

      T2 high3 exp3
            = cond (exp1 /= _FMINEXP)
                   -- don't add hidden bit to denorms
                   (T2 (high2 .|. _FHIGHBIT) exp1)
                   -- a denorm, normalise the mantissa
                   (while (\(T2 h _) -> (h .&. _FHIGHBIT) /= 0 )
                          (\(T2 h e) -> T2 (h `unsafeShiftL` 1) (e-1))
                          (T2 high2 exp2))

      high4 = cond (fromIntegral i < (0 :: Exp Int32)) (-high3) high3
  in
  cond (high1 .&. complement _FMSBIT == 0)
       (T2 0 0)
       (T2 high4 exp3)


ieee754_f64_decode :: Exp Word64 -> Exp (Int64, Int)
ieee754_f64_decode i =
  let T4 s h l e = ieee754_f64_decode2 i
   in T2 (fromIntegral s * (fromIntegral h `unsafeShiftL` 32 .|. fromIntegral l)) e

ieee754_f64_decode2 :: Exp Word64 -> Exp (Int, Word32, Word32, Int)
ieee754_f64_decode2 i =
  let
      _DHIGHBIT                     = 0x00100000
      _DMSBIT                       = 0x80000000
      _DMINEXP                      = ((_DBL_MIN_EXP) - (_DBL_MANT_DIG) - 1)
      _DBL_MANT_DIG                 = floatDigits (undefined::Exp Double)
      T2 _DBL_MIN_EXP _DBL_MAX_EXP  = floatRange  (undefined::Exp Double)

      low   = fromIntegral i
      high  = fromIntegral (i `unsafeShiftR` 32)

      iexp  = (fromIntegral ((high `unsafeShiftR` 20) .&. 0x7FF) + _DMINEXP)
      sign  = cond (fromIntegral i < (0 :: Exp Int64)) (-1) 1

      high2 = high .&. (_DHIGHBIT - 1)
      iexp2 = iexp + 1

      T3 hi lo ie
            = cond (iexp2 /= _DMINEXP)
                   -- don't add hidden bit to denorms
                   (T3 (high2 .|. _DHIGHBIT) low iexp)
                   -- a denorm, nermalise the mantissa
                   (while (\(T3 h _ _) -> (h .&. _DHIGHBIT) /= 0)
                          (\(T3 h l e) ->
                            let h1 = h `unsafeShiftL` 1
                                h2 = cond ((l .&. _DMSBIT) /= 0) (h1+1) h1
                            in  T3 h2 (l `unsafeShiftL` 1) (e-1))
                          (T3 high2 low iexp2))

  in
  cond (low == 0 && (high .&. (complement _DMSBIT)) == 0)
       (T4 1 0 0 0)
       (T4 sign hi lo ie)

ieee754_f128_decode :: Exp Word128 -> Exp (Int128, Int)
ieee754_f128_decode = undefined

