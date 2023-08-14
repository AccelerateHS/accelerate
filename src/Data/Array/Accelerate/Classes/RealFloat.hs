{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
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

import Data.Array.Accelerate.AST                                    ( BitOrMask, PrimMask )
import Data.Array.Accelerate.Language                               ( (^), cond, while )
import Data.Array.Accelerate.Pattern.Tuple
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Elt                              ( Elt(..) )
import Data.Array.Accelerate.Sugar.Vec
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.Data.Bits

import Data.Array.Accelerate.Classes.Eq
import Data.Array.Accelerate.Classes.Floating
import Data.Array.Accelerate.Classes.FromIntegral
import Data.Array.Accelerate.Classes.Integral
import Data.Array.Accelerate.Classes.Num
import Data.Array.Accelerate.Classes.Ord
import Data.Array.Accelerate.Classes.RealFrac
import Data.Array.Accelerate.Classes.VEq
import Data.Array.Accelerate.Classes.VOrd

import Data.Coerce
import Data.Kind
import Data.Type.Equality
import Text.Printf
import Prelude                                                      ( (.), ($), String, error, undefined, unlines )
import qualified Prelude                                            as P


-- | Efficient, machine-independent access to the components of a floating-point
-- number
--
class (RealFrac a, Floating a, Integral (Exponent a)) => RealFloat a where
  type Exponent  a :: Type

  -- | The radix of the representation (often 2) (constant)
  floatRadix :: Exp a -> Exp Int

  -- | The number of digits of 'floatRadix' in the significand (constant)
  floatDigits :: Exp a -> Exp Int

  -- | The lowest and highest values the exponent may assume (constant)
  floatRange :: Exp a -> Exp (Int, Int)

  -- | Return the significand and an appropriately scaled exponent. If
  -- @(m,n) = 'decodeFloat' x@ then @x = m*b^^n@, where @b@ is the
  -- floating-point radix ('floatRadix'). Furthermore, either @m@ and @n@ are
  -- both zero, or @b^(d-1) <= 'abs' m < b^d@, where @d = 'floatDigits' x@.
  decodeFloat :: Exp a -> Exp (Significand a, Exponent a)

  -- | Inverse of 'decodeFloat'
  encodeFloat :: Exp (Significand a) -> Exp (Exponent a) -> Exp a

  -- | Corresponds to the second component of 'decodeFloat'
  exponent :: Exp a -> Exp (Exponent a)

  -- | The first component of 'decodeFloat', scaled to lie in the open interval (-1,1).
  significand :: Exp a -> Exp a

  -- | Multiply a floating point number by an integer power of the radix
  scaleFloat :: Exp Int -> Exp a -> Exp a

  -- | 'True' if the argument is an IEEE \"not-a-number\" (NaN) value
  isNaN :: (BitOrMask (EltR a) ~ EltR b) => Exp a -> Exp b

  -- | 'True' if the argument is an IEEE infinity or negative-infinity
  isInfinite :: (BitOrMask (EltR a) ~ EltR b) => Exp a -> Exp b

  -- | 'True' if the argument is too small to be represented in normalized format
  isDenormalized :: (BitOrMask (EltR a) ~ EltR b) => Exp a -> Exp b

  -- | 'True' if the argument is an IEEE negative zero
  isNegativeZero :: (BitOrMask (EltR a) ~ EltR b) => Exp a -> Exp b

  -- | 'True' if the argument is an IEEE floating point number
  isIEEE :: Exp a -> Exp Bool

  -- | A version of arctangent taking two real floating-point arguments.
  -- For real floating @x@ and @y@, @'atan2' y x@ computes the angle (from the
  -- positive x-axis) of the vector from the origin to the point @(x,y)@.
  -- @'atan2' y x@ returns a value in the range [@-pi@, @pi@].
  atan2 :: Exp a -> Exp a -> Exp a


instance RealFrac Float16 where
  type Significand Float16 = Int16
  properFraction = defaultProperFraction

instance RealFrac Float32 where
  type Significand Float32 = Int32
  properFraction = defaultProperFraction

instance RealFrac Float64 where
  type Significand Float64 = Int64
  properFraction = defaultProperFraction

instance RealFrac Float128 where
  type Significand Float128 = Int128
  properFraction = defaultProperFraction

instance KnownNat n => RealFrac (Vec n Float16) where
  type Significand (Vec n Float16) = Vec n Int16
  properFraction = defaultProperFraction'

instance KnownNat n => RealFrac (Vec n Float32) where
  type Significand (Vec n Float32) = Vec n Int32
  properFraction = defaultProperFraction'

instance KnownNat n => RealFrac (Vec n Float64) where
  type Significand (Vec n Float64) = Vec n Int64
  properFraction = defaultProperFraction'

instance KnownNat n => RealFrac (Vec n Float128) where
  type Significand (Vec n Float128) = Vec n Int128
  properFraction = defaultProperFraction'

instance RealFloat Float16 where
  type Exponent Float16 = Int
  floatRadix      = defaultFloatRadix
  floatDigits     = defaultFloatDigits
  floatRange      = defaultFloatRange
  encodeFloat s   = mkCoerce . defaultEncodeFloat @1 @Float16 (mkBitcast s) . mkBitcast
  exponent        = mkCoerce . defaultExponent . mkBitcast @(Vec 1 Float16)
  significand     = mkCoerce . defaultSignificand . mkBitcast @(Vec 1 Float16)
  scaleFloat k    = mkCoerce . defaultScaleFloat k . mkBitcast @(Vec 1 Float16)
  atan2           = mkAtan2
  isNaN           = mkIsNaN
  isInfinite      = mkIsInfinite
  isIEEE          = defaultIsIEEE
  isDenormalized  = mkCoerce . ieee754_f16_is_denormalized . mkBitcast @(Vec 1 Word16)
  isNegativeZero  = mkCoerce . ieee754_f16_is_negative_zero . mkBitcast @(Vec 1 Word16)
  decodeFloat     = mkCoerce . ieee754_f16_decode . mkBitcast @(Vec 1 Word16)

instance RealFloat Float32 where
  type Exponent Float32 = Int
  floatRadix      = defaultFloatRadix
  floatDigits     = defaultFloatDigits
  floatRange      = defaultFloatRange
  encodeFloat s   = mkCoerce . defaultEncodeFloat @1 @Float32 (mkBitcast s) . mkBitcast
  exponent        = mkCoerce . defaultExponent . mkBitcast @(Vec 1 Float32)
  significand     = mkCoerce . defaultSignificand . mkBitcast @(Vec 1 Float32)
  scaleFloat k    = mkCoerce . defaultScaleFloat k . mkBitcast @(Vec 1 Float32)
  atan2           = mkAtan2
  isNaN           = mkIsNaN
  isInfinite      = mkIsInfinite
  isIEEE          = defaultIsIEEE
  isDenormalized  = mkCoerce . ieee754_f32_is_denormalized . mkBitcast @(Vec 1 Word32)
  isNegativeZero  = mkCoerce . ieee754_f32_is_negative_zero . mkBitcast @(Vec 1 Word32)
  decodeFloat     = mkCoerce . ieee754_f32_decode . mkBitcast @(Vec 1 Word32)

instance RealFloat Float64 where
  type Exponent Float64 = Int
  floatRadix      = defaultFloatRadix
  floatDigits     = defaultFloatDigits
  floatRange      = defaultFloatRange
  encodeFloat s   = mkCoerce . defaultEncodeFloat @1 @Float64 (mkBitcast s) . mkBitcast
  exponent        = mkCoerce . defaultExponent . mkBitcast @(Vec 1 Float64)
  significand     = mkCoerce . defaultSignificand . mkBitcast @(Vec 1 Float64)
  scaleFloat k    = mkCoerce . defaultScaleFloat k . mkBitcast @(Vec 1 Float64)
  atan2           = mkAtan2
  isNaN           = mkIsNaN
  isInfinite      = mkIsInfinite
  isIEEE          = defaultIsIEEE
  isDenormalized  = mkCoerce . ieee754_f64_is_denormalized . mkBitcast @(Vec 1 Word64)
  isNegativeZero  = mkCoerce . ieee754_f64_is_negative_zero . mkBitcast @(Vec 1 Word64)
  decodeFloat     = mkCoerce . ieee754_f64_decode . mkBitcast @(Vec 1 Word64)

instance RealFloat Float128 where
  type Exponent Float128 = Int
  floatRadix      = defaultFloatRadix
  floatDigits     = defaultFloatDigits
  floatRange      = defaultFloatRange
  encodeFloat s   = mkCoerce . defaultEncodeFloat @1 @Float128 (mkBitcast s) . mkBitcast
  exponent        = mkCoerce . defaultExponent . mkBitcast @(Vec 1 Float128)
  significand     = mkCoerce . defaultSignificand . mkBitcast @(Vec 1 Float128)
  scaleFloat k    = mkCoerce . defaultScaleFloat k . mkBitcast @(Vec 1 Float128)
  atan2           = mkAtan2
  isNaN           = mkIsNaN
  isInfinite      = mkIsInfinite
  isIEEE          = defaultIsIEEE
  isDenormalized  = mkCoerce . ieee754_f128_is_denormalized . mkBitcast @(Vec 1 Word128)
  isNegativeZero  = mkCoerce . ieee754_f128_is_negative_zero . mkBitcast @(Vec 1 Word128)
  decodeFloat     = mkCoerce . ieee754_f128_decode . mkBitcast @(Vec 1 Word128)

instance KnownNat n => RealFloat (Vec n Float16) where
  type Exponent (Vec n Float16) = Vec n Int
  floatRadix _    = defaultFloatRadix (undefined :: Exp Float16)
  floatDigits _   = defaultFloatDigits (undefined :: Exp Float16)
  floatRange _    = defaultFloatRange (undefined :: Exp Float16)
  decodeFloat     = ieee754_f16_decode . mkBitcast'
  encodeFloat     = defaultEncodeFloat
  exponent        = defaultExponent
  significand     = defaultSignificand
  scaleFloat      = defaultScaleFloat
  isNaN           = mkIsNaN
  isInfinite      = mkIsInfinite
  isDenormalized  = coerce . ieee754_f16_is_denormalized . mkBitcast'
  isNegativeZero  = coerce . ieee754_f16_is_negative_zero . mkBitcast'
  isIEEE _        = defaultIsIEEE (undefined :: Exp Float16)
  atan2           = mkAtan2


instance KnownNat n => RealFloat (Vec n Float32) where
  type Exponent (Vec n Float32) = Vec n Int
  floatRadix _    = defaultFloatRadix (undefined :: Exp Float32)
  floatDigits _   = defaultFloatDigits (undefined :: Exp Float32)
  floatRange _    = defaultFloatRange (undefined :: Exp Float32)
  decodeFloat     = ieee754_f32_decode . mkBitcast'
  encodeFloat     = defaultEncodeFloat
  exponent        = defaultExponent
  significand     = defaultSignificand
  scaleFloat      = defaultScaleFloat
  isNaN           = mkIsNaN
  isInfinite      = mkIsInfinite
  isDenormalized  = coerce . ieee754_f32_is_denormalized . mkBitcast'
  isNegativeZero  = coerce . ieee754_f32_is_negative_zero . mkBitcast'
  isIEEE _        = defaultIsIEEE (undefined :: Exp Float32)
  atan2           = mkAtan2

instance KnownNat n => RealFloat (Vec n Float64) where
  type Exponent (Vec n Float64) = Vec n Int
  floatRadix _    = defaultFloatRadix (undefined :: Exp Float64)
  floatDigits _   = defaultFloatDigits (undefined :: Exp Float64)
  floatRange _    = defaultFloatRange (undefined :: Exp Float64)
  decodeFloat     = ieee754_f64_decode . mkBitcast'
  encodeFloat     = defaultEncodeFloat
  exponent        = defaultExponent
  significand     = defaultSignificand
  scaleFloat      = defaultScaleFloat
  isNaN           = mkIsNaN
  isInfinite      = mkIsInfinite
  isDenormalized  = coerce . ieee754_f64_is_denormalized . mkBitcast'
  isNegativeZero  = coerce . ieee754_f64_is_negative_zero . mkBitcast'
  isIEEE _        = defaultIsIEEE (undefined :: Exp Float64)
  atan2           = mkAtan2


instance KnownNat n => RealFloat (Vec n Float128) where
  type Exponent (Vec n Float128) = Vec n Int
  floatRadix _    = defaultFloatRadix (undefined :: Exp Float128)
  floatDigits _   = defaultFloatDigits (undefined :: Exp Float128)
  floatRange _    = defaultFloatRange (undefined :: Exp Float128)
  decodeFloat     = ieee754_f128_decode . mkBitcast'
  encodeFloat     = defaultEncodeFloat
  exponent        = defaultExponent
  significand     = defaultSignificand
  scaleFloat      = defaultScaleFloat
  isNaN           = mkIsNaN
  isInfinite      = mkIsInfinite
  isDenormalized  = coerce . ieee754_f128_is_denormalized . mkBitcast'
  isNegativeZero  = coerce . ieee754_f128_is_negative_zero . mkBitcast'
  isIEEE _        = defaultIsIEEE (undefined :: Exp Float128)
  atan2           = mkAtan2


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


-- GHC's type level natural normalisation isn't strong enough to deduce (n * 32) == (n * 32)
mkBitcast'
    :: forall b a n. (IsScalar (VecR n a), IsScalar (VecR n b), BitSizeEq (EltR a) (EltR b))
    => Exp (Vec n a)
    -> Exp (Vec n b)
mkBitcast' (Exp a) = mkExp $ Bitcast (scalarType @(VecR n a)) (scalarType @(VecR n b)) a

defaultFloatRadix :: forall a. P.RealFloat a => Exp a -> Exp Int
defaultFloatRadix _ = P.fromInteger (P.floatRadix (undefined::a))

defaultFloatDigits :: forall a. P.RealFloat a => Exp a -> Exp Int
defaultFloatDigits _ = constant (P.floatDigits (undefined::a))

defaultFloatRange :: forall a. P.RealFloat a => Exp a -> Exp (Int, Int)
defaultFloatRange _ = constant (P.floatRange (undefined::a))

defaultIsIEEE :: forall a. P.RealFloat a => Exp a -> Exp Bool
defaultIsIEEE _ = constant (P.isIEEE (undefined::a))

defaultEncodeFloat
    :: forall n a. (SIMD n a, RealFloat a, RealFloat (Vec n a), FromIntegral Int a, FromIntegral (Significand (Vec n a)) (Vec n a), FromIntegral (Exponent (Vec n a)) (Vec n a))
    => Exp (Significand (Vec n a))
    -> Exp (Exponent (Vec n a))
    -> Exp (Vec n a)
defaultEncodeFloat x e =
  let d = splat (fromIntegral (floatRadix (undefined :: Exp a)))
   in fromIntegral x * (d ** fromIntegral e)

defaultExponent
    :: (RealFloat (Vec n a), Significand (Vec n a) ~ Vec n s, Exponent (Vec n a) ~ Vec n Int, VEq n a, VEq n s)
    => Exp (Vec n a)
    -> Exp (Vec n Int)
defaultExponent x =
  let T2 m n = decodeFloat x
      d      = splat (floatDigits x)
   in
   select (m ==* 0) 0 (n + d)

defaultSignificand
    :: (RealFloat (Vec n a), Exponent (Vec n a) ~ Vec n Int, KnownNat n)
    => Exp (Vec n a)
    -> Exp (Vec n a)
defaultSignificand x =
  let T2 m _ = decodeFloat x
      d      = splat (floatDigits x)
   in encodeFloat m (negate d)

defaultScaleFloat
    :: (RealFloat (Vec n a), Exponent (Vec n a) ~ Vec n Int, BitOrMask (EltR (Vec n a)) ~ PrimMask n, VEq n a)
    => Exp Int
    -> Exp (Vec n a)
    -> Exp (Vec n a)
defaultScaleFloat k x =
  select (k' ==* 0 ||* isFix) x (encodeFloat m (n + clamp b))
  where
    k'     = splat k
    isFix  = x ==* 0 ||* isNaN x ||* isInfinite x
    T2 m n = decodeFloat x
    T2 l h = floatRange x
    d      = floatDigits x
    b      = splat (h - l + 4*d)
    -- n+k may overflow, which would lead to incorrect results, hence we clamp
    -- the scaling parameter. If (n+k) would be larger than h, (n + clamp b k)
    -- must be too, similar for smaller than (l-d).
    clamp bd = max (-bd) (min bd k')

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

-- defaultProperFraction'
--     :: (SIMD n a, SIMD n b, RealFloat (Vec n a), Exponent (Vec n a) ~ Vec n Int, FromIntegral (Significand (Vec n a)) (Vec n b), Integral (Vec n b))
--     => Exp (Vec n a)
--     -> Exp (Vec n b, Vec n a)
-- defaultProperFraction' x =
--   T2 (select p (fromIntegral m * (2 ^ n)) (fromIntegral q))
--      (select p 0.0                        (encodeFloat r n))
--   where
--     T2 m n = decodeFloat x
--     (q, r) = quotRem m (2 ^ (negate n))
--     p      = n >=* 0

-- This is a bit weird because we really want to apply the function late-wise,
-- but there isn't really a way we can do that. Boo. ---TLM 2022-09-20
--
defaultProperFraction'
    :: (SIMD n a, RealFloat (Vec n a), Exponent (Vec n a) ~ Vec n Int, FromIntegral (Significand (Vec n a)) b, Integral b)
    => Exp (Vec n a)
    -> Exp (b, Vec n a)
defaultProperFraction' x =
  T2 (cond   (n >=  0) (fromIntegral m * (2 ^ n)) (fromIntegral q))
     (select (n >=* 0) 0.0                        (encodeFloat r n))
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
ieee754_f128_is_denormalized :: KnownNat n => Exp (Vec n Word128) -> Exp (Vec n Bool)
ieee754_f128_is_denormalized x =
  ieee754_f128_mantissa x ==* 0 &&*
  ieee754_f128_exponent x /=* 0

ieee754_f64_is_denormalized :: KnownNat n => Exp (Vec n Word64) -> Exp (Vec n Bool)
ieee754_f64_is_denormalized x =
  ieee754_f64_mantissa x ==* 0 &&*
  ieee754_f64_exponent x /=* 0

ieee754_f32_is_denormalized :: KnownNat n => Exp (Vec n Word32) -> Exp (Vec n Bool)
ieee754_f32_is_denormalized x =
  ieee754_f32_mantissa x ==* 0 &&*
  ieee754_f32_exponent x /=* 0

ieee754_f16_is_denormalized :: KnownNat n => Exp (Vec n Word16) -> Exp (Vec n Bool)
ieee754_f16_is_denormalized x =
  ieee754_f16_mantissa x ==* 0 &&*
  ieee754_f16_exponent x /=* 0

-- Negative zero if only the sign bit is set
--
ieee754_f128_is_negative_zero :: KnownNat n => Exp (Vec n Word128) -> Exp (Vec n Bool)
ieee754_f128_is_negative_zero x =
  ieee754_f128_negative x &&*
  ieee754_f128_exponent x ==* 0 &&*
  ieee754_f128_mantissa x ==* 0

ieee754_f64_is_negative_zero :: KnownNat n => Exp (Vec n Word64) -> Exp (Vec n Bool)
ieee754_f64_is_negative_zero x =
  ieee754_f64_negative x &&*
  ieee754_f64_exponent x ==* 0 &&*
  ieee754_f64_mantissa x ==* 0

ieee754_f32_is_negative_zero :: KnownNat n => Exp (Vec n Word32) -> Exp (Vec n Bool)
ieee754_f32_is_negative_zero x =
  ieee754_f32_negative x &&*
  ieee754_f32_exponent x ==* 0 &&*
  ieee754_f32_mantissa x ==* 0

ieee754_f16_is_negative_zero :: KnownNat n => Exp (Vec n Word16) -> Exp (Vec n Bool)
ieee754_f16_is_negative_zero x =
  ieee754_f16_negative x &&*
  ieee754_f16_exponent x ==* 0 &&*
  ieee754_f16_mantissa x ==* 0


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
ieee754_f128_mantissa :: KnownNat n => Exp (Vec n Word128) -> Exp (Vec n Word128)
ieee754_f128_mantissa x = x .&. 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFF

ieee754_f128_exponent :: KnownNat n => Exp (Vec n Word128) -> Exp (Vec n Word16)
ieee754_f128_exponent x = fromIntegral (x `unsafeShiftR` 112) .&. 0x7FFF

ieee754_f128_negative :: KnownNat n => Exp (Vec n Word128) -> Exp (Vec n Bool)
ieee754_f128_negative x = testBit x 127

-- Representation of a double precision IEEE floating point number:
--
-- sign         63           sign bit (0==positive, 1==negative)
-- exponent     62-52        exponent (biased by 1023)
-- fraction     51-0         fraction (bits to right of binary point)
--
ieee754_f64_mantissa :: KnownNat n => Exp (Vec n Word64) -> Exp (Vec n Word64)
ieee754_f64_mantissa x = x .&. 0xFFFFFFFFFFFFF

ieee754_f64_exponent :: KnownNat n => Exp (Vec n Word64) -> Exp (Vec n Word16)
ieee754_f64_exponent x = fromIntegral (x `unsafeShiftR` 52) .&. 0x7FF

ieee754_f64_negative :: KnownNat n => Exp (Vec n Word64) -> Exp (Vec n Bool)
ieee754_f64_negative x = testBit x 63

-- Representation of a single precision IEEE floating point number:
--
-- sign         31           sign bit (0==positive, 1==negative)
-- exponent     30-23        exponent (biased by 127)
-- fraction     22-0         fraction (bits to right of binary point)
--
ieee754_f32_mantissa :: KnownNat n => Exp (Vec n Word32) -> Exp (Vec n Word32)
ieee754_f32_mantissa x = x .&. 0x7FFFFF

ieee754_f32_exponent :: KnownNat n => Exp (Vec n Word32) -> Exp (Vec n Word8)
ieee754_f32_exponent x = fromIntegral (x `unsafeShiftR` 23)

ieee754_f32_negative :: KnownNat n => Exp (Vec n Word32) -> Exp (Vec n Bool)
ieee754_f32_negative x = testBit x 31

-- Representation of a half precision IEEE floating point number:
--
-- sign         15           sign bit (0==positive, 1==negative)
-- exponent     14-10        exponent (biased by 15)
-- fraction     9-0          fraction (bits to right of binary point)
--
ieee754_f16_mantissa :: KnownNat n => Exp (Vec n Word16) -> Exp (Vec n Word16)
ieee754_f16_mantissa x = x .&. 0x3FF

ieee754_f16_exponent :: KnownNat n => Exp (Vec n Word16) -> Exp (Vec n Word8)
ieee754_f16_exponent x = fromIntegral (x `unsafeShiftR` 10) .&. 0x1F

ieee754_f16_negative :: KnownNat n => Exp (Vec n Word16) -> Exp (Vec n Bool)
ieee754_f16_negative x = testBit x 15


-- reverse engineered following the below

ieee754_f16_decode :: forall n. KnownNat n => Exp (Vec n Word16) -> Exp (Vec n Int16, Vec n Int)
ieee754_f16_decode i =
  let
      _HHIGHBIT                       = 0x0400
      _HMSBIT                         = 0x8000
      _HMINEXP                        = splat ((_HALF_MIN_EXP) - (_HALF_MANT_DIG) - 1)
      _HALF_MANT_DIG                  = floatDigits (undefined::Exp Float16)
      T2 _HALF_MIN_EXP _HALF_MAX_EXP  = floatRange  (undefined::Exp Float16)

      high1 = fromIntegral i
      high2 = high1 .&. (_HHIGHBIT - 1)

      exp1  = ((fromIntegral high1 `unsafeShiftR` 10) .&. 0x1F) + _HMINEXP
      exp2  = exp1 + 1

      T2 high3 exp3
            = cond (exp1 /= _HMINEXP)
                   -- don't add hidden bit to denorms
                   (T2 (high2 .|. _HHIGHBIT) exp1)
                   -- a denorm, normalise the mantissa
                   (while (\(T2 h _) -> (h .&. _HHIGHBIT) /= 0)
                          (\(T2 h e) -> let p = (h .&. _HHIGHBIT) /=* 0
                                         in T2 (select p (h `unsafeShiftL` 1) h)
                                               (select p (e-1) e))
                          (T2 high2 exp2))

      high4 = select (fromIntegral i <* (0 :: Exp (Vec n Int16))) (-high3) high3
      z     = high1 .&. complement _HMSBIT ==* 0
  in
  T2 (select z 0 high4)
     (select z 0 exp3)


-- From: ghc/rts/StgPrimFloat.c
-- ----------------------------
--
-- The fast-path (no denormalised values) looks good to me, but if any one of
-- the lanes contains a denormalised value then all lanes need to continue
-- looping in predicated style to normalise the mantissa. We do a bit of
-- redundant work here that could be avoided; maybe the codegen will clean that
-- up for us, but if not it shouldn't matter too much anyway (slow path...).
--    -- TLM 2022-09-20.
--

ieee754_f32_decode :: forall n. KnownNat n => Exp (Vec n Word32) -> Exp (Vec n Int32, Vec n Int)
ieee754_f32_decode i =
  let
      _FHIGHBIT                     = 0x00800000
      _FMSBIT                       = 0x80000000
      _FMINEXP                      = splat ((_FLT_MIN_EXP) - (_FLT_MANT_DIG) - 1)
      _FLT_MANT_DIG                 = floatDigits (undefined::Exp Float32)
      T2 _FLT_MIN_EXP _FLT_MAX_EXP  = floatRange  (undefined::Exp Float32)

      high1 = fromIntegral i
      high2 = high1 .&. (_FHIGHBIT - 1)

      exp1  = ((fromIntegral high1 `unsafeShiftR` 23) .&. 0xFF) + _FMINEXP
      exp2  = exp1 + 1

      T2 high3 exp3
            = cond (exp1 /= _FMINEXP)
                   -- don't add hidden bit to denorms
                   (T2 (high2 .|. _FHIGHBIT) exp1)
                   -- a denorm, normalise the mantissa
                   (while (\(T2 h _) -> (h .&. _FHIGHBIT) /= 0)
                          (\(T2 h e) -> let p = (h .&. _FHIGHBIT) /=* 0
                                         in T2 (select p (h `unsafeShiftL` 1) h)
                                               (select p (e-1) e))
                          (T2 high2 exp2))

      high4 = select (fromIntegral i <* (0 :: Exp (Vec n Int32))) (-high3) high3
      z     = high1 .&. complement _FMSBIT ==* 0
  in
  T2 (select z 0 high4)
     (select z 0 exp3)


ieee754_f64_decode :: KnownNat n => Exp (Vec n Word64) -> Exp (Vec n Int64, Vec n Int)
ieee754_f64_decode i =
  let T4 s h l e = ieee754_f64_decode2 i
   in T2 (fromIntegral s * (fromIntegral h `unsafeShiftL` 32 .|. fromIntegral l)) e

ieee754_f64_decode2 :: forall n. KnownNat n => Exp (Vec n Word64) -> Exp (Vec n Int64, Vec n Word32, Vec n Word32, Vec n Int)
ieee754_f64_decode2 i =
  let
      _DHIGHBIT                     = 0x00100000
      _DMSBIT                       = 0x80000000
      _DMINEXP                      = splat ((_DBL_MIN_EXP) - (_DBL_MANT_DIG) - 1)
      _DBL_MANT_DIG                 = floatDigits (undefined::Exp Float64)
      T2 _DBL_MIN_EXP _DBL_MAX_EXP  = floatRange  (undefined::Exp Float64)

      low   = fromIntegral i
      high  = fromIntegral (i `unsafeShiftR` 32)

      iexp  = (fromIntegral ((high `unsafeShiftR` 20) .&. 0x7FF) + _DMINEXP)
      sign  = select (fromIntegral i <* (0 :: Exp (Vec n Int64))) (-1) 1

      high2 = high .&. (_DHIGHBIT - 1)
      iexp2 = iexp + 1

      T3 hi lo ie
            = cond (iexp2 /= _DMINEXP)
                   -- don't add hidden bit to denorms
                   (T3 (high2 .|. _DHIGHBIT) low iexp)
                   -- a denorm, nermalise the mantissa
                   (while (\(T3 h _ _) -> (h .&. _DHIGHBIT) /= 0)
                          (\(T3 h l e) ->
                            let p = (h .&. _DHIGHBIT) /=* 0
                                h1 = h `unsafeShiftL` 1
                                h2 = cond ((l .&. _DMSBIT) /= 0) (h1+1) h1
                            in  T3 (select p h2 h)
                                   (select p (l `unsafeShiftL` 1) l)
                                   (select p (e-1) e))
                          (T3 high2 low iexp2))

      z     = low ==* 0 &&* (high .&. (complement _DMSBIT)) ==* 0
  in
  T4 (select z 1 sign)
     (select z 0 hi)
     (select z 0 lo)
     (select z 0 ie)

ieee754_f128_decode :: KnownNat n => Exp (Vec n Word128) -> Exp (Vec n Int128, Vec n Int)
ieee754_f128_decode = error "TODO: ieee754_f128_decode"

