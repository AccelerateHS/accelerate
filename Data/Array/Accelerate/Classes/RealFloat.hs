{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.RealFloat
-- Copyright   : [2016..2017] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Classes.RealFloat (

  RealFloat(..),

) where

import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.Data.Bits

import Data.Array.Accelerate.Classes.Eq
import Data.Array.Accelerate.Classes.Floating
import Data.Array.Accelerate.Classes.FromIntegral
import Data.Array.Accelerate.Classes.RealFrac

import Text.Printf
import Prelude                                                      ( (.), String, error, undefined, otherwise )
import qualified Prelude                                            as P


-- | Efficient, machine-independent access to the components of a floating-point
-- number
--
class (RealFrac a, Floating a) => RealFloat a where
  -- | The radix of the representation (often 2) (constant)
  floatRadix     :: Exp a -> Exp Int64  -- Integer
  default floatRadix :: P.RealFloat a => Exp a -> Exp Int64
  floatRadix _    = P.fromInteger (P.floatRadix (undefined::a))

  -- | The number of digits of 'floatRadix' in the significand (constant)
  floatDigits    :: Exp a -> Exp Int
  default floatDigits :: P.RealFloat a => Exp a -> Exp Int
  floatDigits _   = constant (P.floatDigits (undefined::a))

  -- | The lowest and highest values the exponent may assume (constant)
  floatRange     :: Exp a -> (Exp Int, Exp Int)
  default floatRange :: P.RealFloat a => Exp a -> (Exp Int, Exp Int)
  floatRange _    = let (m,n) = P.floatRange (undefined::a)
                    in (constant m, constant n)

  -- | Return the significand and an appropriately scaled exponent. If
  -- @(m,n) = 'decodeFloat' x@ then @x = m*b^^n@, where @b@ is the
  -- floating-point radix ('floatRadix'). Furthermore, either @m@ and @n@ are
  -- both zero, or @b^(d-1) <= 'abs' m < b^d@, where @d = 'floatDigits' x@.
  decodeFloat    :: Exp a -> (Exp Int64, Exp Int)    -- Integer

  -- | Inverse of 'decodeFloat'
  encodeFloat    :: Exp Int64 -> Exp Int -> Exp a    -- Integer

  -- | Corresponds to the second component of 'decodeFloat'
  exponent       :: Exp a -> Exp Int

  -- | Corresponds to the first component of 'decodeFloat'
  significand    :: Exp a -> Exp a

  -- | Multiply a floating point number by an integer power of the radix
  scaleFloat     :: Exp Int -> Exp a -> Exp a

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
  isIEEE _        = constant (P.isIEEE (undefined::Float))

  -- | A version of arctangent taking two real floating-point arguments.
  -- For real floating @x@ and @y@, @'atan2' y x@ computes the angle (from the
  -- positive x-axis) of the vector from the origin to the point @(x,y)@.
  -- @'atan2' y x@ returns a value in the range [@-pi@, @pi@].
  atan2          :: Exp a -> Exp a -> Exp a

  decodeFloat     = $internalError "RealFloat.decodeFloat" "Not implemented yet"
  encodeFloat     = $internalError "RealFloat.encodeFloat" "Not implemented yet"
  exponent        = $internalError "RealFloat.exponent"    "Not implemented yet"
  significand     = $internalError "RealFloat.significand" "Not implemented yet"
  scaleFloat      = $internalError "RealFloat.scaleFloat"  "Not implemented yet"


instance RealFloat Float where
  atan2           = mkAtan2
  isNaN           = mkIsNaN
  isInfinite      = mkIsInfinite
  isDenormalized  = ieee754 "isDenormalized" (ieee754_f32_is_denormalized . mkUnsafeCoerce)
  isNegativeZero  = ieee754 "isNegativeZero" (ieee754_f32_is_negative_zero . mkUnsafeCoerce)

instance RealFloat Double where
  atan2           = mkAtan2
  isNaN           = mkIsNaN
  isInfinite      = mkIsInfinite
  isDenormalized  = ieee754 "isDenormalized" (ieee754_f64_is_denormalized . mkUnsafeCoerce)
  isNegativeZero  = ieee754 "isNegativeZero" (ieee754_f64_is_negative_zero . mkUnsafeCoerce)

instance RealFloat CFloat where
  atan2           = mkAtan2
  isNaN           = mkIsNaN
  isInfinite      = mkIsInfinite
  isDenormalized  = ieee754 "isDenormalized" (ieee754_f32_is_denormalized . mkUnsafeCoerce)
  isNegativeZero  = ieee754 "isNegativeZero" (ieee754_f32_is_negative_zero . mkUnsafeCoerce)

instance RealFloat CDouble where
  atan2           = mkAtan2
  isNaN           = mkIsNaN
  isInfinite      = mkIsInfinite
  isDenormalized  = ieee754 "isDenormalized" (ieee754_f64_is_denormalized . mkUnsafeCoerce)
  isNegativeZero  = ieee754 "isNegativeZero" (ieee754_f64_is_negative_zero . mkUnsafeCoerce)


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
preludeError x = error (printf "Prelude.%s applied to EDSL types: use Data.Array.Accelerate.%s instead" x x)


ieee754 :: forall a b. P.RealFloat a => String -> (Exp a -> b) -> Exp a -> b
ieee754 name f x
  | P.isIEEE (undefined::a) = f x
  | otherwise               = $internalError (printf "RealFloat.%s" name) "Not implemented for non-IEEE floating point"


-- An IEEE754 number is denormalised iff:
--   * exponent is zero
--   * mantissa is non-zero.
--   * (don't care about setting of sign bit.)
--
ieee754_f64_is_denormalized :: Exp Word64 -> Exp Bool
ieee754_f64_is_denormalized x =
  ieee754_f64_mantissa x == 0 &&
  ieee754_f64_exponent x /= 0

ieee754_f32_is_denormalized :: Exp Word32 -> Exp Bool
ieee754_f32_is_denormalized x =
  ieee754_f32_mantissa x == 0 &&
  ieee754_f32_exponent x /= 0

-- Negative zero if only the sign bit is set
--
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


-- Assume the host processor stores integers and floating point numbers in the
-- same endianness (true for modern processors).
--
-- To recap, here's the representation of a double precision
-- IEEE floating point number:
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

-- Representation of single precision IEEE floating point number:
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

