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

import Data.Array.Accelerate.Classes.Floating
import Data.Array.Accelerate.Classes.RealFrac

import Text.Printf
import Prelude                                                      ( String, error, undefined )
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

  -- | Return the significand and an appropriately scaled exponent. if
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

  decodeFloat     = $internalError "RealFloat.decodeFloat"    "Not implemented yet"
  encodeFloat     = $internalError "RealFloat.encodeFloat"    "Not implemented yet"
  exponent        = $internalError "RealFloat.exponent"       "Not implemented yet"
  significand     = $internalError "RealFloat.significand"    "Not implemented yet"
  scaleFloat      = $internalError "RealFloat.scaleFloat"     "Not implemented yet"
  isInfinite      = $internalError "RealFloat.isInfinite"     "Not implemented yet"
  isDenormalized  = $internalError "RealFloat.isDenormalized" "Not implemented yet"
  isNegativeZero  = $internalError "RealFloat.isNegativeZero" "Not implemented yet"


instance RealFloat Float where
  isNaN           = mkIsNaN
  atan2           = mkAtan2

instance RealFloat Double where
  isNaN           = mkIsNaN
  atan2           = mkAtan2

instance RealFloat CFloat where
  isNaN           = mkIsNaN
  atan2           = mkAtan2

instance RealFloat CDouble where
  isNaN           = mkIsNaN
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
preludeError x = error (printf "Prelude.%s applied to EDSL types: use Data.Array.Accelerate.%s instead" x)

