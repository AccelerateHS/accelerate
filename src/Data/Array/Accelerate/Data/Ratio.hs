{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Data.Ratio
-- Copyright   : [2019..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Standard functions on rational numbers
--
-- @since 1.3.0.0
--

module Data.Array.Accelerate.Data.Ratio (

  Ratio, (%),
  pattern (:%), numerator, denominator,

) where

import Data.Array.Accelerate.Annotations
import Data.Array.Accelerate.Language
import Data.Array.Accelerate.Pattern
import Data.Array.Accelerate.Prelude
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.Classes.Enum
import Data.Array.Accelerate.Classes.Eq
import Data.Array.Accelerate.Classes.Fractional
import Data.Array.Accelerate.Classes.FromIntegral
import Data.Array.Accelerate.Classes.Integral
import Data.Array.Accelerate.Classes.Num
import Data.Array.Accelerate.Classes.Ord
import Data.Array.Accelerate.Classes.RealFrac
import Data.Array.Accelerate.Classes.ToFloating

import Text.Printf
import Data.Ratio                                                   ( Ratio )
import Prelude                                                      ( ($), String, error, unlines )
import qualified Data.Ratio                                         as P
import qualified Prelude                                            as P


instance Elt a => Elt (Ratio a)

pattern (:%) :: (HasCallStack, Elt a) => Exp a -> Exp a -> Exp (Ratio a)
pattern (:%) { numerator, denominator } = Pattern (numerator, denominator)
{-# COMPLETE (:%) #-}


-- | 'reduce' is a subsidiary function used only in this module. It normalises
-- a ratio by dividing both numerator and denominator by their greatest common
-- divisor.
--
reduce :: (HasCallStack, Integral a) => Exp a -> Exp a -> Exp (Ratio a)
reduce x y =
  if y == 0
    then infinity
    else let d = gcd x y
         in  (x `quot` d) :% (y `quot` d)

-- | Form the ratio of two integral numbers
--
infixl 7 %
(%) :: (HasCallStack, Integral a) => Exp a -> Exp a -> Exp (Ratio a)
x % y = withFrozenCallStack $ reduce (x * signum y) (abs y)

infinity :: (HasCallStack, Integral a) => Exp (Ratio a)
infinity = withFrozenCallStack $ 1 :% 0


-- Instances
-- ---------

instance Integral a => Eq (Ratio a) where
  (==) = withFrozenCallStack $ \(x :% y) (z :% w) -> x == z && y == w
  (/=) = withFrozenCallStack $ \(x :% y) (z :% w) -> x /= z || y /= w

instance Integral a => Ord (Ratio a)  where
  (<=) = withFrozenCallStack $ \(x :% y) (z :% w) -> x * w <= z * y
  (<)  = withFrozenCallStack $ \(x :% y) (z :% w) -> x * w <  z * y

-- TODO: Can we provide frozen call stacks for prelude classes?
instance Integral a => P.Num (Exp (Ratio a)) where
  (x :% y) + (z :% w) = reduce (x*w + z*y) (y*w)
  (x :% y) - (z :% w) = reduce (x*w - z*y) (y*w)
  (x :% y) * (z :% w) = reduce (x * z) (y * w)
  negate (x:%y)       = (-x) :% y
  abs (x:%y)          = abs x :% y
  signum (x:%_)       = signum x :% 1
  fromInteger x       = fromInteger x :% 1

instance Integral a => P.Fractional (Exp (Ratio a))  where
  (x :% y) / (z :% w) = (x*w) % (y*z)
  recip (x :% y)      =
    if x == 0 then infinity else
    if x <  0 then negate y :% negate x
              else y :% x
  fromRational r = fromInteger (P.numerator r) % fromInteger (P.denominator r)

instance (Integral a, FromIntegral a Int64) => RealFrac (Ratio a) where
  properFraction = withFrozenCallStack $ \(x :% y) ->
    let (q,r) = quotRem x y
    in  (fromIntegral (fromIntegral q :: Exp Int64), r :% y)


instance (Integral a, ToFloating a b) => ToFloating (Ratio a) b where
  toFloating = withFrozenCallStack $ \(x :% y) ->
    let x' :% y' = reduce x y
    in  toFloating x' / toFloating y'

instance (FromIntegral a b, Integral b) => FromIntegral a (Ratio b) where
  fromIntegral x = withFrozenCallStack $ fromIntegral x :% 1

instance Integral a => P.Enum (Exp (Ratio a))  where
  succ x   = x + 1
  pred x   = x - 1
  toEnum   = preludeError "Enum" "toEnum"
  fromEnum = preludeError "Enum" "fromEnum"


preludeError :: String -> String -> a
preludeError x y
  = error
  $ unlines [ printf "Prelude.%s is not supported for Accelerate types" y
            , ""
            , printf "These Prelude.%s instances are present only to fulfil superclass" x
            , "constraints for subsequent classes in the standard Haskell numeric hierarchy."
            ]

