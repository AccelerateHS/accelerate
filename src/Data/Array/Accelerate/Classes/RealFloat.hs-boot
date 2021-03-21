{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.RealFloat
-- Copyright   : [2019..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Classes.RealFloat
  where

import Data.Array.Accelerate.Annotations
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.Classes.Floating
import Data.Array.Accelerate.Classes.FromIntegral
import {-# SOURCE #-} Data.Array.Accelerate.Classes.RealFrac

import Prelude                                                      ( Bool )
import qualified Prelude                                            as P


class (RealFrac a, Floating a) => RealFloat a where
  floatRadix     :: HasCallStack => Exp a -> Exp Int64  -- Integer
  floatDigits    :: HasCallStack => Exp a -> Exp Int
  floatRange     :: HasCallStack => Exp a -> (Exp Int, Exp Int)
  decodeFloat    :: HasCallStack => Exp a -> (Exp Int64, Exp Int)    -- Integer
  encodeFloat    :: HasCallStack => Exp Int64 -> Exp Int -> Exp a    -- Integer
  exponent       :: HasCallStack => Exp a -> Exp Int
  significand    :: HasCallStack => Exp a -> Exp a
  scaleFloat     :: HasCallStack => Exp Int -> Exp a -> Exp a
  isNaN          :: HasCallStack => Exp a -> Exp Bool
  isInfinite     :: HasCallStack => Exp a -> Exp Bool
  isDenormalized :: HasCallStack => Exp a -> Exp Bool
  isNegativeZero :: HasCallStack => Exp a -> Exp Bool
  isIEEE         :: HasCallStack => Exp a -> Exp Bool
  atan2          :: HasCallStack => Exp a -> Exp a -> Exp a

  exponent        = P.undefined
  significand     = P.undefined
  scaleFloat      = P.undefined

  default floatRadix  :: (HasCallStack, P.RealFloat a) => Exp a -> Exp Int64
  floatRadix _    = P.undefined

  default floatDigits :: (HasCallStack, P.RealFloat a) => Exp a -> Exp Int
  floatDigits _   = P.undefined

  default floatRange  :: (HasCallStack, P.RealFloat a) => Exp a -> (Exp Int, Exp Int)
  floatRange _    = P.undefined

  default encodeFloat :: (HasCallStack, FromIntegral Int a, FromIntegral Int64 a) => Exp Int64 -> Exp Int -> Exp a
  encodeFloat _ _ = P.undefined

  default isIEEE      :: (HasCallStack, P.RealFloat a) => Exp a -> Exp Bool
  isIEEE _        = P.undefined

instance RealFloat Half
instance RealFloat Float
instance RealFloat Double
instance RealFloat CFloat
instance RealFloat CDouble

