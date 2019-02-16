{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.RealFloat
-- Copyright   : [2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Classes.RealFloat
  where

import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.Classes.Floating
import Data.Array.Accelerate.Classes.FromIntegral
import {-# SOURCE #-} Data.Array.Accelerate.Classes.RealFrac

import qualified Prelude                                            as P


class (RealFrac a, Floating a) => RealFloat a where
  floatRadix     :: Exp a -> Exp Int64  -- Integer
  floatDigits    :: Exp a -> Exp Int
  floatRange     :: Exp a -> (Exp Int, Exp Int)
  decodeFloat    :: Exp a -> (Exp Int64, Exp Int)    -- Integer
  encodeFloat    :: Exp Int64 -> Exp Int -> Exp a    -- Integer
  exponent       :: Exp a -> Exp Int
  significand    :: Exp a -> Exp a
  scaleFloat     :: Exp Int -> Exp a -> Exp a
  isNaN          :: Exp a -> Exp Bool
  isInfinite     :: Exp a -> Exp Bool
  isDenormalized :: Exp a -> Exp Bool
  isNegativeZero :: Exp a -> Exp Bool
  isIEEE         :: Exp a -> Exp Bool
  atan2          :: Exp a -> Exp a -> Exp a

  exponent        = P.undefined
  significand     = P.undefined
  scaleFloat      = P.undefined

  default floatRadix  :: P.RealFloat a => Exp a -> Exp Int64
  floatRadix _    = P.undefined

  default floatDigits :: P.RealFloat a => Exp a -> Exp Int
  floatDigits _   = P.undefined

  default floatRange  :: P.RealFloat a => Exp a -> (Exp Int, Exp Int)
  floatRange _    = P.undefined

  default encodeFloat :: (FromIntegral Int a, FromIntegral Int64 a) => Exp Int64 -> Exp Int -> Exp a
  encodeFloat _ _ = P.undefined

  default isIEEE      :: P.RealFloat a => Exp a -> Exp Bool
  isIEEE _        = P.undefined

instance RealFloat Half
instance RealFloat Float
instance RealFloat Double
instance RealFloat CFloat
instance RealFloat CDouble

