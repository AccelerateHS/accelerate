{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.RealFrac
-- Copyright   : [2019..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Classes.RealFrac
  where

import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.Classes.Fractional
import Data.Array.Accelerate.Classes.FromIntegral
import Data.Array.Accelerate.Classes.Integral
import Data.Array.Accelerate.Classes.Ord

import qualified Prelude                                            as P

class (Ord a, Fractional a) => RealFrac a where
  properFraction :: (Integral b, FromIntegral Int64 b) => Exp a -> (Exp b, Exp a)
  truncate :: (Integral b, FromIntegral Int64 b) => Exp a -> Exp b
  round    :: (Integral b, FromIntegral Int64 b) => Exp a -> Exp b
  ceiling  :: (Integral b, FromIntegral Int64 b) => Exp a -> Exp b
  floor    :: (Integral b, FromIntegral Int64 b) => Exp a -> Exp b

  truncate = P.undefined
  round    = P.undefined
  ceiling  = P.undefined
  floor    = P.undefined

instance RealFrac Half
instance RealFrac Float
instance RealFrac Double
instance RealFrac CFloat
instance RealFrac CDouble

