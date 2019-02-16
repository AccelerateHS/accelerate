{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module      : Data.Array.Accelerate.Classes
-- Copyright   : [2016..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module provides instances for standard Haskell 98 classes where
-- possible, and provides new implementations of those classes where the types
-- do not match.
--

module Data.Array.Accelerate.Classes (

  -- *** Basic type classes
  module Eq,
  module Ord,
  module Enum,
  module Bounded,

  -- *** Numeric type classes
  module Num,
  module Integral,
  module Rational,
  module Fractional,
  module Floating,
  module RealFrac,
  module RealFloat,

  -- *** Numeric conversions
  module FromIntegral,
  module ToFloating,

) where

import Data.Array.Accelerate.Classes.Bounded                        as Bounded
import Data.Array.Accelerate.Classes.Enum                           as Enum
import Data.Array.Accelerate.Classes.Eq                             as Eq
import Data.Array.Accelerate.Classes.Floating                       as Floating
import Data.Array.Accelerate.Classes.Fractional                     as Fractional
import Data.Array.Accelerate.Classes.FromIntegral                   as FromIntegral
import Data.Array.Accelerate.Classes.Integral                       as Integral
import Data.Array.Accelerate.Classes.Rational                       as Rational
import Data.Array.Accelerate.Classes.Num                            as Num
import Data.Array.Accelerate.Classes.Ord                            as Ord
import Data.Array.Accelerate.Classes.RealFloat                      as RealFloat
import Data.Array.Accelerate.Classes.RealFrac                       as RealFrac
import Data.Array.Accelerate.Classes.ToFloating                     as ToFloating

