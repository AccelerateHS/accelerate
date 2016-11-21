{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module      : Data.Array.Accelerate.Classes
-- Copyright   : [2016] Manuel M T Chakravarty, Gabriele Keller
--               [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
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
  module Real,
  module Integral,
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
import Data.Array.Accelerate.Classes.Num                            as Num
import Data.Array.Accelerate.Classes.Ord                            as Ord
import Data.Array.Accelerate.Classes.Real                           as Real
import Data.Array.Accelerate.Classes.RealFloat                      as RealFloat
import Data.Array.Accelerate.Classes.RealFrac                       as RealFrac
import Data.Array.Accelerate.Classes.ToFloating                     as ToFloating

