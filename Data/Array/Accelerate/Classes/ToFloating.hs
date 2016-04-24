{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.ToFloating
-- Copyright   : [2016] Manuel M T Chakravarty, Gabriele Keller
--               [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Classes.ToFloating (

  ToFloating(..),

) where

import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.Classes.Floating
import Data.Array.Accelerate.Classes.Num


-- | Accelerate lacks an arbitrary-precision 'Prelude.Rational' type, which the
-- standard 'Prelude,realToFrac' uses as an intermediate value when coercing
-- to floating-point types. Instead, we use this class to capture a direct
-- coercion between to types.
--
class ToFloating a b where
  -- | General coercion to floating types
  toFloating :: (Num a, Floating b) => Exp a -> Exp b

instance (Elt a, Elt b, IsNum a, IsFloating b) => ToFloating a b where
  toFloating = mkToFloating

