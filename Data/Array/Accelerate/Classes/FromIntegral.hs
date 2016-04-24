{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.FromIntegral
-- Copyright   : [2016] Manuel M T Chakravarty, Gabriele Keller
--               [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Classes.FromIntegral (

  FromIntegral(..),

) where

import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.Classes.Integral
import Data.Array.Accelerate.Classes.Num


-- | Accelerate lacks a most-general lossless 'Prelude.Integer' type, which the
-- standard 'Prelude.fromIntegral' function uses as an intermediate value when
-- coercing from integral types. Instead, we use this class to capture a direct
-- coercion between two types.
--
class FromIntegral a b where
  -- | General coercion from integral types
  fromIntegral :: (Integral a, Num b) => Exp a -> Exp b

instance (Elt a, Elt b, IsIntegral a, IsNum b) => FromIntegral a b where
  fromIntegral = mkFromIntegral

