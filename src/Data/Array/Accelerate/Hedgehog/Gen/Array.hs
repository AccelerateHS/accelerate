{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Hedgehog.Gen.Array
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module is meant to be imported qualified
--

module Data.Array.Accelerate.Hedgehog.Gen.Array
  where

import Data.Array.Accelerate.Array.Sugar                  ( Array, Shape, Elt, fromList, size )

import Hedgehog                                           ( Gen )
import qualified Hedgehog.Gen                             as Gen
import qualified Hedgehog.Range                           as Range


-- | Generate an array of the given shape filled with random elements using the
-- supplied generator.
--
array :: (Shape sh, Elt e) => sh -> Gen e -> Gen (Array sh e)
array sh gen =
  fromList sh <$> Gen.list (Range.singleton (size sh)) gen

