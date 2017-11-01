{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Hedgehog.Gen.Shape
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module is meant to be imported qualified
--

module Data.Array.Accelerate.Hedgehog.Gen.Shape
  where

import Data.Array.Accelerate.Array.Sugar                  ( Z(..), (:.)(..) )

import Hedgehog                                           ( Gen, Range )
import Hedgehog.Gen                                       ( int )


-- Generate a randomly sized shape of the given dimensionality
--
class Shape sh where
  shape :: Range Int -> Gen sh

instance Shape Z where
  shape _ = return Z

instance Shape sh => Shape (sh :. Int) where
  shape r = (:.) <$> shape r <*> int r

