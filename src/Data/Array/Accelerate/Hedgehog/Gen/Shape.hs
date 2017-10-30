{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}
-- |
-- Module      : Data.Array.Accelerate.Hedgehog.Gen.Shape
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Hedgehog.Gen.Shape
  where

import Data.Array.Accelerate.Array.Sugar                  ( Z(..), (:.)(..) )

import Hedgehog                                           ( Gen, Range )
import Hedgehog.Gen                                       ( int )


-- Generate a randomly sized shape of the given dimensionality
--
class GenShape sh where
  shape :: Range Int -> Gen sh

instance GenShape Z where
  shape _ = return Z

instance GenShape sh => GenShape (sh :. Int) where
  shape r = (:.) <$> shape r <*> int r

