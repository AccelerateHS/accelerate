{-# LANGUAGE RankNTypes #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Spectral
-- Copyright   : [2009..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Test.NoFib.Spectral (

  test_spectral,

  module Data.Array.Accelerate.Test.NoFib.Spectral.SMVM,
  module Data.Array.Accelerate.Test.NoFib.Spectral.RadixSort,
  module Data.Array.Accelerate.Test.NoFib.Spectral.BlackScholes,

) where

import Test.Tasty

import Data.Array.Accelerate.Test.NoFib.Base
import Data.Array.Accelerate.Test.NoFib.Spectral.SMVM
import Data.Array.Accelerate.Test.NoFib.Spectral.RadixSort
import Data.Array.Accelerate.Test.NoFib.Spectral.BlackScholes


test_spectral :: RunN -> TestTree
test_spectral runN =
  testGroup "spectral"
    [ test_blackscholes runN
    , test_smvm runN
    , test_radixsort runN
    ]

