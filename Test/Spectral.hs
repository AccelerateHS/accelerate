
module Test.Spectral (

  test_spectral

) where

import Config

import Test.Framework
import Test.Spectral.BlackScholes
import Test.Spectral.SMVM
import Test.Spectral.RadixSort

import Data.Array.Accelerate.Examples.Internal


test_spectral :: Backend -> Config -> Test
test_spectral be conf =
  testGroup "spectral"
    [ test_blackscholes be conf
    , test_smvm be conf
    , test_radixsort be conf
    ]

