
module Test.Spectral (

  test_spectral

) where

import Config

import Test.Framework
import Test.Spectral.BlackScholes
import Test.Spectral.SMVM
import Test.Spectral.RadixSort


test_spectral :: Config -> Test
test_spectral conf =
  testGroup "spectral"
    [ test_blackscholes conf
    , test_smvm conf
    , test_radixsort conf
    ]

