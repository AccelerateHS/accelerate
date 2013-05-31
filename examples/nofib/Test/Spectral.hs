
module Test.Spectral (

  test_spectral

) where

import Config

import Test.Framework
import Test.Spectral.SMVM


test_spectral :: Config -> Test
test_spectral conf =
  testGroup "spectral"
    [ test_smvm conf
    ]

