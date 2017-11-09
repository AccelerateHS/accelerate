
module Test.Imaginary (

  test_imaginary

) where

import Config

import Test.Framework
import Test.Imaginary.DotP
import Test.Imaginary.SASUM
import Test.Imaginary.SAXPY

import Data.Array.Accelerate.Examples.Internal


test_imaginary :: Backend -> Config -> Test
test_imaginary be conf =
  testGroup "imaginary"
    [ test_sasum be conf
    , test_saxpy be conf
    , test_dotp be conf
    ]

