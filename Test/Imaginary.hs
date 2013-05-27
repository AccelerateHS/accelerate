
module Test.Imaginary (

  test_imaginary

) where

import Config

import Test.Framework
import Test.Imaginary.DotP
import Test.Imaginary.SASUM
import Test.Imaginary.SAXPY


test_imaginary :: Config -> Test
test_imaginary conf =
  testGroup "imaginary"
    [ test_sasum conf
    , test_saxpy conf
    , test_dotp conf
    ]

