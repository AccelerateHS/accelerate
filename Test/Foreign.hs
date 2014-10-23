
module Test.Foreign (

  test_foreign

) where

import Config
import Test.Framework
import Test.Foreign.CUDA

import Data.Array.Accelerate.Examples.Internal


test_foreign :: Backend -> Config -> Test
test_foreign be conf =
  testGroup "foreign"
    [ test_cuda be conf
    ]

