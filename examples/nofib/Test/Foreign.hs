{-# LANGUAGE CPP #-}

module Test.Foreign (

  test_foreign

) where

import Config
#ifdef ACCELERATE_CUDA_BACKEND
import Test.Foreign.CUDA
#endif

import Data.Array.Accelerate.Examples.Internal


test_foreign :: Backend -> Config -> Test
test_foreign _be _conf =
  testGroup "foreign"
    [
#ifdef ACCELERATE_CUDA_BACKEND
      test_cuda _be _conf
#endif
    ]

