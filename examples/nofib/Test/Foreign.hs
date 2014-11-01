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
test_foreign be conf =
  testGroup "foreign"
    [
#ifdef ACCELERATE_CUDA_BACKEND
      test_cuda be conf
#endif
    ]

