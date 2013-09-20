{-# LANGUAGE FlexibleContexts    #-}
module Test.Foreign (

  test_foreign

) where

import Config

import Data.Label

import Prelude                                  as P
import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.CUDA.Foreign       as A
import Test.Prelude.Mapping
import Test.Framework
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2
import Test.Base

import ParseArgs

test_foreign :: Config -> Test
test_foreign conf = testGroup "foreign"
  [ testExpf
  , testFmaf
  ]
  where
    backend = get configBackend conf

    testExpf :: Test
    testExpf = testProperty "expf" test_expf
      where
        test_expf :: Array DIM1 Float -> Property
        test_expf xs =     run backend (A.map (A.foreignExp (A.CUDAForeignExp [] "__expf") exp) (A.use xs))
                       ~?= mapRef exp xs

    testFmaf :: Test
    testFmaf = testProperty "fmaf" test_fmaf
      where
        test_fmaf :: Array DIM1 (Float, Float, Float) -> Property
        test_fmaf xs =     run backend (A.map (A.foreignExp (A.CUDAForeignExp [] "__fmaf_rz") fmaf) (A.use xs))
                       ~?= mapRef (\(x,y,z) -> x * y + z) xs
          where
            fmaf v = let (x,y,z) = unlift v in x * y + z