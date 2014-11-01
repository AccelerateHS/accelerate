
module Test.Foreign.CUDA
  where

import Config
import Test.Base
import Test.Prelude.Mapping

import Prelude                                                  as P
import Data.Array.Accelerate                                    as A
import Data.Array.Accelerate.Examples.Internal                  as A
import Test.Framework
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2
import Data.Array.Accelerate.CUDA.Foreign                       as A


test_cuda :: Backend -> Config -> Test
test_cuda backend _conf = testGroup "CUDA"
  $ if backend == CUDA
       then [ testExpf, testFmaf ]
       else [ ]
  where
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

