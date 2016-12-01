{-# LANGUAGE FlexibleContexts #-}

module Test.Foreign.CUDA
  where

import Config
import QuickCheck.Arbitrary.Array                               ()

import Prelude                                                  as P
import Data.Array.Accelerate                                    as A
import Data.Array.Accelerate.Examples.Internal                  as A
import Data.Array.Accelerate.CUDA.Foreign                       as A


test_cuda :: Backend -> Config -> Test
test_cuda backend _conf = testGroup "CUDA"
  $ case backend of
      CUDA -> [ testExpf, testFmaf ]
      _    -> [ ]
  where
    testExpf :: Test
    testExpf = testProperty "expf" test_expf
      where
        test_expf :: Array DIM1 Float -> Property
        test_expf xs =     run1 backend (A.map (A.foreignExp (A.CUDAForeignExp [] "__expf") exp)) xs
                       ~?= mapRef exp xs

    testFmaf :: Test
    testFmaf = testProperty "fmaf" test_fmaf
      where
        test_fmaf :: Array DIM1 (Float, Float, Float) -> Property
        test_fmaf xs =     run1 backend (A.map (A.foreignExp (A.CUDAForeignExp [] "__fmaf_rz") fmaf)) xs
                       ~?= mapRef (\(x,y,z) -> x * y + z) xs
          where
            fmaf v = let (x,y,z) = unlift v in x * y + z


mapRef :: (Shape sh, Elt b) => (a -> b) -> Array sh a -> Array sh b
mapRef f xs
  = fromList (arrayShape xs)
  $ P.map f
  $ toList xs

