{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

-- https://github.com/AccelerateHS/accelerate/issues/255
--

module Test.Issues.Issue255 (test_issue255)
  where

import Config
import Test.HUnit                                               ( assertFailure )
import Test.Framework
import Test.Framework.Providers.HUnit

import Data.Array.Accelerate                                    as A
import Data.Array.Accelerate.Examples.Internal                  as A

import Control.Exception
import System.Timeout
import Data.List                                                as P
import Prelude                                                  as P


test_issue255 :: Backend -> Config -> Test
test_issue255 backend _conf =
  testGroup "255"
    [ testCase "0"   (within lIMIT $ total (as P.!! 0))
    , testCase "2"   (within lIMIT $ total (as P.!! 2))
    , testCase "4"   (within lIMIT $ total (as P.!! 4))
    , testCase "20"  (within lIMIT $ total (as P.!! 20))
    , testCase "100" (within lIMIT $ total (as P.!! 100))
    -- , testCase "200" (within lIMIT $ total (as P.!! 200))
    -- , testCase "300" (within lIMIT $ total (as P.!! 300))
    ]
  where
    lIMIT     = 30 * 1000 * 1000  -- microseconds
    n         = 20 * 1024 * 1024 -- 160 * MiB (8 bytes per Double)

    as :: [A.Vector Double]
    as = sums (A.fromList (Z:.n) (repeat 0)) (A.fromList (Z:.n) (repeat 1))

    sums :: A.Vector Double -> A.Vector Double -> [A.Vector Double]
    sums a0 b
      = a0
      : ( P.snd
        $ P.mapAccumL
            (\a' i -> let a = run1 backend step (a', b, scalar i)
                      in (a, a))
            a0
            [0 .. 500]
        )

    step :: Acc (A.Vector Double, A.Vector Double, A.Scalar Int) -> Acc (A.Vector Double)
    step (unlift -> (a, b, A.the -> _i::Exp Int)) = A.zipWith (+) a b

    total :: A.Vector Double -> A.Scalar Double
    total a =
      run backend $ A.sum $ A.map id $ A.use a

  -- print $ total $ as P.!! 0
  -- print $ total $ as P.!! 2
  -- print $ total $ as P.!! 4
  -- print $ total $ as P.!! 20
  -- print $ total $ as P.!! 100
  -- print $ total $ as P.!! 200
  -- print $ total $ as P.!! 300


within :: Int -> Scalar Double -> Assertion
within n arr = do
  r <- timeout n $ evaluate (indexArray arr Z `seq` ())
  case r of
    Nothing -> assertFailure "timeout: backend is too slow or memory manager stuck?"
    Just () -> return ()

