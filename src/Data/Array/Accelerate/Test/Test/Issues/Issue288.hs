-- https://github.com/AccelerateHS/accelerate/issues/288
--

module Test.Issues.Issue288 (test_issue288)
  where

import Config
import Test.Framework
import Test.Framework.Providers.HUnit

import Prelude                                                  as P
import Data.Array.Accelerate                                    as A
import Data.Array.Accelerate.Examples.Internal                  as A


test_issue288 :: Backend -> Config -> Test
test_issue288 backend _conf =
  testCase "288" (assertEqual xs $ run1 backend (A.map f) xs)

f :: Exp (Int, Int) -> Exp (Int, Int)
f e = while (const (lift False)) id e

xs :: Vector (Int, Int)
xs = fromList (Z:.10) (P.zip [1..] [1..])

