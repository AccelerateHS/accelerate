
module Test.Issues.Issue123 (test_issue123)
  where

import Config
import ParseArgs
import Test.Base
import Test.Framework
import Test.Framework.Providers.HUnit

import Prelude                                  as P
import Data.Array.Accelerate                    as A
import Data.Label


test_issue123 :: Config -> Test
test_issue123 conf =
  testCase "123" (assertEqual ref1 $ run backend (test1 n))
  where
    backend     = get configBackend conf
    n           = 100
    ref1        = fromList Z [n]


test1 :: Int -> Acc (Scalar Int)
test1 n
  = fold (+) 0
  $ fill (constant (Z:.n)) 1

