
module Test.Issues.Issue93 (test_issue93)
  where

import Config
import Test.Framework
import Test.Framework.Providers.HUnit

import Prelude                                                  as P
import Data.Array.Accelerate                                    as A
import Data.Array.Accelerate.Examples.Internal                  as A


test_issue93 :: Backend -> Config -> Test
test_issue93 backend _conf =
  testCase "93" (assertEqual xs $ run backend test1)


xs :: Array DIM2 Int
xs = fromList (Z :. 1 :. 1) [5]

test1 :: Acc (Array DIM2 Int)
test1 = permute (\c _ -> c) (fill (shape xs') (constant 0)) id xs'
  where
    xs' = use xs

