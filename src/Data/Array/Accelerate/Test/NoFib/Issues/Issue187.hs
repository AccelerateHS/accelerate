{-# LANGUAGE NoImplicitPrelude #-}

module Test.Issues.Issue187 (test_issue187)
  where

import Config
import Test.Framework
import Test.Framework.Providers.HUnit

import Data.Array.Accelerate                                    as A
import Data.Array.Accelerate.Examples.Internal                  as A


test_issue187 :: Backend -> Config -> Test
test_issue187 backend _conf = testGroup "187"
  [ testCase "A" (assertEqual ref1 $ run backend test1)
  , testCase "B" (assertEqual ref2 $ run backend test2)
  ]


ref1 :: Scalar Bool
ref1 = fromList Z [True]

test1 :: Acc (Scalar Bool)
test1 =
  let x = constant 1 :: Exp Int
      v = use (fromList (Z :. 5) [1,2,3,4,5] :: Vector Int)
      y = (x == 1 || v A.!! (-1) == 1)
  in generate (constant Z) (const y)


ref2 :: Scalar Bool
ref2 = fromList Z [True]

test2 :: Acc (Scalar Bool)
test2 =
  let x  = constant 1 :: Exp Int
      x' = unit x
      v  = use (fromList (Z :. 5) [1,2,3,4,5] :: Vector Int)
      y  = (x == the x' || let y' = v A.!! (-1) in y'*y' == 1)
  in generate (constant Z) (const y)

