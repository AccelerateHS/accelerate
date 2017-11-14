{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Issues.Issue187
-- Copyright   : [2009..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- https://github.com/AccelerateHS/accelerate/issues/187
--

module Data.Array.Accelerate.Test.NoFib.Issues.Issue187 (

  test_issue187

) where

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Test.NoFib.Base

import Test.Tasty
import Test.Tasty.HUnit


test_issue187 :: RunN -> TestTree
test_issue187 runN =
  testGroup "187"
    [ testCase "A"  $ ref1 @=? runN test1
    , testCase "B"  $ ref2 @=? runN test2
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

