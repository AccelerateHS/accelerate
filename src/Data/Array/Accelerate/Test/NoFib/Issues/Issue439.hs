{-# LANGUAGE RankNTypes #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Issues.Issue439
-- Copyright   : [2009..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- https://github.com/AccelerateHS/accelerate-llvm/issues/46
--

module Data.Array.Accelerate.Test.NoFib.Issues.Issue439 (

  test_issue439

) where

import Data.Array.Accelerate                              as A
import Data.Array.Accelerate.Test.NoFib.Base

import Test.Tasty
import Test.Tasty.HUnit


test_issue439 :: RunN -> TestTree
test_issue439 runN
  = testCase "439"
  $ e1 @=? runN t1

e1 :: Scalar Float
e1 = fromList Z [2]

t1 :: Acc (Scalar Float)
t1 = compute . A.map (* 2) . compute $ fill Z_ 1

