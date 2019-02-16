{-# LANGUAGE RankNTypes #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Issues.Issue123
-- Copyright   : [2009..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- https://github.com/AccelerateHS/accelerate/issues/123
--

module Data.Array.Accelerate.Test.NoFib.Issues.Issue123 (

  test_issue123

) where

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Test.NoFib.Base

import Test.Tasty
import Test.Tasty.HUnit


test_issue123 :: RunN -> TestTree
test_issue123 runN =
  testCase "123"  $ ref1 @=? runN (test1 n)
  where
    n     = 100
    ref1  = fromList Z [n]


test1 :: Int -> Acc (Scalar Int)
test1 n
  = fold (+) 0
  $ fill (constant (Z:.n)) 1

