{-# LANGUAGE RankNTypes #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Issues.Issue93
-- Copyright   : [2009..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- https://github.com/AccelerateHS/accelerate/issues/93
--

module Data.Array.Accelerate.Test.NoFib.Issues.Issue93 (

  test_issue93

) where

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Test.NoFib.Base

import Test.Tasty
import Test.Tasty.HUnit


test_issue93 :: RunN -> TestTree
test_issue93 runN =
  testCase "93" $ xs @=? runN test1

xs :: Array DIM2 Int
xs = fromList (Z :. 1 :. 1) [5]

test1 :: Acc (Array DIM2 Int)
test1 = permute (\c _ -> c) (fill (shape xs') (constant 0)) id xs'
  where
    xs' = use xs

