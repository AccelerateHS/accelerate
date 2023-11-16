{-# LANGUAGE RankNTypes #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Issues.Issue288
-- Copyright   : [2009..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- https://github.com/AccelerateHS/accelerate/issues/288
--

module Data.Array.Accelerate.Test.NoFib.Issues.Issue288 (

  test_issue288

) where

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Test.NoFib.Base

import Test.Tasty
import Test.Tasty.HUnit

import Prelude                                                      as P hiding ( Bool(..) )


test_issue288 :: RunN -> TestTree
test_issue288 runN =
  testCase "288"  $ xs @=? runN (A.map f) xs

f :: Exp (Int, Int) -> Exp (Int, Int)
f e = while (const False) id e

xs :: Vector (Int, Int)
xs = fromList (Z:.10) (P.zip [1..] [1..])

