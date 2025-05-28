{-# LANGUAGE RankNTypes #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Misc.Scanl1
-- Copyright   : [2009..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Test.NoFib.Misc.Scanl1 (
  test_misc_scanl1
) where

import Prelude as P

import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate ( Z(Z), (:.)((:.)) )
import Data.Array.Accelerate.Test.NoFib.Base

import Test.Tasty
import Test.Tasty.HUnit


-- In the PTX backend, __syncthreads was called under non-uniform control flow
-- for a very long time; this issue was greatly exacerbated by the initial
-- implementation of https://github.com/AccelerateHS/accelerate-llvm/pull/97 .
-- This test is to reproduce the resulting failure and prevent it from
-- occurring again.

test_misc_scanl1 :: RunN -> TestTree
test_misc_scanl1 runN =
  testCase "scanl1" $
    let sh@(_ :. innerN) = Z :. 257 :. 31
        intsum n m = m * (m + 1) `quot` 2 - (n - 1) * n `quot` 2  -- == sum [n .. m]
        ref = A.fromFunction sh (\(Z :. i :. j) -> intsum (1 + i*innerN) (1 + i*innerN + j))
        arr = A.fromList sh [1::Int ..]
    in ref @=? runN (A.scanl1 (+)) arr
