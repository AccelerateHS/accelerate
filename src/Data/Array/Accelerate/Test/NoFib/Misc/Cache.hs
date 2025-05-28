{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Misc.Cache
-- Copyright   : [2009..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Test.NoFib.Misc.Cache (
  test_misc_cache
) where

import Prelude as P

import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate ( Acc, Scalar, pattern I1, shape, Z(Z), (:.)((:.)), (!) )
import Data.Array.Accelerate.Test.NoFib.Base

import Test.Tasty
import Test.Tasty.HUnit


-- AST hashing in Analysis.Hash ignores certain parts of the AST when 'perfect'
-- is set to False. This is to prevent "irrelevant" parts of the AST, namely
-- parts that do not get compiled into the kernel, from being taken into
-- account when checking whether a kernel has already been compiled. An example
-- of such an "irrelevant" part is the expression in a Backpermute that
-- computes the target shape; in accelerate-llvm, this is computed on the host
-- outside of the kernel, not on the device.
--
-- However, the code in accelerate-llvm:DAA.LLVM.Compile that computes the free
-- array variables of a combinator to be compiled (to be turned into arguments
-- of the compiled kernel) _does_ traverse those "irrelevant" AST parts. This
-- means that the number of parameters to a compiled kernel can depend on
-- information in the AST that is not included in the hash.
--
-- This leads to segfaults and worse. This test is here to witness this bug.

test_misc_cache :: RunN -> TestTree
test_misc_cache runN =
  testGroup "cache"
    [ testCase "generate" $ casus testGenerate
    , testCase "backpermute" $ casus testBackpermute
    ]
  where
    casus :: (A.Arrays a, Eq a, Show a) => (Acc a, a) -> Assertion
    casus (prog, expected) = expected @=? runN prog

testGenerate :: (Acc (Scalar Float), Scalar Float)
testGenerate =
  ( let a = A.use (A.fromList (Z :. 10) [0..9 :: Float])
        b = A.use (A.fromList (Z :. 10) [10..19 :: Float])
        c = A.compute $ A.generate (let I1 n = shape a ; I1 m = shape b in I1 (n + m))
                       (\(I1 i) -> a ! I1 (i `rem` 10))
        d = A.compute $ A.generate (let I1 n = shape a in I1 (3 * n))
                       (\(I1 i) -> a ! I1 (i `rem` 10))
    in A.zipWith (+) (A.sum c) (A.sum d)
  , A.fromList Z [5 * P.sum [0..9 :: Float]] )

testBackpermute :: (Acc (Scalar Float), Scalar Float)
testBackpermute =
  ( let a = A.use (A.fromList (Z :. 10) [0..9 :: Float])
        b = A.use (A.fromList (Z :. 10) [10..19 :: Float])
        c = A.compute $ A.backpermute (let I1 n = shape a ; I1 m = shape b in I1 (min n m))
                          id a
        d = A.compute $ A.backpermute (let I1 n = shape a in I1 n)
                          id a
    in A.zipWith (+) (A.sum c) (A.sum d)
  , A.fromList Z [2 * P.sum [0..9 :: Float]] )
