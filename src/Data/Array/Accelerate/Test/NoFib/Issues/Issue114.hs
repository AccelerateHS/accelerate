{-# LANGUAGE RankNTypes #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Issues.Issue114
-- Copyright   : [2009..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- https://github.com/AccelerateHS/accelerate/issues/114
--

module Data.Array.Accelerate.Test.NoFib.Issues.Issue114 (

  test_issue114

) where

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Test.Similar
import Data.Array.Accelerate.Test.NoFib.Base

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad
import Prelude                                                      as P


test_issue114 :: RunN -> TestTree
test_issue114 runN =
  testGroup "114"
    [ testCase "A"  $ ref1 @~? runN test1
    , testCase "B"  $ ref2 @~? runN test2
    ]


(@~?) :: (Similar a, Show a) => a -> a -> Assertion
expected @~? actual =
  unless (expected ~= actual) $
    assertFailure $ "expected: " P.++ show expected P.++ "\n but got: " P.++ show actual


type EFloat = (Float, Float) -- Represents a real number with a value and error

fromFloat :: Float -> EFloat
fromFloat x = (x, 0)

-- toFloat :: EFloat -> Float
-- toFloat (val, err) = val + err

add :: Exp EFloat -> Exp EFloat -> Exp EFloat
add = lift2 f
    where
        f :: (Exp Float, Exp Float) -> (Exp Float, Exp Float) -> (Exp Float, Exp Float)
        f (aval, aerr) (bval, berr) = (val, err)
            where
                val = aval + bval
                err = aval - (val - det) + (bval - det) + aerr + berr
                det = val - aval

esum :: Acc (Vector EFloat) -> Acc (Scalar EFloat)
esum  x = A.fold1  add x

epsum :: Acc (Vector EFloat) -> Acc (Vector EFloat)
epsum x = A.scanl1 add x

xs :: Acc (Vector EFloat)
xs = use $ fromFunction (Z :. 100) (\_ -> fromFloat 1.01)


ref1 :: Scalar EFloat
ref1 = fromList Z [(101,0)]

test1 :: Acc (Scalar EFloat)
test1 = esum xs


ref2 :: Vector EFloat
ref2 = fromList (Z :. 100) [ (1.01 * i, 0) | i <- [1..]]

test2 :: Acc (Vector EFloat)
test2 = epsum xs

