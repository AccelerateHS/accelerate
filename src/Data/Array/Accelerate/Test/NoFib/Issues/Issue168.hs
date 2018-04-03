{-# LANGUAGE RankNTypes #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Issues.Issue168
-- Copyright   : [2009..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- https://github.com/AccelerateHS/accelerate/issues/168
--

module Data.Array.Accelerate.Test.NoFib.Issues.Issue168 (

  test_issue168

) where

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Test.NoFib.Base

import Test.Tasty
import Test.Tasty.HUnit

import Prelude                                                      as P


test_issue168 :: RunN -> TestTree
test_issue168 runN =
  testGroup "168"
    [ testCase "A"  $ ref1 @=? runN (A.fill sh test1)
    , testCase "B"  $ ref2 @=? runN (A.fill sh test2)
    , testCase "C"  $ ref3 @=? runN (A.fill sh test3)
    ]
  where
    sh          = index1 (constant 1) :: Exp DIM1

    -- Test 1
    -- ------
    dqc1 :: (Exp Float, Exp Float)
    dqc1 = (2,1)

    qMult1 :: (Exp Float, Exp Float) -> (Exp Float, Exp Float)
    qMult1 (a1,_) = (3, a1)

    ref1 = fromList (Z:.1) [(3.0,2.0)]

    test1 :: Exp (Float, Float)
    test1 = P.iterate (lift1 qMult1) (lift dqc1) P.!! 1

    ref2 = ref1

    test2 :: Exp (Float, Float)
    test2 = A.iterate (constant 1) (lift1 qMult1) (lift dqc1)

    -- Test 2
    -- ------
    ref3 = fromList (Z:.1) [(1.0,2.0,3.0,4.0,5.0,6.0)]

    dqc3 :: (Exp Float, Exp Float, Exp Float, Exp Float, Exp Float, Exp Float)
    dqc3 = (6,5,4,3,2,1)

    qMult3 :: (Exp Float, Exp Float, Exp Float, Exp Float, Exp Float, Exp Float)
           -> (Exp Float, Exp Float, Exp Float, Exp Float, Exp Float, Exp Float)
    qMult3 (a1,b1,c1,d1,e1,f1) = (f1,e1,d1,c1,b1,a1)

    test3 :: Exp (Float,Float,Float,Float,Float,Float)
    test3 = A.iterate (constant 1) (lift1 qMult3) (lift dqc3)

