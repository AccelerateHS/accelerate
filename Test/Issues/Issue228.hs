{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
-- https://github.com/AccelerateHS/accelerate/issues/228
--

module Test.Issues.Issue228 (test_issue228)
  where

import Config
import Test.Framework
import Test.Framework.Providers.HUnit

import Prelude                                                  as P
import Data.Array.Accelerate                                    as A
import Data.Array.Accelerate.Examples.Internal                  as A


test_issue228 :: Backend -> Config -> Test
test_issue228 backend _conf =
  testGroup "228"
    [ testCase "A" (assertEqual ref1 $ run backend (A.fold  mergeExp z1 (use test1)))
    , testCase "B" (assertEqual ref1 $ run backend (A.fold1 mergeExp    (use test1)))
    ]

test1 :: Vector (Int,Int)
test1 = fromList (Z:.3) [(1,1),(0,0),(1,1)]

z1 :: Exp (Int,Int)
z1 = constant (1,0)

ref1 :: Scalar (Int,Int)
ref1 = fromList Z [(0,1)]

{--
testAssociativity =
  quickCheck $ \(x, y, z) -> mergeB x (mergeB y z) == mergeB (mergeB x y) z

mergeB :: (Bool,Int) -> (Bool,Int) -> (Bool,Int)
mergeB (b1,l1) (b2,l2) = (toEnum v3,l3)
  where
    (v3,l3) = merge (fromEnum b1,l1) (fromEnum b2,l2)
--}

merge :: P.Num a => (a,a) -> (a,a) -> (a,a)
merge (onL, lenL) (onR, lenR) = (onL * onR, onL * lenR + lenL)

mergeExp :: Exp (Int,Int) -> Exp (Int,Int) -> Exp (Int,Int)
mergeExp e1 e2 =
  let
    v1 = unlift e1 :: (Exp Int,Exp Int)
    v2 = unlift e2 :: (Exp Int,Exp Int)
  in
  lift $ merge v1 v2

