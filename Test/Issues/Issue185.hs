{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Test.Issues.Issue185 (test_issue185)
  where

import Config
import Test.Framework
import Test.Framework.Providers.HUnit

import Prelude                                          as P
import Data.Array.Accelerate                            as A
import Data.Array.Accelerate.Examples.Internal          as A


test_issue185 :: Backend -> Config -> Test
test_issue185 backend _conf = testGroup "185"
  [ testCase "A" (assertEqual ref1 (run backend acc1 :: Vector Int))
  , testCase "B" (assertEqual ref2 (run backend acc2 :: Vector Int))
  , testCase "C" (assertEqual ref3 (run backend acc3 :: Vector Int))
  , testCase "D" (assertEqual ref4 (run backend acc4 :: Vector Int))
  , testCase "E" (assertEqual ref5 (run backend acc5 :: Vector Int))
  , testCase "F" (assertEqual ref6 (run backend acc6 :: Vector Int))
  ]


ref1 :: (Elt a, Num a) => Vector a
ref1 = fromList (Z :. 9) [0,1,4,9,0,4,0,6,2]

acc1 :: (Elt a, IsNum a) => Acc (Vector a)
acc1 = A.scatter to over xs
  where
    over        = use [0, 0, 0, 0, 0, 0, 0, 0, 0]
    xs          = use [1, 9, 6, 4, 4, 2, 5]
    to          = use [1, 3, 7, 2, 5, 8]


ref2 :: (Elt a, Num a) => Vector a
ref2 = fromList (Z :. 9) [0,1,0,9,0,0,0,6,0]

acc2 :: (Elt a, IsNum a) => Acc (Vector a)
acc2 = A.scatter to over xs
  where
    over        = use [0, 0, 0, 0, 0, 0, 0, 0, 0]
    xs          = use [1, 9, 6]
    to          = use [1, 3, 7, 2, 5, 8]


ref3 :: (Elt a, Num a) => Vector a
ref3 = fromList (Z :. 9) [0,0,0,0,0,4,0,6,2]

acc3 :: (Elt a, IsNum a) => Acc (Vector a)
acc3 = A.scatterIf to mask p over xs
  where
    over        = use [0, 0, 0, 0, 0, 0, 0, 0, 0]
    to          = use [1, 3, 7, 2, 5, 8]
    xs          = use [1, 9, 6, 4, 4, 2, 5]

    mask :: Acc (Vector Int32)
    mask        = use [3, 4, 9, 2, 7, 5]
    p           = (>* 4)


ref4 :: (Elt a, Num a) => Vector a
ref4 = fromList (Z :. 9) [0,0,0,0,0,0,0,6,0]

acc4 :: (Elt a, IsNum a) => Acc (Vector a)
acc4 = A.scatterIf to mask p over xs
  where
    over        = use [0, 0, 0, 0, 0, 0, 0, 0, 0]
    to          = use [1, 3, 7, 2, 5, 8]
    xs          = use [1, 9, 6]

    mask :: Acc (Vector Int32)
    mask        = use [3, 4, 9, 2, 7, 5]
    p           = (>* 4)


ref5 :: (Elt a, Num a) => Vector a
ref5 = fromList (Z :. 6) [9,4,1,6,2,4]

acc5 :: (Elt a, IsNum a) => Acc (Vector a)
acc5 = A.gather from xs
  where
    from        = use [1, 3, 7, 2, 5, 3]
    xs          = use [1, 9, 6, 4, 4, 2, 0, 1, 2]


ref6 :: (Elt a, Num a) => Vector a
ref6 = fromList (Z :. 6) [6,6,1,6,2,4]

acc6 :: (Elt a, IsNum a) => Acc (Vector a)
acc6 = A.gatherIf from mask p over xs
  where
    over        = use [6, 6, 6, 6, 6, 6]
    from        = use [1, 3, 7, 2, 5, 3]
    xs          = use [1, 9, 6, 4, 4, 2, 0, 1, 2]

    mask :: Acc (Vector Int32)
    mask        = use [3, 4, 9, 2, 7, 5]
    p           = (>* 4)

