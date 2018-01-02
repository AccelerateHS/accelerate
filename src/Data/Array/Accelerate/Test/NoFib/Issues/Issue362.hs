{-# LANGUAGE RankNTypes #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Issues.Issue362
-- Copyright   : [2009..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- https://github.com/AccelerateHS/accelerate-llvm/issues/12
--

module Data.Array.Accelerate.Test.NoFib.Issues.Issue362 (

  test_issue362

) where

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Test.NoFib.Base

import Test.Tasty
import Test.Tasty.HUnit


test_issue362 :: RunN -> TestTree
test_issue362 runN =
  testGroup "362"
    [ testCase "A"  $ e1 @=? runN t1
    , testCase "B"  $ e2 @=? runN t2
    ]

-- should work
e1 :: Vector (Int,Int,Int,Int)
e1 = fromList (Z:.10) [(0,0,0,0),(1,1,1,1),(3,3,3,3),(6,6,6,6),(10,10,10,10),(15,15,15,15),(21,21,21,21),(28,28,28,28),(36,36,36,36),(45,45,45,45)]

t1 :: Acc (Vector (Int,Int,Int,Int))
t1 = A.scanl1 (lift2 f) xs
  where
    xs = use $ fromList (Z:.10) [ (x,x,x,x) | x <- [0..9] ]
    --
    f :: (Exp Int, Exp Int, Exp Int, Exp Int)
      -> (Exp Int, Exp Int, Exp Int, Exp Int)
      -> (Exp Int, Exp Int, Exp Int, Exp Int)
    f (x1,x2,x3,x4) (y1,y2,y3,y4) = (x1+y1,x2+y2,x3+y3,x4+y4)


-- was broken on pascal
e2 :: Vector (Int,Int,Int,Int,Int)
e2 = fromList (Z:.10) [(0,0,0,0,0),(1,1,1,1,1),(3,3,3,3,3),(6,6,6,6,6),(10,10,10,10,10),(15,15,15,15,15),(21,21,21,21,21),(28,28,28,28,28),(36,36,36,36,36),(45,45,45,45,45)]

t2 :: Acc (Vector (Int,Int,Int,Int,Int))
t2 = A.scanl1 (lift2 f) xs
  where
    xs = use $ fromList (Z:.10) [ (x,x,x,x,x) | x <- [0..9] ]
    --
    f :: (Exp Int, Exp Int, Exp Int, Exp Int, Exp Int)
      -> (Exp Int, Exp Int, Exp Int, Exp Int, Exp Int)
      -> (Exp Int, Exp Int, Exp Int, Exp Int, Exp Int)
    f (x1,x2,x3,x4,x5) (y1,y2,y3,y4,y5) = (x1+y1,x2+y2,x3+y3,x4+y4,x5+y5)

