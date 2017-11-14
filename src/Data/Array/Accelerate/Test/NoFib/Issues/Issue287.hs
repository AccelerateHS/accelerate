{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Issues.Issue287
-- Copyright   : [2009..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- https://github.com/AccelerateHS/accelerate/issues/287
-- https://gist.github.com/cpdurham/7c11134bc345f12a8863
--

module Data.Array.Accelerate.Test.NoFib.Issues.Issue287 (

  test_issue287

) where

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Test.NoFib.Base

import Test.Tasty
import Test.Tasty.HUnit


test_issue287 :: RunN -> TestTree
test_issue287 runN =
  testGroup "287"
    [ testCase "A"  $ ref1 @=? runN (\x -> A.scanl1 f x) arr1
    , testCase "B"  $ ref1 @=? runN (\x -> A.scanl1Seg (\_ b -> b) x (use segs)) arr1
    , testCase "C"  $ ref1 @=? runN (\x -> A.scanl1Seg f x (use segs)) arr1
    ]

ref1 :: Vector (Int,Int,Int,Int,Int,Int)
ref1 = fromList (Z:.4) [(0,0,0,0,0,0),(0,0,0,0,0,0),(0,0,0,0,0,0),(0,0,0,0,0,0)]

arr1 :: Vector (Int,Int,Int,Int,Int,Int)
arr1 = fromList (Z:.4) [(0,0,0,0,0,0),(0,0,0,0,0,0),(0,0,0,0,0,0),(0,0,0,0,0,0)]

segs :: Segments Int
segs = A.fromList (Z:.2) [2,2]

f :: forall a. (A.Num a, A.Ord a)
  => Exp (a,a,a,a,a,a)
  -> Exp (a,a,a,a,a,a)
  -> Exp (a,a,a,a,a,a)
f x y = lift (mi3,l3,c3,r3,li3,ri3)
  where
    ( mi1,l1, c1,r1, li1,ri1) = unlift x :: (Exp a, Exp a, Exp a, Exp a, Exp a, Exp a)
    (_mi2,l2,_c2,r2,_li2,ri2) = unlift y :: (Exp a, Exp a, Exp a, Exp a, Exp a, Exp a)
    --
    l3  = l1
    c3  = c1
    r3  = r2 * r1
    mi3 = A.max mi1 (ri1+l2)
    li3 = li1
    ri3 = ri2

-- fExp
--   :: Exp (Int,Int,Int,Int,Int,Int)
--   -> Exp (Int,Int,Int,Int,Int,Int)
--   -> Exp (Int,Int,Int,Int,Int,Int)
-- fExp e1 e2 =
--     let
--         v1 = unlift6E e1
--         v2 = unlift6E e2
--     in
--       lift $ f v1 v2

-- unlift6E
--   :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f)
--   => Exp (a,b,c,d,e,f)
--   -> (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f)
-- unlift6E = unlift

-- lift6E
--   :: (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f)
--   => (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f)
--   -> Exp (a,b,c,d,e,f)
-- lift6E = lift

-- convert
--   :: Acc (Array DIM1 Int)
--   -> Acc (Array DIM1 (Int,Int,Int,Int,Int,Int))
-- convert = A.map (\_ -> lift6E (0,0,0,0,0,0))

-- arr2 = I.run1 convert (A.fromList (Z :. 4 :: DIM1) ([1,1,1,1]) :: Array DIM1 Int)

-- print $ C.run1 (\x -> A.scanl1 fExp x) $ arr2
-- --no problem

-- print $ C.run1 (\x -> A.scanl1Seg (\_ b -> b) x segs) $ arr2
-- --no problem

-- print $ C.run1 (\x -> A.scanl1Seg fExp x segs) $ arr2
-- -- CUDA exception: invalid argument

