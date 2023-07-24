{-# LANGUAGE RankNTypes #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Issues.Issue439
-- Copyright   : [2009..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- https://github.com/AccelerateHS/accelerate/issues/517
--

module Data.Array.Accelerate.Test.NoFib.Issues.Issue517 (

  test_issue517

) where

import Data.Array.Accelerate                              as A
import Data.Array.Accelerate.Data.Semigroup               as A
import Data.Array.Accelerate.Test.NoFib.Base

import Test.Tasty
import Test.Tasty.HUnit


test_issue517 :: RunN -> TestTree
test_issue517 runN
  = testCase "517"
  $ e1 @=? runN t1

type Tup5 a = (a, a, a, a, a)

e1 :: Scalar (Tup5 (Maybe (Max Float)))
e1 = fromList Z [(Nothing, Just 2, Just 3, Just 5, Just 7)]

t1 :: Acc (Scalar (Tup5 (Maybe (Max Float))))
t1 = unit $
  T5 (Nothing_ <> Nothing_)
     (Nothing_ <> Just_ 2)
     (Just_ 3 <> Nothing_)
     (Just_ 4 <> Just_ 5)
     (Just_ 7 <> Just_ 6)

