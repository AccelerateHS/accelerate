{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Issues.Issue436
-- Copyright   : [2009..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- https://github.com/AccelerateHS/accelerate/issues/436
--

module Data.Array.Accelerate.Test.NoFib.Issues.Issue436 (

  test_issue436

) where

import Data.Array.Accelerate                              as A
import Data.Array.Accelerate.Test.NoFib.Base

import Test.Tasty
import Test.Tasty.HUnit


test_issue436 :: RunN -> TestTree
test_issue436 runN =
  testGroup "436"
    [ testCase "A" $ e1 @=? runN t1
    , testCase "B" $ e2 @=? runN t2
    ]


t1 :: Acc (Vector Bool, Scalar Int)
t1 = test 3 (bools (Z :. 5))

e1 :: (Vector Bool, Scalar Int)
e1 = ( fromList (Z :. 3) [True,False,True]
     , fromList Z [3])

t2 :: Acc (Vector Bool, Vector Int)
t2 = test 3 (bools (Z :. 5 :. 5))

e2 :: (Vector Bool, Vector Int)
e2 = ( fromList (Z :. 15) [True,False,True,False,True,False,True,False,True,False,True,False,True,False,True]
     , fromList (Z :. 5) [3,3,3,3,3]
     )

test :: (Shape sh, Elt e)
     => Int
     -> Acc (Array (sh:.Int) e)
     -> Acc (Vector e, Array sh Int)
test n xs = A.filter (const (constant True)) (A.take (constant n) xs)

bools :: Shape sh => sh -> Acc (Array sh Bool)
bools sh = use $ fromList sh (cycle [True, False])

