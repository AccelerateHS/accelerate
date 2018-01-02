{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Issues.Issue409
-- Copyright   : [2009..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- https://github.com/AccelerateHS/accelerate/issues/409
--

module Data.Array.Accelerate.Test.NoFib.Issues.Issue409 (

  test_issue409

) where

import Data.Proxy
import Data.Typeable
import Prelude                                                      as P

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Test.NoFib.Base

import Test.Tasty
import Test.Tasty.HUnit


test_issue409 :: RunN -> TestTree
test_issue409 runN =
  testGroup "409"
    [ testElt (Proxy::Proxy Float)
    , testElt (Proxy::Proxy Double)
    ]
  where
    testElt
        :: forall a. (P.Floating a, P.Eq a, A.Floating a)
        => Proxy a
        -> TestTree
    testElt _ =
      testGroup (show (typeOf (undefined :: a)))
        [ testCase "A" $ e1 @=? indexArray (runN (A.map f) t1) Z
        ]
      where
        e1 :: a
        e1 = 1 + tanh (-1)

        t1 :: Scalar a
        t1 = fromList Z [1]

        f :: A.Floating a => Exp a -> Exp a
        f x = let y = recip x
                  b = (-y) * y
              in
              y + tanh b

