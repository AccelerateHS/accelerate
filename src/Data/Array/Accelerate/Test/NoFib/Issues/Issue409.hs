{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Issues.Issue409
-- Copyright   : [2009..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- https://github.com/AccelerateHS/accelerate/issues/409
--

module Data.Array.Accelerate.Test.NoFib.Issues.Issue409 (

  test_issue409

) where

import Data.Typeable
import Prelude                                                      as P

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Test.NoFib.Base

import Test.Tasty
import Test.Tasty.HUnit


test_issue409 :: RunN -> TestTree
test_issue409 runN =
  testGroup "409"
    [ testElt @Float
    , testElt @Double
    ]
  where
    testElt
        :: forall a. (P.Floating a, P.Eq a, A.Floating a)
        => TestTree
    testElt =
      testGroup (show (typeOf (undefined :: a)))
        [ testCase "A" $ e1 @=? indexArray (runN (A.map f) t1) Z
        ]
      where
        e1 :: a
        e1 = 1 + tanh (-1)

        t1 :: Scalar a
        t1 = fromList Z [1]

        f :: Exp a -> Exp a
        f x = let y = recip x
                  b = (-y) * y
              in
              y + tanh b

