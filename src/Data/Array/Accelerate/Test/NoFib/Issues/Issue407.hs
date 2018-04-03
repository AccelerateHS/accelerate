{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists     #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Issues.Issue407
-- Copyright   : [2009..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- https://github.com/AccelerateHS/accelerate/issues/407
-- https://github.com/AccelerateHS/accelerate-llvm/pull/27
--

module Data.Array.Accelerate.Test.NoFib.Issues.Issue407 (

  test_issue407

) where

import Data.Proxy
import Data.Typeable
import Prelude                                                      as P

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Test.NoFib.Base

import Test.Tasty
import Test.Tasty.HUnit


test_issue407 :: RunN -> TestTree
test_issue407 runN =
  testGroup "407"
    [ testElt (Proxy::Proxy Float)
    , testElt (Proxy::Proxy Double)
    ]
  where
    testElt
        :: forall a. (P.Fractional a, A.RealFloat a)
        => Proxy a
        -> TestTree
    testElt _ =
      testGroup (show (typeOf (undefined :: a)))
        [ testCase "isNaN"      $ eNaN @=? runN (A.map A.isNaN) xs
        , testCase "isInfinite" $ eInf @=? runN (A.map A.isInfinite) xs
        ]
        where
          xs :: Vector a
          xs   = [0/0,   -2/0,  -0/0,  0.1,   1/0,   0.5,   5/0]
          eNaN = [True,  False, True,  False, False, False, False]  -- expected: isNaN
          eInf = [False, True,  False, False, True,  False, True]   -- expected: isInfinite

