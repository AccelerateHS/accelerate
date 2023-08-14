{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Issues.Issue407
-- Copyright   : [2009..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- https://github.com/AccelerateHS/accelerate/issues/407
-- https://github.com/AccelerateHS/accelerate-llvm/pull/27
--

module Data.Array.Accelerate.Test.NoFib.Issues.Issue407 (

  test_issue407

) where

import Prelude                                                      as P hiding ( Bool(..) )

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.AST                                    ( BitOrMask, PrimBool )
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Test.NoFib.Base

import Test.Tasty
import Test.Tasty.HUnit


test_issue407 :: RunN -> TestTree
test_issue407 runN =
  testGroup "407"
    [ testElt @Float
    , testElt @Double
    ]
  where
    testElt
        :: forall a. (Show a, P.Fractional a, A.RealFloat a, BitOrMask (EltR a) ~ PrimBool)
        => TestTree
    testElt =
      let xs :: Vector a
          xs   = [0/0,   -2/0,  -0/0,  0.1,   1/0,   0.5,   5/0]

          eNaN, eInf :: Vector Bool
          eNaN = [True,  False, True,  False, False, False, False]  -- expected: isNaN
          eInf = [False, True,  False, False, True,  False, True]   -- expected: isInfinite
      in
      testGroup (show (eltR @a))
        [ testCase "isNaN"      $ eNaN @=? runN (A.map A.isNaN) xs
        , testCase "isInfinite" $ eInf @=? runN (A.map A.isInfinite) xs
        ]

