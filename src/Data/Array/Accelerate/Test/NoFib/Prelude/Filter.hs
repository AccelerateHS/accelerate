{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Prelude.Filter
-- Copyright   : [2009..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Test.NoFib.Prelude.Filter (

  test_filter

) where

import Prelude                                                      as P

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Sugar.Array                            as S
import Data.Array.Accelerate.Sugar.Elt                              as S
import Data.Array.Accelerate.Sugar.Shape                            as S
import Data.Array.Accelerate.Test.NoFib.Base
import Data.Array.Accelerate.Test.NoFib.Config
import Data.Array.Accelerate.Test.Similar

import Hedgehog

import Test.Tasty
import Test.Tasty.Hedgehog


test_filter :: RunN -> TestTree
test_filter runN =
  testGroup "filter"
    [ at @TestInt8   $ testIntegralElt i8
    , at @TestInt16  $ testIntegralElt i16
    , at @TestInt32  $ testIntegralElt i32
    , at @TestInt64  $ testIntegralElt i64
    , at @TestWord8  $ testIntegralElt w8
    , at @TestWord16 $ testIntegralElt w16
    , at @TestWord32 $ testIntegralElt w32
    , at @TestWord64 $ testIntegralElt w64
    , at @TestHalf   $ testFloatingElt f16
    , at @TestFloat  $ testFloatingElt f32
    , at @TestDouble $ testFloatingElt f64
    ]
  where
    testIntegralElt
        :: forall a. (P.Integral a, A.Integral a, Similar a, Show a)
        => Gen a
        -> TestTree
    testIntegralElt e =
      testGroup (show (eltR @a))
        [ testDim dim1
        , testDim dim2
        , testDim dim3
        ]
      where
        testDim
            :: forall sh. (Shape sh, Slice sh, Show sh, P.Eq sh)
            => Gen (sh:.Int)
            -> TestTree
        testDim sh =
          testGroup ("DIM" P.++ show (rank @(sh:.Int)))
            [ testProperty "even"     $ test_even runN sh e
            ]

    testFloatingElt
        :: forall a. (P.Floating a, P.Ord a, A.Floating a, A.Ord a, Similar a, Show a)
        => Gen a
        -> TestTree
    testFloatingElt e =
      testGroup (show (eltR @a))
        [ testDim dim1
        , testDim dim2
        , testDim dim3
        ]
      where
        testDim
            :: forall sh. (Shape sh, Slice sh, Show sh, P.Eq sh)
            => Gen (sh:.Int)
            -> TestTree
        testDim sh =
          testGroup ("DIM" P.++ show (rank @(sh:.Int)))
            [ testProperty "positive" $ test_positive runN sh e
            ]


{-# NOINLINE test_even #-}
test_even
    :: (Shape sh, Slice sh, Show sh, Similar e, Show e, P.Eq sh, P.Integral e, A.Integral e)
    => RunN
    -> Gen (sh:.Int)
    -> Gen e
    -> Property
test_even runN dim e =
  property $ do
    sh <- forAll dim
    xs <- forAll (array sh e)
    let !go = runN (A.filter A.even) in go xs ~~~ filterRef P.even xs

{-# NOINLINE test_positive #-}
test_positive
    :: (Shape sh, Slice sh, Show sh, Similar e, Show e, P.Eq sh, P.Num e, P.Ord e, A.Num e, A.Ord e)
    => RunN
    -> Gen (sh:.Int)
    -> Gen e
    -> Property
test_positive runN dim e =
  property $ do
    sh <- forAll dim
    xs <- forAll (array sh e)
    let !go = runN (A.filter (A.> 0)) in go xs ~~~ filterRef (P.> 0) xs


filterRef
    :: (Shape sh, Elt e)
    => (e -> Bool)
    -> Array (sh:.Int) e
    -> (Vector e, Array sh Int)
filterRef f arr = (fromList (Z:.total) (concat result), fromList sh len)
  where
    sh :. n   = S.shape arr
    result    = P.take (S.size sh) [ P.filter f sub | sub <- splitEvery n (toList arr) ]
    len       = P.map P.length result
    total     = P.sum len

