{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Prelude.Filter
-- Copyright   : [2009..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Test.NoFib.Prelude.Filter (

  test_filter

) where

import Data.Proxy
import Data.Typeable
import Prelude                                                  as P

import Data.Array.Accelerate                                    as A
import Data.Array.Accelerate.Array.Sugar                        as S
import Data.Array.Accelerate.Test.NoFib.Base
import Data.Array.Accelerate.Test.NoFib.Config
import Data.Array.Accelerate.Test.Similar

import Hedgehog

import Test.Tasty
import Test.Tasty.Hedgehog


test_filter :: RunN -> TestTree
test_filter runN =
  testGroup "filter"
    [ at (Proxy::Proxy TestInt8)   $ testIntegralElt i8
    , at (Proxy::Proxy TestInt16)  $ testIntegralElt i16
    , at (Proxy::Proxy TestInt32)  $ testIntegralElt i32
    , at (Proxy::Proxy TestInt64)  $ testIntegralElt i64
    , at (Proxy::Proxy TestWord8)  $ testIntegralElt w8
    , at (Proxy::Proxy TestWord16) $ testIntegralElt w16
    , at (Proxy::Proxy TestWord32) $ testIntegralElt w32
    , at (Proxy::Proxy TestWord64) $ testIntegralElt w64
    , at (Proxy::Proxy TestFloat)  $ testFloatingElt f32
    , at (Proxy::Proxy TestDouble) $ testFloatingElt f64
    ]
  where
    testIntegralElt
        :: forall a. (P.Integral a, A.Integral a, Similar a)
        => Gen a
        -> TestTree
    testIntegralElt e =
      testGroup (show (typeOf (undefined :: a)))
        [ testDim dim1
        , testDim dim2
        , testDim dim3
        ]
      where
        testDim
            :: forall sh. (Shape sh, Slice sh, P.Eq sh)
            => Gen (sh:.Int)
            -> TestTree
        testDim sh =
          testGroup ("DIM" P.++ show (rank (undefined::(sh:.Int))))
            [ testProperty "even"     $ test_even runN sh e
            ]

    testFloatingElt
        :: forall a. (P.Floating a, P.Ord a, A.Floating a, A.Ord a, Similar a)
        => Gen a
        -> TestTree
    testFloatingElt e =
      testGroup (show (typeOf (undefined :: a)))
        [ testDim dim1
        , testDim dim2
        , testDim dim3
        ]
      where
        testDim
            :: forall sh. (Shape sh, Slice sh, P.Eq sh)
            => Gen (sh:.Int)
            -> TestTree
        testDim sh =
          testGroup ("DIM" P.++ show (rank (undefined::(sh:.Int))))
            [ testProperty "positive" $ test_positive runN sh e
            ]


test_even
    :: (Shape sh, Slice sh, Similar e, P.Eq sh, P.Integral e, A.Integral e)
    => RunN
    -> Gen (sh:.Int)
    -> Gen e
    -> Property
test_even runN dim e =
  property $ do
    sh <- forAll dim
    xs <- forAll (array sh e)
    let !go = runN (A.filter A.even) in go xs ~~~ filterRef P.even xs

test_positive
    :: (Shape sh, Slice sh, Similar e, P.Eq sh, P.Num e, P.Ord e, A.Num e, A.Ord e)
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

