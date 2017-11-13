{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Prelude.Scan
-- Copyright   : [2009..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Test.NoFib.Prelude.Scan (

  test_scanl, test_scanlSeg,
  test_scanl1, test_scanl1Seg,
  test_scanl', test_scanl'Seg,

  test_scanr, test_scanrSeg,
  test_scanr1, test_scanr1Seg,
  test_scanr', test_scanr'Seg,

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
import qualified Hedgehog.Gen                                   as Gen
import qualified Hedgehog.Range                                 as Range

import Test.Tasty
import Test.Tasty.Hedgehog


test_scanl :: RunN -> TestTree
test_scanl runN =
  testGroup "scanl"
    [ at (Proxy::Proxy TestInt8)   $ testElt i8
    , at (Proxy::Proxy TestInt16)  $ testElt i16
    , at (Proxy::Proxy TestInt32)  $ testElt i32
    , at (Proxy::Proxy TestInt64)  $ testElt i64
    , at (Proxy::Proxy TestWord8)  $ testElt w8
    , at (Proxy::Proxy TestWord16) $ testElt w16
    , at (Proxy::Proxy TestWord32) $ testElt w32
    , at (Proxy::Proxy TestWord64) $ testElt w64
    , at (Proxy::Proxy TestFloat)  $ testElt f32
    , at (Proxy::Proxy TestDouble) $ testElt f64
    ]
  where
    testElt :: forall a. (P.Num a, P.Ord a , A.Num a, A.Ord a , Similar a)
        => Gen a
        -> TestTree
    testElt e =
      testGroup (show (typeOf (undefined :: a)))
        [ testDim dim1
        , testDim dim2
        , testDim dim3
        ]
      where
        testDim
            :: forall sh. (Shape sh, P.Eq sh)
            => Gen (sh:.Int)
            -> TestTree
        testDim sh =
          testGroup ("DIM" P.++ show (rank (undefined::(sh:.Int))))
            [ testProperty "sum"              $ test_scanl_sum runN sh (return 0) e
            , testProperty "non-neutral sum"  $ test_scanl_sum runN sh e e
            , testProperty "non-commutative"  $ test_scanl_interval runN sh e
            ]

test_scanl1 :: RunN -> TestTree
test_scanl1 runN =
  testGroup "scanl1"
    [ at (Proxy::Proxy TestInt8)   $ testElt i8
    , at (Proxy::Proxy TestInt16)  $ testElt i16
    , at (Proxy::Proxy TestInt32)  $ testElt i32
    , at (Proxy::Proxy TestInt64)  $ testElt i64
    , at (Proxy::Proxy TestWord8)  $ testElt w8
    , at (Proxy::Proxy TestWord16) $ testElt w16
    , at (Proxy::Proxy TestWord32) $ testElt w32
    , at (Proxy::Proxy TestWord64) $ testElt w64
    , at (Proxy::Proxy TestFloat)  $ testElt f32
    , at (Proxy::Proxy TestDouble) $ testElt f64
    ]
  where
    testElt :: forall a. (P.Num a, P.Ord a , A.Num a, A.Ord a , Similar a)
        => Gen a
        -> TestTree
    testElt e =
      testGroup (show (typeOf (undefined :: a)))
        [ testDim dim1
        , testDim dim2
        , testDim dim3
        ]
      where
        testDim
            :: forall sh. (Shape sh, P.Eq sh)
            => Gen (sh:.Int)
            -> TestTree
        testDim sh =
          testGroup ("DIM" P.++ show (rank (undefined::(sh:.Int))))
            [ testProperty "sum"              $ test_scanl1_sum runN sh e
            , testProperty "non-commutative"  $ test_scanl1_interval runN sh e
            ]

test_scanl' :: RunN -> TestTree
test_scanl' runN =
  testGroup "scanl'"
    [ at (Proxy::Proxy TestInt8)   $ testElt i8
    , at (Proxy::Proxy TestInt16)  $ testElt i16
    , at (Proxy::Proxy TestInt32)  $ testElt i32
    , at (Proxy::Proxy TestInt64)  $ testElt i64
    , at (Proxy::Proxy TestWord8)  $ testElt w8
    , at (Proxy::Proxy TestWord16) $ testElt w16
    , at (Proxy::Proxy TestWord32) $ testElt w32
    , at (Proxy::Proxy TestWord64) $ testElt w64
    , at (Proxy::Proxy TestFloat)  $ testElt f32
    , at (Proxy::Proxy TestDouble) $ testElt f64
    ]
  where
    testElt :: forall a. (P.Num a, P.Ord a , A.Num a, A.Ord a , Similar a)
        => Gen a
        -> TestTree
    testElt e =
      testGroup (show (typeOf (undefined :: a)))
        [ testDim dim1
        , testDim dim2
        , testDim dim3
        ]
      where
        testDim
            :: forall sh. (Shape sh, P.Eq sh)
            => Gen (sh:.Int)
            -> TestTree
        testDim sh =
          testGroup ("DIM" P.++ show (rank (undefined::(sh:.Int))))
            [ testProperty "sum"              $ test_scanl'_sum runN sh (return 0) e
            , testProperty "non-neutral sum"  $ test_scanl'_sum runN sh e e
            , testProperty "non-commutative"  $ test_scanl'_interval runN sh e
            ]

test_scanr :: RunN -> TestTree
test_scanr runN =
  testGroup "scanr"
    [ at (Proxy::Proxy TestInt8)   $ testElt i8
    , at (Proxy::Proxy TestInt16)  $ testElt i16
    , at (Proxy::Proxy TestInt32)  $ testElt i32
    , at (Proxy::Proxy TestInt64)  $ testElt i64
    , at (Proxy::Proxy TestWord8)  $ testElt w8
    , at (Proxy::Proxy TestWord16) $ testElt w16
    , at (Proxy::Proxy TestWord32) $ testElt w32
    , at (Proxy::Proxy TestWord64) $ testElt w64
    , at (Proxy::Proxy TestFloat)  $ testElt f32
    , at (Proxy::Proxy TestDouble) $ testElt f64
    ]
  where
    testElt :: forall a. (P.Num a, P.Ord a , A.Num a, A.Ord a , Similar a)
        => Gen a
        -> TestTree
    testElt e =
      testGroup (show (typeOf (undefined :: a)))
        [ testDim dim1
        , testDim dim2
        , testDim dim3
        ]
      where
        testDim
            :: forall sh. (Shape sh, P.Eq sh)
            => Gen (sh:.Int)
            -> TestTree
        testDim sh =
          testGroup ("DIM" P.++ show (rank (undefined::(sh:.Int))))
            [ testProperty "sum"              $ test_scanr_sum runN sh (return 0) e
            , testProperty "non-neutral sum"  $ test_scanr_sum runN sh e e
            , testProperty "non-commutative"  $ test_scanr_interval runN sh e
            ]

test_scanr1 :: RunN -> TestTree
test_scanr1 runN =
  testGroup "scanr1"
    [ at (Proxy::Proxy TestInt8)   $ testElt i8
    , at (Proxy::Proxy TestInt16)  $ testElt i16
    , at (Proxy::Proxy TestInt32)  $ testElt i32
    , at (Proxy::Proxy TestInt64)  $ testElt i64
    , at (Proxy::Proxy TestWord8)  $ testElt w8
    , at (Proxy::Proxy TestWord16) $ testElt w16
    , at (Proxy::Proxy TestWord32) $ testElt w32
    , at (Proxy::Proxy TestWord64) $ testElt w64
    , at (Proxy::Proxy TestFloat)  $ testElt f32
    , at (Proxy::Proxy TestDouble) $ testElt f64
    ]
  where
    testElt :: forall a. (P.Num a, P.Ord a , A.Num a, A.Ord a , Similar a)
        => Gen a
        -> TestTree
    testElt e =
      testGroup (show (typeOf (undefined :: a)))
        [ testDim dim1
        , testDim dim2
        , testDim dim3
        ]
      where
        testDim
            :: forall sh. (Shape sh, P.Eq sh)
            => Gen (sh:.Int)
            -> TestTree
        testDim sh =
          testGroup ("DIM" P.++ show (rank (undefined::(sh:.Int))))
            [ testProperty "sum"              $ test_scanr1_sum runN sh e
            , testProperty "non-commutative"  $ test_scanr1_interval runN sh e
            ]

test_scanr' :: RunN -> TestTree
test_scanr' runN =
  testGroup "scanr'"
    [ at (Proxy::Proxy TestInt8)   $ testElt i8
    , at (Proxy::Proxy TestInt16)  $ testElt i16
    , at (Proxy::Proxy TestInt32)  $ testElt i32
    , at (Proxy::Proxy TestInt64)  $ testElt i64
    , at (Proxy::Proxy TestWord8)  $ testElt w8
    , at (Proxy::Proxy TestWord16) $ testElt w16
    , at (Proxy::Proxy TestWord32) $ testElt w32
    , at (Proxy::Proxy TestWord64) $ testElt w64
    , at (Proxy::Proxy TestFloat)  $ testElt f32
    , at (Proxy::Proxy TestDouble) $ testElt f64
    ]
  where
    testElt :: forall a. (P.Num a, P.Ord a , A.Num a, A.Ord a , Similar a)
        => Gen a
        -> TestTree
    testElt e =
      testGroup (show (typeOf (undefined :: a)))
        [ testDim dim1
        , testDim dim2
        , testDim dim3
        ]
      where
        testDim
            :: forall sh. (Shape sh, P.Eq sh)
            => Gen (sh:.Int)
            -> TestTree
        testDim sh =
          testGroup ("DIM" P.++ show (rank (undefined::(sh:.Int))))
            [ testProperty "sum"              $ test_scanr'_sum runN sh (return 0) e
            , testProperty "non-neutral sum"  $ test_scanr'_sum runN sh e e
            , testProperty "non-commutative"  $ test_scanr'_interval runN sh e
            ]

test_scanlSeg :: RunN -> TestTree
test_scanlSeg runN =
  testGroup "scanlSeg"
    [ at (Proxy::Proxy TestInt8)   $ testElt i8
    , at (Proxy::Proxy TestInt16)  $ testElt i16
    , at (Proxy::Proxy TestInt32)  $ testElt i32
    , at (Proxy::Proxy TestInt64)  $ testElt i64
    , at (Proxy::Proxy TestWord8)  $ testElt w8
    , at (Proxy::Proxy TestWord16) $ testElt w16
    , at (Proxy::Proxy TestWord32) $ testElt w32
    , at (Proxy::Proxy TestWord64) $ testElt w64
    , at (Proxy::Proxy TestFloat)  $ testElt f32
    , at (Proxy::Proxy TestDouble) $ testElt f64
    ]
  where
    testElt :: forall a. (P.Num a, P.Ord a , A.Num a, A.Ord a , Similar a)
        => Gen a
        -> TestTree
    testElt e =
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
            [ testProperty "sum"              $ test_scanlSeg_sum runN sh (return 0) e
            , testProperty "non-neutral sum"  $ test_scanlSeg_sum runN sh e e
            ]

test_scanl1Seg :: RunN -> TestTree
test_scanl1Seg runN =
  testGroup "scanl1Seg"
    [ at (Proxy::Proxy TestInt8)   $ testElt i8
    , at (Proxy::Proxy TestInt16)  $ testElt i16
    , at (Proxy::Proxy TestInt32)  $ testElt i32
    , at (Proxy::Proxy TestInt64)  $ testElt i64
    , at (Proxy::Proxy TestWord8)  $ testElt w8
    , at (Proxy::Proxy TestWord16) $ testElt w16
    , at (Proxy::Proxy TestWord32) $ testElt w32
    , at (Proxy::Proxy TestWord64) $ testElt w64
    , at (Proxy::Proxy TestFloat)  $ testElt f32
    , at (Proxy::Proxy TestDouble) $ testElt f64
    ]
  where
    testElt :: forall a. (P.Num a, P.Ord a , A.Num a, A.Ord a , Similar a)
        => Gen a
        -> TestTree
    testElt e =
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
            [ testProperty "sum"  $ test_scanl1Seg_sum runN sh e
            ]

test_scanl'Seg :: RunN -> TestTree
test_scanl'Seg runN =
  testGroup "scanl'Seg"
    [ at (Proxy::Proxy TestInt8)   $ testElt i8
    , at (Proxy::Proxy TestInt16)  $ testElt i16
    , at (Proxy::Proxy TestInt32)  $ testElt i32
    , at (Proxy::Proxy TestInt64)  $ testElt i64
    , at (Proxy::Proxy TestWord8)  $ testElt w8
    , at (Proxy::Proxy TestWord16) $ testElt w16
    , at (Proxy::Proxy TestWord32) $ testElt w32
    , at (Proxy::Proxy TestWord64) $ testElt w64
    , at (Proxy::Proxy TestFloat)  $ testElt f32
    , at (Proxy::Proxy TestDouble) $ testElt f64
    ]
  where
    testElt :: forall a. (P.Num a, P.Ord a , A.Num a, A.Ord a , Similar a)
        => Gen a
        -> TestTree
    testElt e =
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
            [ testProperty "sum"              $ test_scanl'Seg_sum runN sh (return 0) e
            , testProperty "non-neutral sum"  $ test_scanl'Seg_sum runN sh e e
            ]

test_scanrSeg :: RunN -> TestTree
test_scanrSeg runN =
  testGroup "scanrSeg"
    [ at (Proxy::Proxy TestInt8)   $ testElt i8
    , at (Proxy::Proxy TestInt16)  $ testElt i16
    , at (Proxy::Proxy TestInt32)  $ testElt i32
    , at (Proxy::Proxy TestInt64)  $ testElt i64
    , at (Proxy::Proxy TestWord8)  $ testElt w8
    , at (Proxy::Proxy TestWord16) $ testElt w16
    , at (Proxy::Proxy TestWord32) $ testElt w32
    , at (Proxy::Proxy TestWord64) $ testElt w64
    , at (Proxy::Proxy TestFloat)  $ testElt f32
    , at (Proxy::Proxy TestDouble) $ testElt f64
    ]
  where
    testElt :: forall a. (P.Num a, P.Ord a , A.Num a, A.Ord a , Similar a)
        => Gen a
        -> TestTree
    testElt e =
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
            [ testProperty "sum"              $ test_scanrSeg_sum runN sh (return 0) e
            , testProperty "non-neutral sum"  $ test_scanrSeg_sum runN sh e e
            ]

test_scanr1Seg :: RunN -> TestTree
test_scanr1Seg runN =
  testGroup "scanr1Seg"
    [ at (Proxy::Proxy TestInt8)   $ testElt i8
    , at (Proxy::Proxy TestInt16)  $ testElt i16
    , at (Proxy::Proxy TestInt32)  $ testElt i32
    , at (Proxy::Proxy TestInt64)  $ testElt i64
    , at (Proxy::Proxy TestWord8)  $ testElt w8
    , at (Proxy::Proxy TestWord16) $ testElt w16
    , at (Proxy::Proxy TestWord32) $ testElt w32
    , at (Proxy::Proxy TestWord64) $ testElt w64
    , at (Proxy::Proxy TestFloat)  $ testElt f32
    , at (Proxy::Proxy TestDouble) $ testElt f64
    ]
  where
    testElt :: forall a. (P.Num a, P.Ord a , A.Num a, A.Ord a , Similar a)
        => Gen a
        -> TestTree
    testElt e =
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
            [ testProperty "sum"  $ test_scanr1Seg_sum runN sh e
            ]

test_scanr'Seg :: RunN -> TestTree
test_scanr'Seg runN =
  testGroup "scanr'Seg"
    [ at (Proxy::Proxy TestInt8)   $ testElt i8
    , at (Proxy::Proxy TestInt16)  $ testElt i16
    , at (Proxy::Proxy TestInt32)  $ testElt i32
    , at (Proxy::Proxy TestInt64)  $ testElt i64
    , at (Proxy::Proxy TestWord8)  $ testElt w8
    , at (Proxy::Proxy TestWord16) $ testElt w16
    , at (Proxy::Proxy TestWord32) $ testElt w32
    , at (Proxy::Proxy TestWord64) $ testElt w64
    , at (Proxy::Proxy TestFloat)  $ testElt f32
    , at (Proxy::Proxy TestDouble) $ testElt f64
    ]
  where
    testElt :: forall a. (P.Num a, P.Ord a , A.Num a, A.Ord a , Similar a)
        => Gen a
        -> TestTree
    testElt e =
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
            [ testProperty "sum"              $ test_scanr'Seg_sum runN sh (return 0) e
            , testProperty "non-neutral sum"  $ test_scanr'Seg_sum runN sh e e
            ]


scalar :: Elt e => e -> Scalar e
scalar x = fromFunction Z (const x)

test_scanl_sum
    :: (Shape sh, Similar e, P.Eq sh, P.Num e, A.Num e)
    => RunN
    -> Gen (sh:.Int)
    -> Gen e
    -> Gen e
    -> Property
test_scanl_sum runN dim z e =
  property $ do
    x   <- forAll z
    sh  <- forAll dim
    arr <- forAll (array sh e)
    let !go = runN (\v -> A.scanl (+) (the v)) in go (scalar x) arr ~~~ scanlRef (+) x arr

test_scanl1_sum
    :: (Shape sh, Similar e, P.Eq sh, P.Num e, A.Num e)
    => RunN
    -> Gen (sh:.Int)
    -> Gen e
    -> Property
test_scanl1_sum runN dim e =
  property $ do
    sh  <- forAll (dim `except` \v -> S.size v P.== 0)
    arr <- forAll (array sh e)
    let !go = runN (A.scanl1 (+)) in go arr ~~~ scanl1Ref (+) arr

test_scanl'_sum
    :: (Shape sh, Similar e, P.Eq sh, P.Num e, A.Num e)
    => RunN
    -> Gen (sh:.Int)
    -> Gen e
    -> Gen e
    -> Property
test_scanl'_sum runN dim z e =
  property $ do
    x   <- forAll z
    sh  <- forAll dim
    arr <- forAll (array sh e)
    let !go = runN (\v -> A.scanl' (+) (the v)) in go (scalar x) arr ~~~ scanl'Ref (+) x arr

test_scanr_sum
    :: (Shape sh, Similar e, P.Eq sh, P.Num e, A.Num e)
    => RunN
    -> Gen (sh:.Int)
    -> Gen e
    -> Gen e -> Property
test_scanr_sum runN dim z e =
  property $ do
    x   <- forAll z
    sh  <- forAll dim
    arr <- forAll (array sh e)
    let !go = runN (\v -> A.scanr (+) (the v)) in go (scalar x) arr ~~~ scanrRef (+) x arr

test_scanr1_sum
    :: (Shape sh, Similar e, P.Eq sh, P.Num e, A.Num e)
    => RunN
    -> Gen (sh:.Int)
    -> Gen e
    -> Property
test_scanr1_sum runN dim e =
  property $ do
    sh  <- forAll (dim `except` \v -> S.size v P.== 0)
    arr <- forAll (array sh e)
    let !go = runN (A.scanr1 (+)) in go arr ~~~ scanr1Ref (+) arr

test_scanr'_sum
    :: (Shape sh, Similar e, P.Eq sh, P.Num e, A.Num e)
    => RunN
    -> Gen (sh:.Int)
    -> Gen e
    -> Gen e
    -> Property
test_scanr'_sum runN dim z e =
  property $ do
    x   <- forAll z
    sh  <- forAll dim
    arr <- forAll (array sh e)
    let !go = runN (\v -> A.scanr' (+) (the v)) in go (scalar x) arr ~~~ scanr'Ref (+) x arr

test_scanl_interval
    :: (Shape sh, Similar e, P.Eq sh, P.Eq e, P.Num e, A.Eq e, A.Num e)
    => RunN
    -> Gen (sh:.Int)
    -> Gen e
    -> Property
test_scanl_interval runN dim e =
  property $ do
    sh :. n <- forAll (dim `except` \(_:.n) -> n P.== 0)
    let arr  = intervalArray sh n e
    let !go = runN (A.scanl iappend (constant one)) in go arr ~~~ scanlRef iappendRef one arr

test_scanl1_interval
    :: (Shape sh, Similar e, P.Eq sh, P.Eq e, P.Num e, A.Eq e, A.Num e)
    => RunN
    -> Gen (sh:.Int)
    -> Gen e
    -> Property
test_scanl1_interval runN dim e =
  property $ do
    sh :. n <- forAll (dim `except` \v -> S.size v P.== 0)
    let arr  = intervalArray sh n e
    let !go = runN (A.scanl1 iappend) in go arr ~~~ scanl1Ref iappendRef arr

test_scanl'_interval
    :: (Shape sh, Similar e, P.Eq sh, P.Eq e, P.Num e, A.Eq e, A.Num e)
    => RunN
    -> Gen (sh:.Int)
    -> Gen e
    -> Property
test_scanl'_interval runN dim e =
  property $ do
    sh :. n <- forAll (dim `except` \(_:.n) -> n P.== 0)
    let arr  = intervalArray sh n e
    let !go = runN (A.scanl' iappend (constant one)) in go arr ~~~ scanl'Ref iappendRef one arr

test_scanr_interval
    :: (Shape sh, Similar e, P.Eq sh, P.Eq e, P.Num e, A.Eq e, A.Num e)
    => RunN
    -> Gen (sh:.Int)
    -> Gen e
    -> Property
test_scanr_interval runN dim e =
  property $ do
    sh :. n <- forAll (dim `except` \(_:.n) -> n P.== 0)
    let arr  = intervalArray sh n e
    let !go = runN (A.scanr iappend (constant one)) in go arr ~~~ scanrRef iappendRef one arr

test_scanr1_interval
    :: (Shape sh, Similar e, P.Eq sh, P.Eq e, P.Num e, A.Eq e, A.Num e)
    => RunN
    -> Gen (sh:.Int)
    -> Gen e
    -> Property
test_scanr1_interval runN dim e =
  property $ do
    sh :. n <- forAll (dim `except` \v -> S.size v P.== 0)
    let arr  = intervalArray sh n e
    let !go = runN (A.scanr1 iappend) in go arr ~~~ scanr1Ref iappendRef arr

test_scanr'_interval
    :: (Shape sh, Similar e, P.Eq sh, P.Eq e, P.Num e, A.Eq e, A.Num e)
    => RunN
    -> Gen (sh:.Int)
    -> Gen e
    -> Property
test_scanr'_interval runN dim e =
  property $ do
    sh :. n <- forAll (dim `except` \(_:.n) -> n P.== 0)
    let arr  = intervalArray sh n e
    let !go = runN (A.scanr' iappend (constant one)) in go arr ~~~ scanr'Ref iappendRef one arr

test_scanlSeg_sum
    :: forall sh e. (Shape sh, Slice sh, Similar e, P.Eq sh, P.Num e, A.Num e)
    => RunN
    -> Gen (sh:.Int)
    -> Gen e
    -> Gen e
    -> Property
test_scanlSeg_sum runN dim z e =
  property $ do
    x       <- forAll z
    sh:.n1  <- forAll dim
    n2      <- forAll (Gen.int (Range.linear 0 64))
    n       <- return (P.min n1 n2) -- don't generate too many segments
    seg     <- forAll (array (Z:.n) (Gen.int (Range.linear 0 (128 `quot` 2 P.^ (rank (undefined::sh))))))
    arr     <- forAll (array (sh:.P.sum (toList seg)) e)
    let !go = runN (\v -> A.scanlSeg (+) (the v)) in go (scalar x) arr seg ~~~ scanlSegRef (+) x arr seg

test_scanl1Seg_sum
    :: forall sh e. (Shape sh, Slice sh, Similar e, P.Eq sh, P.Num e, A.Num e)
    => RunN
    -> Gen (sh:.Int)
    -> Gen e
    -> Property
test_scanl1Seg_sum runN dim e =
  property $ do
    sh:.n1  <- forAll (dim `except` \v -> S.size v P.== 0)
    n2      <- forAll (Gen.int (Range.linear 1 64))
    n       <- return (P.min n1 n2) -- don't generate too many segments
    seg     <- forAll (array (Z:.n) (Gen.int (Range.linear 1 (128 `quot` 2 P.^ (rank (undefined::sh))))))
    arr     <- forAll (array (sh:.P.sum (toList seg)) e)
    let !go = runN (A.scanl1Seg (+)) in go arr seg ~~~ scanl1SegRef (+) arr seg

test_scanl'Seg_sum
    :: forall sh e. (Shape sh, Slice sh, Similar e, P.Eq sh, P.Num e, A.Num e)
    => RunN
    -> Gen (sh:.Int)
    -> Gen e
    -> Gen e
    -> Property
test_scanl'Seg_sum runN dim z e =
  property $ do
    x       <- forAll z
    sh:.n1  <- forAll dim
    n2      <- forAll (Gen.int (Range.linear 0 64))
    n       <- return (P.min n1 n2) -- don't generate too many segments
    seg     <- forAll (array (Z:.n) (Gen.int (Range.linear 0 (128 `quot` 2 P.^ (rank (undefined::sh))))))
    arr     <- forAll (array (sh:.P.sum (toList seg)) e)
    let !go = runN (\v -> A.scanl'Seg (+) (the v)) in go (scalar x) arr seg ~~~ scanl'SegRef (+) x arr seg

test_scanrSeg_sum
    :: forall sh e. (Shape sh, Slice sh, Similar e, P.Eq sh, P.Num e, A.Num e)
    => RunN
    -> Gen (sh:.Int)
    -> Gen e
    -> Gen e
    -> Property
test_scanrSeg_sum runN dim z e =
  property $ do
    x       <- forAll z
    sh:.n1  <- forAll dim
    n2      <- forAll (Gen.int (Range.linear 0 64))
    n       <- return (P.min n1 n2) -- don't generate too many segments
    seg     <- forAll (array (Z:.n) (Gen.int (Range.linear 0 (128 `quot` 2 P.^ (rank (undefined::sh))))))
    arr     <- forAll (array (sh:.P.sum (toList seg)) e)
    let !go = runN (\v -> A.scanrSeg (+) (the v)) in go (scalar x) arr seg ~~~ scanrSegRef (+) x arr seg

test_scanr1Seg_sum
    :: forall sh e. (Shape sh, Slice sh, Similar e, P.Eq sh, P.Num e, A.Num e)
    => RunN
    -> Gen (sh:.Int)
    -> Gen e
    -> Property
test_scanr1Seg_sum runN dim e =
  property $ do
    sh:.n1  <- forAll (dim `except` \v -> S.size v P.== 0)
    n2      <- forAll (Gen.int (Range.linear 1 64))
    n       <- return (P.min n1 n2) -- don't generate too many segments
    seg     <- forAll (array (Z:.n) (Gen.int (Range.linear 1 (128 `quot` 2 P.^ (rank (undefined::sh))))))
    arr     <- forAll (array (sh:.P.sum (toList seg)) e)
    let !go = runN (A.scanr1Seg (+)) in go arr seg ~~~ scanr1SegRef (+) arr seg

test_scanr'Seg_sum
    :: forall sh e. (Shape sh, Slice sh, Similar e, P.Eq sh, P.Num e, A.Num e)
    => RunN
    -> Gen (sh:.Int)
    -> Gen e
    -> Gen e
    -> Property
test_scanr'Seg_sum runN dim z e =
  property $ do
    x       <- forAll z
    sh:.n1  <- forAll dim
    n2      <- forAll (Gen.int (Range.linear 0 64))
    n       <- return (P.min n1 n2) -- don't generate too many segments
    seg     <- forAll (array (Z:.n) (Gen.int (Range.linear 0 (128 `quot` 2 P.^ (rank (undefined::sh))))))
    arr     <- forAll (array (sh:.P.sum (toList seg)) e)
    let !go = runN (\v -> A.scanr'Seg (+) (the v)) in go (scalar x) arr seg ~~~ scanr'SegRef (+) x arr seg


-- Interval of summations monoid
--
one, top :: P.Num e => (e,e)
one = (-1,-1)
top = (-2,-2)

iappendRef :: (P.Num e, P.Eq e) => (e,e) -> (e,e) -> (e,e)
iappendRef x y
  | x P.== one                 = y
  | y P.== one                 = x
  | x P.== top P.|| y P.== top = top
iappendRef (x1,x2) (y1,y2)
  | x2 + 1 P.== y1             = (x1,y2)
  | otherwise                  = top

iappend :: forall e. (A.Eq e, A.Num e, P.Num e) => Exp (e,e) -> Exp (e,e) -> Exp (e,e)
iappend x y
  = x A.== constant one ? ( y
  , y A.== constant one ? ( x
  , x A.== constant top ? ( constant top -- A.|| y A.== constant top; see AccelerateHS/accelerate#364
  , let
        (x1,x2) = unlift x :: (Exp e, Exp e)
        (y1,y2) = unlift y :: (Exp e, Exp e)
    in
    x2 + 1 A.== y1 ? ( lift (x1,y2) , constant top )
  )))

intervalArray :: (Shape sh, Elt e, P.Num e) => sh -> Int -> proxy e -> Array (sh:.Int) (e,e)
intervalArray sh n _ = fromFunction (sh:.n) (\(_:.i) -> let x = P.fromIntegral i in (x,x))


-- Reference implementation
-- ------------------------

scanlRef
    :: (Shape sh, Elt e)
    => (e -> e -> e)
    -> e
    -> Array (sh:.Int) e
    -> Array (sh:.Int) e
scanlRef f z arr =
  let sz :. n     = arrayShape arr
      arr'        = [ P.scanl f z sub | sub <- splitEvery n (toList arr) ]
  in
  A.fromList (sz :. n+1) (concat arr')

scanl'Ref
    :: (Shape sh, Elt e)
    => (e -> e -> e)
    -> e
    -> Array (sh:.Int) e
    -> (Array (sh:.Int) e, Array sh e)
scanl'Ref f z arr =
  let sz :. n     = arrayShape arr
      (arr',sums) = P.unzip [ P.splitAt n (P.scanl f z sub) | sub <- splitEvery n (toList arr) ]
  in
  ( A.fromList (sz:.n) (concat arr'), A.fromList sz (concat sums) )

scanl1Ref
    :: (Shape sh, Elt e)
    => (e -> e -> e)
    -> Array (sh:.Int) e
    -> Array (sh:.Int) e
scanl1Ref f arr =
  let sz :. n     = arrayShape arr
      arr'        = [ P.scanl1 f sub | sub <- splitEvery n (toList arr) ]
  in
  A.fromList (sz:.n) (concat arr')

scanrRef
    :: (Shape sh, Elt e)
    => (e -> e -> e)
    -> e
    -> Array (sh:.Int) e
    -> Array (sh:.Int) e
scanrRef f z arr =
  let sz :. n     = arrayShape arr
      arr'        = [ P.scanr f z sub | sub <- splitEvery n (toList arr) ]
  in
  A.fromList (sz :. n+1) (concat arr')

scanr'Ref
    :: (Shape sh, Elt e)
    => (e -> e -> e)
    -> e
    -> Array (sh:.Int) e
    -> (Array (sh:.Int) e, Array sh e)
scanr'Ref f z arr =
  let sz :. n     = arrayShape arr
      (sums,arr') = P.unzip [ P.splitAt 1 (P.scanr f z sub) | sub <- splitEvery n (toList arr) ]
  in
  ( A.fromList (sz:.n) (concat arr'), A.fromList sz (concat sums) )

scanr1Ref
    :: (Shape sh, Elt e)
    => (e -> e -> e)
    -> Array (sh:.Int) e
    -> Array (sh:.Int) e
scanr1Ref f arr =
  let sz :. n     = arrayShape arr
      arr'        = [ P.scanr1 f sub | sub <- splitEvery n (toList arr) ]
  in
  A.fromList (sz:.n) (concat arr')


-- segmented operations
--
scanlSegRef
    :: (Shape sh, Elt e)
    => (e -> e -> e)
    -> e
    -> Array (sh:.Int) e
    -> Segments Int
    -> Array (sh:.Int) e
scanlSegRef f z arr seg =
  let
      sz :. n   = arrayShape arr
      seg'      = toList seg
      n'        = P.sum $ P.map (\x -> P.fromIntegral x + 1) seg'
      arr'      = [ P.scanl f z sec | sub <- splitEvery n (toList arr)
                                    , sec <- splitPlaces seg' sub ]
  in
  A.fromList (sz:.n') (concat arr')

scanl1SegRef
    :: (Shape sh, Elt e)
    => (e -> e -> e)
    -> Array (sh:.Int) e
    -> Segments Int
    -> Array (sh:.Int) e
scanl1SegRef f arr seg =
  let
      sz :. n   = arrayShape arr
      seg'      = toList seg
      n'        = P.fromIntegral (P.sum seg')
      arr'      = [ P.scanl1 f sec | sub <- splitEvery n (toList arr)
                                   , sec <- splitPlaces seg' sub ]
  in
  A.fromList (sz:.n') (concat arr')

scanl'SegRef
    :: (Shape sh, Elt e)
    => (e -> e -> e)
    -> e
    -> Array (sh:.Int) e
    -> Segments Int
    -> (Array (sh:.Int) e, Array (sh:.Int) e)
scanl'SegRef f z arr seg =
  let
      sz :. n     = arrayShape arr
      Z  :. s     = arrayShape seg
      scanl'_ v   = P.splitAt (P.length v) (P.scanl f z v)
      (arr',sums) = P.unzip [ scanl'_ sec | sub <- splitEvery n (toList arr)
                                          , sec <- splitPlaces (toList seg) sub ]
  in
  ( A.fromList (sz:.n) (concat arr'), A.fromList (sz:.s) (concat sums) )

scanrSegRef
    :: (Shape sh, Elt e)
    => (e -> e -> e)
    -> e
    -> Array (sh:.Int) e
    -> Segments Int
    -> Array (sh:.Int) e
scanrSegRef f z arr seg =
  let
      sz :. n   = arrayShape arr
      seg'      = toList seg
      n'        = P.sum $ P.map (\x -> P.fromIntegral x + 1) seg'
      arr'      = [ P.scanr f z sec | sub <- splitEvery n (toList arr)
                                    , sec <- splitPlaces seg' sub ]
  in
  A.fromList (sz:.n') (concat arr')

scanr1SegRef
    :: (Shape sh, Elt e)
    => (e -> e -> e)
    -> Array (sh:.Int) e
    -> Segments Int
    -> Array (sh:.Int) e
scanr1SegRef f arr seg =
  let sz :. n   = arrayShape arr
      seg'      = toList seg
      n'        = P.fromIntegral (P.sum seg')
      arr'      = [ P.scanr1 f sec | sub <- splitEvery n (toList arr)
                                   , sec <- splitPlaces seg' sub ]
  in
  A.fromList (sz:.n') (concat arr')

scanr'SegRef
    :: (Shape sh, Elt e)
    => (e -> e -> e)
    -> e
    -> Array (sh:.Int) e
    -> Segments Int
    -> (Array (sh:.Int) e, Array (sh:.Int) e)
scanr'SegRef f z arr seg =
  let
      sz :. n       = arrayShape arr
      Z  :. s       = arrayShape seg
      (sums, arr')  = P.unzip [ P.splitAt 1 (P.scanr f z sec) | sub <- splitEvery n (toList arr)
                                                              , sec <- splitPlaces (toList seg) sub ]
  in
  ( A.fromList (sz:.n) (concat arr'), A.fromList (sz:.s) (concat sums) )

