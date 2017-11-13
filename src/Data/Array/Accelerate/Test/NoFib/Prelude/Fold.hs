{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Prelude.Fold
-- Copyright   : [2009..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Test.NoFib.Prelude.Fold (

  test_fold,
  test_foldSeg,

) where

import Data.Proxy
import Data.Typeable
import Prelude                                                  as P

import Data.Array.Accelerate                                    as A
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Test.NoFib.Base
import Data.Array.Accelerate.Test.NoFib.Config

import Data.Array.Accelerate.Hedgehog.Similar
import qualified Data.Array.Accelerate.Hedgehog.Gen.Array       as Gen

import Hedgehog
import qualified Hedgehog.Gen                                   as Gen
import qualified Hedgehog.Range                                 as Range

import Test.Tasty
import Test.Tasty.Hedgehog


test_fold :: RunN -> TestTree
test_fold runN =
  testGroup "fold"
    [ at (Proxy::Proxy TestInt8)   $ testElt i8  (Gen.int8   (Range.linearFrom 0 (-1) 1))
    , at (Proxy::Proxy TestInt16)  $ testElt i16 (Gen.int16  (Range.linearFrom 0 (-10) 10))
    , at (Proxy::Proxy TestInt32)  $ testElt i32 (Gen.int32  (Range.linearFrom 0 (-1000) 1000))
    , at (Proxy::Proxy TestInt64)  $ testElt i64 (Gen.int64  (Range.linearFrom 0 (-10000) 10000))
    , at (Proxy::Proxy TestWord8)  $ testElt w8  (Gen.word8  (Range.linear 0 1))
    , at (Proxy::Proxy TestWord16) $ testElt w16 (Gen.word16 (Range.linear 0 10))
    , at (Proxy::Proxy TestWord32) $ testElt w32 (Gen.word32 (Range.linear 0 1000))
    , at (Proxy::Proxy TestWord64) $ testElt w64 (Gen.word64 (Range.linear 0 10000))
    , at (Proxy::Proxy TestFloat)  $ testElt f32 f32
    , at (Proxy::Proxy TestDouble) $ testElt f64 f64
    ]
  where
    testElt :: forall a. (P.Num a, P.Ord a , A.Num a, A.Ord a , Similar a)
        => Gen a
        -> Gen a
        -> TestTree
    testElt e small =
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
            [
              testProperty "sum"              $ test_sum runN sh (return 0) e
            , testProperty "non-neutral sum"  $ test_sum runN sh e e
            , testProperty "non-commutative"  $ test_mss runN sh small
            , testProperty "minimum"          $ test_minimum runN sh e
            , testProperty "maximum"          $ test_maximum runN sh e
            ]


test_foldSeg :: RunN -> TestTree
test_foldSeg runN =
  testGroup "foldSeg"
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
            [
              testProperty "sum"              $ test_segmented_sum runN sh (return 0) e
            , testProperty "non-neutral sum"  $ test_segmented_sum runN sh e e
            , testProperty "minimum"          $ test_segmented_minimum runN sh e
            , testProperty "maximum"          $ test_segmented_maximum runN sh e
            ]


scalar :: Elt e => e -> Scalar e
scalar x = fromFunction Z (const x)

test_sum
    :: (Shape sh, Similar e, P.Eq sh, P.Num e, A.Num e)
    => RunN
    -> Gen (sh:.Int)
    -> Gen e
    -> Gen e
    -> Property
test_sum runN dim z e =
  property $ do
    x  <- forAll z
    sh <- forAll dim
    xs <- forAll (Gen.array sh e)
    let !go = runN (\v -> A.fold (+) (the v)) in go (scalar x) xs ~~~ foldRef (+) x xs

test_mss
    :: (Shape sh, Similar e, P.Eq sh, P.Num e, P.Ord e, A.Num e, A.Ord e)
    => RunN
    -> Gen (sh:.Int)
    -> Gen e
    -> Property
test_mss runN dim e =
  property $ do
    sh <- forAll (dim `except` \(_:.v) -> v P.== 0)
    xs <- forAll (Gen.array sh e)
    let !go = runN maximumSegmentSum in go xs ~~~ maximumSegmentSumRef xs

test_minimum
    :: (Shape sh, Similar e, P.Eq sh, P.Num e, P.Ord e, A.Num e, A.Ord e)
    => RunN
    -> Gen (sh:.Int)
    -> Gen e
    -> Property
test_minimum runN dim e =
  property $ do
    sh <- forAll (dim `except` \(_:.v) -> v P.== 0)
    xs <- forAll (Gen.array sh e)
    let !go = runN A.minimum in go xs ~~~ fold1Ref P.min xs

test_maximum
    :: (Shape sh, Similar e, P.Eq sh, P.Num e, P.Ord e, A.Num e, A.Ord e)
    => RunN
    -> Gen (sh:.Int)
    -> Gen e
    -> Property
test_maximum runN dim e =
  property $ do
    sh <- forAll (dim `except` \(_:.v) -> v P.== 0)
    xs <- forAll (Gen.array sh e)
    let !go = runN A.maximum in go xs ~~~ fold1Ref P.max xs

test_segmented_sum
    :: forall sh e. (Shape sh, Similar e, P.Eq sh, P.Num e, A.Num e)
    => RunN
    -> Gen (sh:.Int)
    -> Gen e
    -> Gen e
    -> Property
test_segmented_sum runN dim z e =
  property $ do
    x       <- forAll z
    sh:.n1  <- forAll dim
    n2      <- forAll (Gen.int (Range.linear 0 64))
    n       <- return (P.min n1 n2) -- don't generate too many segments
    seg     <- forAll (Gen.array (Z:.n) (Gen.int (Range.linear 0 (128 `quot` 2 P.^ (rank (undefined::sh))))))
    xs      <- forAll (Gen.array (sh:.P.sum (toList seg)) e)
    let !go = runN (\v -> A.foldSeg (+) (the v)) in go (scalar x) xs seg ~~~ foldSegRef (+) x xs seg

test_segmented_minimum
    :: forall sh e. (Shape sh, Similar e, P.Eq sh, P.Num e, P.Ord e, A.Num e, A.Ord e)
    => RunN
    -> Gen (sh:.Int)
    -> Gen e
    -> Property
test_segmented_minimum runN dim e =
  property $ do
    sh:.n1  <- forAll dim
    n2      <- forAll (Gen.int (Range.linear 0 64))
    n       <- return (P.min n1 n2) -- don't generate too many segments
    seg     <- forAll (Gen.array (Z:.n) (Gen.int (Range.linear 1 (128 `quot` 2 P.^ (rank (undefined::sh))))))
    xs      <- forAll (Gen.array (sh:.P.sum (toList seg)) e)
    let !go = runN (A.fold1Seg A.min) in go xs seg ~~~ fold1SegRef P.min xs seg

test_segmented_maximum
    :: forall sh e. (Shape sh, Similar e, P.Eq sh, P.Num e, P.Ord e, A.Num e, A.Ord e)
    => RunN
    -> Gen (sh:.Int)
    -> Gen e
    -> Property
test_segmented_maximum runN dim e =
  property $ do
    sh:.n1  <- forAll dim
    n2      <- forAll (Gen.int (Range.linear 0 64))
    n       <- return (P.min n1 n2) -- don't generate too many segments
    seg     <- forAll (Gen.array (Z:.n) (Gen.int (Range.linear 1 (128 `quot` 2 P.^ (rank (undefined::sh))))))
    xs      <- forAll (Gen.array (sh:.P.sum (toList seg)) e)
    let !go = runN (A.fold1Seg A.max) in go xs seg ~~~ fold1SegRef P.max xs seg


-- Reference implementation
-- ------------------------

foldRef
    :: (Shape sh, Elt e)
    => (e -> e -> e)
    -> e
    -> Array (sh :. Int) e
    -> Array sh e
foldRef f z arr =
  let (sh :. n) = arrayShape arr
      sh'       = listToShape . P.map (P.max 1) . shapeToList $ sh
  in  fromList sh' [ foldl f z sub | sub <- splitEvery n (toList arr) ]

fold1Ref
    :: (Shape sh, Elt e)
    => (e -> e -> e)
    -> Array (sh :. Int) e
    -> Array sh e
fold1Ref f arr =
  let (sh :. n) = arrayShape arr
  in  fromList sh [ foldl1 f sub | sub <- splitEvery n (toList arr) ]

foldSegRef
    :: (Shape sh, Elt e)
    => (e -> e -> e)
    -> e
    -> Array (sh :. Int) e
    -> Segments Int
    -> Array (sh :. Int) e
foldSegRef f z arr seg =
  let
      (sh :. n)   = arrayShape arr
      (Z  :. sz)  = arrayShape seg
      seg'        = toList seg
      arr'        = [ foldl f z sec | sub <- splitEvery n (toList arr)
                                    , sec <- splitPlaces seg' sub ]
  in
  fromList (sh :. sz) arr'

fold1SegRef
    :: (Shape sh, Elt e)
    => (e -> e -> e)
    -> Array (sh :. Int) e
    -> Segments Int
    -> Array (sh :. Int) e
fold1SegRef f arr seg =
  let
      (sh :. n)   = arrayShape arr
      (Z  :. sz)  = arrayShape seg
      seg'        = toList seg
      arr'        = [ foldl1 f sec | sub <- splitEvery n (toList arr)
                                   , sec <- splitPlaces seg' sub ]
  in
  fromList (sh :. sz) arr'

maximumSegmentSum
    :: forall sh e. (Shape sh, A.Num e, A.Ord e)
    => Acc (Array (sh :. Int) e)
    -> Acc (Array sh e)
maximumSegmentSum
  = A.map (\v -> let (x,_,_,_) = unlift v :: (Exp e, Exp e, Exp e, Exp e) in x)
  . A.fold1 f
  . A.map g
  where
    f :: (A.Num a, A.Ord a) => Exp (a,a,a,a) -> Exp (a,a,a,a) -> Exp (a,a,a,a)
    f x y =
      let (mssx, misx, mcsx, tsx) = unlift x
          (mssy, misy, mcsy, tsy) = unlift y
      in
      lift ( mssx `A.max` (mssy `A.max` (mcsx+misy))
           , misx `A.max` (tsx+misy)
           , mcsy `A.max` (mcsx+tsy)
           , tsx+tsy
           )

    g :: (A.Num a, A.Ord a) => Exp a -> Exp (a,a,a,a)
    g x = let y = A.max x 0
          in  lift (y,y,y,x)

maximumSegmentSumRef
    :: (P.Num e, P.Ord e, Shape sh, Elt e)
    => Array (sh :. Int) e
    -> Array sh e
maximumSegmentSumRef arr = fromList sh [ go 0 0 sub | sub <- splitEvery n (toList arr) ]
  where
    sh :. n       = arrayShape arr
    --
    go _ v []     = v
    go u v (x:xs) =
      let u' = 0 `P.max` (u+x)
          v' = v `P.max` u'
      in
      go u' v' xs

