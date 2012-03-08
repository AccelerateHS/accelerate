{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Reduction where

import Prelude                                          as P
import Data.List
import Data.Typeable
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Config
import Test.Base
import Arbitrary.Array
import Data.Array.Accelerate                            as Acc
import Data.Array.Accelerate.Array.Sugar                as Sugar


test_reduction :: Options -> Test
test_reduction opt =
  testGroup "reduction"
    [
      test_foldAll opt
    , test_fold opt
    , test_foldSeg opt
    ]



-- foldAll
-- -------

test_foldAll :: Options -> Test
test_foldAll opt = testGroup "foldAll"
  [ testElt (undefined :: Float)
  , testElt (undefined :: Int)
  , testElt (undefined :: Int32)
  , testElt (undefined :: Int64)
  ]
  where
    testElt :: forall e. (Elt e, IsNum e, Ord e, Similar e, Arbitrary e) => e -> Test
    testElt _ = testGroup (show (typeOf (undefined :: e)))
      [ testDim dim0
      , testDim dim1
      , testDim dim2
      ]
      where
        testDim :: forall sh. (Shape sh, Arbitrary sh, Arbitrary (Array sh e)) => sh -> Test
        testDim sh = testGroup ("DIM" ++ show (dim sh))
          [
          -- simple tests: sum, minimum, maximum
          --
            testProperty "sum"
          $ \(xs :: Array sh e)
          -> run opt (Acc.foldAll (+) 0 (use xs)) .==. foldAllRef (+) 0 xs

          -- reduction with a non-neutral seed element
          -- lift the seed to an indexed scalar array to avoid recompilation
          --
          , testProperty "non-neutral sum"
          $ \(xs :: Array sh e) z
          -> let z' = unit (constant z)
             in  run opt (Acc.foldAll (+) (the z') (use xs)) .==. foldAllRef (+) z xs

          , testProperty "minimum"
          $ \(xs :: Array sh e)
          -> arraySize (arrayShape xs) > 0
            ==> run opt (Acc.fold1All Acc.min (use xs)) .==. fold1AllRef P.min xs

          , testProperty "maximum"
          $ \(xs :: Array sh e)
          -> arraySize (arrayShape xs) > 0
            ==> run opt (Acc.fold1All Acc.max (use xs)) .==. fold1AllRef P.max xs
          ]


-- multidimensional fold
-- ---------------------

test_fold :: Options -> Test
test_fold opt = testGroup "fold"
  [ testElt (undefined :: Float)
  , testElt (undefined :: Int)
  , testElt (undefined :: Int32)
  , testElt (undefined :: Int64)
  ]
  where
    testElt :: forall e. (Elt e, IsNum e, Ord e, Similar e, Arbitrary e) => e -> Test
    testElt _ = testGroup (show (typeOf (undefined :: e)))
      [ testDim dim1
      , testDim dim2
      ]
      where
        testDim :: forall sh. (Shape sh, Eq sh, Arbitrary sh, Arbitrary (Array (sh:.Int) e)) => (sh:.Int) -> Test
        testDim sh = testGroup ("DIM" ++ show (dim sh))
          [
          -- simple tests: sum, minimum, maximum
            testProperty "sum"
          $ \(xs :: Array (sh:.Int) e)
          -> run opt (Acc.fold (+) 0 (use xs)) .==. foldRef (+) 0 xs

          , testProperty "non-neutral sum"
          $ \(xs :: Array (sh:.Int) e) z
          -> let z' = unit (constant z)
             in  run opt (Acc.fold (+) (the z') (use xs)) .==. foldRef (+) z xs

          , testProperty "minimum"
          $ \(xs :: Array (sh:.Int) e)
          -> indexHead (arrayShape xs) > 0
            ==> run opt (Acc.fold1 Acc.min (use xs)) .==. fold1Ref P.min xs

          , testProperty "maximum"
          $ \(xs :: Array (sh:.Int) e)
          -> indexHead (arrayShape xs) > 0
            ==> run opt (Acc.fold1 Acc.max (use xs)) .==. fold1Ref P.max xs

          ]


-- segmented fold
-- --------------

test_foldSeg :: Options -> Test
test_foldSeg opt = testGroup "foldSeg"
  [ testElt (undefined :: Float)
  ]
  where
    testElt :: forall e. (Elt e, IsNum e, Ord e, Similar e, Arbitrary e) => e -> Test
    testElt _ = testGroup (show (typeOf (undefined :: e)))
      [ testDim dim1
      , testDim dim2
      ]
      where
        testDim :: forall sh. (Shape sh, Eq sh, Arbitrary sh, Arbitrary (Array (sh:.Int) e)) => (sh:.Int) -> Test
        testDim sh = testGroup ("DIM" ++ show (dim sh))
          [
            testProperty "sum"
          $ forAll (arbitrarySegments           :: Gen (Segments Int32))    $ \seg ->
            forAll (arbitrarySegmentedArray seg :: Gen (Array (sh:.Int) e)) $ \xs  ->
              run opt (Acc.foldSeg (+) 0 (use xs) (use seg)) .==. foldSegRef (+) 0 xs seg

          , testProperty "non-neutral sum"
          $ forAll (arbitrarySegments           :: Gen (Segments Int32))    $ \seg ->
            forAll (arbitrarySegmentedArray seg :: Gen (Array (sh:.Int) e)) $ \xs  ->
            forAll arbitrary                                                $ \z   ->
              let z' = unit (constant z)
              in  run opt (Acc.foldSeg (+) (the z') (use xs) (use seg)) .==. foldSegRef (+) z xs seg

          , testProperty "minimum"
          $ forAll (arbitrarySegments1          :: Gen (Segments Int32))    $ \seg ->
            forAll (arbitrarySegmentedArray seg :: Gen (Array (sh:.Int) e)) $ \xs  ->
              run opt (Acc.fold1Seg Acc.min (use xs) (use seg)) .==. fold1SegRef P.min xs seg
          ]


-- Reference implementation
-- ------------------------

foldAllRef :: Elt e => (e -> e -> e) -> e -> Array sh e -> Array Z e
foldAllRef f z
  = Acc.fromList Z
  . return
  . foldl f z
  . Acc.toList


fold1AllRef :: Elt e => (e -> e -> e) -> Array sh e -> Array Z e
fold1AllRef f
  = Acc.fromList Z
  . return
  . foldl1 f
  . Acc.toList

foldRef :: (Shape sh, Elt e) => (e -> e -> e) -> e -> Array (sh :. Int) e -> Array sh e
foldRef f z arr =
  let (sh :. n) = arrayShape arr
  in  fromList sh [ foldl f z sub | sub <- splitEvery n (toList arr) ]

fold1Ref :: (Shape sh, Elt e) => (e -> e -> e) -> Array (sh :. Int) e -> Array sh e
fold1Ref f arr =
  let (sh :. n) = arrayShape arr
  in  fromList sh [ foldl1 f sub | sub <- splitEvery n (toList arr) ]

foldSegRef :: (Shape sh, Elt e, Elt i, Integral i) => (e -> e -> e) -> e -> Array (sh :. Int) e -> Segments i -> Array (sh :. Int) e
foldSegRef f z arr seg = fromList (sh :. sz) $ concat [ foldseg sub | sub <- splitEvery n (toList arr) ]
  where
    (sh :. n)   = arrayShape arr
    (Z  :. sz)  = arrayShape seg
    seg'        = toList seg
    foldseg xs  = P.map (foldl' f z) (split seg' xs)

fold1SegRef :: (Shape sh, Elt e, Elt i, Integral i) => (e -> e -> e) -> Array (sh :. Int) e -> Segments i -> Array (sh :. Int) e
fold1SegRef f arr seg = fromList (sh :. sz) $ concat [ foldseg sub | sub <- splitEvery n (toList arr) ]
  where
    (sh :. n)   = arrayShape arr
    (Z  :. sz)  = arrayShape seg
    seg'        = toList seg
    foldseg xs  = P.map (foldl1' f) (split seg' xs)

split :: Integral i => [i] -> [a] -> [[a]]
split []     _  = []
split (i:is) vs =
  let (h,t) = splitAt (P.fromIntegral i) vs
  in  h : split is t

