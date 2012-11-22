{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Reduction where

import Prelude                                          as P
import Data.List
import Data.Label
import Data.Maybe
import Data.Typeable
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Config
import Test.Base
import Arbitrary.Array
import Data.Array.Accelerate                            as Acc hiding (indexHead, indexTail)
import Data.Array.Accelerate.Array.Sugar                as Sugar



--
-- Reduction -------------------------------------------------------------------
--

-- foldAll
-- -------

test_foldAll :: Options -> Test
test_foldAll opt = testGroup "foldAll" $ catMaybes
  [ testElt int8   (undefined :: Int8)
  , testElt int16  (undefined :: Int16)
  , testElt int32  (undefined :: Int32)
  , testElt int64  (undefined :: Int64)
  , testElt int8   (undefined :: Word8)
  , testElt int16  (undefined :: Word16)
  , testElt int32  (undefined :: Word32)
  , testElt int64  (undefined :: Word64)
  , testElt float  (undefined :: Float)
  , testElt double (undefined :: Double)
  ]
  where
    testElt :: forall e. (Elt e, IsNum e, Ord e, Similar e, Arbitrary e) => (Options :-> Bool) -> e -> Maybe Test
    testElt ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testGroup (show (typeOf (undefined :: e)))
          [ testDim dim0
          , testDim dim1
          , testDim dim2
          ]
      where
        testDim :: forall sh. (Shape sh, Arbitrary sh, Arbitrary (Array sh e)) => sh -> Test
        testDim sh = testGroup ("DIM" ++ show (dim sh))
          [
            testProperty "sum"             (test_sum  :: Array sh e -> Property)
          , testProperty "non-neutral sum" (test_sum' :: Array sh e -> e -> Property)
          , testProperty "minimum"         (test_min  :: Array sh e -> Property)
          , testProperty "maximum"         (test_max  :: Array sh e -> Property)
          ]
    --
    -- The tests
    --
    test_min xs
      =   arraySize (arrayShape xs) > 0
      ==> run opt (Acc.fold1All Acc.min (use xs)) .==. fold1AllRef P.min xs

    test_max xs
      =   arraySize (arrayShape xs) > 0
      ==> run opt (Acc.fold1All Acc.max (use xs)) .==. fold1AllRef P.max xs

    test_sum xs         = run opt (Acc.foldAll (+) 0 (use xs)) .==. foldAllRef (+) 0 xs
    test_sum' xs z      =
      let z' = unit (constant z)
      in  run opt (Acc.foldAll (+) (the z') (use xs)) .==. foldAllRef (+) z xs



-- multidimensional fold
-- ---------------------

test_fold :: Options -> Test
test_fold opt = testGroup "fold" $ catMaybes
  [ testElt int8   (undefined :: Int8)
  , testElt int16  (undefined :: Int16)
  , testElt int32  (undefined :: Int32)
  , testElt int64  (undefined :: Int64)
  , testElt int8   (undefined :: Word8)
  , testElt int16  (undefined :: Word16)
  , testElt int32  (undefined :: Word32)
  , testElt int64  (undefined :: Word64)
  , testElt float  (undefined :: Float)
  , testElt double (undefined :: Double)
  ]
  where
    testElt :: forall e. (Elt e, IsNum e, Ord e, Similar e, Arbitrary e) => (Options :-> Bool) -> e -> Maybe Test
    testElt ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testGroup (show (typeOf (undefined :: e)))
          [ testDim dim1
          , testDim dim2
          ]
      where
        testDim :: forall sh. (Shape sh, Eq sh, Arbitrary sh, Arbitrary (Array (sh:.Int) e)) => (sh:.Int) -> Test
        testDim sh = testGroup ("DIM" ++ show (dim sh))
          [
            testProperty "sum"             (test_sum  :: Array (sh :. Int) e -> Property)
          , testProperty "non-neutral sum" (test_sum' :: Array (sh :. Int) e -> e -> Property)
          , testProperty "minimum"         (test_min  :: Array (sh :. Int) e -> Property)
          , testProperty "maximum"         (test_max  :: Array (sh :. Int) e -> Property)
          ]
    --
    -- The tests
    --
    test_min xs
      =   indexHead (arrayShape xs) > 0
      ==> run opt (Acc.fold1 Acc.min (use xs)) .==. fold1Ref P.min xs

    test_max xs
      =   indexHead (arrayShape xs) > 0
      ==> run opt (Acc.fold1 Acc.max (use xs)) .==. fold1Ref P.max xs

    test_sum xs         = run opt (Acc.fold (+) 0 (use xs)) .==. foldRef (+) 0 xs
    test_sum' xs z      =
      let z' = unit (constant z)
      in  run opt (Acc.fold (+) (the z') (use xs)) .==. foldRef (+) z xs


-- segmented fold
-- --------------

test_foldSeg :: Options -> Test
test_foldSeg opt = testGroup "foldSeg" $ catMaybes
  [ testElt int8   (undefined :: Int8)
  , testElt int16  (undefined :: Int16)
  , testElt int32  (undefined :: Int32)
  , testElt int64  (undefined :: Int64)
  , testElt int8   (undefined :: Word8)
  , testElt int16  (undefined :: Word16)
  , testElt int32  (undefined :: Word32)
  , testElt int64  (undefined :: Word64)
  , testElt float  (undefined :: Float)
  , testElt double (undefined :: Double)
  ]
  where
    testElt :: forall e. (Elt e, IsNum e, Ord e, Similar e, Arbitrary e) => (Options :-> Bool) -> e -> Maybe Test
    testElt ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testGroup (show (typeOf (undefined :: e)))
          [ testDim dim1
          , testDim dim2
          ]
      where
        testDim :: forall sh. (Shape sh, Eq sh, Arbitrary sh, Arbitrary (Array (sh:.Int) e)) => (sh:.Int) -> Test
        testDim sh = testGroup ("DIM" ++ show (dim sh))
          [
            testProperty "sum"
          $ forAll arbitrarySegments             $ \(seg :: Segments Int32)    ->
            forAll (arbitrarySegmentedArray seg) $ \(xs  :: Array (sh:.Int) e) ->
              run opt (Acc.foldSeg (+) 0 (use xs) (use seg)) .==. foldSegRef (+) 0 xs seg

          , testProperty "non-neutral sum"
          $ forAll arbitrarySegments             $ \(seg :: Segments Int32)    ->
            forAll (arbitrarySegmentedArray seg) $ \(xs  :: Array (sh:.Int) e) ->
            forAll arbitrary                     $ \z                          ->
              let z' = unit (constant z)
              in  run opt (Acc.foldSeg (+) (the z') (use xs) (use seg)) .==. foldSegRef (+) z xs seg

          , testProperty "minimum"
          $ forAll arbitrarySegments1            $ \(seg :: Segments Int32)    ->
            forAll (arbitrarySegmentedArray seg) $ \(xs  :: Array (sh:.Int) e) ->
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
    foldseg xs  = P.map (foldl' f z) (splitPlaces seg' xs)

fold1SegRef :: (Shape sh, Elt e, Elt i, Integral i) => (e -> e -> e) -> Array (sh :. Int) e -> Segments i -> Array (sh :. Int) e
fold1SegRef f arr seg = fromList (sh :. sz) $ concat [ foldseg sub | sub <- splitEvery n (toList arr) ]
  where
    (sh :. n)   = arrayShape arr
    (Z  :. sz)  = arrayShape seg
    seg'        = toList seg
    foldseg xs  = P.map (foldl1' f) (splitPlaces seg' xs)

