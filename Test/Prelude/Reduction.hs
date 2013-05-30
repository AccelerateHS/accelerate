{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Test.Prelude.Reduction (

  test_fold,
  test_foldAll,
  test_foldSeg,

) where

import Prelude                                          as P
import Data.List
import Data.Label
import Data.Maybe
import Data.Typeable
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Data.Array.Accelerate                            as A hiding (indexHead, indexTail)
import Data.Array.Accelerate.Array.Sugar                as Sugar

import Config
import ParseArgs
import Test.Base
import QuickCheck.Arbitrary.Array



--
-- Reduction -------------------------------------------------------------------
--

-- foldAll
-- -------

test_foldAll :: Config -> Test
test_foldAll opt = testGroup "foldAll" $ catMaybes
  [ testElt configInt8   (undefined :: Int8)
  , testElt configInt16  (undefined :: Int16)
  , testElt configInt32  (undefined :: Int32)
  , testElt configInt64  (undefined :: Int64)
  , testElt configWord8  (undefined :: Word8)
  , testElt configWord16 (undefined :: Word16)
  , testElt configWord32 (undefined :: Word32)
  , testElt configWord64 (undefined :: Word64)
  , testElt configFloat  (undefined :: Float)
  , testElt configDouble (undefined :: Double)
  ]
  where
    testElt :: forall e. (Elt e, IsNum e, Ord e, Similar e, Arbitrary e) => (Config :-> Bool) -> e -> Maybe Test
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
      ==> run backend (A.fold1All A.min (use xs)) ~?= fold1AllRef P.min xs

    test_max xs
      =   arraySize (arrayShape xs) > 0
      ==> run backend (A.fold1All A.max (use xs)) ~?= fold1AllRef P.max xs

    test_sum xs         = run backend (A.foldAll (+) 0 (use xs)) ~?= foldAllRef (+) 0 xs
    test_sum' xs z      =
      let z' = unit (constant z)
      in  run backend (A.foldAll (+) (the z') (use xs)) ~?= foldAllRef (+) z xs

    backend = get configBackend opt


-- multidimensional fold
-- ---------------------

test_fold :: Config -> Test
test_fold opt = testGroup "fold" $ catMaybes
  [ testElt configInt8   (undefined :: Int8)
  , testElt configInt16  (undefined :: Int16)
  , testElt configInt32  (undefined :: Int32)
  , testElt configInt64  (undefined :: Int64)
  , testElt configWord8  (undefined :: Word8)
  , testElt configWord16 (undefined :: Word16)
  , testElt configWord32 (undefined :: Word32)
  , testElt configWord64 (undefined :: Word64)
  , testElt configFloat  (undefined :: Float)
  , testElt configDouble (undefined :: Double)
  ]
  where
    testElt :: forall e. (Elt e, IsNum e, Ord e, Similar e, Arbitrary e) => (Config :-> Bool) -> e -> Maybe Test
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
      ==> run backend (A.fold1 A.min (use xs)) ~?= fold1Ref P.min xs

    test_max xs
      =   indexHead (arrayShape xs) > 0
      ==> run backend (A.fold1 A.max (use xs)) ~?= fold1Ref P.max xs

    test_sum xs         = run backend (A.fold (+) 0 (use xs)) ~?= foldRef (+) 0 xs
    test_sum' xs z      =
      let z' = unit (constant z)
      in  run backend (A.fold (+) (the z') (use xs)) ~?= foldRef (+) z xs

    backend = get configBackend opt


-- segmented fold
-- --------------

test_foldSeg :: Config -> Test
test_foldSeg opt = testGroup "foldSeg" $ catMaybes
  [ testElt configInt8   (undefined :: Int8)
  , testElt configInt16  (undefined :: Int16)
  , testElt configInt32  (undefined :: Int32)
  , testElt configInt64  (undefined :: Int64)
  , testElt configWord8  (undefined :: Word8)
  , testElt configWord16 (undefined :: Word16)
  , testElt configWord32 (undefined :: Word32)
  , testElt configWord64 (undefined :: Word64)
  , testElt configFloat  (undefined :: Float)
  , testElt configDouble (undefined :: Double)
  ]
  where
    testElt :: forall e. (Elt e, IsNum e, Ord e, Similar e, Arbitrary e) => (Config :-> Bool) -> e -> Maybe Test
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
              run backend (A.foldSeg (+) 0 (use xs) (use seg)) ~?= foldSegRef (+) 0 xs seg

          , testProperty "non-neutral sum"
          $ forAll arbitrarySegments             $ \(seg :: Segments Int32)    ->
            forAll (arbitrarySegmentedArray seg) $ \(xs  :: Array (sh:.Int) e) ->
            forAll arbitrary                     $ \z                          ->
              let z' = unit (constant z)
              in  run backend (A.foldSeg (+) (the z') (use xs) (use seg)) ~?= foldSegRef (+) z xs seg

          , testProperty "minimum"
          $ forAll arbitrarySegments1            $ \(seg :: Segments Int32)    ->
            forAll (arbitrarySegmentedArray seg) $ \(xs  :: Array (sh:.Int) e) ->
              run backend (A.fold1Seg A.min (use xs) (use seg)) ~?= fold1SegRef P.min xs seg
          ]

    backend = get configBackend opt


-- Reference implementation
-- ------------------------

foldAllRef :: Elt e => (e -> e -> e) -> e -> Array sh e -> Array Z e
foldAllRef f z
  = A.fromList Z
  . return
  . foldl f z
  . A.toList

fold1AllRef :: Elt e => (e -> e -> e) -> Array sh e -> Array Z e
fold1AllRef f
  = A.fromList Z
  . return
  . foldl1 f
  . A.toList

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

