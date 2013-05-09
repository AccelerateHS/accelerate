{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module QuickCheck.Test.PrefixSum where

import Prelude                                          as P
import Test.QuickCheck
import Data.Label
import Data.Maybe
import Data.Typeable
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Config
import ParseArgs
import QuickCheck.Test.Base
import QuickCheck.Arbitrary.Array
import Data.Array.Accelerate                            as Acc


--
-- prefix sum ------------------------------------------------------------------
--

test_prefixsum :: Config -> Test
test_prefixsum opt = testGroup "prefix sum" $ catMaybes
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
      | P.not (get ok opt)  = Nothing
      | otherwise           = Just $ testGroup (show (typeOf (undefined :: e)))
          [ testProperty "scanl"        (test_scanl  :: Vector e -> Property)
          , testProperty "scanl'"       (test_scanl' :: Vector e -> Property)
          , testProperty "scanl1"       (test_scanl1 :: Vector e -> Property)
          , testProperty "scanr"        (test_scanr  :: Vector e -> Property)
          , testProperty "scanr'"       (test_scanr' :: Vector e -> Property)
          , testProperty "scanr1"       (test_scanr1 :: Vector e -> Property)
          --
          , testProperty "scanl1Seg"    (test_scanl1seg :: Vector e -> Property)
          , testProperty "scanr1Seg"    (test_scanr1seg :: Vector e -> Property)
          , testProperty "scanlSeg"     (test_scanlseg  :: Vector e -> Property)
          , testProperty "scanrSeg"     (test_scanrseg  :: Vector e -> Property)
          , testProperty "scanl'Seg"    (test_scanl'seg :: Vector e -> Property)
          , testProperty "scanr'Seg"    (test_scanr'seg :: Vector e -> Property)
          ]

    backend = get configBackend opt

    -- left scan
    --
    test_scanl  xs = run backend (Acc.scanl (+) 0 (use xs))             .==. scanlRef (+) 0 xs
    test_scanl1 xs = run backend (Acc.scanl1 Acc.min (use xs))          .==. scanl1Ref P.min xs
    test_scanl' xs = run backend (Acc.lift $ Acc.scanl' (+) 0 (use xs)) .==. scanl'Ref (+) 0 xs

    -- right scan
    --
    test_scanr  xs = run backend (Acc.scanr (+) 0 (use xs))             .==. scanrRef (+) 0 xs
    test_scanr1 xs = run backend (Acc.scanr1 Acc.max (use xs))          .==. scanr1Ref P.max xs
    test_scanr' xs = run backend (Acc.lift $ Acc.scanr' (+) 0 (use xs)) .==. scanr'Ref (+) 0 xs

    -- segmented left/right scan
    --
    test_scanl1seg elt =
      forAll arbitrarySegments1            $ \(seg :: Vector Int32) ->
      forAll (arbitrarySegmentedArray seg) $ \xs  ->
        run backend (Acc.scanl1Seg (+) (use xs) (use seg)) .==. scanl1SegRef (+) (xs `asTypeOf` elt) seg

    test_scanr1seg elt =
      forAll arbitrarySegments1            $ \(seg :: Vector Int32) ->
      forAll (arbitrarySegmentedArray seg) $ \xs  ->
        run backend (Acc.scanr1Seg (+) (use xs) (use seg)) .==. scanr1SegRef (+) (xs `asTypeOf` elt) seg

    test_scanlseg elt =
      forAll arbitrarySegments             $ \(seg :: Vector Int32) ->
      forAll (arbitrarySegmentedArray seg) $ \xs  ->
        run backend (Acc.scanlSeg (+) 0 (use xs) (use seg)) .==. scanlSegRef (+) 0 (xs `asTypeOf` elt) seg

    test_scanrseg elt =
      forAll arbitrarySegments             $ \(seg :: Vector Int32) ->
      forAll (arbitrarySegmentedArray seg) $ \xs  ->
        run backend (Acc.scanrSeg (+) 0 (use xs) (use seg)) .==. scanrSegRef (+) 0 (xs `asTypeOf` elt) seg

    test_scanl'seg elt =
      forAll arbitrarySegments             $ \(seg :: Vector Int32) ->
      forAll (arbitrarySegmentedArray seg) $ \xs  ->
        run backend (lift $ Acc.scanl'Seg (+) 0 (use xs) (use seg)) .==. scanl'SegRef (+) 0 (xs `asTypeOf` elt) seg

    test_scanr'seg elt =
      forAll arbitrarySegments             $ \(seg :: Vector Int32) ->
      forAll (arbitrarySegmentedArray seg) $ \xs  ->
        run backend (lift $ Acc.scanr'Seg (+) 0 (use xs) (use seg)) .==. scanr'SegRef (+) 0 (xs `asTypeOf` elt) seg


-- Reference implementation
-- ------------------------

scanlRef :: Elt e => (e -> e -> e) -> e -> Vector e -> Vector e
scanlRef f z vec =
  let (Z :. n)  = arrayShape vec
  in  Acc.fromList (Z :. n+1) . P.scanl f z . Acc.toList $ vec

scanl'Ref :: Elt e => (e -> e -> e) -> e -> Vector e -> (Vector e, Scalar e)
scanl'Ref f z vec =
  let (Z :. n)  = arrayShape vec
      result    = P.scanl f z (Acc.toList vec)
  in  (Acc.fromList (Z :. n) result, Acc.fromList Z (P.drop n result))

scanl1Ref :: Elt e => (e -> e -> e) -> Vector e -> Vector e
scanl1Ref f vec
  = Acc.fromList (arrayShape vec)
  . P.scanl1 f
  . Acc.toList $ vec

scanrRef :: Elt e => (e -> e -> e) -> e -> Vector e -> Vector e
scanrRef f z vec =
  let (Z :. n)  = arrayShape vec
  in  Acc.fromList (Z :. n+1) . P.scanr f z . Acc.toList $ vec

scanr'Ref :: Elt e => (e -> e -> e) -> e -> Vector e -> (Vector e, Scalar e)
scanr'Ref f z vec =
  let (Z :. n)  = arrayShape vec
      result    = P.scanr f z (Acc.toList vec)
  in  (Acc.fromList (Z :. n) (P.tail result), Acc.fromList Z result)

scanr1Ref :: Elt e => (e -> e -> e) -> Vector e -> Vector e
scanr1Ref f vec
  = Acc.fromList (arrayShape vec)
  . P.scanr1 f
  . Acc.toList $ vec


-- segmented operations
--
scanlSegRef :: (Elt e, Integral i) => (e -> e -> e) -> e -> Vector e -> Vector i -> Vector e
scanlSegRef f z vec seg =
  let seg'      = toList seg
      vec'      = toList vec
      n         = P.sum $ P.map (\x -> P.fromIntegral x + 1) seg'
  in  fromList (Z :. n) $
        concat [ P.scanl f z v | v <- splitPlaces seg' vec' ]

scanl1SegRef :: (Elt e, Integral i) => (e -> e -> e) -> Vector e -> Vector i -> Vector e
scanl1SegRef f vec seg =
  let seg'      = toList seg
      vec'      = toList vec
      n         = P.sum $ P.map P.fromIntegral seg'
  in  fromList (Z :. n) $
        concat [ P.scanl1 f v | v <- splitPlaces seg' vec' ]

scanl'SegRef :: (Elt e, Integral i) => (e -> e -> e) -> e -> Vector e -> Vector i -> (Vector e, Vector e)
scanl'SegRef f z vec seg =
  let seg'              = toList seg
      vec'              = toList vec
      scanl'_ v         = let res = P.scanl f z v in (P.init res, P.last res)
      (scans, sums)     = P.unzip [ scanl'_ v | v <- splitPlaces seg' vec']
  in  ( fromList (arrayShape vec) (concat scans)
      , fromList (arrayShape seg) sums )

scanrSegRef :: (Elt e, Integral i) => (e -> e -> e) -> e -> Vector e -> Vector i -> Vector e
scanrSegRef f z vec seg =
  let seg'      = toList seg
      vec'      = toList vec
      n         = P.sum $ P.map (\x -> P.fromIntegral x + 1) seg'
  in  fromList (Z :. n) $
        concat [ P.scanr f z v | v <- splitPlaces seg' vec' ]

scanr1SegRef :: (Elt e, Integral i) => (e -> e -> e) -> Vector e -> Vector i -> Vector e
scanr1SegRef f vec seg =
  let seg'      = toList seg
      vec'      = toList vec
      n         = P.sum $ P.map P.fromIntegral seg'
  in  fromList (Z :. n) $
        concat [ P.scanr1 f v | v <- splitPlaces seg' vec' ]

scanr'SegRef :: (Elt e, Integral i) => (e -> e -> e) -> e -> Vector e -> Vector i -> (Vector e, Vector e)
scanr'SegRef f z vec seg =
  let seg'              = toList seg
      vec'              = toList vec
      scanr'_ v         = let res = P.scanr f z v in (P.tail res, P.head res)
      (scans, sums)     = P.unzip [ scanr'_ v | v <- splitPlaces seg' vec']
  in  ( fromList (arrayShape vec) (concat scans)
      , fromList (arrayShape seg) sums )

