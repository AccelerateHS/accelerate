{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Test.Prelude.Scan (

  test_scan,

) where

import Prelude                                                  as P
import Test.QuickCheck
import Data.Label
import Data.Maybe
import Data.Typeable
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Config
import Test.Base
import QuickCheck.Arbitrary.Array
import QuickCheck.Arbitrary.Shape                               ()
import Data.Array.Accelerate                                    as A hiding ( Ord(..) )
import Data.Array.Accelerate.Examples.Internal                  as A
import Data.Array.Accelerate.Array.Sugar                        as Sugar
import qualified Data.Array.Accelerate                          as A


--
-- scan ------------------------------------------------------------------------
--

test_scan :: Backend -> Config -> Test
test_scan backend opt = testGroup "scan" $ catMaybes
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
    testElt :: forall e. (P.Num e, P.Ord e, A.Num e, A.Ord e, P.Enum e, Similar e, Arbitrary e) => (Config :-> Bool) -> e -> Maybe Test
    testElt ok _
      | P.not (get ok opt)  = Nothing
      | otherwise           = Just $ testGroup (show (typeOf (undefined :: e)))
          [ testDim dim1
          , testDim dim2
          ]
      where
        testDim :: forall sh. (Shape sh, Slice sh, P.Eq sh, Arbitrary sh, Arbitrary (Array (sh:.Int) e)) => sh:.Int -> Test
        testDim _sh = testGroup ("DIM" P.++ show (rank _sh))
          [ testGroup "scanl"
            [ testProperty "sum"        (test_scanl  :: Array (sh:.Int) e -> Property)
            , testProperty "interval"   (intv_scanl  :: sh -> NonNegative Int -> Property)
            ]
          , testGroup "scanl'"
            [ testProperty "sum"        (test_scanl' :: Array (sh:.Int) e -> Property)
            , testProperty "interval"   (intv_scanl' :: sh -> NonNegative Int -> Property)
            ]
          , testGroup "scanl1"
            [ testProperty "sum"        (test_scanl1 :: Array (sh:.Int) e -> Property)
            , testProperty "interval"   (intv_scanl1 :: sh -> Positive Int -> Property)
            ]
          , testGroup "scanr"
            [ testProperty "sum"        (test_scanr  :: Array (sh:.Int) e -> Property)
            , testProperty "interval"   (intv_scanr  :: sh -> NonNegative Int -> Property)
            ]
          , testGroup "scanr'"
            [ testProperty "sum"        (test_scanr' :: Array (sh:.Int) e -> Property)
            , testProperty "interval"   (intv_scanr' :: sh -> NonNegative Int -> Property)
            ]
          , testGroup "scanr1"
            [ testProperty "sum"        (test_scanr1 :: Array (sh:.Int) e -> Property)
            , testProperty "interval"   (intv_scanr1 :: sh -> Positive Int -> Property)
            ]
          --
          , testProperty "scanl1Seg"    (test_scanl1seg (undefined::Array (sh:.Int) e))
          , testProperty "scanr1Seg"    (test_scanr1seg (undefined::Array (sh:.Int) e))
          , testProperty "scanlSeg"     (test_scanlseg  (undefined::Array (sh:.Int) e))
          , testProperty "scanrSeg"     (test_scanrseg  (undefined::Array (sh:.Int) e))
          , testProperty "scanl'Seg"    (test_scanl'seg (undefined::Array (sh:.Int) e))
          , testProperty "scanr'Seg"    (test_scanr'seg (undefined::Array (sh:.Int) e))
          ]
          where
            -- left scan
            --
            test_scanl  xs = (run1 backend (A.scanl (+) 0)) xs           ~?= scanlRef (+) 0 xs
            test_scanl' xs = (run1 backend (A.lift . A.scanl' (+) 0)) xs ~?= scanl'Ref (+) 0 xs
            test_scanl1 xs =
              arraySize (arrayShape xs) > 0 ==>
                (run1 backend (A.scanl1 A.min)) xs ~?= scanl1Ref P.min xs

            intv_scanl sh (NonNegative sz) =
              let xs = intervalArray sh sz
              in (run1 backend (A.scanl iappend' (constant one))) xs ~?= scanlRef iappend one xs

            intv_scanl' sh (NonNegative sz) =
              let xs = intervalArray sh sz
              in (run1 backend (A.lift . A.scanl' iappend' (constant one))) xs ~?= scanl'Ref iappend one xs

            intv_scanl1 sh (Positive sz) =
              arraySize sh > 0 ==>
                let xs = intervalArray sh sz
                in (run1 backend (A.scanl1 iappend')) xs ~?= scanl1Ref iappend xs

            -- right scan
            --
            test_scanr  xs = run1 backend (A.scanr (+) 0) xs           ~?= scanrRef (+) 0 xs
            test_scanr' xs = run1 backend (A.lift . A.scanr' (+) 0) xs ~?= scanr'Ref (+) 0 xs
            test_scanr1 xs =
              arraySize (arrayShape xs) > 0 ==>
              (run1 backend (A.scanr1 A.max)) xs ~?= scanr1Ref P.max xs

            intv_scanr sh (NonNegative sz) =
              let xs = intervalArray sh sz
              in (run1 backend (A.scanr iappend' (constant one))) xs ~?= scanrRef iappend one xs

            intv_scanr' sh (NonNegative sz) =
              let xs = intervalArray sh sz
              in (run1 backend (A.lift . A.scanr' iappend' (constant one))) xs ~?= scanr'Ref iappend one xs

            intv_scanr1 sh (Positive sz) =
              arraySize sh > 0 ==>
                let xs = intervalArray sh sz
                in (run1 backend (A.scanr1 iappend')) xs ~?= scanr1Ref iappend xs

            -- segmented left/right scan
            --
            test_scanl1seg elt =
              forAllShrink arbitrarySegments1            shrinkSegments1      $ \(seg :: Vector Int32) ->
              forAllShrink (arbitrarySegmentedArray seg) shrinkSegmentedArray $ \xs  ->
                arraySize (arrayShape xs) > 0 ==>
                  (runN backend (A.scanl1Seg (+))) xs seg
                  ~?=
                  scanl1SegRef (+) (xs `asTypeOf` elt) seg

            test_scanr1seg elt =
              forAllShrink arbitrarySegments1            shrinkSegments1      $ \(seg :: Vector Int32) ->
              forAllShrink (arbitrarySegmentedArray seg) shrinkSegmentedArray $ \xs  ->
                arraySize (arrayShape xs) > 0 ==>
                  (runN backend (A.scanr1Seg (+))) xs seg
                  ~?=
                  scanr1SegRef (+) (xs `asTypeOf` elt) seg

            test_scanlseg elt =
              forAllShrink arbitrarySegments             shrinkSegments       $ \(seg :: Vector Int32) ->
              forAllShrink (arbitrarySegmentedArray seg) shrinkSegmentedArray $ \xs  ->
                (runN backend (A.scanlSeg (+) 0)) xs seg
                ~?=
                scanlSegRef (+) 0 (xs `asTypeOf` elt) seg

            test_scanrseg elt =
              forAllShrink arbitrarySegments             shrinkSegments       $ \(seg :: Vector Int32) ->
              forAllShrink (arbitrarySegmentedArray seg) shrinkSegmentedArray $ \xs  ->
                (runN backend (A.scanrSeg (+) 0)) xs seg
                ~?=
                scanrSegRef (+) 0 (xs `asTypeOf` elt) seg

            test_scanl'seg elt =
              forAllShrink arbitrarySegments             shrinkSegments       $ \(seg :: Vector Int32) ->
              forAllShrink (arbitrarySegmentedArray seg) shrinkSegmentedArray $ \xs  ->
                (runN backend (A.scanl'Seg (+) 0)) xs seg
                ~?=
                scanl'SegRef (+) 0 (xs `asTypeOf` elt) seg

            test_scanr'seg elt =
              forAllShrink arbitrarySegments             shrinkSegments       $ \(seg :: Vector Int32) ->
              forAllShrink (arbitrarySegmentedArray seg) shrinkSegmentedArray $ \xs  ->
                (runN backend (A.scanr'Seg (+) 0)) xs seg
                ~?=
                scanr'SegRef (+) 0 (xs `asTypeOf` elt) seg

            -- interval of summations monoid
            --
            one,top :: (e,e)
            one = (-1,-1)
            top = (-2,-2)

            iappend :: (e,e) -> (e,e) -> (e,e)
            iappend x y
              | x P.== one                 = y
              | y P.== one                 = x
              | x P.== top P.|| y P.== top = top
            iappend (x1,x2) (y1,y2)
              | x2 + 1 P.== y1             = (x1,y2)
              | otherwise                  = top

            iappend' :: Exp (e,e) -> Exp (e,e) -> Exp (e,e)
            iappend' x y
              = x A.== constant one ? ( y
              , y A.== constant one ? ( x
              , x A.== constant top ? ( constant top -- A.|| y A.== constant top; see AccelerateHS/accelerate#364
              , let
                    (x1,x2) = unlift x :: (Exp e, Exp e)
                    (y1,y2) = unlift y :: (Exp e, Exp e)
                in
                x2 + 1 A.== y1 ? ( lift (x1,y2) , constant top )
              )))

            intervalArray :: sh -> Int -> Array (sh:.Int) (e,e)
            intervalArray sh n = fromList (sh:.n)
                               . concat
                               $ P.replicate (Sugar.size sh) [ (i,i) | i <- [0.. (P.fromIntegral n-1)] ]


-- Reference implementation
-- ------------------------

scanlRef :: (Shape sh, Elt e) => (e -> e -> e) -> e -> Array (sh:.Int) e -> Array (sh:.Int) e
scanlRef f z arr =
  let sz :. n     = arrayShape arr
      arr'        = [ P.scanl f z sub | sub <- splitEvery n (toList arr) ]
  in
  A.fromList (sz :. n+1) (concat arr')

scanl'Ref :: (Shape sh, Elt e) => (e -> e -> e) -> e -> Array (sh:.Int) e -> (Array (sh:.Int) e, Array sh e)
scanl'Ref f z arr =
  let sz :. n     = arrayShape arr
      (arr',sums) = P.unzip [ P.splitAt n (P.scanl f z sub) | sub <- splitEvery n (toList arr) ]
  in
  ( A.fromList (sz:.n) (concat arr'), A.fromList sz (concat sums) )

scanl1Ref :: (Shape sh, Elt e) => (e -> e -> e) -> Array (sh:.Int) e -> Array (sh:.Int) e
scanl1Ref f arr =
  let sz :. n     = arrayShape arr
      arr'        = [ P.scanl1 f sub | sub <- splitEvery n (toList arr) ]
  in
  A.fromList (sz:.n) (concat arr')

scanrRef :: (Shape sh, Elt e) => (e -> e -> e) -> e -> Array (sh:.Int) e -> Array (sh:.Int) e
scanrRef f z arr =
  let sz :. n     = arrayShape arr
      arr'        = [ P.scanr f z sub | sub <- splitEvery n (toList arr) ]
  in
  A.fromList (sz :. n+1) (concat arr')

scanr'Ref :: (Shape sh, Elt e) => (e -> e -> e) -> e -> Array (sh:.Int) e -> (Array (sh:.Int) e, Array sh e)
scanr'Ref f z arr =
  let sz :. n     = arrayShape arr
      (sums,arr') = P.unzip [ P.splitAt 1 (P.scanr f z sub) | sub <- splitEvery n (toList arr) ]
  in
  ( A.fromList (sz:.n) (concat arr'), A.fromList sz (concat sums) )

scanr1Ref :: (Shape sh, Elt e) => (e -> e -> e) -> Array (sh:.Int) e -> Array (sh:.Int) e
scanr1Ref f arr =
  let sz :. n     = arrayShape arr
      arr'        = [ P.scanr1 f sub | sub <- splitEvery n (toList arr) ]
  in
  A.fromList (sz:.n) (concat arr')


-- segmented operations
--
scanlSegRef
    :: (Shape sh, Elt e, P.Integral i)
    => (e -> e -> e)
    -> e
    -> Array (sh:.Int) e
    -> Segments i
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
    :: (Shape sh, Elt e, P.Integral i)
    => (e -> e -> e)
    -> Array (sh:.Int) e
    -> Segments i
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
    :: (Shape sh, Elt e, P.Integral i)
    => (e -> e -> e)
    -> e
    -> Array (sh:.Int) e
    -> Segments i
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
    :: (Shape sh, Elt e, P.Integral i)
    => (e -> e -> e)
    -> e
    -> Array (sh:.Int) e
    -> Segments i
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
    :: (Shape sh, Elt e, P.Integral i)
    => (e -> e -> e)
    -> Array (sh:.Int) e
    -> Segments i
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
    :: (Shape sh, Elt e, P.Integral i)
    => (e -> e -> e)
    -> e
    -> Array (sh:.Int) e
    -> Segments i
    -> (Array (sh:.Int) e, Array (sh:.Int) e)
scanr'SegRef f z arr seg =
  let
      sz :. n       = arrayShape arr
      Z  :. s       = arrayShape seg
      (sums, arr')  = P.unzip [ P.splitAt 1 (P.scanr f z sec) | sub <- splitEvery n (toList arr)
                                                              , sec <- splitPlaces (toList seg) sub ]
  in
  ( A.fromList (sz:.n) (concat arr'), A.fromList (sz:.s) (concat sums) )

