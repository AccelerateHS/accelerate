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
import QuickCheck.Arbitrary.Shape
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
  , Just testIntervals
  ]
  where
    testElt :: forall e. (P.Num e, P.Ord e, A.Num e, A.Ord e, Similar e, Arbitrary e) => (Config :-> Bool) -> e -> Maybe Test
    testElt ok _
      | P.not (get ok opt)  = Nothing
      | otherwise           = Just $ testGroup (show (typeOf (undefined :: e)))
          [ testDim dim1
          , testDim dim2
          ]
      where
        testDim :: forall sh. (Shape sh, Slice sh, P.Eq sh, Arbitrary sh, Arbitrary (Array (sh:.Int) e)) => sh:.Int -> Test
        testDim sh = testGroup ("DIM" P.++ show (rank sh))
          [ testProperty "scanl"        (test_scanl  :: Array (sh:.Int) e -> Property)
          , testProperty "scanl'"       (test_scanl' :: Array (sh:.Int) e -> Property)
          , testProperty "scanl1"       (test_scanl1 :: Array (sh:.Int) e -> Property)
          , testProperty "scanr"        (test_scanr  :: Array (sh:.Int) e -> Property)
          , testProperty "scanr'"       (test_scanr' :: Array (sh:.Int) e -> Property)
          , testProperty "scanr1"       (test_scanr1 :: Array (sh:.Int) e -> Property)
          --
          , testProperty "scanl1Seg"    (test_scanl1seg (undefined::Array (sh:.Int) e))
          , testProperty "scanr1Seg"    (test_scanr1seg (undefined::Array (sh:.Int) e))
          , testProperty "scanlSeg"     (test_scanlseg  (undefined::Array (sh:.Int) e))
          , testProperty "scanrSeg"     (test_scanrseg  (undefined::Array (sh:.Int) e))
          , testProperty "scanl'Seg"    (test_scanl'seg (undefined::Array (sh:.Int) e))
          , testProperty "scanr'Seg"    (test_scanr'seg (undefined::Array (sh:.Int) e))
          ]

        -- left scan
        --
        test_scanl  xs = (run1 backend (A.scanl (+) 0)) xs           ~?= scanlRef (+) 0 xs
        test_scanl' xs = (run1 backend (A.lift . A.scanl' (+) 0)) xs ~?= scanl'Ref (+) 0 xs
        test_scanl1 xs =
          arraySize (arrayShape xs) > 0 ==>
            (run1 backend (A.scanl1 A.min)) xs ~?= scanl1Ref P.min xs

        -- right scan
        --
        test_scanr  xs = run1 backend (A.scanr (+) 0) xs           ~?= scanrRef (+) 0 xs
        test_scanr' xs = run1 backend (A.lift . A.scanr' (+) 0) xs ~?= scanr'Ref (+) 0 xs
        test_scanr1 xs =
          arraySize (arrayShape xs) > 0 ==>
          (run1 backend (A.scanr1 A.max)) xs ~?= scanr1Ref P.max xs

        -- segmented left/right scan
        --
        test_scanl1seg elt =
          forAllShrink arbitrarySegments1            shrinkSegments1      $ \(seg :: Vector Int32) ->
          forAllShrink (arbitrarySegmentedArray seg) shrinkSegmentedArray $ \xs  ->
            arraySize (arrayShape xs) > 0 ==>
              (run2 backend (A.scanl1Seg (+))) xs seg
              ~?=
              scanl1SegRef (+) (xs `asTypeOf` elt) seg

        test_scanr1seg elt =
          forAllShrink arbitrarySegments1            shrinkSegments1      $ \(seg :: Vector Int32) ->
          forAllShrink (arbitrarySegmentedArray seg) shrinkSegmentedArray $ \xs  ->
            arraySize (arrayShape xs) > 0 ==>
              (run2 backend (A.scanr1Seg (+))) xs seg
              ~?=
              scanr1SegRef (+) (xs `asTypeOf` elt) seg

        test_scanlseg elt =
          forAllShrink arbitrarySegments             shrinkSegments       $ \(seg :: Vector Int32) ->
          forAllShrink (arbitrarySegmentedArray seg) shrinkSegmentedArray $ \xs  ->
            (run2 backend (A.scanlSeg (+) 0)) xs seg
            ~?=
            scanlSegRef (+) 0 (xs `asTypeOf` elt) seg

        test_scanrseg elt =
          forAllShrink arbitrarySegments             shrinkSegments       $ \(seg :: Vector Int32) ->
          forAllShrink (arbitrarySegmentedArray seg) shrinkSegmentedArray $ \xs  ->
            (run2 backend (A.scanrSeg (+) 0)) xs seg
            ~?=
            scanrSegRef (+) 0 (xs `asTypeOf` elt) seg

        test_scanl'seg elt =
          forAllShrink arbitrarySegments             shrinkSegments       $ \(seg :: Vector Int32) ->
          forAllShrink (arbitrarySegmentedArray seg) shrinkSegmentedArray $ \xs  ->
            (run2 backend (lift $$ A.scanl'Seg (+) 0)) xs seg
            ~?=
            scanl'SegRef (+) 0 (xs `asTypeOf` elt) seg

        test_scanr'seg elt =
          forAllShrink arbitrarySegments             shrinkSegments       $ \(seg :: Vector Int32) ->
          forAllShrink (arbitrarySegmentedArray seg) shrinkSegmentedArray $ \xs  ->
            (run2 backend (lift $$ A.scanr'Seg (+) 0)) xs seg
            ~?=
            scanr'SegRef (+) 0 (xs `asTypeOf` elt) seg

    testIntervals :: Test
    testIntervals = testGroup "intervals"
      [ testDim dim1
      , testDim dim2
      ]
      where
        testDim :: forall sh. (Shape sh, Slice sh, P.Eq sh, Arbitrary sh) => sh:.Int -> Test
        testDim sh = testGroup ("DIM" P.++ show (rank sh))
          [ testProperty "scanl"        (test_scanl  :: Property)
          , testProperty "scanl'"       (test_scanl' :: Property)
          , testProperty "scanl1"       (test_scanl1 :: Property)
          , testProperty "scanr"        (test_scanr  :: Property)
          , testProperty "scanr'"       (test_scanr' :: Property)
          , testProperty "scanr1"       (test_scanr1 :: Property)
          ]
          where
            -- left scan
            --
            test_scanl =
              forAllShrink arbitraryInterval shrinkInterval $ \(xs :: Array (sh:.Int) (Int, Int)) ->
                (run1 backend (A.scanl (.+*) one')) xs ~?= scanlRef (.+) one xs
            test_scanl' =
              forAllShrink arbitraryInterval shrinkInterval $ \(xs :: Array (sh:.Int) (Int, Int)) ->
                (run1 backend (A.lift . A.scanl' (.+*) one')) xs ~?= scanl'Ref (.+) one xs
            test_scanl1 =
              forAllShrink arbitraryInterval shrinkInterval $ \(xs :: Array (sh:.Int) (Int, Int)) ->
                arraySize (arrayShape xs) > 0 ==>
                  (run1 backend (A.scanl1 (.+*))) xs ~?= scanl1Ref (.+) xs

            -- right scan
            --
            test_scanr =
              forAllShrink arbitraryInterval shrinkInterval $ \(xs :: Array (sh:.Int) (Int, Int)) ->
                run1 backend (A.scanr (.+*) one') xs ~?= scanrRef (.+) one xs
            test_scanr' =
              forAllShrink arbitraryInterval shrinkInterval $ \(xs :: Array (sh:.Int) (Int, Int)) ->
                run1 backend (A.lift . A.scanr' (.+*) one') xs ~?= scanr'Ref (.+) one xs
            test_scanr1 =
              forAllShrink arbitraryInterval shrinkInterval $ \(xs :: Array (sh:.Int) (Int, Int)) ->
                arraySize (arrayShape xs) > 0 ==>
                (run1 backend (A.scanr1 (.+*))) xs ~?= scanr1Ref (.+) xs


-- Interval of summations monoid
-- -----------------------------

one, top :: (Int, Int)
one = (-1,-1)
top = (-2,-2)

one', top' :: Exp (Int, Int)
one' = lift one
top' = lift top

(.+) :: (Int, Int) -> (Int, Int) -> (Int, Int)
x       .+ y       | x P.== one                 = y
                   | y P.== one                 = x
                   | x P.== top P.|| y P.== top = top
(x1,x2) .+ (y1,y2) | x2 + 1 P.== y1             = (x1,y2)
                   | otherwise                  = top

(.+*) :: Exp (Int, Int) -> Exp (Int, Int) -> Exp (Int, Int)
x       .+* y      = x A.== one' ?                  ( y
                   , y A.== one' ?                  ( x
                   , x A.== top' A.|| y A.== top' ? ( top'
                   , let
                       (x1,x2) = unlift x :: (Exp Int, Exp Int)
                       (y1,y2) = unlift y :: (Exp Int, Exp Int)
                     in x2 + 1 A.== y1 ?            ( lift (x1,y2)
                      , top' ))))

intervalArray :: Shape sh => sh:.Int -> Array (sh :. Int) (Int, Int)
intervalArray (sh:.n) = fromList (sh:.n) . concat . P.replicate (Sugar.size sh)
                      $ [(i,i) | i <- [0..n-1]]

arbitraryInterval :: (Shape sh, Arbitrary (sh:.Int)) => Gen (Array (sh:.Int) (Int,Int))
arbitraryInterval = intervalArray <$> arbitrary

shrinkInterval :: (Shape sh, Arbitrary (sh:.Int)) => Array (sh :. Int) (Int, Int) -> [Array (sh :. Int) (Int, Int)]
shrinkInterval arr = P.map intervalArray (shrink (Sugar.shape arr))


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

