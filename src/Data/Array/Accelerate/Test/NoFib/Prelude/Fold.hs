{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}

module Test.Prelude.Fold (

  test_fold,
  test_foldAll,
  test_foldSeg,

) where

import Prelude                                                  as P
import Data.List
import Data.Label
import Data.Maybe
import Data.Typeable
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Data.Array.Accelerate                                    as A hiding ( Ord(..), indexHead, indexTail )
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Sugar                        as Sugar
import Data.Array.Accelerate.Examples.Internal                  as A
import qualified Data.Array.Accelerate                          as A

import Config
import Test.Base
import System.Random
import QuickCheck.Arbitrary.Array



--
-- Reduction -------------------------------------------------------------------
--

-- foldAll
-- -------

test_foldAll :: Backend -> Config -> Test
test_foldAll backend opt = testGroup "foldAll" $ catMaybes
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
    testElt :: forall e. (P.Num e, P.Ord e, A.Num e, A.Ord e, Similar e, Arbitrary e, Random e) => (Config :-> Bool) -> e -> Maybe Test
    testElt ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testGroup (show (typeOf (undefined :: e)))
          [ testDim dim0
          , testDim dim1
          , testDim dim2
          ]
      where
        testDim :: forall sh. (Shape sh, Arbitrary sh, Arbitrary (Array sh e)) => sh -> Test
        testDim sh = testGroup ("DIM" P.++ show (rank sh))
          [
            testProperty "sum"             (test_sum  :: Array sh e -> Property)
          , testProperty "non-neutral sum" (test_sum' :: Array sh e -> NonZero e -> Property)
          , testProperty "non-commutative" (test_mss  :: sh -> e -> Property)
          , testProperty "minimum"         (test_min  :: Array sh e -> Property)
          , testProperty "maximum"         (test_max  :: Array sh e -> Property)
          ]
          where
            --
            -- The tests
            --
            test_min :: Array sh e -> Property
            test_min xs
              =   arraySize (arrayShape xs) > 0
              ==> run1 backend (A.fold1All A.min) xs ~?= fold1AllRef P.min xs

            test_max :: Array sh e -> Property
            test_max xs
              =   arraySize (arrayShape xs) > 0
              ==> run1 backend (A.fold1All A.max) xs ~?= fold1AllRef P.max xs

            test_sum :: Array sh e -> Property
            test_sum xs = run1 backend (A.foldAll (+) 0) xs ~?= foldAllRef (+) 0 xs

            test_sum' :: Array sh e -> NonZero e -> Property
            test_sum' xs (NonZero z) =
              runN backend (\z' -> A.foldAll (+) (the z')) (scalar z) xs
              ~?=
              foldAllRef (+) z xs

            test_mss :: sh -> e -> Property
            test_mss (arraySize -> n) _
              =   n > 0
              ==> forAll (arbitraryArrayOf (Z:.n) smallArbitrary) $ \(xs :: Vector e) ->
                    run1 backend maximumSegmentSum xs ~?= maximumSegmentSumRef xs


-- multidimensional fold
-- ---------------------

test_fold :: Backend -> Config -> Test
test_fold backend opt = testGroup "fold" $ catMaybes
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
    testElt :: forall e. (P.Num e, P.Ord e, A.Num e, A.Ord e, Similar e, Arbitrary e, Random e) => (Config :-> Bool) -> e -> Maybe Test
    testElt ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testGroup (show (typeOf (undefined :: e)))
          [ testDim dim1
          , testDim dim2
          ]
      where
        testDim :: forall sh. (Shape sh, P.Eq sh, Arbitrary (sh :. Int), Arbitrary (Array (sh:.Int) e)) => (sh:.Int) -> Test
        testDim sh = testGroup ("DIM" P.++ show (rank sh))
          [
            testProperty "sum"             (test_sum  :: Array (sh :. Int) e -> Property)
          , testProperty "non-neutral sum" (test_sum' :: Array (sh :. Int) e -> NonZero e -> Property)
          , testProperty "non-commutative" (test_mss  :: (sh :. Int) -> e -> Property)
          , testProperty "minimum"         (test_min  :: Array (sh :. Int) e -> Property)
          , testProperty "maximum"         (test_max  :: Array (sh :. Int) e -> Property)
          ]
          where
            --
            -- The tests
            --
            test_min :: Array (sh:.Int) e -> Property
            test_min xs
              =   indexHead (arrayShape xs) > 0
              ==> run1 backend (A.fold1 A.min) xs ~?= fold1Ref P.min xs

            test_max :: Array (sh:.Int) e -> Property
            test_max xs
              =   indexHead (arrayShape xs) > 0
              ==> run1 backend (A.fold1 A.max) xs ~?= fold1Ref P.max xs

            test_sum :: Array (sh:.Int) e -> Property
            test_sum xs = run1 backend (A.fold (+) 0) xs ~?= foldRef (+) 0 xs

            test_sum' :: Array (sh:.Int) e -> NonZero e -> Property
            test_sum' xs (NonZero z) =
              runN backend (\z' -> A.fold (+) (the z')) (scalar z) xs ~?= foldRef (+) z xs

            test_mss :: (sh:.Int) -> e -> Property
            test_mss sz _
              =   indexHead sz > 0
              ==> forAll (arbitraryArrayOf sz smallArbitrary) $ \(xs :: Array (sh:.Int) e) ->
                    run1 backend maximumSegmentSum xs ~?= maximumSegmentSumRef xs


-- segmented fold
-- --------------

test_foldSeg :: Backend -> Config -> Test
test_foldSeg backend opt = testGroup "foldSeg" $ catMaybes
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
    testElt :: forall e. (P.Num e, P.Ord e, A.Num e, A.Ord e, Similar e, Arbitrary e) => (Config :-> Bool) -> e -> Maybe Test
    testElt ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testGroup (show (typeOf (undefined :: e)))
          [ testDim dim1
          , testDim dim2
          ]
      where
        testDim :: forall sh. (Shape sh, P.Eq sh, Arbitrary sh) => (sh:.Int) -> Test
        testDim sh = testGroup ("DIM" P.++ show (rank sh))
          [
            testProperty "sum"
          $ forAllShrink arbitrarySegments             shrinkSegments       $ \(seg :: Segments Int32)    ->
            forAllShrink (arbitrarySegmentedArray seg) shrinkSegmentedArray $ \(xs  :: Array (sh:.Int) e) ->
              runN backend (A.foldSeg (+) 0) xs seg ~?= foldSegRef (+) 0 xs seg

          , testProperty "non-neutral sum"
          $ forAllShrink arbitrarySegments             shrinkSegments       $ \(seg :: Segments Int32)    ->
            forAllShrink (arbitrarySegmentedArray seg) shrinkSegmentedArray $ \(xs  :: Array (sh:.Int) e) ->
            forAll arbitrary                                                      $ \(NonZero z)                ->
              runN backend (\z' -> A.foldSeg (+) (the z')) (scalar z) xs seg ~?= foldSegRef (+) z xs seg

          , testProperty "minimum"
          $ forAllShrink arbitrarySegments1            shrinkSegments1      $ \(seg :: Segments Int32)    ->
            forAllShrink (arbitrarySegmentedArray seg) shrinkSegmentedArray $ \(xs  :: Array (sh:.Int) e) ->
              runN backend (A.fold1Seg A.min) xs seg ~?= fold1SegRef P.min xs seg
          ]


-- Reference implementation
-- ------------------------

foldAllRef :: Elt e => (e -> e -> e) -> e -> Array sh e -> Array Z e
foldAllRef f z
  = A.fromList Z
  . return
  . foldl' f z
  . A.toList

fold1AllRef :: Elt e => (e -> e -> e) -> Array sh e -> Array Z e
fold1AllRef f
  = A.fromList Z
  . return
  . foldl1' f
  . A.toList

foldRef :: (Shape sh, Elt e) => (e -> e -> e) -> e -> Array (sh :. Int) e -> Array sh e
foldRef f z arr =
  let (sh :. n) = arrayShape arr
      sh'       = listToShape . P.map (P.max 1) . shapeToList $ sh
  in  fromList sh' [ foldl' f z sub | sub <- splitEvery n (toList arr) ]

fold1Ref :: (Shape sh, Elt e) => (e -> e -> e) -> Array (sh :. Int) e -> Array sh e
fold1Ref f arr =
  let (sh :. n) = arrayShape arr
  in  fromList sh [ foldl1' f sub | sub <- splitEvery n (toList arr) ]

foldSegRef :: (Shape sh, Elt e, P.Integral i) => (e -> e -> e) -> e -> Array (sh :. Int) e -> Segments i -> Array (sh :. Int) e
foldSegRef f z arr seg = fromList (sh :. sz) $ concat [ foldseg sub | sub <- splitEvery n (toList arr) ]
  where
    (sh :. n)   = arrayShape arr
    (Z  :. sz)  = arrayShape seg
    seg'        = toList seg
    foldseg xs  = P.map (foldl' f z) (splitPlaces seg' xs)

fold1SegRef :: (Shape sh, Elt e, P.Integral i) => (e -> e -> e) -> Array (sh :. Int) e -> Segments i -> Array (sh :. Int) e
fold1SegRef f arr seg = fromList (sh :. sz) $ concat [ foldseg sub | sub <- splitEvery n (toList arr) ]
  where
    (sh :. n)   = arrayShape arr
    (Z  :. sz)  = arrayShape seg
    seg'        = toList seg
    foldseg xs  = P.map (foldl1' f) (splitPlaces seg' xs)

maximumSegmentSum :: forall sh e. (Shape sh, A.Num e, A.Ord e) => Acc (Array (sh :. Int) e) -> Acc (Array sh e)
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


maximumSegmentSumRef :: (P.Num e, P.Ord e, Shape sh, Elt e) => Array (sh :. Int) e -> Array sh e
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


smallArbitrary :: forall e. (P.Num e, Elt e, Arbitrary e, Random e) => Gen e
smallArbitrary
  | SingleTuple t <- eltType (undefined::e)
  , NumScalarType s <- t
  , IntegralNumType i <- s
  = case i of
      TypeInt{}     -> choose (-100,100)
      TypeInt8{}    -> choose (-1,1)
      TypeInt16{}   -> choose (-10,10)
      TypeInt32{}   -> choose (-1000,1000)
      TypeInt64{}   -> choose (-10000,10000)
      TypeWord{}    -> choose (0,1000)
      TypeWord8{}   -> choose (0,1)
      TypeWord16{}  -> choose (0,10)
      TypeWord32{}  -> choose (0,1000)
      TypeWord64{}  -> choose (0,10000)
      _             -> arbitrary

  | otherwise
  = arbitrary

