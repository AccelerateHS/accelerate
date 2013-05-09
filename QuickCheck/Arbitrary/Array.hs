{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module QuickCheck.Arbitrary.Array where

import QuickCheck.Arbitrary.Shape

import Data.List
import Test.QuickCheck
import System.Random                                    ( Random )
import Data.Array.Accelerate.Array.Sugar                ( Array, Segments, Shape, Elt, Z(..), (:.)(..), (!), DIM0, DIM1, DIM2 )
import qualified Data.Array.Accelerate.Array.Sugar      as Sugar


instance (Elt e, Arbitrary e) => Arbitrary (Array DIM0 e) where
  arbitrary = do
    e           <- arbitrary
    return      $! Sugar.fromList Z [e]
  --
  shrink arr =
    [ Sugar.fromList Z [x] | x <- shrink (arr ! Z) ]


instance (Elt e, Arbitrary e) => Arbitrary (Array DIM1 e) where
  arbitrary = do
    sh          <- sized arbitraryShape
    adata       <- vectorOf (Sugar.size sh) arbitrary
    return      $! Sugar.fromList sh adata
  --
  shrink arr =
    let (Z :. n)        = Sugar.shape arr
        indices         = [ map (Z:.) (nub sz) | sz <- shrink [0 .. n-1] ]
    in
    [ Sugar.fromList (Z :. length sl) (map (arr!) sl) | sl <- indices ]


instance (Elt e, Arbitrary e) => Arbitrary (Array DIM2 e) where
  arbitrary = do
    sh          <- sized arbitraryShape
    adata       <- vectorOf (Sugar.size sh) arbitrary
    return      $! Sugar.fromList sh adata
  --
  shrink arr =
    let (Z :. width :. height)   = Sugar.shape arr
    in
    [ Sugar.fromList (Z :. length slx :. length sly) [ arr ! (Z:.x:.y) | x <- slx, y <- sly ]
        | slx <- map nub $ shrink [0 .. width  - 1]
        , sly <- map nub $ shrink [0 .. height - 1]
    ]


arbitraryArrayOfShape
    :: (Shape sh, Elt e, Arbitrary e)
    => sh
    -> Gen (Array sh e)
arbitraryArrayOfShape sh = do
  adata         <- vectorOf (Sugar.size sh) arbitrary
  return        $! Sugar.fromList sh adata

arbitrarySegmentedArray
    :: (Integral i, Shape sh, Elt e, Arbitrary sh, Arbitrary e)
    => Segments i -> Gen (Array (sh :. Int) e)
arbitrarySegmentedArray segs = do
  let sz        =  fromIntegral . sum $ Sugar.toList segs
  sh            <- sized $ \n -> arbitraryShape (n `div` 2)
  adata         <- vectorOf (Sugar.size sh * sz) arbitrary
  return        $! Sugar.fromList (sh :. sz) adata


arbitrarySegments :: (Elt i, Integral i, Arbitrary i, Random i) => Gen (Segments i)
arbitrarySegments = do
  seg    <- listOf (sized $ \n -> choose (0, fromIntegral n))
  return $! Sugar.fromList (Z :. length seg) seg

arbitrarySegments1 :: (Elt i, Integral i, Arbitrary i, Random i) => Gen (Segments i)
arbitrarySegments1 = do
  seg    <- listOf (sized $ \n -> choose (1, fromIntegral n))
  return $! Sugar.fromList (Z :. length seg) seg

