{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Arbitrary.Array where

import Arbitrary.Shape

import Data.List
import Test.QuickCheck
import Data.Array.Accelerate.Array.Sugar
import System.Random                                    ( Random )


instance (Elt e, Arbitrary e) => Arbitrary (Array DIM0 e) where
  arbitrary = do
    e           <- arbitrary
    return      $! fromList Z [e]
  --
  shrink arr =
    [ fromList Z [x] | x <- shrink (arr ! Z) ]


instance (Elt e, Arbitrary e) => Arbitrary (Array DIM1 e) where
  arbitrary = do
    sh          <- sized arbitrarySmallShape
    adata       <- vectorOf (size sh) arbitrary
    return      $! fromList sh adata
  --
  shrink arr =
    let (Z :. n)        = shape arr
        indices         = [ map (Z:.) (nub sz) | sz <- shrink [0 .. n-1] ]
    in
    [ fromList (Z :. length sl) (map (arr!) sl) | sl <- indices ]


instance (Elt e, Arbitrary e) => Arbitrary (Array DIM2 e) where
  arbitrary = do
    sh          <- sized arbitrarySmallShape
    adata       <- vectorOf (size sh) arbitrary
    return      $! fromList sh adata
  --
  shrink arr =
    let (Z :. width :. height)   = shape arr
    in
    [ fromList (Z :. length slx :. length sly) [ arr ! (Z:.x:.y) | x <- slx, y <- sly ]
        | slx <- map nub $ shrink [0 .. width  - 1]
        , sly <- map nub $ shrink [0 .. height - 1]
    ]


arbitraryArrayOfShape
    :: (Shape sh, Elt e, Arbitrary e)
    => sh
    -> Gen (Array sh e)
arbitraryArrayOfShape sh = do
  adata         <- vectorOf (size sh) arbitrary
  return        $! fromList sh adata

arbitrarySegmentedArray
    :: (Integral i, Shape sh, Elt e, Arbitrary sh, Arbitrary e)
    => Segments i -> Gen (Array (sh :. Int) e)
arbitrarySegmentedArray segs = do
  let sz        =  fromIntegral . sum $ toList segs
  sh            <- sized $ \n -> arbitrarySmallShape (n `div` 2)
  adata         <- vectorOf (size sh * sz) arbitrary
  return        $! fromList (sh :. sz) adata

arbitrarySegments :: (Elt i, Integral i, Arbitrary i, Random i) => Gen (Segments i)
arbitrarySegments = do
  seg    <- listOf (sized $ \n -> choose (0, fromIntegral n))
  return $! fromList (Z :. length seg) seg

arbitrarySegments1 :: (Elt i, Integral i, Arbitrary i, Random i) => Gen (Segments i)
arbitrarySegments1 = do
  seg    <- listOf (sized $ \n -> choose (1, fromIntegral n))
  return $! fromList (Z :. length seg) seg

