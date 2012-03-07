{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Arbitrary.Array where

import Arbitrary.Shape

import Data.List
import Test.QuickCheck
import Data.Array.Accelerate.Array.Sugar


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
    [ fromList (Z :. length slx :. length sly) [ arr ! (Z:.x:.y) | x <- nub slx, y <- nub sly ]
        | slx <- shrink [0 .. width  - 1]
        , sly <- shrink [0 .. height - 1]
--        , null slx == null sly  -- only (Z:.0:.0) and non-empty arrays
    ]

