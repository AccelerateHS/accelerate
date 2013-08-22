{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Test.Prelude.Mapping (

  test_map,
  test_zipWith,

) where

import Prelude                                                  as P
import Data.Label
import Data.Maybe
import Data.Typeable
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Config
import ParseArgs
import Test.Base
import QuickCheck.Arbitrary.Array                               ()
import Data.Array.Accelerate                                    as A
import Data.Array.Accelerate.Array.Sugar                        as Sugar
import qualified Data.Array.Accelerate.Array.Representation     as R

--
-- Map -------------------------------------------------------------------------
--

test_map :: Config -> Test
test_map opt = testGroup "map" $ catMaybes
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
    backend        = get configBackend opt
    test_square xs = run1 backend (A.map (\x -> x*x)) xs ~?= mapRef (\x -> x*x) xs
    test_abs    xs = run1 backend (A.map abs) xs         ~?= mapRef abs xs
    test_plus z xs =
      let z' = unit (constant z)
      in  run1 backend (A.map (+ the z')) xs ~?= mapRef (+ z) xs

    testElt :: forall a. (Elt a, IsNum a, Similar a, Arbitrary a)
            => (Config :-> Bool)
            -> a
            -> Maybe Test
    testElt ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testGroup (show (typeOf (undefined :: a)))
          [ testDim dim0
          , testDim dim1
          , testDim dim2
          ]
      where
        testDim :: forall sh. (Shape sh, Eq sh, Arbitrary sh, Arbitrary (Array sh a)) => sh -> Test
        testDim sh = testGroup ("DIM" P.++ show (dim sh))
          [ testProperty "abs"    (test_abs    :: Array sh a -> Property)
          , testProperty "plus"   (test_plus   :: a -> Array sh a -> Property)
          , testProperty "square" (test_square :: Array sh a -> Property)
          ]


test_zipWith :: Config -> Test
test_zipWith opt = testGroup "zipWith" $ catMaybes
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
    backend         = get configBackend opt
    test_zip  xs ys = run2 backend A.zip           xs ys ~?= zipWithRef (,) xs ys
    test_plus xs ys = run2 backend (A.zipWith (+)) xs ys ~?= zipWithRef (+) xs ys
    test_min  xs ys = run2 backend (A.zipWith min) xs ys ~?= zipWithRef min xs ys

    testElt :: forall a. (Elt a, IsNum a, Ord a, Similar a, Arbitrary a)
            => (Config :-> Bool)
            -> a
            -> Maybe Test
    testElt ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testGroup (show (typeOf (undefined :: a)))
          [ testDim dim0
          , testDim dim1
          , testDim dim2
          ]
      where
        testDim :: forall sh. (Shape sh, Eq sh, Arbitrary sh, Arbitrary (Array sh a)) => sh -> Test
        testDim sh = testGroup ("DIM" P.++ show (dim sh))
          [ testProperty "zip"  (test_zip  :: Array sh a -> Array sh a -> Property)
          , testProperty "plus" (test_plus :: Array sh a -> Array sh a -> Property)
          , testProperty "min"  (test_min  :: Array sh a -> Array sh a -> Property)
          ]


-- Reference Implementation
-- ------------------------

mapRef :: (Shape sh, Elt b) => (a -> b) -> Array sh a -> Array sh b
mapRef f xs
  = fromList (arrayShape xs)
  $ P.map f
  $ toList xs

zipWithRef :: (Shape sh, Elt c) => (a -> b -> c) -> Array sh a -> Array sh b -> Array sh c
zipWithRef f xs ys =
  let shx       = fromElt (arrayShape xs)
      shy       = fromElt (arrayShape ys)
      sh        = toElt (R.intersect shx shy)
  in
  newArray sh (\ix -> f (xs Sugar.! ix) (ys Sugar.! ix))

