{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module Test.Mapping where

import Prelude                                          as P
import Data.Label
import Data.Maybe
import Data.Typeable
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Config
import Test.Base
import Arbitrary.Array                                          ()
import Data.Array.Accelerate                                    as Acc
import Data.Array.Accelerate.Array.Sugar                        as Sugar
import qualified Data.Array.Accelerate.Array.Representation     as R

--
-- Map -------------------------------------------------------------------------
--

test_map :: Options -> Test
test_map opt = testGroup "map" $ catMaybes
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
    test_square xs = run opt (Acc.map (\x -> x*x) (use xs)) .==. mapRef (\x -> x*x) xs
    test_abs    xs = run opt (Acc.map abs (use xs))         .==. mapRef abs xs
    test_plus z xs =
      let z' = unit (constant z)
      in  run opt (Acc.map (+ the z') (use xs)) .==. mapRef (+ z) xs

    testElt :: forall a. (Elt a, IsNum a, Similar a, Arbitrary a)
            => (Options :-> Bool)
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
        testDim sh = testGroup ("DIM" ++ show (dim sh))
          [ testProperty "abs"    (test_abs    :: Array sh a -> Property)
          , testProperty "plus"   (test_plus   :: a -> Array sh a -> Property)
          , testProperty "square" (test_square :: Array sh a -> Property)
          ]


test_zipWith :: Options -> Test
test_zipWith opt = testGroup "zipWith" $ catMaybes
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
    test_zip  xs ys = run opt (Acc.zip             (use xs) (use xs)) .==. zipWithRef (,) xs ys
    test_plus xs ys = run opt (Acc.zipWith (+)     (use xs) (use ys)) .==. zipWithRef (+) xs ys
    test_min  xs ys = run opt (Acc.zipWith Acc.min (use xs) (use ys)) .==. zipWithRef P.min xs ys

    testElt :: forall a. (Elt a, IsNum a, Ord a, Similar a, Arbitrary a)
            => (Options :-> Bool)
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
        testDim sh = testGroup ("DIM" ++ show (dim sh))
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

