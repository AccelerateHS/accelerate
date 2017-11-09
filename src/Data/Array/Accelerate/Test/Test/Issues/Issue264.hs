{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-- https://github.com/AccelerateHS/accelerate/issues/264
--

module Test.Issues.Issue264 (test_issue264)
  where

import Config
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck                                          hiding ( (.&.) )
import QuickCheck.Arbitrary.Array                               ()

import Prelude                                                  as P
import Data.Bits                                                as P
import Data.Label
import Data.Maybe
import Data.Typeable
import Data.Array.Accelerate                                    as A
import Data.Array.Accelerate.Data.Bits                          as A
import Data.Array.Accelerate.Examples.Internal                  as A

import qualified Data.Array.Accelerate.Array.Sugar              as S
import qualified Data.Array.Accelerate.Array.Representation     as R


test_issue264 :: Backend -> Config -> Test
test_issue264 backend opt
  = testGroup "264"
  $ catMaybes
  [ testBool
  , testIntegralElt configInt8   (undefined :: Int8)
  , testIntegralElt configInt16  (undefined :: Int16)
  , testIntegralElt configInt32  (undefined :: Int32)
  , testIntegralElt configInt64  (undefined :: Int64)
  , testIntegralElt configWord8  (undefined :: Word8)
  , testIntegralElt configWord16 (undefined :: Word16)
  , testIntegralElt configWord32 (undefined :: Word32)
  , testIntegralElt configWord64 (undefined :: Word64)
  , testFloatingElt configFloat  (undefined :: Float)
  , testFloatingElt configDouble (undefined :: Double)
  ]
  where
    testBool :: Maybe Test
    testBool = Just $ testGroup "Bool"
      [ testProperty "not.not"        (test_notnot    :: Vector Bool -> Property)
      , testProperty "not(x&&y)"      (test_andnot    :: Vector Bool -> Vector Bool -> Property)
      , testProperty "not(x||y)"      (test_ornot     :: Vector Bool -> Vector Bool -> Property)
      , testProperty "not(not(x&&y))" (test_andnotnot :: Vector Bool -> Vector Bool -> Property)
      , testProperty "not(not(x||y))" (test_ornotnot  :: Vector Bool -> Vector Bool -> Property)
      ]

    testIntegralElt :: forall a. (P.Integral a, P.Bits a, A.Integral a, A.Bits a, Arbitrary a, Similar a, A.FromIntegral a Float) => (Config :-> Bool) -> a -> Maybe Test
    testIntegralElt ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testGroup (show (typeOf (undefined :: a)))
          [ testProperty "neg.neg"    (test_negneg  :: Vector a -> Property)
          ]

    testFloatingElt :: forall a. (P.Floating a, P.RealFloat a, A.Floating a, A.RealFloat a, Arbitrary a, Similar a) => (Config :-> Bool) -> a -> Maybe Test
    testFloatingElt ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testGroup (show (typeOf (undefined :: a)))
          [ testProperty "neg.neg"    (test_negneg  :: Vector a -> Property)
          ]

    test_negneg xs        = run1 backend (A.map negate . A.map negate) xs ~?= mapRef (negate . negate) xs
    test_notnot xs        = run1 backend (A.map A.not . A.map A.not) xs ~?= mapRef (P.not . P.not) xs

    test_andnot xs ys     = runN backend (A.zipWith (\x y -> A.not (x A.&& y))) xs ys
                            ~?=
                            zipWithRef (\x y -> P.not (x P.&& y)) xs ys

    test_ornot xs ys      = runN backend (A.zipWith (\x y -> A.not (x A.|| y))) xs ys
                            ~?=
                            zipWithRef (\x y -> P.not (x P.|| y)) xs ys

    test_andnotnot xs ys  = runN backend (A.zipWith (\x y -> A.not (A.not (x A.&& y)))) xs ys
                            ~?=
                            zipWithRef (\x y -> P.not (P.not (x P.&& y))) xs ys

    test_ornotnot xs ys   = runN backend (A.zipWith (\x y -> A.not (A.not (x A.|| y)))) xs ys
                            ~?=
                            zipWithRef (\x y -> P.not (P.not (x P.|| y))) xs ys

mapRef :: (Shape sh, Elt b) => (a -> b) -> Array sh a -> Array sh b
mapRef f xs
  = fromList (arrayShape xs)
  $ P.map f
  $ toList xs

zipWithRef :: (Shape sh, Elt c) => (a -> b -> c) -> Array sh a -> Array sh b -> Array sh c
zipWithRef f xs ys =
  let shx       = S.fromElt (S.shape xs)
      shy       = S.fromElt (S.shape ys)
      sh        = S.toElt (R.intersect shx shy)
  in
  fromFunction sh (\ix -> f (xs S.! ix) (ys S.! ix))

