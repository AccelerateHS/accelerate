{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.FFT (

  test_fft

)  where

import Config
import ParseArgs

import Control.Applicative
import Data.Label
import Data.Maybe
import Data.Typeable

import Prelude as P

import Test.Base
import Test.QuickCheck                        hiding ( (.&.) )
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import QuickCheck.Arbitrary.Array

import Data.Array.Accelerate.Math.FFT         as FFT
import Data.Array.Accelerate                  as A hiding ( (!) )
import Data.Array.Accelerate.Array.Sugar      ( (!) )
import Data.Array.Accelerate.Math.Complex

instance (Similar e, RealFloat e) => Similar (Complex e) where
  -- CUFFT can actually give quite large errors, so we have to
  -- increase the epsilon for the absolute relative value difference.
  (r1 :+ i1) ~= (r2 :+ i2) = absRelTol 0.005 r1 r2 && absRelTol 0.005 i1 i2

newtype PowerOf2Array a = PowerOf2Array (Array DIM2 a) deriving Show

instance (Arbitrary a, Elt a) => Arbitrary (PowerOf2Array a) where
  arbitrary =
    do
      Z:.y:.x <- arbitrary
      PowerOf2Array <$> arbitraryArray (Z:.ceil2n y:.ceil2n x)
    where
      ceil2n :: Int -> Int
      ceil2n = (2^) . (P.floor :: Float -> Int) . P.logBase 2 . (+1) . (P.fromIntegral :: Int -> Float)
  shrink (PowerOf2Array a)
    = let Z:.h:.w = arrayShape a
      in if h > 0 && w >0
         then [ PowerOf2Array . fromList (Z:.(h `div` 2):.w) $ [a ! (Z:.y:.x) | y <- [0..h `div` 2], x <- [0..w - 1]]
              , PowerOf2Array . fromList (Z:.h:.(w `div` 2)) $ [a ! (Z:.y:.x) | y <- [0..h - 1], x <- [0..w `div` 2]] ]
         else []

test_fft :: Config -> Test
test_fft opt =  testGroup "fft" $ catMaybes
  [ testElt configFloat (undefined::Float)
  , testElt configDouble (undefined::Double) ]
  where
    backend = get configBackend opt

    testElt :: forall a. (Elt a, Similar a, Arbitrary a, IsFloating a, RealFloat a)
            => (Config :-> Bool)
            -> a
            -> Maybe Test
    testElt ok _
      | P.not (get ok opt)      = Nothing
      | otherwise             = Just $ testGroup (show (typeOf (undefined :: a)))
          [ testProperty "size"  (test_size :: PowerOf2Array (Complex a) -> Property)
          , testProperty "trans" (test_trans :: PowerOf2Array (Complex a) -> Property) ]

    test_trans :: (Elt a, RealFloat a, IsFloating a, Similar a) => PowerOf2Array (Complex a) -> Property
    test_trans (PowerOf2Array xs)
      = let Z:.h:.w = arrayShape xs
        in     run1 backend (transpose . fft2D' Forward w h) xs
           ~?= run1 backend (fft2D' Forward h w . transpose) xs

    test_size :: (Elt a, IsFloating a, RealFloat a, Similar a) => PowerOf2Array (Complex a) -> Property
    test_size (PowerOf2Array xs)
      = let Z:.h:.w = arrayShape xs
        in     arrayShape xs
           ~?= arrayShape (run1 backend (fft2D' Forward w h) xs)
