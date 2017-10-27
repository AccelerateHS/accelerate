{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Test.FFT (

  test_fft

)  where

import Config

import Test.QuickCheck                                          hiding ( (.&.) )
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Data.Array.Accelerate.Math.DFT
import Data.Array.Accelerate.Math.FFT

import QuickCheck.Arbitrary.Array                               ()
import Data.Array.Accelerate                                    as A hiding ( (!), Ord(..), Eq(..) )
import Data.Array.Accelerate.Examples.Internal                  as A
import Data.Array.Accelerate.Data.Complex

import Data.Label
import Data.Maybe
import Data.Typeable
import Prelude                                                  as P


test_fft :: Backend -> Config -> Test
test_fft backend opt = testGroup "fft" $ catMaybes
  [ testElt configFloat  (undefined::Float)
  , testElt configDouble (undefined::Double)
  ]
  where
    testElt :: forall a. (Similar a, Arbitrary a, P.RealFloat a, A.RealFloat a, A.IsFloating a, A.FromIntegral Int a)
            => (Config :-> Bool)
            -> a
            -> Maybe Test
    testElt ok _
      | P.not (get ok opt)    = Nothing
      | otherwise             = Just $ testGroup (show (typeOf (undefined :: a)))
          [ testDIM1
          , testDIM2
          , testDIM3
          ]
      where
        testDIM1 :: Test
        testDIM1 =
          testGroup "DIM1"
            [ testProperty "ifft.fft" (test_fft_ifft :: Array DIM1 (Complex a) -> Property)
            , testProperty "fft==dft" (test_fft_dft  :: Array DIM1 (Complex a) -> Property)
            ]
            where
              test_fft_ifft xs =
                arraySize (arrayShape xs) > 0 ==>
                  runN backend (fft1D Inverse . fft1D Forward) xs ~?= xs

              test_fft_dft xs =
                arraySize (arrayShape xs) > 0 ==>
                  runN backend (fft1D Forward) xs ~?= run1 backend dft xs

        testDIM2 :: Test
        testDIM2 =
          testGroup "DIM2"
            [ testProperty "ifft.fft"  (test_fft_ifft :: Array DIM2 (Complex a) -> Property)
            , testProperty "transpose" (test_trans    :: Array DIM2 (Complex a) -> Property)
            ]
            where
              test_trans xs =
                arraySize (arrayShape xs) > 0 ==>
                      runN backend (A.transpose . fft2D Forward) xs
                  ~?= runN backend (fft2D Forward . A.transpose) xs

              test_fft_ifft xs =
                arraySize (arrayShape xs) > 0 ==>
                  runN backend (fft2D Inverse . fft2D Forward) xs ~?= xs

        testDIM3 :: Test
        testDIM3 =
          testGroup "DIM3"
            [ testProperty "ifft.fft"  (test_fft_ifft :: Array DIM3 (Complex a) -> Property)
            ]
            where
              test_fft_ifft xs =
                arraySize (arrayShape xs) > 0 ==>
                  runN backend (fft3D Inverse . fft3D Forward) xs ~?= xs

