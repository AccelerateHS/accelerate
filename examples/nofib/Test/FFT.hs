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
import QuickCheck.Arbitrary.Array
import QuickCheck.Arbitrary.Shape

import Data.Array.Accelerate.Math.DFT
import Data.Array.Accelerate.Math.FFT

import Data.Array.Accelerate                                    as A hiding ( (!), Ord(..), Eq(..) )
import Data.Array.Accelerate.Examples.Internal                  as A
import Data.Array.Accelerate.Array.Sugar                        ( (!) )
import Data.Array.Accelerate.Data.Complex

import Data.Bits
import Data.Label
import Data.Maybe
import Data.Typeable
import Data.List                                                as P
import Prelude                                                  as P


newtype PowerOf2Array sh e = PowerOf2Array (Array sh e)
  deriving Show

instance (Arbitrary e, Elt e) => Arbitrary (PowerOf2Array DIM1 e) where
  arbitrary = do
    Z :. n <- sized arbitraryShape
    arr    <- arbitraryArray (Z :. ceilPow2 n)
    return $  PowerOf2Array arr

  shrink (PowerOf2Array arr) =
    let Z :. n = arrayShape arr
    in
    [ PowerOf2Array (fromList (Z :. P.length slx) [ arr ! (Z :. x) | x <- slx ])
        | slx <- P.filter (isPow2 . P.length) $ P.map nub $ shrink [0 .. n-1]
    ]

instance (Arbitrary e, Elt e) => Arbitrary (PowerOf2Array DIM2 e) where
  arbitrary = do
    Z :. height :. width <- sized arbitraryShape
    arr                  <- arbitraryArray (Z :. ceilPow2 height :. ceilPow2 width)
    return $ PowerOf2Array arr
  --
  shrink (PowerOf2Array arr) =
    let Z :. height :. width = arrayShape arr
    in
    [ PowerOf2Array (fromList (Z :. P.length sly :. P.length slx) [ arr ! (Z :. y :. x) | y <- sly, x <- slx ])
        | sly <- P.filter (isPow2 . P.length) $ P.map nub $ shrink [0 .. height - 1]
        , slx <- P.filter (isPow2 . P.length) $ P.map nub $ shrink [0 .. width  - 1]
    ]

instance (Arbitrary e, Elt e) => Arbitrary (PowerOf2Array DIM3 e) where
  arbitrary = do
    Z :. depth :. height :. width <- sized arbitraryShape
    arr                           <- arbitraryArray (Z :. ceilPow2 depth :. ceilPow2 height :. ceilPow2 width)
    return $ PowerOf2Array arr
  --
  shrink (PowerOf2Array arr) =
    let Z :. depth :. height :. width = arrayShape arr
    in
    [ PowerOf2Array (fromList (Z :. P.length slz :. P.length sly :. P.length slx) [ arr ! (Z :. z :. y :. x) | z <- slz, y <- sly, x <- slx ])
        | slz <- P.filter (isPow2 . P.length) $ P.map nub $ shrink [0 .. depth  - 1]
        , sly <- P.filter (isPow2 . P.length) $ P.map nub $ shrink [0 .. height - 1]
        , slx <- P.filter (isPow2 . P.length) $ P.map nub $ shrink [0 .. width  - 1]
    ]

isPow2 :: Int -> Bool
isPow2 n =
  n .&. (n - 1) == 0

ceilPow2 :: Int -> Int
ceilPow2 n
  | isPow2 n  = n
  | otherwise =
      let x = P.logBase 2 (P.fromIntegral n) :: Double
          y = P.floor x + 1
      in
      1 `shiftL` y


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
          -- , testDIM3 -- too slow?
          ]
      where
        testDIM1 :: Test
        testDIM1 =
          testGroup "DIM1"
            [ testProperty "ifft.fft" (test_fft_ifft :: PowerOf2Array DIM1 (Complex a) -> Property)
            , testProperty "fft==dft" (test_fft_dft  :: PowerOf2Array DIM1 (Complex a) -> Property)
            ]
            where
              test_fft_ifft (PowerOf2Array xs) =
                let sh = arrayShape xs
                in  arraySize sh > 0 ==>
                      run1 backend (fft1D' Inverse sh . fft1D' Forward sh) xs ~?= xs

              test_fft_dft (PowerOf2Array xs) =
                let sh = (arrayShape xs)
                in  arraySize sh > 0 ==>
                      run1 backend (fft1D' Forward sh) xs ~?= run1 backend dft xs

        testDIM2 :: Test
        testDIM2 =
          testGroup "DIM2"
            [ testProperty "ifft.fft"  (test_fft_ifft :: PowerOf2Array DIM2 (Complex a) -> Property)
            , testProperty "transpose" (test_trans    :: PowerOf2Array DIM2 (Complex a) -> Property)
            ]
            where
              test_trans (PowerOf2Array xs) =
                let sh = arrayShape xs
                in  arraySize sh > 0 ==>
                      run1 backend (A.transpose . fft2D' Forward sh) xs
                  ~?= run1 backend (fft2D' Forward sh . A.transpose) xs

              test_fft_ifft (PowerOf2Array xs) =
                let sh = arrayShape xs
                in  arraySize (arrayShape xs) > 0 ==>
                      run1 backend (fft2D' Inverse sh . fft2D' Forward sh) xs ~?= xs

        -- testDIM3 :: Test
        -- testDIM3 =
        --   testGroup "DIM3"
        --     [ testProperty "ifft.fft"  (test_fft_ifft :: PowerOf2Array DIM3 (Complex a) -> Property)
        --     ]
        --     where
        --       test_fft_ifft (PowerOf2Array xs) =
        --         let sh = arrayShape xs
        --         in  arraySize (arrayShape xs) > 0 ==>
        --               run1 backend (fft3D' Inverse sh . fft3D' Forward sh) xs ~?= xs


    -- test_dft_fft :: (Similar a, P.RealFloat a, A.RealFloat a, A.IsFloating a, A.FromIntegral Int a) => PowerOf2Array (Complex a) -> Property
    -- test_dft_fft (PowerOf2Array xs) =
    --   arraySize (arrayShape xs) > 0 ==>
    --     let Z :. h :. w = arrayShape xs
    --     in
    --     run1 backend (fft2D' Forward w h) xs ~?= run1 backend dft xs

