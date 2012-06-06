{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.IndexSpace where

import Prelude                                          as P
import Data.Label
import Data.Maybe
import Data.Typeable
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Config
import Test.Base
import Arbitrary.Array                                  ( )
import Data.Array.Unboxed                               as IArray hiding ( Array )
import Data.Array.Accelerate                            as Acc
import Data.Array.Accelerate.Array.Sugar                ( newArray )


--
-- Forward permutation ---------------------------------------------------------
--

test_permute :: Options -> Test
test_permute opt = testGroup "permute" $ catMaybes
  [ testIntegralElt int8   (undefined :: Int8)
  , testIntegralElt int16  (undefined :: Int16)
  , testIntegralElt int32  (undefined :: Int32)
  , testIntegralElt int64  (undefined :: Int64)
  , testIntegralElt int8   (undefined :: Word8)
  , testIntegralElt int16  (undefined :: Word16)
  , testIntegralElt int32  (undefined :: Word32)
  , testIntegralElt int64  (undefined :: Word64)
  , testFloatingElt float  (undefined :: Float)
  , testFloatingElt double (undefined :: Double)
  ]
  where
    testIntegralElt :: forall e. (Elt e, Integral e, IsIntegral e, Arbitrary e) => (Options :-> Bool) -> e -> Maybe Test
    testIntegralElt ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testGroup (show (typeOf (undefined :: e)))
          [ testProperty "histogram" (test_histogram Acc.fromIntegral P.fromIntegral :: Vector e -> Property)
          ]

    testFloatingElt :: forall e. (Elt e, RealFrac e, IsFloating e, Arbitrary e) => (Options :-> Bool) -> e -> Maybe Test
    testFloatingElt ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testGroup (show (typeOf (undefined :: e)))
          [ testProperty "histogram" (test_histogram Acc.floor P.floor :: Vector e -> Property)
          ]

    test_histogram f g xs =
      sized $ \n -> run opt (histogramAcc n f xs) .==. histogramRef n g xs

    histogramAcc n f xs =
      let n'        = unit (constant n)
          xs'       = use xs
          zeros     = generate (constant (Z :. n)) (const 0)
          ones      = generate (shape xs')         (const 1)
      in
      permute (+) zeros (\ix -> index1 $ f (xs' Acc.! ix) `mod` the n') ones

    histogramRef n f xs =
      let arr :: UArray Int Int32
          arr =  accumArray (+) 0 (0, n-1) [ (f e `mod` n, 1) | e <- toList xs ]
      in
      fromIArray arr


--
-- Backward permutation --------------------------------------------------------
--

test_backpermute :: Options -> Test
test_backpermute opt = testGroup "backpermute" $ catMaybes
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
    testElt :: forall e. (Elt e, Similar e, Arbitrary e) => (Options :-> Bool) -> e -> Maybe Test
    testElt ok _
      | P.not (get ok opt)  = Nothing
      | otherwise           = Just $ testGroup (show (typeOf (undefined::e)))
          [ testProperty "reverse"   (test_reverse   :: Array DIM1 e -> Property)
          , testProperty "transpose" (test_transpose :: Array DIM2 e -> Property)
          ]

    test_reverse xs   = run opt (reverseAcc xs)   .==. reverseRef xs
    test_transpose xs = run opt (transposeAcc xs) .==. transposeRef xs

    -- Reverse a vector
    --
    reverseAcc xs = Acc.reverse (use xs)
    reverseRef xs = fromList (arrayShape xs) (P.reverse $ toList xs)

    -- Transpose a 2D matrix
    --
    transposeAcc xs = Acc.transpose (use xs)
    transposeRef xs =
      let swap (Z:.x:.y)    = Z :. y :. x
      in  newArray (swap (arrayShape xs)) (\ix -> indexArray xs (swap ix))

