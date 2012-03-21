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
test_permute opt = testGroup "permute"
  [ test_histogram
  ]
  where
    --
    -- Generate a random vector, then build a histogram by mapping the elements
    -- into a smaller range [0,n)
    --
    test_histogram = testGroup "histogram" $ catMaybes
      [ testIntegralElt int32  (undefined :: Int32)
      , testIntegralElt int32  (undefined :: Word32)
      , testIntegralElt int64  (undefined :: Int64)
      , testIntegralElt int64  (undefined :: Word64)
      , testFloatingElt float  (undefined :: Float)
      , testFloatingElt double (undefined :: Double)
      ]

    testIntegralElt :: forall e. (Elt e, Integral e, IsIntegral e, Arbitrary e) => (Options :-> Bool) -> e -> Maybe Test
    testIntegralElt ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just . testProperty (show (typeOf (undefined :: e))) $
          sized            $ \n                 ->
          forAll arbitrary $ \(xs :: Vector e)  ->
            run opt (histogramAcc n Acc.fromIntegral xs) .==. histogramRef n P.fromIntegral xs

    testFloatingElt :: forall e. (Elt e, RealFrac e, IsFloating e, Arbitrary e) => (Options :-> Bool) -> e -> Maybe Test
    testFloatingElt ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just . testProperty (show (typeOf (undefined :: e))) $
          sized            $ \n                 ->
          forAll arbitrary $ \(xs :: Vector e)  ->
            run opt (histogramAcc n Acc.floor xs) .==. histogramRef n P.floor xs

    histogramAcc n f xs =
      let n'        = unit (constant n)
          xs'       = use xs
          zeros     = generate (constant (Z :. n)) (const 0)
          ones      = generate (shape xs')        (const 1)
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
test_backpermute opt = testGroup "backpermute"
  [ test_reverse
  , test_transpose
  ]
  where
    --
    -- Reverse a 1D vector
    --
    test_reverse = testGroup "reverse" $ catMaybes
      [ testElt int32  (undefined :: Int32)
      , testElt int64  (undefined :: Int64)
      , testElt int32  (undefined :: Word32)
      , testElt int64  (undefined :: Word64)
      , testElt float  (undefined :: Float)
      , testElt double (undefined :: Double)
      ]
      where
        testElt :: forall e. (Elt e, Similar e, Arbitrary e) => (Options :-> Bool) -> e -> Maybe Test
        testElt ok _
          | P.not (get ok opt)  = Nothing
          | otherwise           = Just . testProperty (show (typeOf (undefined::e))) $
              \(xs :: Vector e) ->
                run opt (reverseAcc xs) .==. reverseRef xs

        reverseRef xs = fromList (arrayShape xs) (reverse $ toList xs)
        reverseAcc xs =
          let xs'       = use xs
              n         = unindex1 $ shape xs'
          in
          backpermute (shape xs') (\ix -> index1 $ n - unindex1 ix - 1) xs'
    --
    -- Transpose a 2D matrix
    --
    test_transpose = testGroup "transpose" $ catMaybes
      [ testElt int32  (undefined :: Int32)
      , testElt int64  (undefined :: Int64)
      , testElt int32  (undefined :: Word32)
      , testElt int64  (undefined :: Word64)
      , testElt float  (undefined :: Float)
      , testElt double (undefined :: Double)
      ]
      where
        testElt :: forall e. (Elt e, Similar e, Arbitrary e) => (Options :-> Bool) -> e -> Maybe Test
        testElt ok _
          | P.not (get ok opt)  = Nothing
          | otherwise           = Just . testProperty (show (typeOf (undefined::e))) $
              \(xs :: Array DIM2 e) ->
                run opt (transposeAcc xs) .==. transposeRef xs

        transposeAcc xs =
          let xs'       = use xs
              swap      = lift1 $ \(Z:.x:.y) -> Z:.y:.x :: Z :. Exp Int :. Exp Int
          in
          backpermute (swap (shape xs')) swap xs'

        transposeRef xs =
          let swap (Z:.x:.y)    = Z :. y :. x
          in  newArray (swap (arrayShape xs)) (\ix -> indexArray xs (swap ix))

