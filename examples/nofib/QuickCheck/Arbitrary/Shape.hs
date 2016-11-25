{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module QuickCheck.Arbitrary.Shape where

import Test.QuickCheck
import Data.Array.Accelerate                            ( Shape, Z(..), (:.)(..), DIM0, DIM1, DIM2, DIM3, DIM4 )
import qualified Data.Array.Accelerate.Array.Sugar      as Sugar


instance Arbitrary DIM0 where
  arbitrary     = return Z
  shrink        = return

instance Arbitrary DIM1 where
  arbitrary     = do
    NonNegative n <- arbitrary
    return (Z :. n)

  shrink (Z :. n) = [ Z :. n' | n' <- shrink n, n' >= 0 ]

instance Arbitrary DIM2 where
  arbitrary     = do
    NonNegative w <- arbitrary
    NonNegative h <- arbitrary
    return (Z :. h :. w)

  shrink (Z :. h :. w) =
    [ Z :. h' :. w'
        | h' <- shrink h, h' >= 0
        , w' <- shrink w, w' >= 0
    ]

instance Arbitrary DIM3 where
  arbitrary     = do
    NonNegative w <- arbitrary
    NonNegative h <- arbitrary
    NonNegative d <- arbitrary
    return (Z :. h :. w :. d)

  shrink (Z :. h :. w :. d) =
    [ Z :. h' :. w' :. d'
        | h' <- shrink h, h' >= 0
        , w' <- shrink w, w' >= 0
        , d' <- shrink d, d' >= 0
    ]

instance Arbitrary DIM4 where
  arbitrary     = do
    NonNegative w <- arbitrary
    NonNegative h <- arbitrary
    NonNegative d <- arbitrary
    NonNegative t <- arbitrary
    return (Z :. h :. w :. d :. t)

  shrink (Z :. h :. w :. d :. t) =
    [ Z :. h' :. w' :. d' :. t'
        | h' <- shrink h, h' >= 0
        , w' <- shrink w, w' >= 0
        , d' <- shrink d, d' >= 0
        , t' <- shrink t, t' >= 0
    ]


-- Generate an arbitrary shape with approximately this many elements in each
-- dimension. If the generated shape does not contain approximately the
-- specified number of elements (within 10%), the shape is thrown out and a new
-- one is generated.
--
-- NOTE:
--   * Shapes of zero dimension ignore the target size.
--   * Requesting high dimension shapes of very few elements may take same time
--     to generate an appropriate random instance.
--
arbitraryShape :: forall sh. (Shape sh, Arbitrary sh) => Int -> Gen sh
arbitraryShape size =
  let
      eps       = 0.1 :: Double
      dim       = Sugar.rank (undefined :: sh)
      target
        | dim == 0      = 1
        | otherwise     = size * dim

      wiggle    = round $ fromIntegral target * eps
      minsize   = target - wiggle
      maxsize   = target + wiggle
  in
  arbitrary `suchThat` \sh -> let n = Sugar.size sh
                              in  n >= minsize && n <= maxsize

