{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module QuickCheck.Arbitrary.Shape where

import Test.QuickCheck
import Data.Array.Accelerate                            ( Shape, Z(..), (:.)(..), DIM0, DIM1, DIM2 )
import qualified Data.Array.Accelerate.Array.Sugar      as Sugar


instance Arbitrary DIM0 where
  arbitrary     = return Z
  shrink        = return

instance Arbitrary DIM1 where
  arbitrary     = do
    n   <- sized $ \n -> choose (0,n)
    return (Z :. n)

  shrink (Z :. n) = [ Z :. n' | n' <- shrink n ]

instance Arbitrary DIM2 where
  arbitrary     = sized $ \n -> do
    w   <- choose (0, n)
    h   <- choose (0, n)
    return (Z :. h :. w)

  shrink (Z :. h :. w) = [ Z :. h' :. w' | h' <- shrink h, w' <- shrink w ]


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
      dim       = Sugar.dim (undefined :: sh)
      target
        | dim == 0      = 1
        | otherwise     = size * dim

      wiggle    = round $ fromIntegral target * eps
      minsize   = target - wiggle
      maxsize   = target + wiggle
  in
  arbitrary `suchThat` \sh -> let n = Sugar.size sh
                              in  n >= minsize && n <= maxsize

