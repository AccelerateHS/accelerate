{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module QuickCheck.Arbitrary.Shape where

import Test.QuickCheck
import Data.Array.Accelerate                            ( Shape, Z(..), (:.)(..) )
import qualified Data.Array.Accelerate.Array.Sugar      as Sugar


instance Arbitrary Z where
  arbitrary     = return Z
  shrink _      = []

instance Arbitrary sh => Arbitrary (sh :. Int) where
  arbitrary = do
    sh             <- arbitrary
    NonNegative sz <- arbitrary
    return (sh :. sz)
  --
  shrink (sh :. sz) =
    [ sh  :. sz' | sz' <- shrink sz, sz' >= 0 ] ++
    [ sh' :. sz  | sh' <- shrink sh ]


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

