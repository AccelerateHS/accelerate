{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Arbitrary.Shape where

import Test.QuickCheck
import Data.Array.Accelerate.Array.Sugar


instance Arbitrary Z where
  arbitrary     = return Z
  shrink        = return

-- Keep shapes relatively small, because we are generating arrays via lists, but
-- scale the 'sized' parameter so we still get non-trivial test vectors.
--
instance (Shape sh, Arbitrary sh) => Arbitrary (sh :. Int) where
  arbitrary     = do
    sh          <- arbitrary
    sz          <- sized $ \n ->
      let nMax   = 2048  `div` max 1 (size sh)
          nMaxed = (n*2) `mod` nMax
      in
      choose (0, nMaxed)
    --
    return (sh :. sz)

  shrink (sh :. sz) = [ sh' :. sz' | sh' <- shrink sh, sz' <- shrink sz ]


-- Generate an arbitrary shape, where each dimension is less than some specific
-- value. This will generate zero-sized shapes, but will take care not to
-- generate too many of them.
--
arbitrarySmallShape :: forall sh. (Shape sh, Arbitrary sh) => Int -> Gen sh
arbitrarySmallShape maxDim = resize maxDim arbitrary

-- Generate a shape as above, but also restrict the dimensions to be either all
-- zero, or all non-zero.
--
arbitrarySmallNonEmptyShape :: forall sh. (Shape sh, Arbitrary sh) => Int -> Gen sh
arbitrarySmallNonEmptyShape maxDim = do
  sh            <- resize maxDim arbitrary      :: Gen sh
  let ix         = map (`mod` max 1 maxDim) (shapeToList sh)
      sh'
        | all (== 0) ix = ix
        | otherwise     = map (max 1) ix
  --
  return (listToShape sh' :: sh)

