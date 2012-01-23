{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Arbitrary.Array where

import Arbitrary.Shape

import Test.QuickCheck
import Data.Array.Accelerate.Array.Sugar


instance (Shape sh, Elt e, Arbitrary sh, Arbitrary e) => Arbitrary (Array sh e) where
  arbitrary     = do
    sh          <- sized arbitrarySmallShape
    adata       <- vectorOf (size sh) arbitrary
    return      $! fromList sh adata

{--
  -- This is fine for vectors, because the lower shape component is always Z and
  -- thus valid. This is not the case for higher dimensional arrays, and we are
  -- not properly filling out this regions.
  --
  shrink arr    =
    let (sh :. sz)      = shape arr
        (_, sh')        = shapeToRange sh
        indices         = [ map (sh' :.) zs | zs <- shrink [0 .. sz-1] ]
    in
    [ fromList (sh :. length ix) (map (arr!) ix) | ix <- indices ]
--}

