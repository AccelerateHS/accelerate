{-# LANGUAGE RankNTypes #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Misc
-- Copyright   : [2025..] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Test.NoFib.Misc (
  test_misc,
) where

import Test.Tasty

import Data.Array.Accelerate.Test.NoFib.Base
import Data.Array.Accelerate.Test.NoFib.Misc.Cache
import Data.Array.Accelerate.Test.NoFib.Misc.Scanl1


test_misc :: RunN -> TestTree
test_misc runN =
  testGroup "misc"
    [ test_misc_cache runN
    , test_misc_scanl1 runN
    ]
