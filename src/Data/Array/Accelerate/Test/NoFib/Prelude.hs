{-# LANGUAGE RankNTypes #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Prelude
-- Copyright   : [2009..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Test.NoFib.Prelude (

  test_prelude,

  module Data.Array.Accelerate.Test.NoFib.Prelude.Map,
  module Data.Array.Accelerate.Test.NoFib.Prelude.ZipWith,
  module Data.Array.Accelerate.Test.NoFib.Prelude.Fold,
  module Data.Array.Accelerate.Test.NoFib.Prelude.Scan,
  module Data.Array.Accelerate.Test.NoFib.Prelude.Backpermute,
  module Data.Array.Accelerate.Test.NoFib.Prelude.Permute,
  module Data.Array.Accelerate.Test.NoFib.Prelude.Filter,
  module Data.Array.Accelerate.Test.NoFib.Prelude.Stencil,

) where

import Test.Tasty

import Data.Array.Accelerate.Test.NoFib.Base
import Data.Array.Accelerate.Test.NoFib.Prelude.Map
import Data.Array.Accelerate.Test.NoFib.Prelude.ZipWith
import Data.Array.Accelerate.Test.NoFib.Prelude.Fold
import Data.Array.Accelerate.Test.NoFib.Prelude.Scan
import Data.Array.Accelerate.Test.NoFib.Prelude.Backpermute
import Data.Array.Accelerate.Test.NoFib.Prelude.Permute
import Data.Array.Accelerate.Test.NoFib.Prelude.Filter
import Data.Array.Accelerate.Test.NoFib.Prelude.Stencil


test_prelude :: RunN -> TestTree
test_prelude runN =
  testGroup "prelude"
    [ test_map runN
    , test_zipWith runN
    , test_fold runN
    , test_foldSeg runN
    , test_backpermute runN
    , test_permute runN
    , test_scanl runN
    , test_scanl1 runN
    , test_scanl' runN
    , test_scanr runN
    , test_scanr1 runN
    , test_scanr' runN
    , test_scanlSeg runN
    , test_scanl1Seg runN
    , test_scanl'Seg runN
    , test_scanrSeg runN
    , test_scanr1Seg runN
    , test_scanr'Seg runN
    , test_filter runN
    , test_stencil runN
    -- , test_replicate runN conf
    -- , test_sequences runN conf
    ]

