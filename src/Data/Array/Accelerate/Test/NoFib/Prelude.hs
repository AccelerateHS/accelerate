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

) where

import Test.Tasty

import Data.Array.Accelerate.Test.NoFib.Base
import Data.Array.Accelerate.Test.NoFib.Prelude.Map
import Data.Array.Accelerate.Test.NoFib.Prelude.ZipWith
import Data.Array.Accelerate.Test.NoFib.Prelude.Fold

-- import Test.Prelude.Backpermute
-- import Test.Prelude.Filter
-- import Test.Prelude.Permute
-- import Test.Prelude.Replicate
-- import Test.Prelude.Scan
-- import Test.Prelude.Sequences
-- import Test.Prelude.Stencil


test_prelude :: RunN -> TestTree
test_prelude runN =
  testGroup "Prelude"
    [ test_map runN
    , test_zipWith runN
    , test_fold runN
    , test_foldSeg runN
    -- , test_backpermute runN conf
    -- , test_permute runN conf
    -- , test_scan runN conf             -- requires fold
    -- , test_foldSeg runN conf          -- requires scan
    -- , test_stencil runN conf
    -- , test_replicate runN conf
    -- , test_filter runN conf
    -- , test_sequences runN conf
    ]

