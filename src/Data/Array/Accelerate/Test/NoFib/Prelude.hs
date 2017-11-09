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

module Data.Array.Accelerate.Test.NoFib.Prelude
  where

import Test.Tasty

import Data.Array.Accelerate.Test.NoFib.Base
import Data.Array.Accelerate.Test.NoFib.Config
import Data.Array.Accelerate.Test.NoFib.Prelude.Map

-- import Test.Prelude.Backpermute
-- import Test.Prelude.Filter
-- import Test.Prelude.Fold
-- import Test.Prelude.Permute
-- import Test.Prelude.Replicate
-- import Test.Prelude.Scan
-- import Test.Prelude.Sequences
-- import Test.Prelude.Stencil
-- import Test.Prelude.ZipWith


test_prelude :: RunN -> Config -> TestTree
test_prelude runN opt =
  testGroup "Prelude"
    [ test_map runN opt
    -- , test_zipWith runN opt
    -- , test_foldAll runN opt
    -- , test_fold runN opt
    -- , test_backpermute runN opt
    -- , test_permute runN opt
    -- , test_scan runN opt              -- requires fold
    -- , test_foldSeg runN opt           -- requires scan
    -- , test_stencil runN opt
    -- , test_replicate runN opt
    -- , test_filter runN opt
    -- , test_sequences runN opt
    ]

