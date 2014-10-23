
module Test.Prelude where

import Config

import Test.Framework
import Test.Prelude.Filter
import Test.Prelude.IndexSpace
import Test.Prelude.Mapping
import Test.Prelude.PrefixSum
import Test.Prelude.Reduction
import Test.Prelude.Replicate
import Test.Prelude.Stencil
import Test.Prelude.Sequencing

import Data.Array.Accelerate.Examples.Internal


test_prelude :: Backend -> Config -> Test
test_prelude be conf =
  testGroup "prelude"
    [ test_map be conf
    , test_zipWith be conf
    , test_foldAll be conf
    , test_fold be conf
    , test_backpermute be conf
    , test_permute be conf
    , test_prefixsum be conf            -- requires fold
    , test_foldSeg be conf              -- requires scan
    , test_stencil be conf
    , test_replicate be conf
    , test_filter be conf
    , test_sequences be conf
    ]
