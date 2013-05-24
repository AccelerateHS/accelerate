
module Test.Prelude where

import Config

import Test.Framework
import Test.Prelude.IndexSpace
import Test.Prelude.Mapping
import Test.Prelude.PrefixSum
import Test.Prelude.Reduction
import Test.Prelude.Replicate
import Test.Prelude.Stencil


test_prelude :: Config -> Test
test_prelude conf =
  testGroup "prelude"
    [ test_map conf
    , test_zipWith conf
    , test_foldAll conf
    , test_fold conf
    , test_backpermute conf
    , test_permute conf
    , test_prefixsum conf       -- requires fold
    , test_foldSeg conf         -- requires scan
    , test_stencil conf
    , test_replicate conf
    ]

