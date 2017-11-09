
module Test.Prelude where

import Config
import Data.Array.Accelerate.Examples.Internal

import Test.Prelude.Backpermute
import Test.Prelude.Filter
import Test.Prelude.Fold
import Test.Prelude.Map
import Test.Prelude.Permute
import Test.Prelude.Replicate
import Test.Prelude.Scan
-- import Test.Prelude.Sequences
import Test.Prelude.Stencil
import Test.Prelude.ZipWith


test_prelude :: Backend -> Config -> Test
test_prelude be conf =
  testGroup "prelude"
    [ test_map be conf
    , test_zipWith be conf
    , test_foldAll be conf
    , test_fold be conf
    , test_backpermute be conf
    , test_permute be conf
    , test_scan be conf                 -- requires fold
    , test_foldSeg be conf              -- requires scan
    , test_stencil be conf
    , test_replicate be conf
    , test_filter be conf
    -- , test_sequences be conf
    ]
