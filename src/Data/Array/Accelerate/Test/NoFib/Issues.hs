{-# LANGUAGE RankNTypes #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Issues
-- Copyright   : [2009..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Test.NoFib.Issues (

  test_issues,

  module Data.Array.Accelerate.Test.NoFib.Issues.Issue93,
  module Data.Array.Accelerate.Test.NoFib.Issues.Issue102,
  module Data.Array.Accelerate.Test.NoFib.Issues.Issue114,
  module Data.Array.Accelerate.Test.NoFib.Issues.Issue119,
  module Data.Array.Accelerate.Test.NoFib.Issues.Issue123,
  module Data.Array.Accelerate.Test.NoFib.Issues.Issue137,
  module Data.Array.Accelerate.Test.NoFib.Issues.Issue168,
  module Data.Array.Accelerate.Test.NoFib.Issues.Issue184,
  module Data.Array.Accelerate.Test.NoFib.Issues.Issue185,
  module Data.Array.Accelerate.Test.NoFib.Issues.Issue187,
  module Data.Array.Accelerate.Test.NoFib.Issues.Issue228,
  module Data.Array.Accelerate.Test.NoFib.Issues.Issue255,
  module Data.Array.Accelerate.Test.NoFib.Issues.Issue264,
  module Data.Array.Accelerate.Test.NoFib.Issues.Issue286,
  module Data.Array.Accelerate.Test.NoFib.Issues.Issue287,
  module Data.Array.Accelerate.Test.NoFib.Issues.Issue288,
  module Data.Array.Accelerate.Test.NoFib.Issues.Issue362,
  module Data.Array.Accelerate.Test.NoFib.Issues.Issue407,
  module Data.Array.Accelerate.Test.NoFib.Issues.Issue409,

) where

import Test.Tasty

import Data.Array.Accelerate.Test.NoFib.Base
import Data.Array.Accelerate.Test.NoFib.Issues.Issue93
import Data.Array.Accelerate.Test.NoFib.Issues.Issue102
import Data.Array.Accelerate.Test.NoFib.Issues.Issue114
import Data.Array.Accelerate.Test.NoFib.Issues.Issue119
import Data.Array.Accelerate.Test.NoFib.Issues.Issue123
import Data.Array.Accelerate.Test.NoFib.Issues.Issue137
import Data.Array.Accelerate.Test.NoFib.Issues.Issue168
import Data.Array.Accelerate.Test.NoFib.Issues.Issue184
import Data.Array.Accelerate.Test.NoFib.Issues.Issue185
import Data.Array.Accelerate.Test.NoFib.Issues.Issue187
import Data.Array.Accelerate.Test.NoFib.Issues.Issue228
import Data.Array.Accelerate.Test.NoFib.Issues.Issue255
import Data.Array.Accelerate.Test.NoFib.Issues.Issue264
import Data.Array.Accelerate.Test.NoFib.Issues.Issue286
import Data.Array.Accelerate.Test.NoFib.Issues.Issue287
import Data.Array.Accelerate.Test.NoFib.Issues.Issue288
import Data.Array.Accelerate.Test.NoFib.Issues.Issue362
import Data.Array.Accelerate.Test.NoFib.Issues.Issue407
import Data.Array.Accelerate.Test.NoFib.Issues.Issue409


test_issues :: RunN -> TestTree
test_issues runN =
  testGroup "issues"
    [ test_issue93  runN
    , test_issue102 runN
    , test_issue114 runN
    , test_issue119 runN
    , test_issue123 runN
    , test_issue137 runN
    , test_issue168 runN
    , test_issue184 runN
    , test_issue185 runN
    , test_issue187 runN
    , test_issue228 runN
    , test_issue255 runN
    , test_issue264 runN
    , test_issue286 runN
    , test_issue287 runN
    , test_issue288 runN
    , test_issue362 runN
    , test_issue407 runN
    , test_issue409 runN
    ]

