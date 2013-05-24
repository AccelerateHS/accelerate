
module Test.IO where

import Config

import Test.Framework
import Test.IO.Ptr


test_io :: Config -> Test
test_io conf =
  testGroup "io"
    [ test_ptr conf
    ]

