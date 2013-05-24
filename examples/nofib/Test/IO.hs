
module Test.IO where

import Config

import Test.Framework
import Test.IO.Ptr
import Test.IO.Vector


test_io :: Config -> Test
test_io conf =
  testGroup "io"
    [ test_ptr conf
    , test_vector conf
    ]

