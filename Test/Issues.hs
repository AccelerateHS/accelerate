
module Test.Issues (

  test_issues

) where

import Config

import Test.Framework
import Test.Issues.Issue137
import Test.Issues.Issue168


test_issues :: Config -> Test
test_issues conf =
  testGroup "issues"
    [
      test_issue137 conf
    , test_issue168 conf
    ]

