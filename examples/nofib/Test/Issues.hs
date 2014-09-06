
module Test.Issues (

  test_issues

) where

import Config

import Test.Framework
import Test.Issues.Issue137
import Test.Issues.Issue168
import Test.Issues.Issue184
import Test.Issues.Issue185


test_issues :: Config -> Test
test_issues conf =
  testGroup "issues"
    [
      test_issue137 conf
    , test_issue168 conf
    , test_issue184 conf
    , test_issue185 conf
    ]

