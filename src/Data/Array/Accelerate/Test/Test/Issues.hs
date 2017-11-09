
module Test.Issues (

  test_issues

) where

import Config

import Test.Framework
import Test.Issues.Issue93
import Test.Issues.Issue102
import Test.Issues.Issue114
import Test.Issues.Issue119
import Test.Issues.Issue123
import Test.Issues.Issue137
import Test.Issues.Issue168
import Test.Issues.Issue184
import Test.Issues.Issue185
import Test.Issues.Issue187
import Test.Issues.Issue228
import Test.Issues.Issue255
import Test.Issues.Issue264
import Test.Issues.Issue286
import Test.Issues.Issue287
import Test.Issues.Issue288
import Test.Issues.Issue362

import Data.Array.Accelerate.Examples.Internal


test_issues :: Backend -> Config -> Test
test_issues be conf =
  testGroup "issues"
    [
      test_issue93  be conf
    , test_issue102 be conf
    , test_issue114 be conf
    , test_issue119 be conf
    , test_issue123 be conf
    , test_issue137 be conf
    , test_issue168 be conf
    , test_issue184 be conf
    , test_issue185 be conf
    , test_issue187 be conf
    , test_issue228 be conf
    , test_issue255 be conf
    , test_issue264 be conf
    , test_issue286 be conf
    , test_issue287 be conf
    , test_issue288 be conf
    , test_issue362 be conf
    ]

