-- |
-- Module      : Main
-- Copyright   : [2022] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Main where

import Test.Tasty

import qualified Data.Array.Accelerate.Test.Annotations

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "tests"
    [ Data.Array.Accelerate.Test.Annotations.tests
    ]
