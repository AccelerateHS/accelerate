-- |
-- Module      : nofib-interpreter
-- Copyright   : [2017..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Main where

import Data.Array.Accelerate.Test.NoFib
import Data.Array.Accelerate.Interpreter

import System.Environment

main :: IO ()
main = do
  setEnv "TASTY_INTERPRETER" "True"
  setEnv "TASTY_HEDGEHOG_TESTS" "50"
  nofib runN

