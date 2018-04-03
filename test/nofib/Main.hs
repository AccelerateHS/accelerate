-- |
-- Module      : nofib-interpreter
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
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

