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

main :: IO ()
main = nofib runN

