{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Main
-- Copyright   : [2016..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Main where

import Formatting
import Data.Array.Accelerate.Error

-- This should be replaced by the executable built via the Makefiles. If it
-- hasn't been something went wrong...
--
main :: IO ()
main = internalError ("Unexpected error building application " % squoted builder) EXECUTABLE

