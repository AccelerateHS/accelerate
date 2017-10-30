-- |
-- Module      : Main
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Main where

import Build_doctests                           ( flags, pkgs, module_sources )
import Data.Foldable                            ( traverse_ )
import Test.DocTest

main :: IO ()
main = do
  traverse_ putStrLn args
  doctest args
  where
    args = flags ++ pkgs ++ module_sources

