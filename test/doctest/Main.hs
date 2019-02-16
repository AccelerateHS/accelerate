-- |
-- Module      : Main
-- Copyright   : [2017..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
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

