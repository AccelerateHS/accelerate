{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module:      : Data.Array.Accelerate.Examples.Internal.TestFramework.Config
-- Copyright    : [2014] Trevor L. McDonell
-- License      : BSD3
--
-- Maintainer   : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Examples.Internal.TestFramework.Config (

  -- ** Test framework options
  module Data.Array.Accelerate.Examples.Internal.TestFramework.Config

) where

import Data.Monoid
import Data.Label
import Data.Label.Derive
import Test.Framework                           ( RunnerOptions, RunnerOptions', optionsDescription, SuppliedRunnerOptions )
import System.Console.GetOpt

import Data.Array.Accelerate.Examples.Internal.Backend

$(mkLabelsNamed id [''RunnerOptions'])

type Config = RunnerOptions

defaultConfig :: Backend -> Config
defaultConfig b = set ropt_threads (concurrentBackends b) mempty

defaultOptions :: [OptDescr SuppliedRunnerOptions]
defaultOptions = optionsDescription


