{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Array.Accelerate.Examples.Internal.ParseArgs.TestFramework (

  -- ** Test framework options
  module Data.Array.Accelerate.Examples.Internal.ParseArgs.TestFramework

) where

import Data.Monoid
import Data.Label.Derive
import Test.Framework                           ( RunnerOptions, RunnerOptions', optionsDescription, SuppliedRunnerOptions )
import System.Console.GetOpt

type Config = RunnerOptions

defaultConfig :: Config
defaultConfig = mempty

defaultOptions :: [OptDescr SuppliedRunnerOptions]
defaultOptions = optionsDescription

$(mkLabelsNamed id [''RunnerOptions'])

