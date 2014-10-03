{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK hide #-}

module ParseArgs.TestFramework
  where

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

