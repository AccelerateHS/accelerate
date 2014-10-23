{-# LANGUAGE TemplateHaskell #-}

module Config where

import Data.Label
import System.Console.GetOpt


-- Configuration options
--
data Config = Config
  deriving Show

$(mkLabels [''Config])

defaults :: Config
defaults = Config


-- The set of available command line options
--
options :: [OptDescr (Config -> Config)]
options = []


-- Command line decoration
--
header :: [String]
header =
  [ "accelerate-kmeans (c) [2014] The Accelerate Team"
  , ""
  , "Usage: accelerate-kmeans [OPTIONS]"
  , ""
  , "Be sure to first build and run the GenSamples program, in order to"
  , "generate some random data points."
  , ""
  ]

footer :: [String]
footer = [ "" ]

