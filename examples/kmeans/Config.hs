{-# LANGUAGE TemplateHaskell #-}

module Config where

import ParseArgs
import Data.Label


-- Configuration options
--
data Config
  = Config
  {
    _configBackend      :: Backend
  , _configHelp         :: Bool
  }
  deriving Show

$(mkLabels [''Config])

defaults :: Config
defaults = Config
  {
    _configBackend      = maxBound
  , _configHelp         = False
  }


-- The set of available command line options
--
options :: [OptDescr (Config -> Config)]
options =
  [ Option  ['h', '?'] ["help"]
            (NoArg (set configHelp True))
            "show this help message"
  ]


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
footer = []

