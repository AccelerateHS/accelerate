{-# LANGUAGE TemplateHaskell #-}

module Config where

import Data.Label
import System.Console.GetOpt

data Config = Config
  {
    _configSize         :: Int
  , _configZoom         :: Int
  , _configScale        :: Float
  , _configDegree       :: Int
  }
  deriving Show

$(mkLabels [''Config])

defaults :: Config
defaults = Config
  { _configSize         = 200
  , _configZoom         = 3
  , _configScale        = 30
  , _configDegree       = 5
  }


options :: [OptDescr (Config -> Config)]
options =
  [ Option []   ["size"]        (ReqArg (set configSize . read) "INT")          "visualisation size (200)"
  , Option []   ["zoom"]        (ReqArg (set configZoom . read) "INT")          "pixel replication factor (3)"
  , Option []   ["scale"]       (ReqArg (set configScale . read) "FLOAT")       "feature size of visualisation (30)"
  , Option []   ["degree"]      (ReqArg (set configDegree . read) "INT")        "number of waves to sum for each point (5)"
  ]


header :: [String]
header =
  [ "accelerate-crystal (c) [2011..2014] The Accelerate Team"
  , ""
  , "Usage: accelerate-crystal [OPTIONS]"
  , ""
  ]

footer :: [String]
footer = [ "" ]

