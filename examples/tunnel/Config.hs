{-# LANGUAGE TemplateHaskell #-}

module Config where

import Data.Label
import System.Console.GetOpt

data Config = Config
  {
    _configWidth        :: Int
  , _configHeight       :: Int
  , _configZoom         :: Int
  }
  deriving Show

$(mkLabels [''Config])

defaults :: Config
defaults = Config
  { _configWidth        = 320
  , _configHeight       = 240
  , _configZoom         = 2
  }

options :: [OptDescr (Config -> Config)]
options =
  [ Option []   ["width"]       (ReqArg (set configWidth . read) "INT")         "visualisation size (320)"
  , Option []   ["height"]      (ReqArg (set configHeight . read) "INT")        "visualisation size (240)"
  , Option []   ["zoom"]        (ReqArg (set configZoom . read) "INT")          "pixel replication factor (2)"
  ]

header :: [String]
header =
  [ "accelerate-tunnel (c) [2016] The Accelerate Team"
  , ""
  , "Usage: accelerate-tunnel [OPTIONS]"
  , ""
  ]

footer :: [String]
footer = [ "" ]

