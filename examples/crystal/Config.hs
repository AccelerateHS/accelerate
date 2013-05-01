{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

module Config where

import ParseArgs
import Data.Label

data Options = Options
  {
    _optBackend         :: Backend
  , _optSize            :: Int
  , _optZoom            :: Int
  , _optScale           :: Float
  , _optDegree          :: Int
  , _optBench           :: Bool
  , _optHelp            :: Bool
  }
  deriving Show

$(mkLabels [''Options])

defaults :: Options
defaults = Options
  { _optBackend         = maxBound
  , _optSize            = 200
  , _optZoom            = 3
  , _optScale           = 30
  , _optDegree          = 5
#ifdef ACCELERATE_ENABLE_GUI
  , _optBench           = False
#else
  , _optBench           = True
#endif
  , _optHelp            = False
  }


options :: [OptDescr (Options -> Options)]
options =
  [ Option []   ["size"]        (ReqArg (set optSize . read) "INT")     "visualisation size (200)"
  , Option []   ["zoom"]        (ReqArg (set optZoom . read) "INT")     "pixel replication factor (3)"
  , Option []   ["scale"]       (ReqArg (set optScale . read) "FLOAT")  "feature size of visualisation (30)"
  , Option []   ["degree"]      (ReqArg (set optDegree . read) "INT")   "number of waves to sum for each point (5)"
  , Option []   ["benchmark"]   (NoArg  (set optBench True))            "benchmark instead of displaying animation (False)"
  , Option "h?" ["help"]        (NoArg  (set optHelp True))             "show help message"
  ]


header :: [String]
header =
  [ "accelerate-crystal (c) [2011..2013] The Accelerate Team"
  , ""
  , "Usage: accelerate-crystal [OPTIONS]"
  ]

footer :: [String]
footer = []

