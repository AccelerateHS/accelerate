{-# LANGUAGE TemplateHaskell #-}

module Config where

import ParseArgs
import Data.Label

data Config
  = Config
  {
    -- Standard options
    _configBackend              :: Backend
  , _configHelp                 :: Bool
  , _configBenchmark            :: Bool

    -- How to execute the program
  , _configThreshLow            :: Float
  , _configThreshHigh           :: Float

  }

$(mkLabels [''Config])

defaults :: Config
defaults = Config
  {
    _configBackend              = maxBound
  , _configHelp                 = False
  , _configBenchmark            = False

  , _configThreshLow            = 50
  , _configThreshHigh           = 100
  }


options :: [OptDescr (Config -> Config)]
options =
  [ Option  ['w'] ["threshold-low"]
            (ReqArg (set configThreshLow . read) "FLOAT")
            (describe configThreshLow "threshold value for weak edges")

  , Option  ['s'] ["threshold-high"]
            (ReqArg (set configThreshHigh . read) "FLOAT")
            (describe configThreshHigh "threshold value for strong edges")

  , Option  [] ["benchmark"]
            (NoArg (set configBenchmark True))
            (describe configBenchmark "benchmark instead of displaying animation")

  , Option  ['h', '?'] ["help"]
            (NoArg (set configHelp True))
            "show this help message"
  ]
  where
    describe f msg
      = msg ++ " (" ++ show (get f defaults) ++ ")"


-- | Process the command line options
--

header :: [String]
header =
  [ "accelerate-canny (c) [2007..2013] The Accelerate Team"
  , ""
  , "Usage: accelerate-canny [OPTIONS] fileIn.bmp fileOut.bmp"
  , ""
  ]

footer :: [String]
footer = []

