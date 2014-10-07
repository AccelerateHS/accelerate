{-# LANGUAGE TemplateHaskell #-}

module Config where

import Data.Label
import System.Console.GetOpt


data Config
  = Config
  {
    _configThreshLow            :: Float
  , _configThreshHigh           :: Float
  }

$(mkLabels [''Config])

defaults :: Config
defaults = Config
  { _configThreshLow            = 50
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
footer = [ "" ]

