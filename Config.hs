{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

module Config where

import ParseArgs
import Data.Label

data Options = Options
  {
    _optBackend         :: Backend
  , _optWidth           :: Int
  , _optHeight          :: Int
  , _optZoom            :: Int
  , _optFieldOfView     :: Int
  , _optBounces         :: Int
  , _optBench           :: Bool
  , _optHelp            :: Bool
  }
  deriving Show

$(mkLabels [''Options])

defaults :: Options
defaults = Options
  { _optBackend         = maxBound
  , _optWidth           = 800
  , _optHeight          = 600
  , _optFieldOfView     = 100
  , _optZoom            = 4
  , _optBounces         = 4
#ifdef ACCELERATE_ENABLE_GUI
  , _optBench           = False
#else
  , _optBench           = True
#endif
  , _optHelp            = False
  }


options :: [OptDescr (Options -> Options)]
options =
  [ Option []   ["width"]       (ReqArg (set optWidth . read) "INT")            (describe optWidth "visualisation width")
  , Option []   ["height"]      (ReqArg (set optHeight . read) "INT")           (describe optHeight "visualisation height")
  , Option []   ["zoom"]        (ReqArg (set optZoom . read) "INT")             (describe optZoom "pixel replication factor")
  , Option []   ["fov"]         (ReqArg (set optFieldOfView . read) "INT")      (describe optFieldOfView "field of view")
  , Option []   ["bounces"]     (ReqArg (set optBounces . read) "INT")          (describe optBounces "ray bounce limit")
  , Option []   ["benchmark"]   (NoArg  (set optBench True))                    "benchmark instead of displaying animation"
  , Option "h?" ["help"]        (NoArg  (set optHelp True))                     "show help message"
  ]
  where
    describe f msg      = msg ++ " (" ++ show (get f defaults) ++ ")"

header :: [String]
header =
  [ "accelerate-ray (c) [2013] The Accelerate Team"
  , ""
  , "Usage: accelerate-ray [OPTIONS]"
  , ""
  ]

footer :: [String]
footer = []

