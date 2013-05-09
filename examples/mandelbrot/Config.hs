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
  , _optLimit           :: Int
  , _optFramerate       :: Int
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
  , _optLimit           = 255
  , _optFramerate       = 25
#ifdef ACCELERATE_ENABLE_GUI
  , _optBench           = False
#else
  , _optBench           = True
#endif
  , _optHelp            = False
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option []   ["width"]       (ReqArg (set optWidth . read) "INT")    "visualisation width (800)"
  , Option []   ["height"]      (ReqArg (set optHeight . read) "INT")   "visualisation height (600)"
  , Option []   ["limit"]       (ReqArg (set optLimit . read) "INT")    "iteration limit for escape (255)"
  , Option []   ["framerate"]   (ReqArg (set optFramerate . read) "INT")"visualisation framerate (10)"
  , Option []   ["static"]      (NoArg  (set optFramerate 0))           "do not animate the image"
  , Option []   ["benchmark"]   (NoArg  (set optBench True))            "benchmark instead of displaying animation (False)"
  , Option "h?" ["help"]        (NoArg  (set optHelp True))             "show help message"
  ]


header :: [String]
header =
  [ "accelerate-mandelbrot (c) [2011..2013] The Accelerate Team"
  , ""
  , "Usage: accelerate-mandelbrot [OPTIONS]"
  , ""
  ]

footer :: [String]
footer =
  [ ""
  , "Runtime usage:"
  , "     arrows       translate display"
  , "     z ;          zoom in"
  , "     x q          zoom out"
  , "     f            single precision calculations"
  , "     d            double precision calculations (if supported)"
  ]

