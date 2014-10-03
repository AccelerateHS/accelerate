{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

module Config where

import Data.Label
import System.Console.GetOpt


data Config = Config
  {
    _configWidth        :: Int
  , _configHeight       :: Int
  , _configLimit        :: Int
  , _configFramerate    :: Int
  }
  deriving Show

$(mkLabels [''Config])

defaults :: Config
defaults = Config
  { _configWidth        = 800
  , _configHeight       = 600
  , _configLimit        = 255
  , _configFramerate    = 25
  }

options :: [OptDescr (Config -> Config)]
options =
  [ Option []   ["width"]       (ReqArg (set configWidth . read) "INT")         "visualisation width (800)"
  , Option []   ["height"]      (ReqArg (set configHeight . read) "INT")        "visualisation height (600)"
  , Option []   ["limit"]       (ReqArg (set configLimit . read) "INT")         "iteration limit for escape (255)"
  , Option []   ["framerate"]   (ReqArg (set configFramerate . read) "INT")     "visualisation framerate (10)"
  , Option []   ["static"]      (NoArg  (set configFramerate 0))                "do not animate the image"
  ]


header :: [String]
header =
  [ "accelerate-mandelbrot (c) [2011..2014] The Accelerate Team"
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
  , ""
  ]

