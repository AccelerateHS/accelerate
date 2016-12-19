{-# LANGUAGE TemplateHaskell #-}

module Config where

import Data.Label
import System.Console.GetOpt


data Config = Config
  {
    _configWidth        :: Int
  , _configHeight       :: Int
  , _configFilePath     :: Maybe FilePath
  }
  deriving Show

$(mkLabels [''Config])

defaults :: Config
defaults = Config
  { _configWidth        = 800
  , _configHeight       = 600
  , _configFilePath     = Nothing
  }

options :: [OptDescr (Config -> Config)]
options =
  [ Option []   ["width"]       (ReqArg (set configWidth . read) "INT")         "visualisation width (800)"
  , Option []   ["height"]      (ReqArg (set configHeight . read) "INT")        "visualisation height (600)"
  , Option []   ["bmp"]         (ReqArg (set configFilePath . Just) "FILE")     "save image to file"
  ]


header :: [String]
header =
  [ "accelerate-mandelbrot (c) [2011..2016] The Accelerate Team"
  , ""
  , "Usage: accelerate-mandelbrot [OPTIONS]"
  , ""
  ]

footer :: [String]
footer =
  [ ""
  , "Runtime usage:"
  , "     ESC           quit"
  , "     mouse drag    translate display"
  , "     w/s           zoom in/out"
  , "     a/d           iteration count"
  , "     z/c           escape radius"
  , "     0 .. 9        select presets"
  , "     r             reset display"
  , "     .             print current configuration"
  , "     p             switch between single/double precision"
  , ""
  ]

