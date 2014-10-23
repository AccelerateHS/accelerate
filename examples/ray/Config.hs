{-# LANGUAGE TemplateHaskell #-}

module Config where

import Data.Label
import System.Console.GetOpt

data Config = Config
  {
    _configWidth        :: Int
  , _configHeight       :: Int
  , _configZoom         :: Int
  , _configFieldOfView  :: Int
  , _configBounces      :: Int
  , _configFramerate    :: Int
  }
  deriving Show

$(mkLabels [''Config])

defaults :: Config
defaults = Config
  { _configWidth        = 800
  , _configHeight       = 600
  , _configFieldOfView  = 100
  , _configZoom         = 1
  , _configBounces      = 4
  , _configFramerate    = 30
  }


options :: [OptDescr (Config -> Config)]
options =
  [ Option []   ["width"]       (ReqArg (set configWidth . read) "INT")         (describe configWidth "visualisation width")
  , Option []   ["height"]      (ReqArg (set configHeight . read) "INT")        (describe configHeight "visualisation height")
  , Option []   ["zoom"]        (ReqArg (set configZoom . read) "INT")          (describe configZoom "pixel replication factor")
  , Option []   ["fov"]         (ReqArg (set configFieldOfView . read) "INT")   (describe configFieldOfView "field of view")
  , Option []   ["bounces"]     (ReqArg (set configBounces . read) "INT")       (describe configBounces "ray bounce limit")
  , Option []   ["fps"]         (ReqArg (set configFramerate . read) "INT")     (describe configFramerate "frames per second")
  ]
  where
    describe f msg      = msg ++ " (" ++ show (get f defaults) ++ ")"

header :: [String]
header =
  [ "accelerate-ray (c) [2013..2014] The Accelerate Team"
  , ""
  , "Usage: accelerate-ray [OPTIONS]"
  , ""
  ]

footer :: [String]
footer =
  [ ""
  , "Runtime usage:"
  , "     WASD         move the eye position"
  , "     arrows       move the light source"
  , ""
  ]

