{-# LANGUAGE TemplateHaskell #-}

module Config where

import Data.Label
import System.Console.GetOpt


data Config
  = Config
  {
  }

$(mkLabels [''Config])

defaults :: Config
defaults = Config

options :: [OptDescr (Config -> Config)]
options = []


header :: [String]
header =
  [ "accelerate-smvm (c) [2013] The Accelerate Team"
  , ""
  , "Usage: accelerate-smvm [OPTIONS] matrix.mtx"
  , ""
  ]

footer :: [String]
footer = [ "" ]

