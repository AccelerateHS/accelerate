{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

module Config where

import ParseArgs
import Data.Label

data Options = Options
  {
    _optBackend         :: Backend
  , _optSteps           :: Int
  , _optChunkSize       :: Int
  , _optCount           :: Bool
  , _optHelp            :: Bool
  }
  deriving Show

$(mkLabels [''Options])

defaults :: Options
defaults = Options
  { _optBackend         = maxBound
  , _optSteps           = 10
  , _optChunkSize       = 12000000
  , _optCount           = False
  , _optHelp            = False
  }


options :: [OptDescr (Options -> Options)]
options =
  [ Option []   ["steps"]       (ReqArg (set optSteps . read) "INT")     "number of steps to perform"
  , Option []   ["chunk-size"]  (ReqArg (set optChunkSize . read) "INT") "size of chunks to be processed"
  , Option []   ["count"]       (NoArg  (set optCount True))             "count number of pages in the links file"
  , Option "h?" ["help"]        (NoArg  (set optHelp True))              "show help message"
  ]


header :: [String]
header =
  [ "accelerate-pagerank (c) [2011..2013] The Accelerate Team"
  , ""
  , "Usage: accelerate-pagerank [OPTIONS] <LINKS_FILE> <TITLES_FILE> [+RTS -M<HEAP_SIZE>]"
  , ""
  , "  NOTE: The -M flag sets the Haskell heap size. If high performance is"
  , "        desired, then this value, as well as the chunk size, will need to"
  , "        be adjusted. The idea being to maximise the chunk size without"
  , "        running out of device memory. With experimentation, you can arrive"
  , "        at a heap size that will force garbage collection before device"
  , "        memory runs out."
  ]

footer :: [String]
footer = []

