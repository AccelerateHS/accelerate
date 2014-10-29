{-# LANGUAGE TemplateHaskell #-}

module Config where

import Data.Label
import System.Console.GetOpt


data Config = Config
  {
    _configSteps        :: Int
  , _configChunkSize    :: Int
  , _configCount        :: Bool
  , _configNoSeq        :: Bool
  }
  deriving Show

$(mkLabels [''Config])

defaults :: Config
defaults = Config
  { _configSteps        = 10
  , _configChunkSize    = 12000000
  , _configCount        = False
  , _configNoSeq        = True
  }


options :: [OptDescr (Config -> Config)]
options =
  [ Option []   ["steps"]       (ReqArg (set configSteps . read) "INT")     "number of steps to perform"
  , Option []   ["chunk-size"]  (ReqArg (set configChunkSize . read) "INT") "size of chunks to be processed"
  , Option []   ["count"]       (NoArg  (set configCount True))             "count number of pages in the links file"
  , Option []   ["noseq"]       (NoArg  (set configNoSeq True))                "do not use Accelerate sequencing"
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
  , ""
  ]

footer :: [String]
footer = [ "" ]

