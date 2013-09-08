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
  , _configCutoff               :: Int
  , _configClip                 :: Int

  }

$(mkLabels [''Config])

defaults :: Config
defaults = Config
  {
    _configBackend              = maxBound
  , _configHelp                 = False
  , _configBenchmark            = False

  , _configCutoff               = 100
  , _configClip                 = 128
  }


options :: [OptDescr (Config -> Config)]
options =
  [ Option  [] ["cutoff"]
            (ReqArg (set configCutoff . read) "INT")
            (describe configCutoff "high-pass filtering cut-off value")

  , Option  [] ["clip"]
            (ReqArg (set configClip . read) "INT")
            (describe configClip "fft magnitude clipping value")

  , Option  [] ["benchmark"]
            (NoArg (set configBenchmark True))
            (describe configBenchmark "run criterion benchmark")

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
  [ "accelerate-fft (c) [2007..2013] The Accelerate Team"
  , ""
  , "Usage: accelerate-fft [OPTIONS] fileIn.bmp fileOut.bmp"
  , ""
  , "Image dimensions must be a power of two, eg 128x512 or 64x256"
  , ""
  , "For FFT, the output magnitude has a high dynamic range. We need to clip it"
  , "otherwise most of the pixels in the output BMP will be black. Start with a"
  , "value equal to about the width of the image (eg 512)"
  , ""
  ]

footer :: [String]
footer = []

