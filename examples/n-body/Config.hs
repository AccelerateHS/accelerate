{-# LANGUAGE CPP             #-}
{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE TemplateHaskell #-}

module Config where

import ParseArgs
import Common.Type

import Data.Char
import Data.List
import Data.Label


data Solver = Naive | BarnsHut
  deriving (Enum, Bounded, Show)


data Config
  = Config
  {
    -- How to execute the simulation
    _configBackend              :: Backend
  , _configSolver               :: Solver

    -- How to present the output
  , _configWindowSize           :: Int
  , _configShouldDrawTree       :: Bool
  , _configRate                 :: Int

    -- System setup
  , _configBodyCount            :: Int
  , _configBodyMass             :: R
  , _configTimeStep             :: R
  , _configEpsilon              :: R

    -- Initial conditions
  , _configStartDiscSize        :: R
  , _configStartSpeed           :: R

    -- Terminating conditions
  , _configMaxSteps             :: Maybe Int
  , _configBenchmark            :: Bool
  , _configHelp                 :: Bool

    -- Dump final particle locations to file
  , _configDumpFinal            :: Maybe FilePath
  }
  deriving Show

$(mkLabels [''Config])


defaults :: Config
defaults = Config
  {
    _configBackend              = maxBound
  , _configSolver               = Naive         -- no barns-hut yet!

  , _configWindowSize           = 1000
  , _configShouldDrawTree       = False         -- no barns-hut yet!
  , _configRate                 = 30

  , _configBodyCount            = 1000
  , _configBodyMass             = 40
  , _configTimeStep             = 1
  , _configEpsilon              = 50

  , _configStartDiscSize        = 500
  , _configStartSpeed           = 1

  , _configMaxSteps             = Nothing
  , _configBenchmark            = False
  , _configHelp                 = False

  , _configDumpFinal            = Nothing
  }


-- | The set of available command-line options
--
options :: [OptDescr (Config -> Config)]
options =
  [ Option  ['s'] ["solver"]
            (ReqArg (set configSolver . solver) "ALGORITHM")
            ("solver to use, one of: " ++ intercalate ", " (map show [minBound .. maxBound :: Solver]))

  , Option  [] ["size"]
            (ReqArg (set configWindowSize . read) "INT")
            (describe configWindowSize "visualisation size")

  , Option  [] ["framerate"]
            (ReqArg (set configRate . read) "INT")
            (describe configRate "visualisation frame rate")

  , Option  [] ["draw-tree"]
            (NoArg (set configShouldDrawTree True))
            "draw the Barns-Hut quad tree"

  , Option  ['n'] ["bodies"]
            (ReqArg (set configBodyCount . read) "INT")
            (describe configBodyCount "number of bodies in the simulation")

  , Option  [] ["mass"]
            (ReqArg (set configBodyMass . read) "FLOAT")
            (describe configBodyMass "mass of each body")

  , Option  [] ["timestep"]
            (ReqArg (set configTimeStep . read) "FLOAT")
            (describe configTimeStep "time step between simulation states")

  , Option  [] ["epsilon"]
            (ReqArg (set configEpsilon . read) "FLOAT")
            (describe configEpsilon "smoothing parameter")

  , Option  [] ["disc"]
            (ReqArg (set configStartDiscSize . read) "FLOAT")
            (describe configStartDiscSize "initial size of particle disc")

  , Option  [] ["speed"]
            (ReqArg (set configStartSpeed . read) "FLOAT")
            (describe configStartSpeed "initial rotation speed of the disc")

  , Option  [] ["max-steps"]
            (ReqArg (set configMaxSteps . read) "INT")
            (describe configMaxSteps "exit simulation after this many steps")

  , Option  [] ["benchmark"]
            (NoArg (set configBenchmark True))
            (describe configBenchmark "benchmark instead of displaying animation")

  , Option  [] ["dump-final"]
            (ReqArg (set configDumpFinal . Just) "FILE")
            "dump final body positions to file"

  , Option  ['h', '?'] ["help"]
            (NoArg (set configHelp True))
            "show this help message"
  ]
  where
    solver algorithm
      | a `elem` ["n",  "naive"]                        = Naive
      | a `elem` ["bh", "barnshut", "barns-hut"]        = BarnsHut
      | otherwise                                       = error $ "Unknown solver method: " ++ algorithm
      where
        a = map toLower algorithm

    describe f msg
      = msg ++ " (" ++ show (get f defaults) ++ ")"


-- | Process the command line options
--

header :: [String]
header =
  [ "accelerate-nbody (c) [2012..2013] The Accelerate Team"
  , ""
  , "Usage: accelerate-nbody [OPTIONS]"
  ]

footer :: [String]
footer = []

