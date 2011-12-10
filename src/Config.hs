{-# LANGUAGE CPP, TemplateHaskell, PatternGuards #-}
--
-- Configuration parameters
--

module Config (

  Options,
  timestep, viscosity, diffusion, simulationWidth, simulationHeight,
  displayScale, displayFramerate, optBench,

  processArgs, run

) where

import           Data.Label
import           System.Exit
import           System.Console.GetOpt
import           Data.Array.Accelerate                  ( Acc, Arrays )
import qualified Data.Array.Accelerate.Interpreter      as I
#ifdef ACCELERATE_CUDA_BACKEND
import qualified Data.Array.Accelerate.CUDA             as CUDA
#endif

data Backend
  = Interpreter
#ifdef ACCELERATE_CUDA_BACKEND
  | CUDA
#endif
  deriving (Show, Bounded)


data Options = Options
  {
    -- simulation
    _timestep           :: Float
  , _viscosity          :: Float
  , _diffusion          :: Float
  , _simulationWidth    :: Int
  , _simulationHeight   :: Int

  -- visualisation
  , _displayScale       :: Int
  , _displayFramerate   :: Int

  -- misc
  , _optBackend         :: Backend
  , _optBench           :: Bool
  , _optHelp            :: Bool
  }
  deriving Show

$(mkLabels [''Options])

defaultOptions :: Options
defaultOptions = Options
  { _timestep           = 0.5
  , _viscosity          = 0
  , _diffusion          = 0
  , _simulationWidth    = 100
  , _simulationHeight   = 100
  , _displayScale       = 4
  , _displayFramerate   = 10
  , _optBackend         = maxBound
  , _optBench           = False
  , _optHelp            = False
  }

-- Execute an Accelerate expression using the selected backend
--
run :: Arrays a => Options -> Acc a -> a
run opts = case _optBackend opts of
  Interpreter   -> I.run
#ifdef ACCELERATE_CUDA_BACKEND
  CUDA          -> CUDA.run
#endif


-- Process command line arguments
--
options :: [OptDescr (Options -> Options)]
options =
  [ Option []   ["interpreter"] (NoArg  (set optBackend Interpreter))           "reference implementation (sequential)"
#ifdef ACCELERATE_CUDA_BACKEND
  , Option []   ["cuda"]        (NoArg  (set optBackend CUDA))                  "implementation for NVIDIA GPUs (parallel)"
#endif
  , Option []   ["timestep"]    (ReqArg (set timestep . read) "FLOAT")          "size of a simulation time step"
  , Option []   ["viscosity"]   (ReqArg (set viscosity . read) "FLOAT")         "viscosity for velocity dampening"
  , Option []   ["diffusion"]   (ReqArg (set diffusion . read) "FLOAT")         "diffusion rate for mass dispersion"
  , Option []   ["width"]       (ReqArg (set simulationWidth . read) "INT")     "grid width for simulation"
  , Option []   ["height"]      (ReqArg (set simulationHeight . read) "INT")    "grid height for simulation"
  , Option []   ["scale"]       (ReqArg (set displayScale . read) "INT")        "feature size of visualisation"
  , Option []   ["framerate"]   (ReqArg (set displayFramerate . read) "INT")    "frame rate for visualisation"
  --
  , Option []   ["benchmark"]   (NoArg  (set optBench True))                    "benchmark instead of displaying animation (False)"
  , Option "h?" ["help"]        (NoArg  (set optHelp True))                     "show help message"
  ]


processArgs :: [String] -> IO (Options, [String])
processArgs argv =
  case getOpt' Permute options argv of
    (o,_,n,[])  -> case foldl (flip id) defaultOptions o of
                     opts | False <- get optHelp  opts  -> return (opts, n)
--                     opts | True  <- get optBench opts  -> return (opts, "--help":n)
                     _                                  -> putStrLn (helpMsg []) >> exitSuccess
    (_,_,_,err) -> error (helpMsg err)
  where
    helpMsg err = concat err ++ usageInfo header options
    header      = unlines
      [ "accelerate-fluid (c) 2011 Trevor L. McDonell"
      , ""
      , "Usage: accelerate-fluid [OPTIONS]"
      ]

