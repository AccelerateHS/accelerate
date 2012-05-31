{-# LANGUAGE CPP, TemplateHaskell, PatternGuards #-}

module Config (

  Options, optBackend, optSize, optZoom, optScale, optDegree, optBench,
  processArgs, run

) where

import Data.Label
import System.Exit
import System.Console.GetOpt
import Data.Array.Accelerate                            ( Arrays, Acc )
import qualified Data.Array.Accelerate.Interpreter      as Interp
#ifdef ACCELERATE_CUDA_BACKEND
import qualified Data.Array.Accelerate.CUDA             as CUDA
#endif

data Backend = Interpreter
#ifdef ACCELERATE_CUDA_BACKEND
             | CUDA
#endif
  deriving (Bounded, Show)

data Options = Options
  {
    _optBackend         :: Backend
  , _optSize            :: Int
  , _optZoom            :: Int
  , _optScale           :: Float
  , _optDegree          :: Int
  , _optBench           :: Bool
  , _optHelp            :: Bool
  }
  deriving Show

$(mkLabels [''Options])

defaultOptions :: Options
defaultOptions = Options
  { _optBackend         = maxBound
  , _optSize            = 200
  , _optZoom            = 3
  , _optScale           = 30
  , _optDegree          = 5
#ifdef ACCELERATE_ENABLE_GUI
  , _optBench           = False
#else
  , _optBench           = True
#endif
  , _optHelp            = False
  }


run :: (Arrays a, Arrays b) => Options -> (Acc a -> Acc b) -> a -> b
run opts f = case _optBackend opts of
  Interpreter   -> head . Interp.stream f . return
#ifdef ACCELERATE_CUDA_BACKEND
  CUDA          -> CUDA.run1 f
#endif


options :: [OptDescr (Options -> Options)]
options =
  [ Option []   ["interpreter"] (NoArg  (set optBackend Interpreter))   "reference implementation (sequential)"
#ifdef ACCELERATE_CUDA_BACKEND
  , Option []   ["cuda"]        (NoArg  (set optBackend CUDA))          "implementation for NVIDIA GPUs (parallel)"
#endif
  , Option []   ["size"]        (ReqArg (set optSize . read) "INT")     "visualisation size (200)"
  , Option []   ["zoom"]        (ReqArg (set optZoom . read) "INT")     "pixel replication factor (3)"
  , Option []   ["scale"]       (ReqArg (set optScale . read) "FLOAT")  "feature size of visualisation (30)"
  , Option []   ["degree"]      (ReqArg (set optDegree . read) "INT")   "number of waves to sum for each point (5)"
  , Option []   ["benchmark"]   (NoArg  (set optBench True))            "benchmark instead of displaying animation (False)"
  , Option "h?" ["help"]        (NoArg  (set optHelp True))             "show help message"
  ]


processArgs :: [String] -> IO (Options, [String])
processArgs argv =
  case getOpt' Permute options argv of
    (o,_,n,[])  -> case foldl (flip id) defaultOptions o of
                     opts | False <- get optHelp opts   -> return (opts, n)
                     opts | True  <- get optBench opts  -> return (opts, "--help":n)
                     _                                  -> putStrLn (helpMsg []) >> exitSuccess
    (_,_,_,err) -> error (helpMsg err)
  where
    helpMsg err = concat err ++ usageInfo header options
    header      = unlines
      [ "accelerate-crystal (c) [2011..2012] The Accelerate Team"
      , ""
      , "Usage: accelerate-crystal [OPTIONS]"
      ]

