{-# LANGUAGE CPP             #-}
{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE TemplateHaskell #-}

module Config (

  Options, optBackend, optSize, optLimit, optBench,
  processArgs, run, run1

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
  , _optLimit           :: Int
  , _optBench           :: Bool
  , _optHelp            :: Bool
  }
  deriving Show

$(mkLabels [''Options])

defaultOptions :: Options
defaultOptions = Options
  { _optBackend         = maxBound
  , _optSize            = 512
  , _optLimit           = 255
  , _optBench           = False
  , _optHelp            = False
  }


run :: Arrays a => Options -> Acc a -> a
run opts = case _optBackend opts of
  Interpreter   -> Interp.run
#ifdef ACCELERATE_CUDA_BACKEND
  CUDA          -> CUDA.run
#endif

run1 :: (Arrays a, Arrays b) => Options -> (Acc a -> Acc b) -> a -> b
run1 opts f = case _optBackend opts of
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
  , Option []   ["size"]        (ReqArg (set optSize . read) "INT")     "visualisation size (512)"
  , Option []   ["limit"]       (ReqArg (set optLimit . read) "INT")    "iteration limit for escape (255)"
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
      [ "accelerate-mandelbrot (c) [2011..2012] The Accelerate Team"
      , ""
      , "Usage: accelerate-mandelbrot [OPTIONS]"
      ]

