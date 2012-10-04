{-# LANGUAGE CPP             #-}
{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE TemplateHaskell #-}

module Config (

  Options, optBackend, optSize, optLimit, optFramerate, optBench,
  processArgs, run, run1

) where

import qualified Criterion.Main                         as Crit
import qualified Criterion.Config                       as Crit
import Data.Label
import System.Exit
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(Permute), getOpt', usageInfo)
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
  , _optFramerate       :: Int
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
  , _optFramerate       = 25
#ifdef ACCELERATE_ENABLE_GUI
  , _optBench           = False
#else
  , _optBench           = True
#endif
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
  , Option []   ["framerate"]   (ReqArg (set optFramerate . read) "INT")"visualisation framerate (10)"
  , Option []   ["benchmark"]   (NoArg  (set optBench True))            "benchmark instead of displaying animation (False)"
  , Option "h?" ["help"]        (NoArg  (set optHelp True))             "show help message"
  ]


-- | Two levels of argument parsing -- ours and criterions.
processArgs :: [String] -> IO (Options, Crit.Config, [String])
processArgs argv =
  case getOpt' Permute options argv of
    (o,_,n,[])  -> do -- Pass unrecognized options onward:
                      (critConf,rst) <- Crit.parseArgs Crit.defaultConfig Crit.defaultOptions n
                      case foldl (flip id) defaultOptions o of
                        opts | False <- get optHelp opts   -> return (opts, critConf, rst)
                        opts | True  <- get optBench opts  -> return (opts, critConf, "--help":rst)
                        _                                  -> putStrLn (helpMsg []) >> exitSuccess

    (_,_,_,err) -> error (helpMsg err)
  where
    helpMsg err = concat err ++ usageInfo header options ++
                  usageInfo "\nGeneric criterion options:" Crit.defaultOptions
    header      = unlines
      [ "accelerate-mandelbrot (c) [2011..2012] The Accelerate Team"
      , ""
      , "Usage: accelerate-mandelbrot [OPTIONS]"
      , ""
      , "Translate the display using the arrow keys, zoom with 'z' and 'x'"
      ]
