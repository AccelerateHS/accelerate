{-# LANGUAGE CPP, TemplateHaskell, PatternGuards #-}
--
-- Configuration parameters
--

module Config (

  Options,
  viscosity, diffusion, densityBMP, velocityBMP, simulationWidth,
  simulationHeight, displayScale, displayFramerate, optBench,

  processArgs, run, run1

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
    _viscosity          :: Float
  , _diffusion          :: Float

  -- visualisation
  , _densityBMP         :: Maybe FilePath
  , _velocityBMP        :: Maybe FilePath
  , _simulationWidth    :: Int
  , _simulationHeight   :: Int
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
  { _viscosity          = 0
  , _diffusion          = 0

  , _densityBMP         = Nothing
  , _velocityBMP        = Nothing
  , _simulationWidth    = 250
  , _simulationHeight   = 250
  , _displayScale       = 2
  , _displayFramerate   = 20

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

run1 :: (Arrays a, Arrays b) => Options -> (Acc a -> Acc b) -> a -> b
run1 opts f = case _optBackend opts of
  Interpreter   -> head . I.stream f . return
#ifdef ACCELERATE_CUDA_BACKEND
  CUDA          -> CUDA.run1 f
#endif


-- Process command line arguments
--
options :: [OptDescr (Options -> Options)]
options =
  [ Option []   ["interpreter"] (NoArg  (set optBackend Interpreter))
      "reference implementation (sequential)"

#ifdef ACCELERATE_CUDA_BACKEND
  , Option []   ["cuda"]        (NoArg  (set optBackend CUDA))
      "implementation for NVIDIA GPUs (parallel)"

#endif
  , Option []   ["viscosity"]   (ReqArg (set viscosity . read) "FLOAT")
    $ "viscosity for velocity dampening " ++ def viscosity

  , Option []   ["diffusion"]   (ReqArg (set diffusion . read) "FLOAT")
    $ "diffusion rate for mass dispersion " ++ def diffusion

  , Option []   ["width"]       (ReqArg (set simulationWidth . read) "INT")
    $ "grid width for simulation " ++ def simulationWidth

  , Option []   ["height"]      (ReqArg (set simulationHeight . read) "INT")
    $ "grid height for simulation " ++ def simulationHeight

  , Option []   ["scale"]       (ReqArg (set displayScale . read) "INT")
    $ "feature size of visualisation " ++ def displayScale

  , Option []   ["framerate"]   (ReqArg (set displayFramerate . read) "INT")
    $ "frame rate for visualisation " ++ def displayFramerate

  , Option []   ["bmp-density"] (ReqArg (set densityBMP . Just) "FILE.bmp")
      "file for initial fluid density"

  , Option []   ["bmp-velocity"] (ReqArg (set velocityBMP . Just) "FILE.bmp")
      "file for initial fluid velocity"

  --
  , Option []   ["benchmark"]   (NoArg  (set optBench True))
      "benchmark instead of displaying animation (False)"

  , Option "h?" ["help"]        (NoArg  (set optHelp True))
      "show help message"
  ]
  where
    parens s    = "(" ++ s ++ ")"
    def f       = parens (show (get f defaultOptions))


processArgs :: [String] -> IO (Options, [String])
processArgs argv =
  case getOpt' Permute options argv of
    (o,_,n,[])  -> case foldl (flip id) defaultOptions o of
                     opts | False <- get optHelp  opts  -> return (opts, n)
                     opts | True  <- get optBench opts  -> return (opts, "--help":n)
                     _                                  -> putStrLn (helpMsg []) >> exitSuccess
    (_,_,_,err) -> error (helpMsg err)
  where
    helpMsg err = concat err ++ usageInfo header options ++ footer
    header      = unlines
      [ "accelerate-fluid (c) 2011 Trevor L. McDonell"
      , ""
      , "Usage: accelerate-fluid [OPTIONS]"
      ]

    footer      = unlines
      [ ""
      , "Runtime usage:"
      , ""
      , "          click                    add density sources to the image"
      , "          shift-click              add velocity sources"
      , "          r                        reset the image"
      , "          d                        toggle display of density field"
      , "          v                        toggle display of velocity field lines"
      ]

