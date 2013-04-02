{-# LANGUAGE CPP             #-}
{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE TemplateHaskell #-}

module Config
  where

import Prelude                                          as P hiding ((.), id, fst, snd)
import qualified Prelude                                as P
import Data.Char
import Data.List
import Data.Label
import System.Exit
import Control.Category
import System.Console.GetOpt
import qualified Criterion.Main                         as Criterion
import qualified Criterion.Config                       as Criterion

import Data.Array.Accelerate                            ( Array, Arrays, Acc, DIM2 )
import Data.Array.Accelerate.Math.Complex               as A
import qualified Data.Array.Accelerate.Interpreter      as Interp
#ifdef ACCELERATE_CUDA_BACKEND
import qualified Data.Array.Accelerate.CUDA             as CUDA
#endif

-- | Types
type R          = Float
type C          = Complex R
type Matrix a   = Array DIM2 a

data SigmoidFunction
  = Hard
  | Smooth
  | Atan
  | Atancos
  | Overshoot
  | Linear
  | Hermite
  | Sin
  deriving (Eq, Show, Read, Enum, Bounded)


-- | Program configuration
--
data Backend = Interpreter
#ifdef ACCELERATE_CUDA_BACKEND
             | CUDA
#endif
  deriving (Bounded, Show)


data Config = Config
  {
    -- How to execute the simulation
    _configBackend              :: Backend

    -- How to present the output
  , _configWindowSize           :: Int
  , _configWindowZoom           :: Int
  , _configFramerate            :: Int
  , _configTimestep             :: Float

    -- Initial conditions
  , _configRim                  :: R            -- b
  , _configDiscRadius           :: (R,R)        -- (ri, ra)
  , _configBirthInterval        :: (R,R)        -- (b1, b2)
  , _configDeathInterval        :: (R,R)        -- (d1, d2)
  , _configStep                 :: (R,R)        -- (alpha_n, alpha_m)

   -- Timestep and sigmoid functions
  , _configTimestepMode         :: Int
  , _configSigmode              :: Int
  , _configSigtype              :: SigmoidFunction
  , _configMixtype              :: SigmoidFunction

    -- Terminating conditions
  , _configMaxSteps             :: Maybe Int
  , _configBenchmark            :: Bool
  , _configHelp                 :: Bool
  }

$(mkLabels [''Config])

defaultConfig :: Config
defaultConfig = Config
  {
    _configBackend              = maxBound

  , _configWindowSize           = 256
  , _configWindowZoom           = 3
  , _configFramerate            = 10
  , _configTimestep             = 0.1

  -- generic smooth glider
  , _configRim                  = 1
  , _configDiscRadius           = (4, 12)
  , _configBirthInterval        = (0.278, 0.365)
  , _configDeathInterval        = (0.267, 0.445)
  , _configStep                 = (0.028, 0.147)

  , _configTimestepMode         = 0
  , _configSigmode              = 2
  , _configSigtype              = Smooth
  , _configMixtype              = Smooth

  , _configMaxSteps             = Nothing
  , _configBenchmark            = False
  , _configHelp                 = False
  }


-- | Execute Accelerate expressions
--
run :: Arrays a => Config -> Acc a -> a
run config =
  case _configBackend config of
    Interpreter -> Interp.run
#ifdef ACCELERATE_CUDA_BACKEND
    CUDA        -> CUDA.run
#endif


run1 :: (Arrays a, Arrays b) => Config -> (Acc a -> Acc b) -> a -> b
run1 config f =
  case _configBackend config of
    Interpreter -> head . Interp.stream f . return
#ifdef ACCELERATE_CUDA_BACKEND
    CUDA        -> CUDA.run1 f
#endif

-- | The set of available command-line options
--
backends :: [OptDescr (Config -> Config)]
backends =
  [ Option  [] ["interpreter"]
            (NoArg (set configBackend Interpreter))
            "reference implementation (sequential)"

#ifdef ACCELERATE_CUDA_BACKEND
  , Option  [] ["cuda"]
            (NoArg (set configBackend CUDA))
            "implementation for NVIDIA GPUs (parallel)"
#endif
  ]


defaultOptions :: [OptDescr (Config -> Config)]
defaultOptions = backends ++
  [ Option  [] ["size"]
            (ReqArg (set configWindowSize . read) "INT")
            (describe configWindowSize "visualisation size")

  , Option  [] ["zoom"]
            (ReqArg (set configWindowZoom . read) "INT")
            (describe configWindowZoom "visualisation pixel replication factor")

  , Option  [] ["framerate"]
            (ReqArg (set configFramerate . read) "INT")
            (describe configFramerate "visualisation frame rate")

  , Option  [] ["timestep"]
            (ReqArg (set configTimestep . read) "Float")
            (describe configTimestep "simulation timestep")

  , Option  [] ["b", "rim"]
            (ReqArg (set configRim . read) "FLOAT")
            (describe configRim "anti-aliasing zone around the rim")

  , Option  [] ["ri", "inner-radius"]
            (ReqArg (set (fst . configDiscRadius) . read) "FLOAT")
            (describe (fst . configDiscRadius) "inner radius")

  , Option  [] ["ra", "outer-radius"]
            (ReqArg (set (snd . configDiscRadius) . read) "FLOAT")
            (describe (snd . configDiscRadius) "outer radius")

  , Option  [] ["b1", "birth-low"]
            (ReqArg (set (fst . configBirthInterval) . read) "FLOAT")
            (describe (fst . configBirthInterval) "lower birth interval")

  , Option  [] ["b2", "birth-high"]
            (ReqArg (set (snd . configBirthInterval) . read) "FLOAT")
            (describe (snd . configBirthInterval) "upper birth interval")

  , Option  [] ["d1", "death-low"]
            (ReqArg (set (fst . configDeathInterval) . read) "FLOAT")
            (describe (fst . configDeathInterval) "lower death interval")

  , Option  [] ["d2", "death-high"]
            (ReqArg (set (snd . configDeathInterval) . read) "FLOAT")
            (describe (snd . configDeathInterval) "upper death interval")

  , Option  [] ["sn", "alpha_n"]
            (ReqArg (set (fst . configStep) . read) "FLOAT")
            (describe (fst . configStep) "lower step interval")

  , Option  [] ["sm", "alpha_m"]
            (ReqArg (set (snd . configStep) . read) "FLOAT")
            (describe (snd . configStep) "upper step interval")

  , Option  [] ["tsm", "timestep-mode"]
            (ReqArg (set configTimestepMode . read) "INT")
            (describe configTimestepMode "timestep mode 0..2")

  , Option  [] ["sigmode"]
            (ReqArg (set configSigmode . read) "INT")
            (describe configSigmode "sigmode 1..4")

  , Option  [] ["sigtype"]
            (ReqArg (set configSigtype . read) "SIGMOIDFUNCTION")
            (describe configSigtype "sigtype")

  , Option  [] ["mixtype"]
            (ReqArg (set configMixtype . read) "SIGMOIDFUNCTION")
            (describe configMixtype "mixtype")

  , Option  [] ["max-steps"]
            (ReqArg (set configMaxSteps . read) "INT")
            (describe configMaxSteps "exit simulation after this many steps")

  , Option  [] ["benchmark"]
            (NoArg (set configBenchmark True))
            (describe configBenchmark "benchmark instead of displaying animation")

  , Option  "h?" ["help"]
            (NoArg (set configHelp True))
            "show this help message"
  ]
  where
    describe f msg
      = msg ++ " (" ++ show (get f defaultConfig) ++ ")"

    fst = lens P.fst (\a (_,b) -> (a,b))
    snd = lens P.snd (\b (a,_) -> (a,b))

basicHeader :: String
basicHeader = unlines
  [ "accelerate-smoothlife (c) [2012..2013] The Accelerate Team"
  , ""
  , "Usage: accelerate-smoothlife [OPTIONS]"
  ]

fancyHeader :: Config -> String
fancyHeader opts = unlines (header : table)
  where
    active this         = if this == map toLower (show $ get configBackend opts) then "*" else ""
    (ss,bs,ds)          = unzip3 $ map (\(b,d) -> (active b, b, d)) $ concatMap extract backends
    table               = zipWith3 paste (sameLen ss) (sameLen bs) ds
    paste x y z         = "  " ++ x ++ "  " ++ y ++ "  " ++ z
    sameLen xs          = flushLeft ((maximum . map length) xs) xs
    flushLeft n xs      = [ take n (x ++ repeat ' ') | x <- xs ]
    --
    extract (Option _ los _ descr) =
      let losFmt  = intercalate ", " los
      in  case lines descr of
            []          -> [(losFmt, "")]
            (x:xs)      -> (losFmt, x) : [ ("",x') | x' <- xs ]
    --
    header = intercalate "\n" [ basicHeader, "Available backends:" ]


parseArgs :: [String] -> IO (Config, Criterion.Config, [String])
parseArgs argv
  = let
        helpMsg err = concat err
          ++ usageInfo basicHeader                    defaultOptions
          ++ sigmoidDescription
          ++ usageInfo "\nGeneric criterion options:" Criterion.defaultOptions

        sigmoidDescription = unlines
          [ "\nFor options requiring a SIGMOIDFUNCTION the available functions are:"
          , show (enumFrom minBound :: [SigmoidFunction])
          ]

  in case getOpt' Permute defaultOptions argv of
      (o,_,n,[])  -> do

        -- pass unrecognised options to criterion
        (cconf, rest)     <- Criterion.parseArgs Criterion.defaultConfig Criterion.defaultOptions n
        case foldr id defaultConfig o of
          conf | False <- get configHelp conf   -> putStrLn (fancyHeader conf) >> return (conf, cconf, rest)
          _                                     -> putStrLn (helpMsg [])       >> exitSuccess

      (_,_,_,err) -> error (helpMsg err)

