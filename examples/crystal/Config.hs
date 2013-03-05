{-# LANGUAGE CPP, TemplateHaskell, PatternGuards #-}

module Config (

  Options, optBackend, optSize, optZoom, optScale, optDegree, optBench,
  parseArgs, run

) where

import Data.Char
import Data.List
import Data.Label
import System.Exit
import System.Console.GetOpt
import qualified Criterion.Main                         as Criterion
import qualified Criterion.Config                       as Criterion

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

backends :: [OptDescr (Options -> Options)]
backends =
  [ Option []   ["interpreter"] (NoArg  (set optBackend Interpreter))   "reference implementation (sequential)"
#ifdef ACCELERATE_CUDA_BACKEND
  , Option []   ["cuda"]        (NoArg  (set optBackend CUDA))          "implementation for NVIDIA GPUs (parallel)"
#endif
  ]

options :: [OptDescr (Options -> Options)]
options = backends ++
  [ Option []   ["size"]        (ReqArg (set optSize . read) "INT")     "visualisation size (200)"
  , Option []   ["zoom"]        (ReqArg (set optZoom . read) "INT")     "pixel replication factor (3)"
  , Option []   ["scale"]       (ReqArg (set optScale . read) "FLOAT")  "feature size of visualisation (30)"
  , Option []   ["degree"]      (ReqArg (set optDegree . read) "INT")   "number of waves to sum for each point (5)"
  , Option []   ["benchmark"]   (NoArg  (set optBench True))            "benchmark instead of displaying animation (False)"
  , Option "h?" ["help"]        (NoArg  (set optHelp True))             "show help message"
  ]


basicHeader :: String
basicHeader = unlines
  [ "accelerate-crystal (c) [2011..2013] The Accelerate Team"
  , ""
  , "Usage: accelerate-crystal [OPTIONS]"
  ]

fancyHeader :: Options -> String
fancyHeader opts = unlines (header : table)
  where
    active this         = if this == map toLower (show $ get optBackend opts) then "*" else ""
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


parseArgs :: [String] -> IO (Options, Criterion.Config, [String])
parseArgs argv =
  let
      helpMsg err = concat err
        ++ usageInfo basicHeader                    options
        ++ usageInfo "\nGeneric criterion options:" Criterion.defaultOptions

  in case getOpt' Permute options argv of
      (o,_,n,[])  -> do

        -- pass unrecognised options to criterion
        (cconf, rest) <- Criterion.parseArgs Criterion.defaultConfig Criterion.defaultOptions n
        case foldr id defaultOptions o of
          opts | False <- get optHelp opts      -> putStrLn (fancyHeader opts) >> return (opts, cconf, rest)
          _                                     -> putStrLn (helpMsg [])       >> exitSuccess

      (_,_,_,err) -> error (helpMsg err)

