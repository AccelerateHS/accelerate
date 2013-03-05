{-# LANGUAGE CPP             #-}
{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE TemplateHaskell #-}

module Config (

  Options, optBackend, optSize, optLimit, optFramerate, optBench,
  parseArgs, run, run1

) where

import Data.Char
import Data.List
import Data.Label
import System.Exit
import System.Console.GetOpt                            ( OptDescr(..), ArgDescr(..), ArgOrder(Permute), getOpt', usageInfo )
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


backends :: [OptDescr (Options -> Options)]
backends =
  [ Option []   ["interpreter"] (NoArg  (set optBackend Interpreter))   "reference implementation (sequential)"
#ifdef ACCELERATE_CUDA_BACKEND
  , Option []   ["cuda"]        (NoArg  (set optBackend CUDA))          "implementation for NVIDIA GPUs (parallel)"
#endif
  ]

options :: [OptDescr (Options -> Options)]
options = backends ++
  [ Option []   ["size"]        (ReqArg (set optSize . read) "INT")     "visualisation size (512)"
  , Option []   ["limit"]       (ReqArg (set optLimit . read) "INT")    "iteration limit for escape (255)"
  , Option []   ["framerate"]   (ReqArg (set optFramerate . read) "INT")"visualisation framerate (10)"
  , Option []   ["static"]      (NoArg  (set optFramerate 0))           "do not animate the image"
  , Option []   ["benchmark"]   (NoArg  (set optBench True))            "benchmark instead of displaying animation (False)"
  , Option "h?" ["help"]        (NoArg  (set optHelp True))             "show help message"
  ]


basicHeader :: String
basicHeader = unlines
  [ "accelerate-mandelbrot (c) [2011..2013] The Accelerate Team"
  , ""
  , "Usage: accelerate-mandelbrot [OPTIONS]"
  ]

basicFooter :: String
basicFooter = unlines
  [ ""
  , "Runtime usage:"
  , "     arrows       translate display"
  , "     z ;          zoom in"
  , "     x q          zoom out"
  , "     f            single precision calculations"
  , "     d            double precision calculations (if supported)"
  ]

fancyHeader :: Options -> String
fancyHeader opts = unlines (header : table ++ footer)
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
    footer = lines basicFooter


-- | Two levels of argument parsing -- ours and criterions.
--
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

