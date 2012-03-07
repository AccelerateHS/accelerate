{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module Config (

  Options, optBackend,
  run, processArgs

) where

import Data.Char
import Data.List
import Data.Label
import Test.Framework
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
  deriving (Eq, Enum, Bounded, Show)


data Options = Options
  {
    _optBackend         :: Backend
  }
  deriving Show

$( mkLabels [''Options] )


options :: [OptDescr (Options -> Options)]
options =
  [ Option [] ["interpreter"]           (NoArg (set optBackend Interpreter))    "reference implementation (sequential)"
#ifdef ACCELERATE_CUDA_BACKEND
  , Option [] ["cuda"]                  (NoArg (set optBackend CUDA))           "implementation for NVIDIA GPUs (parallel)"
#endif
  ]


run :: Arrays a => Options -> Acc a -> a
run opts = case _optBackend opts of
  Interpreter   -> Interp.run
#ifdef ACCELERATE_CUDA_BACKEND
  CUDA          -> CUDA.run
#endif

processArgs :: [String] -> IO (Options, RunnerOptions)
processArgs argv = do
  args  <- interpretArgs argv
  case args of
    Left msg            -> error msg
    Right (opts, rest)  -> return $! (foldl parse1 defaults rest, opts)
  --
  where
    defaults            = Options maxBound
    parse1 opts x       = case filter (\(Option _ [f] _ _) -> map toLower x `isPrefixOf` f) options of
                            [Option _ _ (NoArg go) _]   -> go opts
                            _                           -> opts

