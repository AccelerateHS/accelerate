{-# LANGUAGE CPP, DeriveDataTypeable #-}

module Config where

import Data.Version
import Text.PrettyPrint
import System.Console.CmdArgs
import Paths_accelerate_examples

-- The Accelerate backends available to test, which should be no larger than the
-- build configuration for the Accelerate library itself.
--
data Backend
  = Interpreter
#ifdef ACCELERATE_CUDA_BACKEND
  | CUDA
#endif
  deriving (Show, Data, Typeable)


-- Program configuration options
--
data Config = Config
  {
    -- common options
    cfgBackend     :: Backend
  , cfgVerify      :: Bool
  , cfgElements    :: Int
  , cfgImage       :: Maybe FilePath
  , cfgMatrix      :: Maybe FilePath

    -- criterion hooks
  , cfgPerformGC   :: Bool
  , cfgConfidence  :: Maybe Double
  , cfgResamples   :: Maybe Int
  , cfgSummaryFile :: Maybe FilePath

    -- names of tests to run (all non-option arguments)
  , cfgArgs        :: [String]
  }
  deriving (Show, Data, Typeable)


-- With list of (name,description) pairs for the available tests
--
defaultConfig :: [(String,String)] -> Config
defaultConfig testPrograms = Config
  {
    cfgBackend = enum
    [ Interpreter
        &= help "Reference implementation (sequential)"
#ifdef ACCELERATE_CUDA_BACKEND
    , CUDA
        &= explicit
        &= name "cuda"
        &= help "Implementation for NVIDIA GPUs (parallel)"
#endif
    ]

  , cfgVerify = def
      &= explicit
      &= name "k"
      &= name "verify"
      &= help "Only verify examples, do not run timing tests"

  , cfgElements = 1000000
      &= explicit
      &= name "n"
      &= name "size"
      &= help "Canonical test data size (1000000)"

  , cfgImage = def
      &= explicit
      &= name "i"
      &= name "image"
      &= help "PGM image file to use for image-processing tests"
      &= typFile

  , cfgMatrix = def
      &= name "m"
      &= name "matrix"
      &= explicit
      &= help "MatrixMarket file to use for SMVM test"
      &= typFile

  , cfgPerformGC = enum
    [ False
        &= name "G"
        &= name "no-gc"
        &= explicit
        &= help "Do not collect garbage between iterations"
    , True
        &= name "g"
        &= name "gc"
        &= explicit
        &= help "Collect garbage between iterations"
    ]

  , cfgConfidence = def
      &= explicit
      &= name "I"
      &= name "ci"
      &= help "Bootstrap confidence interval"
      &= typ  "CI"

  , cfgResamples = def
      &= explicit
      &= name "s"
      &= name "resamples"
      &= help "Number of bootstrap resamples to perform"

  , cfgSummaryFile = def
      &= name "u"
      &= name "summary"
      &= explicit
      &= help "Produce a summary CSV file of all results"
      &= typFile

  , cfgArgs = def
      &= args
      &= typ  "TESTS"
  }
  &= program "accelerate-examples"
  &= summary "accelerate-examples (c) 2011 The Accelerate Team"
  &= versionArg [summary $ "accelerate-examples-" ++ showVersion version]
  &= verbosityArgs [help "Print more output"] [help "Print less output"]
  &= details (
      [ "Available tests, by prefix match:"
      , "  <default>             run all tests"
      ]
      ++
      map (\(n,d) -> render . nest 2 $ text n $$ nest 22 (text d)) testPrograms)
      --
      -- magic number to make the second columns of the help text align

