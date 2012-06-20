{-# LANGUAGE CPP, DeriveDataTypeable #-}

module Config where

import Data.Version
import Text.PrettyPrint
import System.Console.CmdArgs
import Paths_accelerate_examples

import Data.Array.Accelerate                            ( Arrays, Acc )
import qualified Data.Array.Accelerate.Interpreter      as Interpreter

-- -----------------------------------------------------------------------------
-- [NOTE: Adding backend support]
--
-- To add support for additional Accelerate backends:
--
--   1. Edit the cabal file so that the backend can be optionally included via
--      CPP macros, as has been done for the CUDA backend. Add the appropriate
--      import statement below.
--
--   2. Add a new constructor to the 'Backend' data type, and add this case to
--      the 'backend' dispatch function. This function is called to evaluate an
--      Accelerate expression.
--
--   3. Add the new constructor to the 'cfgBackend' list in 'defaultConfig'.
--      This will make the backend appear in the command-line options.
--
#ifdef ACCELERATE_CUDA_BACKEND
import qualified Data.Array.Accelerate.CUDA             as CUDA
#endif

#ifdef ACCELERATE_OPENCL_BACKEND
import qualified Data.Array.Accelerate.OpenCL           as OpenCL
#endif

-- The Accelerate backends available to test, which should be no larger than the
-- build configuration for the Accelerate library itself.
--
data Backend
  = Interpreter
#ifdef ACCELERATE_CUDA_BACKEND
  | CUDA
#endif
#ifdef ACCELERATE_OPENCL_BACKEND
  | OpenCL
#endif
  deriving (Show, Data, Typeable)


-- How to evaluate Accelerate programs with the chosen backend?
--
backend :: Arrays a => Config -> Acc a -> a
backend cfg =
  case cfgBackend cfg of
    Interpreter -> Interpreter.run
#ifdef ACCELERATE_CUDA_BACKEND
    CUDA        -> CUDA.run
#endif
#ifdef ACCELERATE_OPENCL_BACKEND
    OpenCL      -> OpenCL.run
#endif

--
-- -----------------------------------------------------------------------------
--

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
  , cfgSamples     :: Maybe Int
  , cfgResamples   :: Maybe Int
  , cfgSummaryFile :: Maybe FilePath

    -- names of tests to run (all non-option arguments)
  , cfgArgs        :: [String]
  }
  deriving (Show, Data, Typeable)


-- With list of (name,description) pairs for the available tests
--
defaultConfig :: [(String,String)] -> Config
defaultConfig testPrograms =
  let numElements = 100000
  in  Config
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
#ifdef ACCELERATE_OPENCL_BACKEND
    , OpenCL
        &= explicit
        &= name "opencl"
        &= help "Implementation for OpenCL (parallel)"
#endif

    ]

  , cfgVerify = def
      &= explicit
      &= name "k"
      &= name "verify"
      &= help "Only verify examples, do not run timing tests"

  , cfgElements = numElements
      &= explicit
      &= name "n"
      &= name "size"
      &= help ("Canonical test data size (" ++ shows numElements ")")

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
      &= name "resamples"
      &= help "Number of bootstrap resamples to perform"

  , cfgSamples = def
      &= explicit
      &= name "s"
      &= name "samples"
      &= help "Number of samples to collect"

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

