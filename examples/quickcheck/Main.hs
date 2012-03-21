
module Main where

import Test.Framework
import System.Environment

import Config
import Test.Reduction
import Test.PrefixSum
import Test.IndexSpace


main :: IO ()
main = do
  -- process command line args, and print a brief usage message
  --
  (options, runner)     <- processArgs =<< getArgs

  -- the default execution order uses some knowledge of what functionality is
  -- required for each operation in the CUDA backend.
  --
  defaultMainWithOpts
    [ test_foldAll options
    , test_fold options
    , test_scan options                  -- requires fold
    , test_foldSeg options               -- requires scan
    , test_permute options
    , test_backpermute options
    ]
    runner

