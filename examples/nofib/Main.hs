
module Main where

import Test.Framework
import System.Environment

import Config
import ParseArgs
import Test.Mapping
import Test.Reduction
import Test.PrefixSum
import Test.IndexSpace


main :: IO ()
main = do

  -- process command line args, and print a brief usage message
  --
  argv                          <- getArgs
  (conf, cconf, tfconf, nops)   <- parseArgs' configHelp configBackend options defaults header footer argv

  -- Run tests, executing the simplest first. More complex operations, such as
  -- segmented operations, generally depend on simpler tests.
  --
  defaultMainWithOpts
    [ test_map conf
    , test_zipWith conf
    , test_foldAll conf
    , test_fold conf
    , test_backpermute conf
    , test_permute conf
    , test_prefixsum conf       -- requires fold
    , test_foldSeg conf         -- requires scan
    ]
    tfconf

  putStrLn "MOAR THINGS!"

