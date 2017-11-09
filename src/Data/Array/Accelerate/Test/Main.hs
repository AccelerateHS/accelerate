
module Main where

import Data.Label

import Config
import Test.Prelude
import Test.Sharing
import Test.Imaginary
import Test.Spectral
import Test.Issues
import Test.Foreign

import Data.Array.Accelerate.Examples.Internal


main :: IO ()
main = do

  -- Kick off EKG monitoring. Perhaps not particularly useful since we spend a
  -- lot of time just generating random data, etc.
  --
  beginMonitoring

  -- process command line args, and print a brief usage message
  --
  (conf, opts, rest)    <- parseArgs options defaults header footer
  let backend            = get optBackend opts

  -- Run tests, executing the simplest first. More complex operations, such as
  -- segmented operations, generally depend on simpler tests. We build up to the
  -- larger test programs.
  --
  runTests opts rest
    [ test_prelude backend conf
    , test_sharing conf
    , test_imaginary backend conf
    , test_spectral backend conf
    , test_foreign backend conf
    , test_issues backend conf
    ]

  -- Now run some criterion benchmarks.
  --
  -- TODO: We should dump the results to file so that they can be analysed and
  --       checked for regressions.
  --

