-- |
-- Module:      : Data.Array.Accelerate.Examples.Internal.Criterion
-- Copyright    : [2014] Trevor L. McDonell
-- License      : BSD3
--
-- Maintainer   : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Examples.Internal.Criterion (

  -- * Criterion benchmarking
  runBenchmarks,
  nf, whnf, nfIO, whnfIO,
  Benchmark, env, bench, bgroup,

) where

import Data.Array.Accelerate.Examples.Internal.Report
import Data.Array.Accelerate.Examples.Internal.ParseArgs
import Data.Array.Accelerate.Examples.Internal.Criterion.Config

import Data.Label
import Control.Monad
import System.IO
import System.Directory
import System.Environment

import Criterion.IO
import Criterion.Main


-- | Run the given benchmarks, if we have enabled benchmark mode.
--
runBenchmarks :: Options -> [String] -> [Benchmark] -> IO ()
runBenchmarks opt argv benchmarks
  = when (get optBenchmark opt)
  $ withArgs argv
  $ do
        let crit = get optCriterion opt

        -- In order to get the reports, we read them from the raw data file.
        -- This is actually the way that Criterion does it as well, and I
        -- believe is so that gathering the reports does not affect the
        -- measurements when there is a large number of benchmarks.
        --
        rawFile <- case get rawDataFile crit of
            Just f      -> return f
            Nothing     -> do
              tmp   <- getTemporaryDirectory
              (f,h) <- openBinaryTempFile tmp "accelerate-examples.dat"
              hClose h
              removeFile f
              return f

        -- Run the standard benchmark loop
        --
        defaultMainWith (set rawDataFile (Just rawFile) crit) benchmarks

        -- Retrieve the reports. Delete the temporary file if necessary.
        --
        reports <- do
          exists <- doesFileExist rawFile
          if exists
             then either fail return =<< do
                    rs <- readReports rawFile
                    case get rawDataFile crit of
                        Nothing   -> removeFile rawFile >> return rs
                        Just _    -> return rs
             else return []

        -- Analyse the reports, upload to the benchmark server, etc.
        --
        processReports opt reports

