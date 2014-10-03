{-# OPTIONS_HADDOCK hide #-}

module Benchmark (

  runBenchmark,
  nf, nfIO, whnf, whnfIO,

) where

import ParseArgs

import Data.Label
import Control.Monad
import System.Environment

import Criterion
import Criterion.Monad
import Criterion.Report


-- | Run the given benchmark, if we have enabled benchmark mode.
--
runBenchmark :: Options -> [String] -> Benchmarkable -> IO ()
runBenchmark opt argv bm
  = when (get optBenchmark opt)
  $ withArgs argv
  $ do
        -- Run the single benchmark and retrieve the generated report. If we
        -- have multiple calls to 'runBenchmark' it might be necessary to tag
        -- each with a name so that we don't clobber the report file.
        --
        let conf = get optCriterion opt
        r       <- benchmarkWith' conf bm
        withConfig conf (report [r])

        -- TODO: Analyse, upload the results to benchmark server, etc.
        --

