{-# LANGUAGE TupleSections #-}

module Main where

import Test
import Config
import Benchmark

import Prelude                  hiding (catch)
import Data.List
import Data.Maybe
import Control.Arrow
import Control.Monad
import System.Environment
import System.Console.CmdArgs   (cmdArgs, getVerbosity, Verbosity(..))


-- Process command line options, prepare selected test programs for benchmarking
-- or verification
--
processArgs :: IO (Config, [Test])
processArgs = do
  testInfo <- map (title &&& description) `fmap` allTests undefined
  config   <- cmdArgs $ defaultConfig testInfo
  tests    <- filter (selected config) `fmap` allTests config
  --
  return (config, tests)
  where
    selected a = case cfgArgs a of
                   [] -> const True
                   ps -> \x -> any (\p -> p `isPrefixOf` title x) ps


-- Verify results with the chosen backend, turning exceptions into failures.
-- Pass back the tests which succeeded.
--
runVerify :: Config -> [Test] -> IO [Test]
runVerify cfg tests = do
  results <- forM tests $ \t -> (t,) `fmap` verifyTest cfg t
  return . map fst
         $ filter (\(_,r) -> r `elem` [Ok, Skipped]) results


-- Run criterion timing tests in the chosen backend
--
runTiming :: Config -> [Test] -> IO ()
runTiming cfg tests = do
  verbose <- getVerbosity
  unless (verbose == Quiet) $ putStrLn ""
  let args = [ maybe "" (\ci -> "--ci=" ++ show ci)       (cfgConfidence cfg)
             , maybe "" (\r  -> "--resamples=" ++ show r) (cfgResamples cfg)
             , maybe "" (\f  -> "--summary=" ++ f)        (cfgSummaryFile cfg)
             , if cfgPerformGC cfg then "-g" else "-G"
             , case verbose of
                 Loud   -> "--verbose"
                 Quiet  -> "--quiet"
                 Normal -> ""
             ]
  --
  withArgs args
    . runBenchmark
    . catMaybes
    $ map (benchmarkTest cfg) tests


-- Main
-- ====

main :: IO ()
main = do
  (config, tests) <- processArgs
  valid           <- runVerify config tests
  --
  unless (null valid || cfgVerify config) $ runTiming config valid

