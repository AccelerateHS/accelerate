{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Benchmark where

import Data.Array.IArray
import Data.Array.Unboxed                       (UArray)

import Data.List
import Data.Monoid
import Criterion
import Criterion.IO
import Criterion.Config
import Criterion.Main
import Criterion.Monad
import Criterion.Types
import Criterion.Environment
import Control.Monad
import Control.Monad.Trans                      (liftIO)
import Control.DeepSeq
import System.IO
import System.Directory
import System.Environment


instance (Ix dim, IArray UArray e) => NFData (UArray dim e) where
  rnf a = a ! head (indices a) `seq` ()


-- Much like defaultMain, but we ignore any non-flag command line arguments,
-- which we take as the inputs to the program itself (returned via getArg')
--
runBenchmark :: [Benchmark] -> IO ()
runBenchmark = runBenchmarkWith defaultConfig (return ())

runBenchmarkWith :: Config -> Criterion () -> [Benchmark] -> IO ()
runBenchmarkWith defCfg prep bs = do
  (cfg, _) <- parseArgs defCfg defaultOptions =<< getArgs
  withConfig cfg $
    if cfgPrintExit cfg == List
    then do
      _ <- note "Benchmarks:\n"
      mapM_ (note "  %s\n") (sort $ concatMap benchNames bs)
    else do
      case getLast $ cfgSummaryFile cfg of
        Just fn -> liftIO $ writeFileOnce fn "Name,Mean,MeanLB,MeanUB,Stddev,StddevLB,StddevUB\n"
        Nothing -> return ()
      env <- measureEnvironment
      let shouldRun = const True
      prep
      runAndAnalyse shouldRun env $ BenchGroup "" bs

writeFileOnce :: FilePath -> String -> IO ()
writeFileOnce fn line = do
  exists <- doesFileExist fn
  size   <- withFile fn ReadWriteMode hFileSize
  unless (exists && size > 0) $ writeFile fn line

