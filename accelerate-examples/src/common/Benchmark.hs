{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}

module Benchmark where

import Validate

import Data.Array.IArray
import Data.Array.Unboxed                       (UArray)
import qualified Data.Array.Accelerate          as Acc
import qualified Data.Array.Accelerate.CUDA     as Acc

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
import System.Exit
import System.Directory
import System.Environment


instance (Ix dim, IArray UArray e) => NFData (UArray dim e) where
  rnf a = a ! head (indices a) `seq` ()

-- Run and print the analysis of a benchmark test iff the accelerate test
-- succeeds against the reference implementation. Otherwise, the array indices
-- and elements that did not validate are displayed and the program exits with
-- failure status.
--
{--
benchmark
  :: (IArray UArray e, Similar e, Ix ix, Acc.Elt ix, Acc.Shape sh, Acc.Elt e
     ,Acc.EltRepr ix ~ Acc.EltRepr sh)
  => String
  -> (() -> UArray ix e)
  -> (() -> Acc (Acc.Array sh e))
  -> IO ()
--}
benchmark name ref acc
  | null errs = runTest [test]
  | otherwise = do
      mapM_ (\(i,v) -> hPutStrLn stderr $ ">>> " ++ shows i " : " ++ show v) errs
      exitFailure
  where
    errs = validate (ref()) (Acc.toIArray $ Acc.run (acc ()))
    test = bench name $ whnf (Acc.run . acc) ()


-- Return the non-flag command line arguments
--
getArgs' :: IO [String]
getArgs' = do
  (_, args) <- parseArgs defaultConfig defaultOptions =<< getArgs
  return args


-- Much like defaultMain, but we ignore any non-flag command line arguments,
-- which we take as the inputs to the program itself (returned via getArg')
--
runTest :: [Benchmark] -> IO ()
runTest = runTestWith defaultConfig (return ())

runTestWith :: Config -> Criterion () -> [Benchmark] -> IO ()
runTestWith defCfg prep bs = do
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

