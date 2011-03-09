{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Benchmarks
-- Copyright   : [2011] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--

module Benchmarks (runAccBenchmarks) where

import Util
import Config
import BuildBox

import Control.Applicative
import Data.Char
import Data.Maybe
import Text.CSV.ByteString
import System.FilePath
import qualified Data.Sequence              as Seq
import qualified BuildBox.Data.Log          as Log
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lex.Double as B



-- The list of benchmarks. In most cases it is expected that the program write
-- timing information from criterion (or similar) to the given file. It should
-- also be linked with RTS options to query heap usage.
--
-- Note that we require the criterion name for the benchmark to match that below
--
benchmarks :: Config -> FilePath -> [Benchmark]
benchmarks cfg summaryLog =
  let simple name args = variants name [([],args)]
      variants name    = map $ \(tag,args) ->
       Benchmark
        { benchmarkName    = name ++ bool ('-' : tag) [] (null tag)
        , benchmarkSetup   = return ()
        , benchmarkCommand = runAcc summaryLog ("accelerate-examples/dist/build" </> name </> name) args
        , benchmarkCheck   = return [] }
      --
      scratch = configScratchDir cfg
      lena_bw = "accelerate-examples/data/images/lena_bw.pgm"
  in
  concat
    [ -- library primitives
      variants "acc-map"     $ map (\x -> (x,[x])) ["abs", "plus", "square"]
    , variants "acc-fold"    $ map (\x -> (x,[x])) ["sum", "product", "maximum", "minimum", "sum-d2", "product-2d"]
    , variants "acc-stencil" $ map (\x -> (x,[x])) ["3x3"]

      -- simple examples
    , simple "acc-sasum"        []
    , simple "acc-saxpy"        []
    , simple "acc-dotp"         []
    , simple "acc-filter"       []
    , simple "acc-smvm"         ["accelerate-examples/data/matrices/random.mtx"]
    , simple "acc-blackscholes" []
    , simple "acc-radixsort"    []

      -- block copy / IO
    , simple "acc-io"           []

      -- image processing
    , simple "acc-canny"         [lena_bw, scratch </> "canny_out.pgm" ]
    , simple "acc-integralimage" [lena_bw, scratch </> "integral_out.pgm" ]
    ]


-- | Execute all benchmarks and return statistics for each.
--
runAccBenchmarks :: Config -> Build [BenchResult Stats]
runAccBenchmarks config = withTempFile $ \f -> do
  runs <- mapM run (benchmarks config f)
  crit <- readCriterionStats f
  return $ combineBenchStats runs crit
  where
    run  x = statBenchResult
           . BenchResult (benchmarkName x) . unit <$> runBenchmarkOnce 1 x

    combineBenchStats = zipWith . liftBenchRunResult2 $ \b1 b2 ->
      [BenchRunResult 0
        (concatMap benchRunResultQuirks  b1 ++ concatMap benchRunResultQuirks  b2)
        (concatMap benchRunResultAspects b1 ++ concatMap benchRunResultAspects b2) ]


-- Read criterion statistics. The file contains the following header, which we
-- ignore, and corresponding format:
--
--   "Name,Mean,MeanLB,MeanUB,Stddev,StddevLB,StddevUB"
--
readCriterionStats :: FilePath -> Build [BenchResult Stats]
readCriterionStats f = do
  contents <- io $ B.readFile f
  case parseCSV contents of
    Nothing  -> throw  $ ErrorOther "failed to parse criterion results"
    Just csv -> return $ map parse (tail csv)

  where
    seconds         = Seconds . fst . fromJust . B.readDouble
    stats avg lb ub = WithSeconds (Time KernelWall Stats { statsMin = seconds lb
                                                         , statsMax = seconds ub
                                                         , statsAvg = seconds avg })

    parse (n:avg:lb:ub:_) = BenchResult (B.unpack n) [BenchRunResult 0 [] [ stats avg lb ub ]]
    parse _               = error "we should not be here..."


-- Run a standard accelerate-examples benchmark. The executable is expected to
-- write kernel statistics to the given file, which will be read later.
--
-- TLM: return the wall time for the entire program? Includes random number
--      generation and the criterion machinery, but also important Accelerate
--      operations such as sharing recovery and code generation.
--
runAcc :: FilePath -> FilePath -> [String] -> Build [WithUnits (Aspect Single)]
runAcc tlog exe args = do
  let cmd = unwords [exe, "--summary=" ++ tlog, unwords args, "+RTS -K16M -t"]
  (status, logOut, logErr) <- systemTeeLog False cmd Log.empty
  case status of
       ExitSuccess -> return $ parseGCLine logErr
       _           -> throw  $ ErrorSystemCmdFailed cmd status logOut logErr


-- Parse one-line GC summary statistics and return heap allocation and maximum
-- residency aspects.
--
-- Example (from ghc-7.0.1):
--
-- <<ghc: 3074525896 bytes, 4949 GCs, 2674563/6977652 avg/max bytes residency (8 samples), ...
--     15M in use, 0.01 INIT (0.00 elapsed), 5.56 MUT (6.61 elapsed), 0.19 GC (0.21 elapsed) :ghc>>
--
parseGCLine :: Log.Log -> [WithUnits (Aspect Single)]
parseGCLine gc =
  case index of
    Nothing -> []
    Just i  ->
      let toks = B.words $ Seq.index gc i
      in
      [ Used HeapAlloc `bytes` read' (toks !! 1)
      , Used HeapMax   `bytes` read' (snd . B.spanEnd isDigit $ toks !! 5) ]

  where
    read' = fst . fromJust . B.readInteger
    index = Seq.findIndexR (\l -> B.isPrefixOf "<<ghc:" l && B.isSuffixOf ":ghc>>" l) gc
      -- TLM: could reasonably assume that this is the last line of the log,
      --      since it is printed by the RTS after program termination.

