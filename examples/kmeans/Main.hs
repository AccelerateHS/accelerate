--
-- A k-means clustering implementation.
-- Run the generate-samples program first to create some random data.
--

module Main where

import Kmeans
import Config

import Data.Array.Accelerate                            as A
import Data.Array.Accelerate.Examples.Internal          as A

import Control.Applicative
import Control.Monad                                    ( unless )
import Data.Binary                                      ( decodeFile )
import Data.Label                                       ( get )
import System.Directory
import Prelude                                          as P


main :: IO ()
main = do

  beginMonitoring
  (_, opts, rest)       <- parseArgs options defaults header footer

  inputs                <- (P.&&) <$> doesFileExist "points.bin"
                                  <*> doesFileExist "clusters"
  unless inputs $ do
    error "Run the GenSamples program first to generate random data"

  points'               <- decodeFile "points.bin"
  initial'              <- read `fmap` readFile "clusters"

  let nclusters   = P.length initial'
      npoints     = P.length points'

      solve       = run1 backend (kmeans (use points))
      backend     = get optBackend opts

      initial :: Vector (Cluster Float)
      initial = A.fromList (Z:.nclusters) initial'

      points :: Vector (Point Float)
      points = A.fromList (Z:.npoints) points'

  -- Warm up first by printing the expected results
  --
  putStrLn $ "number of points: " P.++ show npoints
  putStrLn $ "final clusters:\n"  P.++
    unlines (P.map show (A.toList (solve initial)))

  -- Now benchmark
  --
  runBenchmarks opts rest
    [ bench "k-means" $ whnf solve initial ]

