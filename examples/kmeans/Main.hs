--
-- A k-means clustering implementation.
-- Run the generate-samples program first to create some random data.
--

module Main where

import Kmeans
import Config
import Monitoring
import ParseArgs

import Prelude                                          as P
import Data.Array.Accelerate                            as A

import Control.Applicative                              ( (<$>), (<*>) )
import Control.Monad                                    ( unless )
import Criterion.Main                                   ( defaultMainWith, bench, whnf )
import Data.Binary                                      ( decodeFile )
import Data.Label                                       ( get )
import System.Directory
import System.Environment


main :: IO ()
main
  = do  beginMonitoring
        argv                    <- getArgs
        (conf, cconf, nops)     <- parseArgs configHelp configBackend options defaults header footer argv

        inputs                  <- (&&) <$> doesFileExist "points.bin"
                                        <*> doesFileExist "clusters"
        unless inputs $ do
          error "Run the GenSamples program first to generate random data"

        points'                 <- decodeFile "points.bin"
        initial'                <- read `fmap` readFile "clusters"

        let nclusters   = P.length initial'
            npoints     = P.length points'

            solve       = run1 backend (kmeans (use points))
            backend     = get configBackend conf

            initial :: Vector (Cluster Float)
            initial = A.fromList (Z:.nclusters) initial'

            points :: Vector (Point Float)
            points = A.fromList (Z:.npoints)   points'

        -- Warm up first by printing the expected results
        --
        putStrLn $ "number of points: " P.++ show npoints
        putStrLn $ "final clusters:\n"  P.++
          unlines (P.map show . A.toList $ solve initial)

        -- Now benchmark
        --
        withArgs nops $ defaultMainWith cconf (return ())
          [ bench "k-means" $ whnf solve initial ]

