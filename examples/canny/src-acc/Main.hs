{-# LANGUAGE TypeOperators #-}

import Canny
import Config
import Wildfire

import Prelude                                          as P
import Data.Label
import System.Exit
import System.Environment

import Data.Array.Accelerate                            as A
import Data.Array.Accelerate.Examples.Internal          as A
import qualified Data.Array.Accelerate.IO               as A
import qualified Data.Array.Repa.IO.BMP                 as R
import qualified Data.Array.Repa.Repr.Unboxed           as R


-- Main routine ----------------------------------------------------------------

main :: IO ()
main
  = do
        beginMonitoring
        argv                    <- getArgs
        (conf, opts, rest)     <- parseArgs options defaults header footer argv
        (fileIn, fileOut)       <- case rest of
          (i:o:_) -> return (i,o)
          _       -> parseArgs options defaults header footer ("--help":argv)
                  >> exitSuccess

        -- Read in the image file
        img   <- either (error . show) id `fmap` A.readImageFromBMP fileIn

        -- Set up the algorithm parameters
        let threshLow   = get configThreshLow conf
            threshHigh  = get configThreshHigh conf
            backend     = get optBackend opts

            -- Set up the partial results so that we can benchmark individual
            -- kernel stages.
            low                 = constant threshLow
            high                = constant threshHigh

            grey'               = run backend $ toGreyscale (use img)
            blurX'              = run backend $ gaussianX (use grey')
            blurred'            = run backend $ gaussianY (use blurX')
            magdir'             = run backend $ gradientMagDir low (use blurred')
            suppress'           = run backend $ nonMaximumSuppression low high (use magdir')

        if P.not (get optBenchmark opts)
           then do
             -- Connect the strong and weak edges of the image using Repa, and
             -- write the final image to file
             --
             let (image, strong) = run backend $ A.lift (canny threshLow threshHigh (use img))
             edges              <- wildfire (A.toRepa image) (A.toRepa strong)
             R.writeImageToBMP fileOut (R.zip3 edges edges edges)

          else do
            -- Run each of the individual kernel stages through criterion, as
            -- well as the end-to-end step process.
            --
            runBenchmarks opts (P.drop 2 rest)
              [ bgroup "kernels"
                [ bench "greyscale"   $ whnf (run1 backend toGreyscale) img
                , bench "blur-x"      $ whnf (run1 backend gaussianX) grey'
                , bench "blur-y"      $ whnf (run1 backend gaussianY) blurX'
                , bench "grad-x"      $ whnf (run1 backend gradientX) blurred'
                , bench "grad-y"      $ whnf (run1 backend gradientY) blurred'
                , bench "mag-orient"  $ whnf (run1 backend (gradientMagDir low)) blurred'
                , bench "suppress"    $ whnf (run1 backend (nonMaximumSuppression low high)) magdir'
                , bench "select"      $ whnf (run1 backend selectStrong) suppress'
                ]

              , bgroup "canny"
                [ bench "run"     $ whnf (run backend . (P.snd . canny threshLow threshHigh)) (use img)
                , bench "run1"    $ whnf (run1 backend  (P.snd . canny threshLow threshHigh)) img
                ]
              ]

