
module Main where

import Config
import FFT
import HighPass

import Prelude                                          as P
import Data.Label
import System.Exit
import System.Environment
import System.FilePath

import Data.Array.Accelerate                            as A
import Data.Array.Accelerate.IO                         as A
import Data.Array.Accelerate.Examples.Internal          as A


-- Main routine ----------------------------------------------------------------

main :: IO ()
main
  = do
        beginMonitoring
        (conf, opts, rest)      <- parseArgs options defaults header footer
        (fileIn, fileOut)       <- case rest of
          (i:o:_) -> return (i,o)
          _       -> withArgs ["--help"] $ parseArgs options defaults header footer
                  >> exitSuccess

        -- Read in the image file
        img   <- either (error . show) id `fmap` A.readImageFromBMP fileIn

        -- Set up the operations
        let (file,bmp)  = splitExtension fileOut
            fileMag     = file P.++ "-mag"   <.> bmp
            filePhase   = file P.++ "-phase" <.> bmp
            fileHP      = file P.++ "-hp"    <.> bmp

            cutoff      = get configCutoff conf
            clip        = get configClip conf
            backend     = get optBackend opts

            sh          = A.arrayShape img

            -- Write out the images to file
            --
            highpass     = run backend $ highpassFFT sh cutoff (use img)
            (mag, phase) = run backend $ imageFFT    sh clip   (use img)

        writeImageToBMP fileHP    highpass
        writeImageToBMP fileMag   mag
        writeImageToBMP filePhase phase

        runBenchmarks opts (P.drop 2 rest)
          [ bench "highpass" $ whnf (run1 backend (highpassFFT sh cutoff)) img
          , bench "fft"      $ whnf (run1 backend (imageFFT    sh clip))   img
          ]


