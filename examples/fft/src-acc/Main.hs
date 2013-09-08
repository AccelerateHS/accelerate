
module Main where

import Config
import FFT
import HighPass
import ParseArgs

import Prelude                                          as P
import Data.Label
import Criterion.Main                                   ( defaultMainWith, bench, whnf )
import System.Exit
import System.Environment
import System.FilePath

import Data.Array.Accelerate                            as A
import Data.Array.Accelerate.IO                         as A

-- Main routine ----------------------------------------------------------------

main :: IO ()
main
  = do  argv                    <- getArgs
        (conf, cconf, nops)     <- parseArgs configHelp configBackend options defaults header footer argv
        (fileIn, fileOut)       <- case nops of
          (i:o:_) -> return (i,o)
          _       -> parseArgs configHelp configBackend options defaults header footer ("--help":argv)
                  >> exitSuccess

        -- Read in the image file
        img   <- either (error . show) id `fmap` A.readImageFromBMP fileIn
        let Z :. height :. width = A.arrayShape img

        -- Set up the operations
        let (file,bmp)  = splitExtension fileOut
            fileMag     = file P.++ "-mag"   <.> bmp
            filePhase   = file P.++ "-phase" <.> bmp
            fileHP      = file P.++ "-hp"    <.> bmp

            cutoff      = get configCutoff conf
            clip        = get configClip conf
            backend     = get configBackend conf

        if P.not (get configBenchmark conf)
           then do
             -- Write out the images to file
             --
             let highpass       = run backend $ highpassFFT width height cutoff (use img)
                 (mag, phase)   = run backend $ imageFFT    width height clip   (use img)

             writeImageToBMP fileHP    highpass
             writeImageToBMP fileMag   mag
             writeImageToBMP filePhase phase

           else do
             -- Run the operations through criterion
             --
             withArgs (P.drop 2 nops) $ defaultMainWith cconf (return ())
               [ bench "highpass" $ whnf (run1 backend (highpassFFT width height cutoff)) img
               , bench "fft"      $ whnf (run1 backend (imageFFT    width height clip))   img
               ]

