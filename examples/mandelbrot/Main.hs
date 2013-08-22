{-# LANGUAGE CPP #-}
--
-- A Mandelbrot set generator.
-- Originally submitted by Simon Marlow as part of Issue #49.
--

import World
import Config
import Monitoring
import ParseArgs

import Data.Label
import Control.Monad
import System.Environment                       ( getArgs, withArgs )
import Criterion.Main                           ( defaultMainWith, bench, whnf )

import Prelude                                  as P
import Data.Array.Accelerate                    as A
import Graphics.Gloss.Accelerate.Data.Picture   as G
import qualified Graphics.Gloss                 as G


-- Main ------------------------------------------------------------------------

makePicture :: World -> G.Picture
makePicture world = bitmapOfArray (renderWorld world) False


main :: IO ()
main
  = do
        beginMonitoring
        argv                    <- getArgs
        (conf, cconf, rest)     <- parseArgs optHelp optBackend options defaults header footer argv

        let world       = initialWorld conf view
            fps         = get optFramerate conf
            width       = get optWidth conf
            height      = get optHeight conf

            -- Centre coordinates: Re(c) = -0.7; Im(c) = 0
            -- View width: 3.0769
            --
            view        = (-2.23, -1.15, 0.83, 1.15)

            force arr   = indexArray arr (Z:.0:.0) `seq` arr

            mandel
              | get optBench conf
              = withArgs rest $ defaultMainWith cconf (return ())
                    [ bench "mandelbrot" $ whnf (force . renderWorld) world ]
#ifdef ACCELERATE_ENABLE_GUI
              | fps == 0
              = G.display
                    (G.InWindow "Mandelbrot" (width, height) (10, 10))
                    G.black
                    (makePicture world)

              | fps > 0
              = G.play
                    (G.InWindow "Mandelbrot" (width, height) (10, 10))
                    G.black
                    fps
                    world
                    makePicture
                    (react conf)
                    (const refocus)
#endif
              | otherwise
              = return ()


        unless (P.null rest) $
          putStrLn $ "Warning: unrecognized options: " P.++ show rest

        mandel

