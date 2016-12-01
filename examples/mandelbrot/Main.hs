--
-- A Mandelbrot set generator.
-- Originally submitted by Simon Marlow as part of Issue #49.
--

import World
import Config

import Data.Label

import Prelude                                  as P
import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.Examples.Internal  as A
import Graphics.Gloss.Accelerate.Data.Picture   as G
import qualified Graphics.Gloss                 as G


-- Main ------------------------------------------------------------------------

makePicture :: World -> G.Picture
makePicture world = bitmapOfArray (renderWorld world) False


main :: IO ()
main = do
  beginMonitoring
  (conf, opts, rest)    <- parseArgs options defaults header footer

  let world     = initialWorld conf opts view
      fps       = get configFramerate conf
      width     = get configWidth conf
      height    = get configHeight conf

      -- Centre coordinates: Re(c) = -0.7; Im(c) = 0
      -- View width: 3.067
      --
      view        = (-2.23, -1.15, 0.83, 1.15)

      force arr   = indexArray arr (Z:.0:.0) `seq` arr

  runBenchmarks opts rest
    [ bench "mandelbrot" $ whnf (force . renderWorld) world ]

  runInteractive opts rest
    $ if fps P.== 0
         then G.display
                  (G.InWindow "Mandelbrot" (width, height) (10, 10))
                  G.black
                  (makePicture world)

         else G.play
                  (G.InWindow "Mandelbrot" (width, height) (10, 10))
                  G.black
                  fps
                  world
                  makePicture
                  (react conf opts)
                  (const refocus)

