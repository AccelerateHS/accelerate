--
-- A Mandelbrot set generator.
-- Originally submitted by Simon Marlow as part of Issue #49.
--

import World
import Config

import Data.Label

import Prelude                                                      as P
import Data.Array.Accelerate.IO                                     as A
import Data.Array.Accelerate.Examples.Internal                      as A
import qualified Graphics.Gloss.Interface.IO.Game                   as G


-- Main ------------------------------------------------------------------------

main :: IO ()
main = do
  beginMonitoring
  (conf, opts, rest)    <- parseArgs options defaults header footer

  let world     = initialWorld conf opts
      bmp       = get configFilePath conf
      width     = get configWidth conf
      height    = get configHeight conf

  runBenchmarks opts rest
    [ bench "mandelbrot" $ whnf renderWorld world ]

  case bmp of
    Just path   -> writeImageToBMP path (renderWorld world)
    Nothing     ->
      runInteractive opts rest $
        G.playIO (G.InWindow "Mandelbrot" (width,height) (10,10))
                 G.black
                 60
                 (updateWorld world)
                 draw
                 (react conf opts)
                 advance

