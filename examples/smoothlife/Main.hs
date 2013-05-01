{-# LANGUAGE CPP #-}
--
-- A cellular automata simulation
--

-- friends
import Config
import ParseArgs
import SmoothLife
import Gloss.Draw
import Random.Array
import Random.Splat

-- system
import Prelude                                  as P
import Data.Array.Accelerate                    as A
import Data.Label
import Control.Exception
import System.Environment
import Graphics.Gloss
import Criterion.Main                           ( defaultMainWith, bench, whnf )


main :: IO ()
main
  = do  argv                    <- getArgs
        (conf, cconf, nops)     <- parseArgs configHelp configBackend options defaults header footer argv

        let -- visualisation configuration
            n           = get configWindowSize conf
            zoom        = get configWindowZoom conf
            fps         = get configFramerate conf
            (ra,rb)     = get configDiscRadius conf

            dish        = Z :. n :. n
            width       = n * zoom
            height      = n * zoom

            backend     = get configBackend conf
            scheme      = get configColourScheme conf

            render      = draw conf
                        . run1 backend (colourise scheme)
            advance     = run1 backend (smoothlife conf)

        -- initialise with patches of random data
        dots    <- randomCircles dish ra rb
        agar    <- randomArrayWithSystemRandom (splat dots) dish

        world   <- evaluate (advance agar)

        -- Rise minions!
        if get configBenchmark conf
           then withArgs nops $ defaultMainWith cconf (return ())
                  [ bench "smoothlife" $ whnf advance world ]

#ifndef ACCELERATE_ENABLE_GUI
           else return ()
#else
           else play
                  (InWindow "Smooth Life" (width, height) (10, 20))
                  black
                  fps
                  world
                  render
                  (\_ -> id)
                  (\_ -> advance)
#endif

