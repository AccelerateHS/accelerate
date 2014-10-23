--
-- A cellular automata simulation
--

-- friends
import Config
import SmoothLife
import Gloss.Draw
import Random.Splat

-- system
import Prelude                                          as P
import Data.Array.Accelerate                            as A
import Data.Array.Accelerate.Examples.Internal          as A
import Data.Label
import Control.Exception
import System.Environment
import Graphics.Gloss


main :: IO ()
main
  = do  beginMonitoring
        argv                    <- getArgs
        (conf, opts, rest)      <- parseArgs options defaults header footer argv

        let -- visualisation configuration
            n           = get configWindowSize conf
            zoom        = get configWindowZoom conf
            fps         = get configFramerate conf
            (ra,rb)     = get configDiscRadius conf

            dish        = Z :. n :. n
            width       = n * zoom
            height      = n * zoom

            backend     = get optBackend opts
            scheme      = get configColourScheme conf

            render      = draw conf
                        . run1 backend (colourise scheme)
            advance     = run1 backend (smoothlife conf opts)

        -- initialise with patches of random data
        dots    <- randomCircles dish ra rb
        agar    <- randomArrayWithSystemRandom (splat dots) dish

        world   <- evaluate (advance agar)

        -- Rise minions!
        runBenchmarks opts rest
          [ bench "smoothlife" $ whnf advance world ]

        runInteractive opts rest
          $ play
              (InWindow "Smooth Life" (width, height) (10, 20))
              black
              fps
              world
              render
              (\_ -> id)
              (\_ -> advance)

