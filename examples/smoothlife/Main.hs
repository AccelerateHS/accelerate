{-# LANGUAGE CPP #-}
--
-- A cellular automata simulation
--

-- friends
import Config
import SmoothLife
import Gloss.Draw
import Random.Array

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
  = do  (conf, cconf, nops)     <- parseArgs =<< getArgs

        let -- visualisation configuration
            n           = get configWindowSize conf
            zoom        = get configWindowZoom conf
            fps         = get configFramerate conf
            (_,ra)      = get configDiscRadius conf

            width       = n * zoom
            height      = n * zoom

            advance     = run1 conf (smoothlife conf)
            render      = draw conf

        -- initialise with patches of random data
        world <- evaluate . advance =<< randomArray (Z:.n:.n) ra

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

