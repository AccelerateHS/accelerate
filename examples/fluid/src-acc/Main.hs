--
-- A stable fluid simulation
--
-- Jos Stam, "Real-time Fluid Dynamics for Games"
--

module Main where

import Config
import World
import Fluid
import Event
import Data.Label
import Control.Exception
import Graphics.Gloss.Interface.IO.Game

import Prelude                                          as P
import Data.Array.Accelerate                            as A
import Data.Array.Accelerate.Examples.Internal          as A


main :: IO ()
main = do
  beginMonitoring
  (conf, opts, rest)    <- initialiseConfig =<< parseArgs options defaults header footer

  let -- configuration parameters
      --
      width     = get simulationWidth  conf * get displayScale conf
      height    = get simulationHeight conf * get displayScale conf
      steps     = get simulationSteps  conf
      fps       = get displayFramerate conf
      dp        = get viscosity conf
      dn        = get diffusion conf
      dt        = get timestep conf
      backend   = get optBackend opts

      -- Prepare user-input density and velocity sources
      --
      sources s = let (ix, ss)  = P.unzip s
                      sh        = Z :. P.length ix
                  in  ( A.fromList sh ix, A.fromList sh ss )

      -- for benchmarking
      --
      force w   =
        indexArray (densityField  w) (Z:.0:.0) `seq`
        indexArray (velocityField w) (Z:.0:.0) `seq` w

      -- Prepare to execute the next step of the simulation.
      --
      -- Critically, we use the run1 execution form to ensure we bypass all
      -- front-end conversion phases.
      --
      step           =  run1 backend (fluid steps dt dp dn)
      simulate world =
        let ds          = sources (densitySource world)
            vs          = sources (velocitySource world)
            (df', vf')  = step ( ds, vs, densityField world, velocityField world )
        in
        force $ world { densityField  = df', velocityField  = vf'
                      , densitySource = [],  velocitySource = [] }

  -- warming up...
  --
  initialWorld  <- evaluate (initialise conf)
  _             <- evaluate (simulate initialWorld)


  runBenchmarks opts rest
    [ bench "fluid" $ whnf simulate initialWorld ]

  runInteractive opts rest
    $ playIO
          (InWindow "accelerate-fluid" (width, height) (10, 20))
          black                                 -- background colour
          fps                                   -- display framerate
          initialWorld                          -- initial state of the simulation
          (render conf)                         -- render world state into a picture
          (react conf)                          -- handle user events
          (\_ -> return . simulate)             -- one step of the simulation

