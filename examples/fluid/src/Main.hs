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
import Criterion.Main
import System.Environment
import Graphics.Gloss.Interface.IO.Game

import Prelude                                  as P
import Data.Array.Accelerate                    as A


main :: IO ()
main = do
  (opt,noms)    <- processArgs =<< getArgs
  let -- configuration parameters
      --
      width     = get simulationWidth  opt * get displayScale opt
      height    = get simulationHeight opt * get displayScale opt
      fps       = get displayFramerate opt
      dp        = get viscosity opt
      dn        = get diffusion opt

      -- Prepare user-input density and velocity sources
      --
      sources s = let (ix, ss)  = P.unzip s
                      sh        = Z :. length ix
                  in  ( A.fromList sh ix, A.fromList sh ss )

      -- for benchmarking
      --
      force world =
        indexArray (densityField  world) (Z:.0:.0) `seq`
        indexArray (velocityField world) (Z:.0:.0) `seq` ()

      -- Prepare to execute the next step of the simulation.
      --
      -- Critically, we use the run1 execution form to ensure we bypass all
      -- front-end conversion phases.
      --
      simulate timestep world =
        let step        = run1 opt (fluid dp dn)
            dt          = A.fromList Z [timestep]
            ds          = sources (densitySource world)
            vs          = sources (velocitySource world)
            (df', vf')  = step ( dt, ds, vs, densityField world, velocityField world )
        in
        return $ world { densityField  = df', velocityField  = vf'
                       , densitySource = [],  velocitySource = [] }

  -- warming up...
  initialWorld  <- initialise opt
  _             <- simulate 0.1 initialWorld

  if get optBench opt
     -- benchmark
     then withArgs noms $ defaultMain
              [ bench "fluid" $ whnfIO (force `fmap` simulate 1.0 initialWorld) ]

     -- simulate
     else playIO
              (InWindow "accelerate-fluid" (width, height) (10, 20))
              black                     -- background colour
              fps                       -- display framerate
              initialWorld              -- initial state of the simulation
              (render opt)              -- render world state into a picture
              (react opt)               -- handle user events
              simulate                  -- one step of the simulation

