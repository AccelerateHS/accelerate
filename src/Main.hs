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
import Control.Exception
import System.Environment
import Graphics.Gloss.Interface.IO.Game


main :: IO ()
main = do
  (opt,noms)    <- processArgs =<< getArgs
  let width     = get simulationWidth  opt * get displayScale opt
      height    = get simulationHeight opt * get displayScale opt
      fps       = get displayFramerate opt
      dp        = get viscosity opt
      dn        = get diffusion opt

  -- warming up...
  initialWorld  <- initialise opt
  _             <- evaluate (simulate opt dp dn 0.1 initialWorld)

  if get optBench opt
     -- benchmark
     then withArgs noms $ defaultMain
              [ bench "fluid" $ whnf (simulate opt dp dn 1.0) initialWorld ]

     -- simulate
     else playIO
              (InWindow "accelerate-fluid" (width, height) (10, 20))
              black                                                     -- background colour
              fps                                                       -- display framerate
              initialWorld                                              -- initial state of the simulation
              (render opt)                                              -- render world state into a picture
              (\event world -> return $ react opt event world)          -- handle user events
              (\event world -> return $ simulate opt dp dn event world) -- one step of the simulation

