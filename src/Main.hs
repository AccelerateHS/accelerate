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
import Graphics.Gloss.Interface.Game


main :: IO ()
main = do
  (opt,noms)    <- processArgs =<< getArgs
  let width     = get simulationWidth  opt * get displayScale opt
      height    = get simulationHeight opt * get displayScale opt
      fps       = get displayFramerate opt
      dp        = get viscosity opt
      dn        = get diffusion opt

  -- warming up...
  initialWorld <- evaluate (simulate opt dp dn 0.1 $ initialise opt)

  if get optBench opt
     -- benchmark
     then withArgs noms $ defaultMain
              [ bench "fluid" $ whnf (simulate opt dp dn 1.0) initialWorld ]

     -- simulate
     else gameInWindow
              "accelerate-fluid"
              (width, height)
              (10, 20)                  -- initial position
              black                     -- background colour
              fps                       -- display framerate
              initialWorld              -- initial state of the simulation
              (render opt)              -- render world state into a picture
              (react opt)               -- handle user events
              (simulate opt dp dn)      -- one step of the simulation

