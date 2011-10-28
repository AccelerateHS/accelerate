--
-- A stable fluid simulation
--
-- Jos Stam, "Real-time Fluid Dynamics for Games"
--

module Main where

import Config
import World
import Fluid
import Graphics.Gloss.Interface.Game


main :: IO ()
main = do
  cfg <- processArgs
  let width  = simulationWidth  cfg * displayScale cfg
      height = simulationHeight cfg * displayScale cfg
      fps    = displayFramerate cfg
  gameInWindow
    "accelerate-fluid"
    (width, height)
    (10, 20)                    -- initial position
    black                       -- background colour
    fps                         -- display framerate
    (initialWorld cfg)          -- initial state of the simulation
    (renderWorld cfg)           -- render world state into a picture
    (\_ w -> w)                 -- handle user events
    (\_ w -> w)                 -- one step of the simulation

