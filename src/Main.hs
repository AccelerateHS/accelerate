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
import Graphics.Gloss.Interface.Game


main :: IO ()
main = do
  cfg <- processArgs
  let width  = simulationWidth  cfg * displayScale cfg
      height = simulationHeight cfg * displayScale cfg
      fps    = displayFramerate cfg
      dp     = viscosity cfg
      dn     = diffusion cfg
  gameInWindow
    "accelerate-fluid"
    (width, height)
    (10, 20)                    -- initial position
    black                       -- background colour
    fps                         -- display framerate
    (initialise cfg)            -- initial state of the simulation
    (render cfg)                -- render world state into a picture
    (react cfg)                 -- handle user events
    (simulate cfg dp dn)        -- one step of the simulation

