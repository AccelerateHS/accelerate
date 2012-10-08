{-# LANGUAGE TemplateHaskell #-}

module Gloss.Simulate (

  Simulate(..), simulateWorld, simulateDrawTree, simulatePause,
  initialise, simulate,

) where


import Common.Type
import Common.World

import Data.Label


data Simulate = Simulate
  {
    _simulateWorld      :: {-# UNPACK #-} !World

    -- parameters to control the visualisation
    --
  , _simulateDrawTree   :: !Bool
  , _simulatePause      :: !Bool
  }


-- | Populate the universe with a set of bodies
--
initialise :: World -> Simulate
initialise world
  = Simulate
  {
    _simulateWorld      = world
  , _simulateDrawTree   = False
  , _simulatePause      = False
  }

$(mkLabels [''Simulate])


-- | Advance the simulation state
--
simulate :: (Time -> World -> World) -> Float -> Simulate -> Simulate
simulate advance timeStep state
  | _simulatePause state        = state
  | otherwise                   = modify simulateWorld (advance (realToFrac timeStep)) state

