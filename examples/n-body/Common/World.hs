
module Common.World (

  World(..), advanceBodies, advanceWorld

) where

import Common.Type
import Common.Body

import Data.Array.Accelerate                    as A


data World
  = World
  {
    worldBodies :: !(Vector Body)                       -- ^ Bodies in the simulation
  , worldSteps  :: {-# UNPACK #-} !Int                  -- ^ Number of steps taken in the simulation so far
  , worldTime   :: {-# UNPACK #-} !Time                 -- ^ Current simulation time
  }


-- | Move bodies under the influence of acceleration
--
advanceBodies
    :: (Acc (Vector Body) -> Acc (Vector Accel))        -- ^ Function to compute accelerations at each point
    -> Acc (Scalar Time)                                -- ^ Time step
    -> Acc (Vector Body)                                -- ^ Bodies
    -> Acc (Vector Body)
advanceBodies calcAccels timeStep bodies
  = let
        -- Calculate the accelerations on each body.
        accels          = calcAccels bodies

        -- Apply the accelerations to the bodies and advance them
        advance b a     = let (ax, ay)  = unlift a              :: (Exp R, Exp R)
                              m         = massOfPointMass (pointMassOfBody b)
                              a'        = lift (ax * m, ay * m)
                          in advanceBody (the timeStep) (setAccelOfBody a' b)
    in
    A.zipWith advance bodies accels


-- | Advance a cluster of bodies forward in time
--
advanceWorld
    :: (Scalar Time -> Vector Body -> Vector Body)      -- ^ Function to update body positions
    -> Time
    -> World
    -> World
advanceWorld advance timeStep world
  = let
        -- Update the bodies
        bodies' = advance (fromList Z [timeStep]) (worldBodies world)

        -- Update the world
        steps'  = worldSteps world + 1
        time'   = worldTime  world + timeStep

    in  world   { worldBodies   = bodies'
                , worldSteps    = steps'
                , worldTime     = time' }

