
module Common.Type (

  V3(..),
  module Common.Type,

) where

import Data.Array.Accelerate.Linear.V3


-- | Not all compute devices support double precision
--
type R          = Float

-- | Units of time
--
type Time       = R

-- | The velocity of a point.
--
type Velocity   = V3 R

-- | The acceleration of a point.
--
type Accel      = V3 R

-- | A point in 2D space with its mass.
--
type Mass       = R
type Position   = V3 R
type PointMass  = (Position, Mass)

-- | Bodies consist of a Position and Mass, but also carry their velocity and
--   acceleration between steps of the simulation.
--
type Body       = (PointMass, Velocity, Accel)

