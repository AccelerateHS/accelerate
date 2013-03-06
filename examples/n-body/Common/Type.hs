
module Common.Type where


-- Types -----------------------------------------------------------------------
-- We're using tuples instead of ADTs and defining Elt instances
--

-- | Not all compute devices support double precision
--
type R          = Float

-- | A data point in space
--
type Vec a      = (a, a, a)

-- | Units of time
--
type Time       = R

-- | The velocity of a point.
--
type Velocity   = Vec R

-- | The acceleration of a point.
--
type Accel      = Vec R

-- | A point in 2D space with its mass.
--
type Mass       = R
type Position   = Vec R
type PointMass  = (Position, Mass)

-- | Bodies consist of a Position and Mass, but also carry their velocity and
--   acceleration between steps of the simulation.
--
type Body       = (PointMass, Velocity, Accel)

