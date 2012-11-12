--
-- Simulation bodies with mass
--

module Common.Body (

  -- * Types
  Velocity, Accel, PointMass, Body,

  -- * Calculations
  accel, advanceBody,

  -- ** Getters
  pointMassOfBody, velocityOfBody, accelOfBody, positionOfPointMass,
  massOfPointMass,

  -- ** Setters
  unitBody, setMassOfBody, setAccelOfBody, setStartVelOfBody,

) where

import Common.Type
import Common.Util

import Data.Array.Accelerate            as A


-- Acceleration ----------------------------------------------------------------
--
-- | Calculate the acceleration on a point due to some other point as an inverse
--   separation-squared relation.
--
accel   :: Exp R                -- ^ Smoothing parameter
        -> Exp Body             -- ^ The point being accelerated
        -> Exp Body             -- ^ Neighbouring point
        -> Exp Accel

accel epsilon body1 body2
  = lift (aabs * dx / r , aabs * dy / r)
  where
    (x1, y1)    = unlift $ positionOfPointMass mp1
    (x2, y2)    = unlift $ positionOfPointMass mp2
    mp1         = pointMassOfBody body1
    mp2         = pointMassOfBody body2
    m1          = massOfPointMass mp1
    m2          = massOfPointMass mp2

    dx          = x1 - x2
    dy          = y1 - y2
    rsqr        = (dx * dx) + (dy * dy) + epsilon * epsilon
    aabs        = (m1 + m2) / rsqr
    r           = sqrt rsqr


-- Body ------------------------------------------------------------------------
--

-- | Make a stationary Body of unit mass
--
unitBody :: Exp R -> Exp R -> Exp Body
unitBody x y = lift (pointmass, constant (0,0), constant (0,0))
  where
    pos         = lift (x, y)                   :: Exp Position
    pointmass   = lift (pos, constant 1)        :: Exp PointMass


-- | Take the Velocity of a Body
--
velocityOfBody :: Exp Body -> Exp Velocity
velocityOfBody body = vel
  where
    (_, vel, _) = unlift body   :: (Exp PointMass, Exp Velocity, Exp Accel)


-- | Take the Acceleration of a Body
--
accelOfBody :: Exp Body -> Exp Accel
accelOfBody body = acc
  where
    (_, _, acc) = unlift body   :: (Exp PointMass, Exp Velocity, Exp Accel)


-- | Take the PointMass of a Body
--
pointMassOfBody :: Exp Body -> Exp PointMass
pointMassOfBody body = mp
  where
    (mp, _, _)  = unlift body   :: (Exp PointMass, Exp Velocity, Exp Accel)


-- | Take the position or mass of a PointMass
--
positionOfPointMass :: Exp PointMass -> Exp Position
positionOfPointMass = A.fst

massOfPointMass :: Exp PointMass -> Exp Mass
massOfPointMass = A.snd


-- | Set the mass of a Body.
--
setMassOfBody :: Exp Mass -> Exp Body -> Exp Body
setMassOfBody mass body = lift (pointmass, vel, acc)
  where
    vel         = velocityOfBody body
    acc         = accelOfBody body
    pos         = positionOfPointMass (pointMassOfBody body)
    pointmass   = lift (pos, mass)      :: Exp PointMass


-- | Set the acceleration of a Body.
--
setAccelOfBody :: Exp Accel -> Exp Body -> Exp Body
setAccelOfBody acc body = lift (pm, vel, acc)
  where
    pm          = pointMassOfBody body
    vel         = velocityOfBody body


-- | Set the starting velocity of a Body.
--   It is set to rotate around the origin, with the speed proportional
--   to the sqrt of the distance from it. This seems to make nice simulations.
--
setStartVelOfBody :: Exp R -> Exp Body -> Exp Body
setStartVelOfBody startVel body = lift (pm, vel'', acc)
  where
    pm          = pointMassOfBody body
    acc         = accelOfBody body
    pos         = positionOfPointMass pm

    pos'        = normaliseV pos
    vel'        = lift (y', -x')
    vel''       = mulSV (sqrt (magV pos) * startVel) vel'

    (x', y')    = unlift pos'   :: (Exp R, Exp R)


-- | Advance a body forwards in time.
--
advanceBody :: Exp Time -> Exp Body -> Exp Body
advanceBody time body = lift ( pm', vel', acc )
  where
    pm          = pointMassOfBody body
    mass        = massOfPointMass pm
    acc         = accelOfBody body
    (px, py)    = unlift $ positionOfPointMass pm
    (vx, vy)    = unlift $ velocityOfBody body
    (ax, ay)    = unlift acc

    pm'         = lift (pos', mass)                     :: Exp PointMass
    pos'        = lift (px + time * vx, py + time * vy) :: Exp Velocity
    vel'        = lift (vx + time * ax, vy + time * ay) :: Exp Accel

