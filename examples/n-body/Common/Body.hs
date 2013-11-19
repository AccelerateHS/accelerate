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
--   separation-squared relation. The force on a body i caused by its
--   gravitational attraction to a body j is given by:
--
--                m_i m_j     r_ij
--      f_ij = G --------- . ------
--                |r_ij|^2   |r_ij|
--
--   The total force on a body F is given by this interaction to all other
--   bodies:
--
--      F_i = Sum_{i/=j} f_ij
--
--                               m_j r_ij
--          = G m_i . Sum_{j/=i} --------
--                                |r_ij|^3
--
--   As the bodies approach each other, the force between them grows without
--   bound, which is an undesirable situation for numerical integration. Since
--   collisions between bodies are precluded, a softening factor (epsilon^2 > 0)
--   is added:
--
--                                  m_j r_ij
--      F_i = G m_i . Sum -----------------------------
--                        ( |r_ij|^2 + epsilon^2) ^ 3/2
--
--   Note that the condition (i /= j) is no longer required, because (f_ii = 0)
--   when (epsilon^2 > 0). The softening factor models the interaction between
--   two Plummer point masses: bodies that behave as if they were spherical
--   galaxies (and thus may pass through each other).
--
--   To integrate over time, we need the acceleration (a_i = F_i / m_i), and so
--   the above can be simplified by removing m_i from the RHS. This function
--   computes the component of the Sum for two bodies i and j.
--
accel   :: Exp R                -- ^ Smoothing parameter
        -> Exp PointMass        -- ^ The point being accelerated
        -> Exp PointMass        -- ^ Neighbouring point
        -> Exp Accel

accel epsilon pmi pmj = s *. r
  where
    mj          = massOfPointMass pmj

    r           = positionOfPointMass pmj .-. positionOfPointMass pmi
    rsqr        = dot r r + epsilon * epsilon
    invr        = 1 / sqrt rsqr
    invr3       = invr * invr * invr

    s           = mj * invr3


-- Body ------------------------------------------------------------------------
--

-- | Make a stationary Body of unit mass
--
unitBody :: Exp (Vec R) -> Exp Body
unitBody pos = lift (pointmass, vec 0, vec 0)
  where
    pointmass = lift (pos, constant 1)          :: Exp PointMass


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

    pos'        = normalise pos
    vel'        = lift (y', -x', z')
    vel''       = (sqrt (magnitude pos) * startVel) *. vel'

    (x',y',z')  = unlift pos'   :: Vec (Exp R)


-- | Advance a body forwards in time.
--
advanceBody :: Exp Time -> Exp Body -> Exp Body
advanceBody time body = lift ( pm', vel', acc )
  where
    pm          = pointMassOfBody body
    pos         = positionOfPointMass pm
    vel         = velocityOfBody body
    acc         = accelOfBody body
    mass        = massOfPointMass pm

    pm'         = lift (pos', mass)             :: Exp PointMass
    pos'        = pos .+. time *. vel
    vel'        = vel .+. time *. acc

