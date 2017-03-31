
module Solver.Naive2
  where

import Common.Type
import Common.Body

import Data.Array.Accelerate                    as A


-- | Calculate accelerations on these particles in a naïve O(n^2) way.
--
--   This maps a _sequential_ reduction to get the total contribution for this
--   body from all other bodies in the system.
--
calcAccels :: Exp R -> Acc (Vector PointMass) -> Acc (Vector Accel)
calcAccels epsilon bodies
  = let move body       = A.sfoldl (\acc next -> acc + accel epsilon body next)
                                   0
                                   (constant Z)
                                   bodies
    in
    A.map move bodies

