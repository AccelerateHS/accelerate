
module Solver.Naive1
  where

import Common.Type
import Common.Body

import Data.Array.Accelerate                    as A


-- | Calculate accelerations on these particles in a naïve O(n^2) way.
--
--   Replicate the bodies out by rows and columns, zip them together to get all
--   interactions, and then perform a parallel reduce along innermost dimension
--   to get the total contribution for each particle.
--
--   This relies on array fusion to combine the replicates into the body of the
--   reduction, otherwise we quickly exhaust the device memory.
--
calcAccels :: Exp R -> Acc (Vector PointMass) -> Acc (Vector Accel)
calcAccels epsilon bodies
  = let n       = A.size bodies

        cols    = A.replicate (lift $ Z :. n :. All) bodies
        rows    = A.replicate (lift $ Z :. All :. n) bodies

    in
    A.fold (+) 0 $ A.zipWith (accel epsilon) rows cols

