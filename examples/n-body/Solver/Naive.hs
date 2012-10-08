
module Solver.Naive
  where

import Common.Type
import Common.Body
import Common.Util

import Data.Array.Accelerate                    as A


-- | Calculate accelerations on these particles in a naïve O(n^2) way
--
calcAccels :: Exp R -> Acc (Vector PointMass) -> Acc (Vector Accel)
calcAccels epsilon particles
  = let n       = A.size particles

        cols    = A.replicate (lift $ Z :. n :. All) particles
        rows    = A.replicate (lift $ Z :. All :. n) particles

    in
    A.fold plusV (constant (0,0)) $ A.zipWith (accel epsilon) rows cols

