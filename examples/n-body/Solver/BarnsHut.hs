
module Solver.BarnsHut
  where

import Common.Type
import Data.Array.Accelerate                    as A

-- | Calculate accelerations on the particles using the Barns-Hut algorithm
--
calcAccels :: Exp R -> Acc (Vector PointMass) -> Acc (Vector Accel)
calcAccels = error "BarnsHut.calcAccels: not implemented yet!"

