{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Visualize () where

import Prelude                                  as P
import Common.World
import Common.Body
import Common.Type

import qualified Solver.Naive                   as Naive

import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.CUDA.Foreign.Export

-- Gives access to the utility functions from the C code.
foreignAccModule

-- The cuda sample's representation of data.
-- ------------------------------------------

-- |An array of float4s with the actual position stored in the first 3 components,
--   and the mass in the last.
type Positions  = Vector Float
-- |An array of float4s with the last component undefined.
type Velocities = Vector Float

-- |One iteration of the algorithm.
stepBodies :: Acc (Scalar Time, Scalar R, Positions, Velocities) -> Acc (Positions, Velocities)
stepBodies ts = splitBodies (stepBodies' dt eps bodies)
  where
    (dt, eps, ps, vs) = A.unlift ts :: (Acc (Scalar Time), Acc (Scalar R), Acc Positions, Acc Velocities)

    bodies :: Acc (Vector Body)
    bodies = generateBodies $ lift (ps,vs)

    stepBodies' :: Acc (Scalar Time) -> Acc (Scalar R) -> Acc (Vector Body) -> Acc (Vector Body)
    stepBodies' dt epsilon = advanceBodies (Naive.calcAccels $ the epsilon) dt

generateBodies :: Acc (Positions, Velocities) -> Acc (Vector Body)
generateBodies ts = A.zip3 pms vs' as'
  where
    (ps, vs)           = A.unlift ts
    (xs, ys, zs, ms)   = A.unzip4 $ deinterleave4 ps
    pms                = A.zip (A.zip3 xs ys zs) ms
    (vxs, vys, vzs, _) = A.unzip4 $ deinterleave4 vs
    vs'                = A.zip3 vxs vys vzs
    as'                = A.fill (A.shape vs') (lift ((0,0,0) :: (Float, Float, Float)))

splitBodies :: Acc (Vector Body) -> Acc (Positions, Velocities)
splitBodies bodies = A.lift (ps', vs')
  where
    uncurry3 f (a,b,c) = f a b c

    (pms, vs, _)   = A.unzip3 bodies
    (ps, ms)       = A.unzip pms
    ps'            = uncurry3 interleave4 (A.unzip3 ps) ms
    vs'            = uncurry3 interleave4 (A.unzip3 vs) (A.fill (index1 (A.size bodies)) 0.0)


interleave4 :: Elt e => Acc (Vector e) -> Acc (Vector e) -> Acc (Vector e) -> Acc (Vector e) -> Acc (Vector e)
interleave4 a b c d = generate sh swizzle
  where
    sh          = index1 (4 * A.size a)
    swizzle ix  =
      let i = indexHead ix
          x = a A.!! (i `div` 4)
          y = b A.!! (i `div` 4)
          z = c A.!! (i `div` 4)
          w = d A.!! (i `div` 4)
          r = i `mod` 4
      in
      r ==* 0 ? (x,
      r ==* 1 ? (y,
      r ==* 2 ? (z,
                 w)))

deinterleave4 :: Elt e => Acc (Vector e) -> Acc (Vector (e,e,e,e))
deinterleave4 arr = generate (index1 $ A.size arr `div` 4) swizzle
  where
    swizzle ix = let i = indexHead ix * 4
                 in  lift (arr A.!! i, arr A.!! (i+1), arr A.!! (i+2), arr A.!! (i+3))

-- Generate the exported version of stepBodies
exportAfun1 'stepBodies