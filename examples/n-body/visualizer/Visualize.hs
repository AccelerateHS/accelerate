{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Visualize () where

import Prelude                                  as P
import Common.World
import Common.Body
import Common.Type

import qualified Solver.Naive2                  as Naive

import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.CUDA.Foreign.Export

-- Gives access to the utility functions from the C code.
foreignAccModule

-- The cuda sample's representation of data.
--
-- Elements alternate between x, y, z and w with the left value being position and the right
-- velocity. For the left w is the mass. For the right it is undefined.
type ForeignBodies = Vector (Float, Float)

-- |One iteration of the algorithm.
stepBodies :: Acc (Scalar Time, Scalar R, ForeignBodies) -> Acc ForeignBodies
stepBodies ts = toForeign (stepBodies' dt eps bodies)
  where
    (dt, eps, fs) = A.unlift ts :: (Acc (Scalar Time), Acc (Scalar R), Acc ForeignBodies)

    bodies :: Acc (Vector Body)
    bodies = generateBodies fs

    stepBodies' :: Acc (Scalar Time) -> Acc (Scalar R) -> Acc (Vector Body) -> Acc (Vector Body)
    stepBodies' dt epsilon = advanceBodies (Naive.calcAccels $ the epsilon) dt

generateBodies :: Acc ForeignBodies -> Acc (Vector Body)
generateBodies fs = A.generate (A.index1 (A.size fs `div` 4)) swizzle
  where
    swizzle :: Exp DIM1 -> Exp Body
    swizzle ix =
      let i  = indexHead ix * 4
          (p_x, v_x) = unlift (fs A.!! i) :: (Exp Float, Exp Float)
          (p_y, v_y) = unlift (fs A.!! (i + 1)) :: (Exp Float, Exp Float)
          (p_z, v_z) = unlift (fs A.!! (i + 2)) :: (Exp Float, Exp Float)
          (m  , _)   = unlift (fs A.!! (i + 3)) :: (Exp Float, Exp Float)
          p          = lift (p_x, p_y, p_z) :: Exp Position
          v          = lift (v_x, v_y, v_z) :: Exp Velocity
          pm         = lift (p, m) :: Exp PointMass
          a          = lift (constant 0.0, constant 0.0, constant 0.0) :: Exp (Float, Float, Float)
      in lift (pm, v, a)

toForeign :: Acc (Vector Body) -> Acc ForeignBodies
toForeign bodies = fs
  where
    sh = A.index1 (4 * A.size bodies)
    fs = A.generate sh swizzle
      where
        swizzle :: Exp DIM1 -> Exp (Float, Float)
        swizzle ix =
          let k = indexHead ix
              i = k `div` 4
              (pm, v, _) = unlift (bodies A.!! i) :: (Exp PointMass, Exp Velocity, Exp Accel)
              (p, m)     = unlift pm :: (Exp Position, Exp Mass)
              (p_x,p_y,p_z) = unlift p :: (Exp Float, Exp Float, Exp Float)
              (v_x,v_y,v_z) = unlift v :: (Exp Float, Exp Float, Exp Float)
              r = k `mod` 4
          in
          r ==* 0 ? (lift (p_x,v_x),
          r ==* 1 ? (lift (p_y,v_y),
          r ==* 2 ? (lift (p_z,v_z),
                     lift (m,A.constant 0.0))))

-- Generate the exported version of stepBodies
exportAfun 'stepBodies "stepBodies_compile"

