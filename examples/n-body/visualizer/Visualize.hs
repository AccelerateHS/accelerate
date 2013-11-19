{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Visualize () where

import Prelude                                  as P
import Common.World
import Common.Body
import Common.Type

import qualified Solver.Naive2                  as Naive

import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.CUDA.Foreign

-- The cuda sample's representation of data.
--
-- Elements alternate between x, y, z and w with the left value being position and the right
-- velocity. For the left w is the mass. For the right it is undefined.
type ForeignBodies = Vector (Float, Float)

-- |One iteration of the algorithm.
stepBodies :: Acc (Scalar Time, ForeignBodies) -> Acc ForeignBodies
stepBodies ts = toForeign bodies'
  where
    (dt, fs) = A.unlift ts :: (Acc (Scalar Time), Acc ForeignBodies)

    bodies :: Acc (Vector Body)
    bodies = generateBodies fs

    bodies' :: Acc (Vector Body)
    bodies' = advanceBodies (Naive.calcAccels $ A.constant epsilon) dt bodies

    -- FIXME: Currently epsilon is constant because, due to constant folding, it gets better
    -- results. We should actually be recompiling the whole function everytime setSoftening() is
    -- called.
    epsilon :: Float
    epsilon = 0.1

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

-- Convert bodies to the foreign representation
--
-- NB: This is written in this way such that its argument will not fuse into it and duplicate work.
-- See issue <https://github.com/AccelerateHS/accelerate/issues/116> for further details.
toForeign :: Acc (Vector Body) -> Acc ForeignBodies
toForeign bodies = A.zip ps' vs'
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

-- Generate the exported version of stepBodies
exportAfun 'stepBodies "stepBodies_compile"

