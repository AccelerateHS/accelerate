{-# LANGUAGE ViewPatterns #-}
-- An implementation of the Canny edge detection algorithm
--
--   J. F. Canny, "A Computational Approach to Edge Detection" in _Pattern
--   Analysis and Machine Intelligence_, (6), 1986.
--
-- This module defines the first seven data-parallel sections of the algorithm.
-- The last phase, which uses a recursive algorithm to "connect" pixels that
-- form the output image, is defined in the module Wildfire and implemented
-- using Repa.
--

module Canny where

import Prelude                                          as P

import Data.Array.Accelerate                            as A
import Data.Array.Accelerate.IO                         as A


-- Canny algorithm -------------------------------------------------------------

canny :: Float -> Float -> Acc (Image RGBA) -> (Acc (Image Float), Acc (Vector Int))
canny (constant -> low) (constant -> high)
  = stage1
  . nonMaximumSuppression low high
  . gradientMagDir low
  . gaussianY
  . gaussianX
  . toGreyscale
  where
    stage1 x = (x, selectStrong x)


-- Accelerate component --------------------------------------------------------

type RGBA               = Word32
type Image a            = Array DIM2 a

type Stencil5x1 a       = (Stencil3 a, Stencil5 a, Stencil3 a)
type Stencil1x5 a       = (Stencil3 a, Stencil3 a, Stencil3 a, Stencil3 a, Stencil3 a)

-- Classification of the output pixel
data Orient     = Undef | PosD | Vert | NegD | Horiz
data Edge       = None  | Weak | Strong

orient :: Orient -> Int
orient Undef    = 0
orient PosD     = 64
orient Vert     = 128
orient NegD     = 192
orient Horiz    = 255

orient' :: Orient -> Exp Int
orient' = constant . orient

edge :: Edge -> Float
edge None       = 0
edge Weak       = 0.5
edge Strong     = 1.0

edge' :: Edge -> Exp Float
edge' = constant . edge

convolve5x1 :: (Elt a, IsNum a) => [Exp a] -> Stencil5x1 a -> Exp a
convolve5x1 kernel (_, (a,b,c,d,e), _)
  = P.sum $ P.zipWith (*) kernel [a,b,c,d,e]

convolve1x5 :: (Elt a, IsNum a) => [Exp a] -> Stencil1x5 a -> Exp a
convolve1x5 kernel ((_,a,_), (_,b,_), (_,c,_), (_,d,_), (_,e,_))
  = P.sum $ P.zipWith (*) kernel [a,b,c,d,e]


-- RGB to Greyscale conversion, in the range [0,255]
--
toGreyscale :: Acc (Image RGBA) -> Acc (Image Float)
toGreyscale = A.map (\rgba -> 255 * luminanceOfRGBA32 rgba)


-- Separable Gaussian blur in the x- and y-directions
--
gaussianX :: Acc (Image Float) -> Acc (Image Float)
gaussianX = stencil (convolve5x1 gaussian) Clamp
  where
    gaussian = [ 1, 4, 6, 4, 1 ]

gaussianY :: Acc (Image Float) -> Acc (Image Float)
gaussianY = stencil (convolve1x5 gaussian) Clamp
  where
    gaussian = P.map (/256) [ 1, 4, 6, 4, 1 ]


-- Gradients in the x- and y- directions
--
gradientX :: Acc (Image Float) -> Acc (Image Float)
gradientX = stencil grad Clamp
  where
    grad :: Stencil3x3 Float -> Exp Float
    grad ((u, _, x)
         ,(v, _, y)
         ,(w, _, z)) = x + (2*y) + z - u - (2*v) - w

gradientY :: Acc (Image Float) -> Acc (Image Float)
gradientY = stencil grad Clamp
  where
    grad :: Stencil3x3 Float -> Exp Float
    grad ((x, y, z)
         ,(_, _, _)
         ,(u, v, w)) = x + (2*y) + z - u - (2*v) - w


-- Classify the magnitude and orientation of the image gradient.
--
-- Because accelerate supports generalised stencil functions, not just
-- convolutions, we can combine the x- and y- sobel operators and save some
-- memory bandwidth.
--
gradientMagDir
    :: Exp Float
    -> Acc (Image Float)
    -> Acc (Array DIM2 (Float,Int))
gradientMagDir low = stencil magdir Clamp
  where
    magdir :: Stencil3x3 Float -> Exp (Float,Int)
    magdir ((v0, v1, v2)
           ,(v3,  _, v4)
           ,(v5, v6, v7)) =
      let
          -- Image gradients
          dx          = v2 + (2*v4) + v7 - v0 - (2*v3) - v5
          dy          = v0 + (2*v1) + v2 - v5 - (2*v6) - v7

          -- Magnitude
          mag         = sqrt (dx * dx + dy * dy)

          -- Direction
          --
          -- Determine the angle of the vector and rotate it around a bit to
          -- make the segments easier to classify
          theta       = atan2 dy dx
          alpha       = (theta - (pi/8)) * (4/pi)

          -- Normalise the angle to between [0..8)
          norm        = alpha + 8 * A.fromIntegral (boolToInt (alpha <=* 0))

          -- Try to avoid doing explicit tests, to avoid warp divergence
          undef       = abs dx <=* low &&* abs dy <=* low
          dir         = boolToInt (A.not undef) * ((64 * (1 + A.floor norm `mod` 4)) `min` 255)
      in
      lift (mag, dir)


-- Non-maximum suppression classifies pixels that are the local maximum along
-- the direction of the image gradient as either strong or weak edges. All other
-- pixels are not considered edges at all.
--
-- The image intensity is in the range [0,1]
--
nonMaximumSuppression
  :: Exp Float
  -> Exp Float
  -> Acc (Image (Float,Int))
  -> Acc (Image Float)
nonMaximumSuppression low high magdir =
  generate (shape magdir) $ \ix ->
    let -- The input parameters
        --
        (mag, dir)      = unlift (magdir ! ix)
        Z :. h :. w     = unlift (shape magdir)
        Z :. y :. x     = unlift ix

        -- Determine the points that lie either side of this point along to the
        -- direction of the image gradient.
        --
        -- The direction coding:
        --
        --   192   128   64
        --          |
        --   255 --- ---
        --
        offsetx         = dir >* orient' Vert  ? (-1, dir <* orient' Vert ? (1, 0))
        offsety         = dir <* orient' Horiz ? (-1, 0)

        (fwd, _)        = unlift $ magdir ! lift (clamp (Z :. y+offsety :. x+offsetx)) :: (Exp Float, Exp Int)
        (rev, _)        = unlift $ magdir ! lift (clamp (Z :. y-offsety :. x-offsetx)) :: (Exp Float, Exp Int)

        clamp (Z:.u:.v) = Z :. 0 `max` u `min` (h-1) :. 0 `max` v `min` (w-1)

        -- Try to avoid doing explicit tests to avoid warp divergence.
        --
        none            = dir ==* orient' Undef ||* mag <* low ||* mag <* fwd ||* mag <* rev
        strong          = mag >=* high
    in
    A.fromIntegral (boolToInt (A.not none) * (1 + boolToInt strong)) * 0.5


-- Extract the linear indices of the strong edges
--
selectStrong
  :: Acc (Image Float)
  -> Acc (Array DIM1 Int)
selectStrong img =
  let strong            = A.map (\x -> boolToInt (x ==* edge' Strong)) (flatten img)
      (targetIdx, len)  = A.scanl' (+) 0 strong
      indices           = A.enumFromN (index1 $ size img) 0
      zeros             = A.fill (index1 $ the len) 0
  in
  A.permute const zeros (\ix -> strong!ix ==* 0 ? (ignore, index1 $ targetIdx!ix)) indices

