{-# LANGUAGE ViewPatterns #-}
-- Example of the slit-scan effect.
-- You must provide your own Doctor Who theme music.
--
-- Based on code from:
--    http://roy.red/slitscan-.html
--

module Main where

import Config

import Data.Label

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Examples.Internal                      as A

import Data.Array.Accelerate.Linear.Metric
import Data.Array.Accelerate.Linear.Vector
import Data.Array.Accelerate.Linear.V2
import Data.Array.Accelerate.Linear.V3

import Data.Array.Accelerate.Control.Lens                           ( (^.) )

import Graphics.Gloss.Accelerate.Raster.Field                       hiding ( clamp )

import Prelude                                                      ( IO, fmap )


-- Fractional part of a number
--
fract :: Exp Float -> Exp Float
fract x = x - toFloating (A.floor x :: Exp Int)

clamp :: Exp Float -> Exp Float -> Exp Float -> Exp Float
clamp v inf sup = inf `A.max` v `A.min` sup

-- Interpolation using Hermite polynomial after clamping values to a range.
--
smoothstep :: Exp Float -> Exp Float -> Exp Float -> Exp Float
smoothstep edge0 edge1 x =
  let t = clamp ((x-edge0) / (edge1-edge0)) 0.0 1.0
  in  t*t*(3 - 2*t)

rand2 :: Exp (V2 Float) -> Exp (V2 Float)
rand2 p =
  let x = constant (V2 127.1 311.7)
      y = constant (V2 269.5 183.3)
      q = lift $ V2 (dot p x) (dot p y)
  in
  lift1 (fmap fract :: V2 (Exp Float) -> V2 (Exp Float))
    $ sin(q) * constant 43758.5453

rand1 :: Exp (V2 Float) -> Exp Float
rand1 p =
  let z = constant (V2 419.2 371.9)
  in  fract (sin (dot p z) * 833458.57832)


-- Procedural pattern generation that generalise cell-noise, perlin-noise, and
-- voronoi tessellation.
--
--   http://iquilezles.org/www/articles/voronoise/voronoise.htm
--   https://www.shadertoy.com/view/Xd23Dh
--
voronoise :: Exp (V2 Float) -> Exp Float -> Exp Float -> Exp Float
voronoise xy irregular smoothness =
  let
      cell        = lift1 (fmap A.toFloating :: V2 (Exp Int) -> V2 (Exp Float))
                  $ lift1 (fmap A.floor :: V2 (Exp Float) -> V2 (Exp Int))   xy
      cellOffset  = lift1 (fmap fract   :: V2 (Exp Float) -> V2 (Exp Float)) xy
      sharpness   = 1.0 + 63.0 * ((1.0-smoothness) ** 4.0)

      -- -- Sample the surrounding cells from [-2..2].
      -- samples = P.foldr1 (+) [ sample i j | i <- [-2..2], j <- [-2..2] ]
      samples =
        iterFromTo (-2) 2 0 $ \i x ->
        iterFromTo (-2) 2 x $ \j y ->
          y + sample i j

      sample :: Exp Int -> Exp Int -> Exp (V2 Float)
      sample i j =
        let
            samplePos = lift $ V2 (A.fromIntegral i) (A.fromIntegral j)

            -- Centre of the cell is not in the centre of the block for
            -- irregular noise. All coordinates are in block-space coordinates;
            -- 0 is the current block, 1 is one block over, etc...
            centre      = rand2 (cell + samplePos) ^* irregular
            centreDist  = norm (samplePos - cellOffset + centre)

            -- High sharpness = only extreme values = hard borders = 64
            -- Low sharpness  = no extreme values   = soft borders = 1
            det         = (1.0 - smoothstep 0.0 1.414 centreDist) ** sharpness

            -- A different "colour" (shade of grey) for each cell
            colour      = rand1 (cell + samplePos)
        in
        lift $ V2 (colour * det) det
  in
  samples^._x / samples^._y


iterFromTo :: Elt a => Exp Int -> Exp Int -> Exp a -> (Exp Int -> Exp a -> Exp a) -> Exp a
iterFromTo inf sup x body
  = A.snd
  $ A.while (\ix -> A.fst ix <= sup)
            (\ix -> A.lift (A.fst ix + 1, A.uncurry body ix))
            (lift (inf, x))

v2OfPoint :: Exp Point -> Exp (V2 Float)
v2OfPoint p =
  let (x,y) = xyOfPoint p
  in  lift (V2 x y)

-- The time vortex
--
tunnel :: Exp Float -> Exp Point -> Exp Colour
tunnel time (v2OfPoint -> pt1) =
  let
      rInv      = 1.0 / norm pt2
      pt2       = 1.2 * pt1
      pt3       = pt2 ^* rInv - lift (V2 (rInv + 2.0 * mod' time 6000.0) 0.0)
      c1        = constant (V3 0.659 0.772 1.000) -- slate-blue-ish
      c2        = c1 ^* ( (voronoise (5.0*pt3) 1.0 1.0) + 0.240*rInv )
      --
      V3 r g b  = unlift c2
  in
  rgba r g b 1.0


main :: IO ()
main = do
  beginMonitoring
  (conf, opts, rest)    <- parseArgs options defaults header footer

  let width     = get configWidth conf
      height    = get configHeight conf
      zoom      = get configZoom conf
      backend   = get optBackend opts

      frame     = run1 backend
                $ makeField width height (\time -> tunnel (the time))

  runBenchmarks opts rest
    [ bench "tunnel" $ whnf frame (A.fromList Z [1.0]) ]

  runInteractive opts rest
    $ animateFieldWith
          (run1 backend)
          (InWindow "Tunnel" (width * zoom, height * zoom) (10, 10))
          (zoom, zoom)
          tunnel

