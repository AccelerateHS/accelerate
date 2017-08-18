{-# LANGUAGE FlexibleContexts #-}
-- Quasicrystals demo.
--
-- Based on code from:
--   http://hackage.haskell.org/package/repa-examples
--   http://mainisusuallyafunction.blogspot.com/2011/10/quasicrystals-as-sums-of-waves-in-plane.html
--

module Main where

import Prelude                                          as P
import Config

import Data.Label                                       ( get )

import Data.Array.Accelerate                            as A hiding ( (==), size, wrap )
import Data.Array.Accelerate.Examples.Internal          as A
import Data.Array.Accelerate.Data.Colour.RGBA

import Graphics.Gloss.Accelerate.Raster.Field           as G


-- Types ----------------------------------------------------------------------
-- | Angle in radians.
type Angle  = Float

-- | Angle offset used for animation.
type Phi    = Float

-- | Number of waves to sum for each pixel.
type Degree = Int

-- | Feature size of visualisation.
type Scale  = Float

-- | Time in seconds since the program started.
type Time   = Float


-- Point ----------------------------------------------------------------------
-- | Compute a single point of the visualisation.
quasicrystal :: Scale -> Degree -> Exp Time -> Exp Point -> Exp Colour
quasicrystal scale degree time p
  = let -- Scale the time to be the phi value of the animation.
        -- The action seems to slow down at increasing phi values, so we
        -- increase phi faster as time moves on.
        phi     = 1 + (time ** 1.5) * 0.005

    in  rampColour
          $ waves degree phi
          $ point scale p


-- | Sum up all the waves at a particular point.
waves :: Degree -> Exp Phi -> Exp Point -> Exp Float
waves degree phi x = wrap $ waver degree 0
  where
    th = pi / phi

    waver :: Int -> Exp Float -> Exp Float
    waver n acc
      | n == 0    = acc
      | otherwise = waver (n - 1) (acc + wave (A.constant (P.fromIntegral n) * th) x)

    wrap :: Exp Float -> Exp Float
    wrap n
      = let n_  = A.truncate n :: Exp Int
            n'  = n - A.fromIntegral n_
        in
        A.odd n_ A.? (1-n', n')

-- | Generate the value for a single wave.
wave :: Exp Angle -> Exp Point -> Exp Float
wave th pt = (cos (cth*x + sth*y) + 1) / 2
  where
    (x, y)      = xyOfPoint pt
    cth         = cos th
    sth         = sin th

-- | Convert an image point to a point on our wave plane.
point :: Scale -> Exp Point -> Exp Point
point scale pt = makePoint (x * scale') (y * scale')
  where
    (x, y)      = xyOfPoint pt
    scale'      = A.constant scale

-- | Colour ramp from red to white, convert into RGBA
rampColour :: Exp Float -> Exp Colour
rampColour v = lift $ RGBA 1 (0.4 + (v * 0.6)) v 1


-- Main -----------------------------------------------------------------------
main :: IO ()
main = do

  beginMonitoring
  (conf, opts, rest)    <- parseArgs options defaults header footer

  let size      = get configSize conf
      zoom      = get configZoom conf
      scale     = get configScale conf
      degree    = get configDegree conf
      backend   = get optBackend opts

      frame     = run1 backend
                $ makeField size size (\time -> quasicrystal scale degree (the time))

  runBenchmarks opts rest
    [ bench "crystal" $ whnf frame (A.fromList Z [1.0]) ]

  runInteractive opts rest
    $ G.animateFieldWith
          (run1 backend)
          (InWindow "Quasicrystals" (size * zoom, size * zoom) (10, 10))
          (zoom, zoom)
          (quasicrystal scale degree)

