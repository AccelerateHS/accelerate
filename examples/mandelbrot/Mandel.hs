{-# LANGUAGE ScopedTypeVariables #-}
--
-- A Mandelbrot set generator.
-- Originally submitted by Simon Marlow as part of Issue #49.
--
module Mandel (

  -- Types
  View, Render, Bitmap,

  -- Pretty pictures
  mandelbrot, prettyRGBA,

) where

import Prelude                                  as P
import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.IO                 as A

-- Types -----------------------------------------------------------------------

-- Current view into the complex plane
type View a             = (a, a, a, a)

-- Complex numbers
type Complex a          = (a, a)
type ComplexPlane a     = Array DIM2 (Complex a)

-- Image data
type Bitmap             = Array DIM2 RGBA32

-- Action to render a frame
type Render a           = Scalar (View a) -> Bitmap


-- Mandelbrot Set --------------------------------------------------------------

-- Compute the mandelbrot as repeated application of the recurrence relation:
--
--   Z_{n+1} = c + Z_n^2
--
-- This is applied until the iteration limit is reached, regardless of whether
-- or not the pixel has diverged. This implies wasted work, but no thread
-- divergence.
--
mandelbrot
    :: forall a. (Elt a, IsFloating a)
    => Int
    -> Int
    -> Int
    -> Acc (Scalar (View a))
    -> Acc (Array DIM2 (Complex a,Int))
mandelbrot screenX screenY depth view
  = foldr ($) zs0 (P.take depth (repeat go))
  where
    cs  = genPlane screenX screenY view
    zs0 = mkinit cs

    go :: Acc (Array DIM2 (Complex a, Int)) -> Acc (Array DIM2 (Complex a, Int))
    go = A.zipWith iter cs


genPlane :: (Elt a, IsFloating a) => Int -> Int -> Acc (Scalar (View a)) -> Acc (ComplexPlane a)
genPlane screenX screenY view
  = generate (constant (Z:.screenY:.screenX))
             (\ix -> let pr = unindex2 ix
                         x = A.fromIntegral (A.snd pr :: Exp Int)
                         y = A.fromIntegral (A.fst pr :: Exp Int)
                     in
                       lift ( xmin + (x * sizex) / viewx
                            , ymin + (y * sizey) / viewy))
  where
    (xmin,ymin,xmax,ymax) = unlift (the view)

    sizex = xmax - xmin
    sizey = ymax - ymin

    viewx = constant (P.fromIntegral screenX)
    viewy = constant (P.fromIntegral screenY)


next :: (Elt a, IsNum a) => Exp (Complex a) -> Exp (Complex a) -> Exp (Complex a)
next c z = c `plus` (z `times` z)


plus :: (Elt a, IsNum a) => Exp (Complex a) -> Exp (Complex a) -> Exp (Complex a)
plus = lift2 f
  where f :: (Elt a, IsNum a) => (Exp a, Exp a) -> (Exp a, Exp a) -> (Exp a, Exp a)
        f (x1,y1) (x2,y2) = (x1+x2, y1+y2)

times :: forall a. (Elt a, IsNum a) => Exp (Complex a) -> Exp (Complex a) -> Exp (Complex a)
times = lift2 f
  where f :: (Elt a, IsNum a) => (Exp a, Exp a) -> (Exp a, Exp a) -> (Exp a, Exp a)
        f (x,y) (x',y')   =  (x*x'-y*y', x*y'+y*x')

dot :: forall a. (Elt a, IsNum a) => Exp (Complex a) -> Exp a
dot = lift1 f
  where f :: (Elt a, IsNum a) => (Exp a, Exp a) -> Exp a
        f (x,y) = x*x + y*y


iter :: forall a. (Elt a, IsNum a) => Exp (Complex a) -> Exp (Complex a, Int) -> Exp (Complex a, Int)
iter c zi = f (A.fst zi) (A.snd zi)
 where
  f :: Exp (Complex a) -> Exp Int -> Exp (Complex a, Int)
  f z i =
    let z' = next c z
    in (dot z' >* 4) ? ( zi , lift (z', i+1) )


mkinit :: Elt a => Acc (ComplexPlane a) -> Acc (Array DIM2 (Complex a, Int))
mkinit cs = A.zip cs (A.fill (A.shape cs) 0)


-- Rendering -------------------------------------------------------------------

prettyRGBA :: forall a. (Elt a, IsFloating a) => Exp Int -> Exp (Complex a, Int) -> Exp RGBA32
prettyRGBA lIMIT s =
  let cmax      = A.fromIntegral lIMIT
      c         = A.fromIntegral (A.snd s)
  in
  c ==* cmax ? ( 0xFF000000, escapeToColour (cmax - c) )

-- Directly convert the iteration count on escape to a colour. The base set
-- (x,y,z) yields a dark background with light highlights.
--
escapeToColour :: Exp Int -> Exp RGBA32
escapeToColour m = constant 0xFFFFFFFF - (packRGBA32 $ lift (x,y,z,w))
  where
    w   = constant 0
    x   = A.fromIntegral (3 * m)
    y   = A.fromIntegral (5 * m)
    z   = A.fromIntegral (7 * m)


{--
-- A simple colour scheme
--
prettyRGBA :: Elt a => Exp Int -> Exp (Complex a, Int) -> Exp RGBA32
prettyRGBA lIMIT s' = r + g + b + a
  where
    s   = A.snd s'
    t   = A.fromIntegral $ ((lIMIT - s) * 255) `quot` lIMIT
    r   = (t     `rem` 128 + 64) * 0x1000000
    g   = (t * 2 `rem` 128 + 64) * 0x10000
    b   = (t * 3 `rem` 256     ) * 0x100
    a   = 0xFF
--}
{--
prettyRGBA :: forall a. (Elt a, IsFloating a) => Exp Int -> Exp (Complex a, Int) -> Exp RGBA32
prettyRGBA lIMIT s =
  let cmax      = A.fromIntegral lIMIT          :: Exp a
      c         = A.fromIntegral (A.snd s)
  in
  c >* 0.98 * cmax ? ( 0xFF000000, rampColourHotToCold 0 cmax c )

-- Standard Hot-to-Cold hypsometric colour ramp. Colour sequence is
--   Red, Yellow, Green, Cyan, Blue
--
rampColourHotToCold
    :: (Elt a, IsFloating a)
    => Exp a                            -- ^ minimum value of the range
    -> Exp a                            -- ^ maximum value of the range
    -> Exp a                            -- ^ data value
    -> Exp RGBA32
rampColourHotToCold vmin vmax vNotNorm
  = let v       = vmin `A.max` vNotNorm `A.min` vmax
        dv      = vmax - vmin
        --
        result  = v <* vmin + 0.28 * dv
                ? ( lift ( constant 0.0
                         , 4 * (v-vmin) / dv
                         , constant 1.0
                         , constant 1.0 )

                , v <* vmin + 0.5 * dv
                ? ( lift ( constant 0.0
                         , constant 1.0
                         , 1 + 4 * (vmin + 0.25 * dv - v) / dv
                         , constant 1.0 )

                , v <* vmin + 0.75 * dv
                ? ( lift ( 4 * (v - vmin - 0.5 * dv) / dv
                         , constant 1.0
                         , constant 0.0
                         , constant 1.0 )

                ,   lift ( constant 1.0
                         , 1 + 4 * (vmin + 0.75 * dv - v) / dv
                         , constant 0.0
                         , constant 1.0 )
                )))
    in
    rgba32OfFloat result
--}
