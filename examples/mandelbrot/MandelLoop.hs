{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
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

import qualified Prelude                                            as P
import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Data.Colour.RGBA
import Data.Array.Accelerate.Data.Complex

-- Types -----------------------------------------------------------------------

-- Current view into the complex plane
type View a             = (a, a, a, a)

-- Image data
type RGBA32             = Word32
type Bitmap             = Array DIM2 RGBA32

-- Action to render a frame
type Render a           = Scalar (View a) -> Bitmap


-- Mandelbrot Set --------------------------------------------------------------

-- Compute the mandelbrot as repeated application of the recurrence relation:
--
--   Z_{n+1} = c + Z_n^2
--
-- This returns the iteration depth 'i' at divergence.
--
mandelbrot
    :: forall a. (P.Floating a, RealFloat a, FromIntegral Int a)
    => Int
    -> Int
    -> Int
    -> Acc (Scalar (View a))
    -> Acc (Array DIM2 Int32)
mandelbrot screenX screenY depth view =
  generate (constant (Z:.screenY:.screenX))
           (\ix -> let c = initial ix
                   in  snd $ while (\zi -> snd zi < lIMIT && dot (fst zi) < 4)
                                   (\zi -> lift1 (next c) zi)
                                   (lift (c, constant 0)))
  where
    -- The view plane
    (xmin,ymin,xmax,ymax)     = unlift (the view)
    sizex                     = xmax - xmin
    sizey                     = ymax - ymin

    viewx                     = constant (P.fromIntegral screenX)
    viewy                     = constant (P.fromIntegral screenY)

    -- initial conditions for a given pixel in the window, translated to the
    -- corresponding point in the complex plane
    initial :: Exp DIM2 -> Exp (Complex a)
    initial ix = lift ( (xmin + (x * sizex) / viewx) :+ (ymin + (y * sizey) / viewy) )
      where
        pr = unindex2 ix
        x  = fromIntegral (snd pr :: Exp Int)
        y  = fromIntegral (fst pr :: Exp Int)

    -- take a single step of the iteration
    next :: Exp (Complex a) -> (Exp (Complex a), Exp Int32) -> (Exp (Complex a), Exp Int32)
    next c (z, i) = (c + (z * z), i+1)

    dot c = let r :+ i = unlift c
            in  r*r + i*i

    lIMIT = P.fromIntegral depth


-- Rendering -------------------------------------------------------------------

prettyRGBA :: Exp Int32 -> Exp Int32 -> Exp RGBA32
prettyRGBA cmax c = c == cmax ? ( 0xFF000000, escapeToColour (cmax - c) )

-- Directly convert the iteration count on escape to a colour. The base set
-- (x,y,z) yields a dark background with light highlights.
--
-- Note that OpenGL reads pixel data in AGBR format, rather than RGBA.
--
escapeToColour :: Exp Int32 -> Exp RGBA32
escapeToColour m = constant 0xFFFFFFFF - (packABGR8 . lift $ RGBA r g b 0)
  where
    r   = fromIntegral (3 * m)
    g   = fromIntegral (5 * m)
    b   = fromIntegral (7 * m)


{--
-- A simple colour scheme
--
prettyRGBA :: Elt a => Exp Int -> Exp (Complex a, Int) -> Exp RGBA32
prettyRGBA lIMIT s' = r + g + b + a
  where
    s   = snd s'
    t   = fromIntegral $ ((lIMIT - s) * 255) `quot` lIMIT
    r   = (t     `rem` 128 + 64) * 0x1000000
    g   = (t * 2 `rem` 128 + 64) * 0x10000
    b   = (t * 3 `rem` 256     ) * 0x100
    a   = 0xFF
--}
{--
prettyRGBA :: forall a. (Elt a, IsFloating a) => Exp Int -> Exp (Complex a, Int) -> Exp RGBA32
prettyRGBA lIMIT s =
  let cmax      = fromIntegral lIMIT          :: Exp a
      c         = fromIntegral (snd s)
  in
  c > 0.98 * cmax ? ( 0xFF000000, rampColourHotToCold 0 cmax c )

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
  = let v       = vmin `max` vNotNorm `min` vmax
        dv      = vmax - vmin
        --
        result  = v < vmin + 0.28 * dv
                ? ( lift ( constant 0.0
                         , 4 * (v-vmin) / dv
                         , constant 1.0
                         , constant 1.0 )

                , v < vmin + 0.5 * dv
                ? ( lift ( constant 0.0
                         , constant 1.0
                         , 1 + 4 * (vmin + 0.25 * dv - v) / dv
                         , constant 1.0 )

                , v < vmin + 0.75 * dv
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
