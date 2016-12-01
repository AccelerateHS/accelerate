{-# LANGUAGE FlexibleContexts #-}

module Gloss.Draw
  where

import Config

import qualified Prelude                                            as P
import Data.Label

import Graphics.Gloss                                               ( Picture, scale )
import Graphics.Gloss.Accelerate.Data.Picture

import Data.Array.Accelerate                                        as A hiding ( size )
import Data.Array.Accelerate.Data.Colour.RGB
import Data.Array.Accelerate.Data.Colour.Names


colourise :: ColourScheme -> Acc (Matrix R) -> Acc (Matrix RGBA32)
colourise scheme = A.map (packRGB . colour scheme)
  where
    phase       = 0.01
    fract x     = x - A.fromIntegral (A.floor x :: Exp Int)
    mix x y a   = x*(1-a) + y*a

    colour :: ColourScheme -> Exp Float -> Exp Colour
    colour RedBlack f    = rgb f 0 0
    colour WhiteBlack f  = rgb f f f
    colour BlackWhite f  = rgb x x x where x = 1-f
    colour BrownGreen f  = rgb (mix 0.5 0.5 f) (mix 0.3 0.75 f) (mix 0 1 f)
    colour GoldBrown f   =
      let ssf = sqrt (sqrt f)
      in  rgb ( mix 0.5 (mix 1 0.3 f) ssf )
              ( mix 0.3 (mix 0.95 0.2 f) ssf )
              0
    colour Rainbow1 f    = rainbow  (fract phase * 6) f
    colour Rainbow2 f    = rainbow  (6 * sqrt (sqrt (1-f))) (sqrt (sqrt f))
    colour Rainbow3 f    = rainbow' (sqrt (sqrt (1-f))) (sqrt (sqrt f))

    rainbow :: Exp Float -> Exp Float -> Exp Colour
    rainbow p x
      = p > 0 && p < 1
              ? (rgb x         (x*p)     0
      , p < 2 ? (rgb (x*(2-p)) x         0
      , p < 3 ? (rgb 0         x         (x*(p-2))
      , p < 4 ? (rgb 0         (x*(4-p)) x
      , p < 5 ? (rgb (x*(p-4)) 0         x
      , p < 6 ? (rgb x         0         (x*(6-p))
      , {-else-}   (constant black)))))))

    rainbow' :: Exp Float -> Exp Float -> Exp Colour
    rainbow' p x
      = let c y = 0.5 * sin (1.7 * cos (pi * (p + y / 3.0 + phase))) + 0.5
        in rgb (x * c 0) (x * c 1) (x * c 2)


draw :: Config -> Matrix RGBA32 -> Picture
draw conf arr = scale zoom zoom pic
  where
    zoom        = P.fromIntegral (get configWindowZoom conf)
    pic         = bitmapOfArray arr False

