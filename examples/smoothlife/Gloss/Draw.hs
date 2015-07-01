{-# LANGUAGE FlexibleContexts #-}

module Gloss.Draw
  where

import Config

import Prelude                                  as P
import Data.Label
import Graphics.Gloss
import Graphics.Gloss.Accelerate.Data.Picture
import Data.Array.Accelerate                    as A hiding ( size )
import Data.Array.Accelerate.IO                 as A


colourise :: ColourScheme -> Acc (Matrix R) -> Acc (Matrix RGBA32)
colourise scheme = A.map (rgba32OfFloat . colour scheme)
  where
    phase       = 0.01
    alpha       = constant 1

    fract x     = x - A.fromIntegral (A.floor x :: Exp Int)
    mix x y a   = x*(1-a) + y*a

    colour RedBlack f    = lift (f, constant 0, constant 0, alpha)
    colour WhiteBlack f  = lift (f, f, f, alpha)
    colour BlackWhite f  = lift (x, x, x, alpha) where x = 1-f
    colour BrownGreen f  = lift (mix 0.5 0.5 f, mix 0.3 0.75 f, mix 0 1 f, alpha)
    colour GoldBrown f   =
      let ssf = sqrt (sqrt f)
      in  lift ( mix 0.5 (mix 1 0.3 f) ssf
               , mix 0.3 (mix 0.95 0.2 f) ssf
               , constant 0
               , alpha)
    colour Rainbow1 f    = rainbow  (fract phase * 6) f
    colour Rainbow2 f    = rainbow  (6 * sqrt (sqrt (1-f))) (sqrt (sqrt f))
    colour Rainbow3 f    = rainbow' (sqrt (sqrt (1-f))) (sqrt (sqrt f))

    rainbow p x
      = p >* 0 &&* p A.<* 1
                 ? (lift (x,          x*p,        constant 0, alpha)
      , p A.<* 2 ? (lift (x*(2-p),    x,          constant 0, alpha)
      , p A.<* 3 ? (lift (constant 0, x,          x*(p-2),    alpha)
      , p A.<* 4 ? (lift (constant 0, x*(4-p),    x,          alpha)
      , p A.<* 5 ? (lift (x*(p-4),    constant 0, x,          alpha)
      , p A.<* 6 ? (lift (x,          constant 0, x*(6-p),    alpha)
      , {-else-} (constant (0,0,0,0))))))))

    rainbow' p x
      = let c y = 0.5 * sin (1.7 * cos (pi * (p + y / 3.0 + phase))) + 0.5
        in lift (x * c 0, x * c 1, x * c 2, alpha)


draw :: Config -> Matrix RGBA32 -> Picture
draw conf arr = scale zoom zoom pic
  where
    zoom        = P.fromIntegral (get configWindowZoom conf)
    pic         = bitmapOfArray arr False

