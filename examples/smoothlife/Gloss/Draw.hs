
module Gloss.Draw
  where

import Config

import Prelude                                  as P
import Data.Label
import Graphics.Gloss                           hiding ( color )
import Data.Array.Accelerate                    as A hiding ( size )
import Data.Array.Accelerate.IO                 as A
import Foreign.Ptr
import Foreign.ForeignPtr
import System.IO.Unsafe

import Data.Array.Accelerate.Array.Data         ( ptrsOfArrayData )
import Data.Array.Accelerate.Array.Sugar        ( Array(..) )


data Scheme = RedBlack | WhiteBlack | BlackWhite | BrownGreen | GoldBrown | Rainbow1 | Rainbow2 | Rainbow3
  deriving (Eq, Show)


colorise :: Exp R -> Exp RGBA32
colorise = rgba32OfFloat . color scheme
  where
    -- TODO: make runtime configurable
    scheme      = WhiteBlack
    phase       = 0.01
    alpha       = constant 1

    fract x     = x - A.fromIntegral (A.floor x :: Exp Int)
    mix x y a   = x*(1-a) + y*a

    color RedBlack f    = lift (f, constant 0, constant 0, alpha)
    color WhiteBlack f  = lift (f, f, f, alpha)
    color BlackWhite f  = lift (x, x, x, alpha) where x = 1-f
    color BrownGreen f  = lift (mix 0.5 0.5 f, mix 0.3 0.75 f, mix 0 1 f, alpha)
    color GoldBrown f   =
      let ssf = sqrt (sqrt f)
      in  lift ( mix 0.5 (mix 1 0.3 f) ssf
               , mix 0.3 (mix 0.95 0.2 f) ssf
               , constant 0
               , alpha)
    color Rainbow1 f    = rainbow  (fract phase * 6) f
    color Rainbow2 f    = rainbow  (6 * sqrt (sqrt (1-f))) (sqrt (sqrt f))
    color Rainbow3 f    = rainbow' (sqrt (sqrt (1-f))) (sqrt (sqrt f))

    rainbow p x
      = p >* 0 &&* p <* 1
               ? (lift (x,          x*p,        constant 0, alpha)
      , p <* 2 ? (lift (x*(2-p),    x,          constant 0, alpha)
      , p <* 3 ? (lift (constant 0, x,          x*(p-2),    alpha)
      , p <* 4 ? (lift (constant 0, x*(4-p),    x,          alpha)
      , p <* 5 ? (lift (x*(p-4),    constant 0, x,          alpha)
      , p <* 6 ? (lift (x,          constant 0, x*(6-p),    alpha)
      , {-else-} (constant (0,0,0,0))))))))

    rainbow' p x
      = let c y = 0.5 * sin (1.7 * cos (pi * (p + y / 3.0 + phase))) + 0.5
        in lift (x * c 0, x * c 1, x * c 2, alpha)


draw :: Config -> Matrix R -> Picture
draw conf arr = scale zoom zoom pic
  where
    zoom        = P.fromIntegral
                $ get configWindowZoom conf
    size        = get configWindowSize conf

    render      = run1 conf (A.map colorise)

    rawData     = let (Array _ adata)   = render arr
                      ((), ptr)         = ptrsOfArrayData adata
                  in
                  unsafePerformIO       $ newForeignPtr_ (castPtr ptr)

    pic         = bitmapOfForeignPtr
                    size size           -- raw image size
                    rawData             -- the image data
                    False               -- don't cache in texture memory

