{-# LANGUAGE NoImplicitPrelude #-}

module HighPass
  where

import Data.Array.Accelerate                            as A
import Data.Array.Accelerate.IO                         as A
import Data.Array.Accelerate.Data.Colour.RGBA           as A
import Data.Array.Accelerate.Math.FFT                   as A
import Data.Array.Accelerate.Math.DFT.Centre            as A
import Data.Array.Accelerate.Data.Complex               as A


highpassFFT :: DIM2 -> Int -> Acc (Array DIM2 RGBA32) -> Acc (Array DIM2 RGBA32)
highpassFFT sh cutoff img = img'
  where
    (r,g,b,a)   = A.unzip4
                $ A.map (\c -> let RGBA x y z w = unlift c :: RGBA (Exp Word8)
                               in lift (x,y,z,w) :: Exp (Word8, Word8, Word8, Word8))
                $ A.map unpackRGBA8 img
    r'          = transform sh cutoff r
    g'          = transform sh cutoff g
    b'          = transform sh cutoff b
    --
    img'        = A.zipWith4 (\x y z w -> packRGBA8 . lift $ RGBA x y z w) r' g' b' a


transform :: DIM2 -> Int -> Acc (Array DIM2 Word8) -> Acc (Array DIM2 Word8)
transform sh@(Z :. height :. width) cutoff' arrReal = arrResult
  where
    cutoff      = the (unit (constant cutoff'))

    arrComplex :: Acc (Array DIM2 (Complex Float))
    arrComplex  = A.map (\r -> lift (A.fromIntegral r :+ constant 0)) arrReal

    -- Do the 2D transform
    arrCentered = centre2D arrComplex
    arrFreq     = fft2D' Forward sh arrCentered

    -- Zap out the low-frequency components
    centreX     = constant (width  `div` 2)
    centreY     = constant (height `div` 2)

    zap ix      = let (Z :. y :. x)     = unlift ix
                      inx               = x > centreX - cutoff && x A.< centreX + cutoff
                      iny               = y > centreY - cutoff && y A.< centreY + cutoff
                  in
                  inx && iny ? (constant (0 :+ 0), arrFreq A.! ix)

    arrFilt     = A.generate (A.shape arrFreq) zap

    -- Do the inverse transform to get back to image space
    arrInv      = fft2D' Inverse sh arrFilt

    -- The magnitude of the transformed array
    arrResult   = A.map (A.truncate . magnitude) arrInv

