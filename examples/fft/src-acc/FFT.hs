
module FFT
  where

import Prelude                                          as P
import Data.Array.Accelerate                            as A
import Data.Array.Accelerate.IO                         as A
import Data.Array.Accelerate.Data.Complex               as A
import Data.Array.Accelerate.Math.FFT                   as A
import Data.Array.Accelerate.Math.DFT.Centre            as A


imageFFT :: Int -> Int -> Int -> Acc (Array DIM2 RGBA32) -> Acc (Array DIM2 RGBA32, Array DIM2 RGBA32)
imageFFT width height cutoff img = lift (arrMag, arrPhase)
  where
    -- Load in the image luminance
    arrComplex :: Acc (Array DIM2 (Complex Float))
    arrComplex  = A.map (\r -> lift (r :+ constant 0))
                $ A.map luminanceOfRGBA32 img

    -- Apply the centering transform so that the output has the zero frequency
    -- in the middle of the image
    arrCentered = centre2D arrComplex

    -- Do the transform
    arrFreq     = fft2D' Forward width height arrCentered

    -- Clip the magnitude of the transformed array
    clipMag     = the (unit (constant (P.fromIntegral cutoff)))
    clip x      = x >* clipMag ? ( 1 , x / clipMag )
    arrMag      = A.map (rgba32OfLuminance . clip . magnitude) arrFreq

    -- Get the phase of the transformed array
    scale x     = (phase x + pi) / (2 * pi)
    arrPhase    = A.map (rgba32OfLuminance . scale) arrFreq

