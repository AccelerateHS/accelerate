{-# LANGUAGE CPP #-}
-- Quasicrystals demo.
--
-- Based on code from:
--   http://hackage.haskell.org/package/repa-examples
--   http://mainisusuallyafunction.blogspot.com/2011/10/quasicrystals-as-sums-of-waves-in-plane.html
--

module Main where

import Config

import Data.Word
import Data.Label
import Foreign.Ptr
import Control.Monad
import Control.Exception
import Criterion.Main                                   ( defaultMainWith, bench, whnf )
import Foreign.ForeignPtr
import System.Environment
import System.IO.Unsafe
import Data.Array.Accelerate                            ( Array, Scalar, Exp, Acc, DIM2, Z(..), (:.)(..) )
import qualified Data.Array.Accelerate                  as A
import qualified Graphics.Gloss                         as G

import Data.Array.Accelerate.Array.Data                 ( ptrsOfArrayData )
import Data.Array.Accelerate.Array.Sugar                ( Array(..) )


-- Types ----------------------------------------------------------------------
-- | Real value
type R      = Float

-- | Point on the 2D plane.
type R2     = (R, R)

-- | Angle in radians.
type Angle  = R

-- | Angle offset used for animation.
type Phi    = Float

-- | Number of waves to sum for each pixel.
type Degree = Int

-- | Feature size of visualisation.
type Scale  = Float

-- | Size of image to render.
type Size   = Int

-- | How many times to duplicate each pixel / image zoom.
type Zoom   = Int

-- | Type of the generated image data
type RGBA   = Word32
type Bitmap = Array DIM2 RGBA

-- | Action to render a frame
type Render = Scalar Phi -> Bitmap


-- Point ----------------------------------------------------------------------
-- | Compute a single point of the visualisation.
quasicrystal :: Size -> Scale -> Degree -> Acc (Scalar Phi) -> Exp DIM2 -> Exp R
quasicrystal size scale degree phi p
  = waves degree phi $ point size scale p


-- | Sum up all the waves at a particular point.
waves :: Degree -> Acc (Scalar Phi) -> Exp R2 -> Exp R
waves degree phi x = wrap $ waver degree 0
  where
    waver :: Int -> Exp Float -> Exp Float
    waver n acc
      | n == 0    = acc
      | otherwise = waver (n - 1) (acc + wave (A.constant (fromIntegral n) * phi') x)

    phi'
      = A.the phi

    wrap n
      = let n_  = A.truncate n :: Exp Int
            n'  = n - A.fromIntegral n_
        in
        (n_ `rem` 2 A./=* 0) A.? (1-n', n')


-- | Generate the value for a single wave.
wave :: Exp Angle -> Exp R2 -> Exp R
wave th pt = (cos (cth*x + sth*y) + 1) / 2
  where
    (x,y)       = A.unlift pt
    cth         = cos th
    sth         = sin th

-- | Convert an image point to a point on our wave plane.
point :: Size -> Scale -> Exp DIM2 -> Exp R2
point size scale ix = A.lift (adj x, adj y)
  where
    (Z:.x:.y)   = A.unlift ix
    denom       = A.constant (fromIntegral size - 1)
    adj n       = A.constant scale * ((2 * A.fromIntegral n / denom) - 1)


-- Computation ----------------------------------------------------------------
-- | Compute a single frame
makeImage :: Size -> Scale -> Degree -> Acc (Scalar Phi) -> Acc Bitmap
makeImage size scale degree phi = arrPixels
  where
    -- Compute values for the wave density at each point in the range [0..1]
    arrVals     :: Acc (Array DIM2 Float)
    arrVals     = A.generate
                      (A.constant $ Z :. size :. size)
                      (quasicrystal size scale degree phi)

    -- Convert the [0..1] values of wave density to an RGBA flat image
    arrPixels   :: Acc Bitmap
    arrPixels   = A.map rampColour arrVals


-- | Colour ramp from red to white, convert into RGBA
rampColour :: Exp Float -> Exp RGBA
rampColour v = ra + g + b
  where
    u           = 0 `A.max` v `A.min` 1
    ra          = 0xFF0000FF
    g           = A.truncate ((0.4 + (u * 0.6)) * 0xFF) * 0x10000
    b           = A.truncate (u                 * 0xFF) * 0x100


-- Rendering ------------------------------------------------------------------

-- | Compute a single frame of the animation as a Gloss picture.
--
frame :: Render -> Size -> Zoom -> Float -> G.Picture
frame render size zoom time = G.scale zoom' zoom' pic
  where
    -- Scale the time to be the phi value of the animation. The action seems to
    -- slow down at increasing phi values, so we increase phi faster as time
    -- moves on.
    x           = 1 + (time ** 1.5) * 0.005

    -- lift to a singleton array, else we would generate new code with the
    -- constant embedded at every frame
    phi         = A.fromList Z [pi / x]

    -- Compute the image
    arrPixels   = render phi

    -- Wrap the array data in a Foreign pointer and turn into a Gloss picture
    {-# NOINLINE rawData #-}
    rawData     = let (Array _ adata)   = arrPixels
                      ((),ptr)          = ptrsOfArrayData adata
                  in
                  unsafePerformIO       $ newForeignPtr_ (castPtr ptr)

    pic         = G.bitmapOfForeignPtr
                      size size                 -- raw image size
                      rawData                   -- the image data
                      False                     -- don't cache this in texture memory

    -- Zoom the image so we get a bigger window.
    zoom'  = fromIntegral zoom


-- Main -----------------------------------------------------------------------
main :: IO ()
main
  = do  (config, crit, nops) <- parseArgs =<< getArgs
        let size        = get optSize config
            zoom        = get optZoom config
            scale       = get optScale config
            degree      = get optDegree config
            render      = run config $ makeImage size scale degree
            force arr   = A.indexArray arr (Z:.0:.0) `seq` arr

        void . evaluate $ render (A.fromList Z [0])

        if get optBench config
           then withArgs nops $ defaultMainWith crit (return ())
                    [ bench "crystal" $ whnf (force . render) (A.fromList Z [1.0]) ]

#ifndef ACCELERATE_ENABLE_GUI
           else return ()
#else
           else G.animate
                    (G.InWindow "Quasicrystals" (size  * zoom, size * zoom) (10, 10))
                    (G.black)
                    (frame render size zoom)
#endif

