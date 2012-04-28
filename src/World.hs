{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE PatternGuards #-}
--
-- Displaying the world state
--

module World (World(..), Source(..), initialise, render) where

import Type
import Config

import Codec.BMP
import Data.Bits
import Data.Int
import Data.Word
import Data.Label
import Control.Monad
import Graphics.Gloss.Interface.IO.Game
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Data.Array.Accelerate                            ( Z(..), (:.)(..), Exp )
import Data.Array.Accelerate.Array.Sugar                ( Array(..) )

import qualified Data.Array.Accelerate.Array.Data       as A
import qualified Data.Array.Accelerate.IO               as A
import qualified Data.Array.Accelerate                  as A


data World = World
  {
    -- current state of the simulation
    densityField   :: !DensityField
  , velocityField  :: !VelocityField
  , indexField     :: Acc IndexField
      -- ^^ because we lack functions to map with indices

    -- user input
  , densitySource  :: [(Index, Density)]
  , velocitySource :: [(Index, Velocity)]
  , currentSource  :: Source
  }
  deriving Show

data Source = Density  (Int, Int)
            | Velocity (Int, Int)
            | None
  deriving (Eq, Show)


-- Initialisation --------------------------------------------------------------
-- --------------                                                             --

initialise :: Options -> IO World
initialise opt =
  let width     = get simulationWidth  opt
      height    = get simulationHeight opt
      indices   = A.use $ A.fromList
                            (Z:.height:.width)
                            [Z:.y:.x | y <- [0..height-1], x <- [0..width-1]]
  in do
  density       <- initialDensity  opt width height
  velocity      <- initialVelocity opt width height

  return $ World
    { densityField      = density
    , velocityField     = velocity
    , indexField        = indices
    , densitySource     = []
    , velocitySource    = []
    , currentSource     = None
    }


initialDensity :: Options -> Int -> Int -> IO DensityField
initialDensity opt width height
  -- Load the file, and use the luminance value as scalar density
  --
  | Just file   <- get densityBMP opt
  = do
      arr               <- readImageFromBMP file
      let (Z:.h:.w)     =  A.arrayShape arr

      when (w /= width || h /= height)
        $ error "fluid: density-bmp does not match width x height"

      return . run opt $ A.map densityOfRGBA (A.use arr)

  -- No density file given, just set the field to zero
  --
  | otherwise
  = return $ A.fromList (Z :. height :. width) (repeat 0)


initialVelocity :: Options -> Int -> Int -> IO VelocityField
initialVelocity opt width height
  -- Load the file, and use the red and green channels for x- and y- velocity
  -- values respectively
  --
  | Just file   <- get velocityBMP opt
  = do
      arr               <- readImageFromBMP file
      let (Z:.h:.w)     =  A.arrayShape arr

      when (w /= width || h /= height)
        $ error "fluid: velocity-bmp does not match width x height"

      return . run opt $ A.map velocityOfRGBA (A.use arr)

  -- No density file given, just set to zero
  --
  | otherwise
  = return $ A.fromList (Z :. height :. width) (repeat (0,0))


readImageFromBMP :: FilePath -> IO (Image RGBA)
readImageFromBMP file = do
  bmp           <- either (error . show) id `fmap` readBMP file
  let (w,h)     =  bmpDimensions bmp
  --
  A.fromByteString (Z :. h :. w) ((), unpackBMPToRGBA32 bmp)


densityOfRGBA :: Exp RGBA -> Exp Density
densityOfRGBA rgba =
  let b = (0.11 / 255) * A.fromIntegral ((rgba `div` 0x100)     .&. 0xFF)
      g = (0.59 / 255) * A.fromIntegral ((rgba `div` 0x10000)   .&. 0xFF)
      r = (0.3  / 255) * A.fromIntegral ((rgba `div` 0x1000000) .&. 0xFF)
  in
  r + g + b

velocityOfRGBA :: Exp RGBA -> Exp Velocity
velocityOfRGBA rgba =
  let g = A.fromIntegral (-128 + A.fromIntegral ((rgba `div` 0x10000)   .&. 0xFF) :: Exp Int32)
      r = A.fromIntegral (-128 + A.fromIntegral ((rgba `div` 0x1000000) .&. 0xFF) :: Exp Int32)
  in
  A.lift (r * 0.001, g * 0.001)


-- Rendering -------------------------------------------------------------------
-- ---------                                                                  --

render :: Options -> World -> IO Picture
render opt world = do
  den   <- renderDensity   $ densityField  world
  vel   <- renderVelocity  $ velocityField world
  --
  return $ Scale zoom zoom $ Pictures [ den, vel ]
  where
    zoom        = fromIntegral $ get displayScale opt


renderDensity :: DensityField -> IO Picture
renderDensity df@(Array _ ad) = do
  dst   <- mallocBytes (n*4)
  fill 0 src dst
  fptr  <- newForeignPtr finalizerFree dst
  return $ bitmapOfForeignPtr w h fptr False
  where
    ((),src)    = A.ptrsOfArrayData ad
    Z:.h:.w     = A.arrayShape df
    n           = h*w
    colour !f   = let c = 0 `max` f `min` 1
                  in  floatToWord8 (255*c)

    fill !i !s !d | i >= n    = return ()
                  | otherwise = do c <- colour `fmap` peek s
                                   poke        d   0xFF         -- A
                                   pokeByteOff d 1 c            -- B
                                   pokeByteOff d 2 c            -- G
                                   pokeByteOff d 3 c            -- R
                                   fill (i+1) (plusPtr s 4) (plusPtr d 4)


renderVelocity :: VelocityField -> IO Picture
renderVelocity vf
  = return
  $ Translate (fromIntegral $ -w `div` 2) (fromIntegral $ -h `div` 2)
  $ Pictures [ field (x,y) | y <- [2,7..h], x <- [2,7..w] ]
  where
    Z:.h:.w       = A.arrayShape vf
    field (x0,y0) =
      let x     = fromIntegral x0
          y     = fromIntegral y0
          (u,v) = A.indexArray vf (Z:.y0:.x0)
      in  Color red $ Line [ (x,y), (x+u, y+v) ]


-- Float to Word8 conversion because the one in the GHC libraries doesn't have
-- enough specialisations and goes via Integer.
{-# INLINE floatToWord8 #-}
floatToWord8 :: Float -> Word8
floatToWord8 f = fromIntegral (truncate f :: Int)

