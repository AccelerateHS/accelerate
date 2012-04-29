module Model
   ( Model (..)
   , DensityField
   , Source (..)
   , VelocityField
   , Field
   , model
   , display
   , floatToWord8
   , CurrentButton (..)
   )
   where

import Graphics.Gloss
import Data.Array.Repa as A
import Data.Array.Repa.Index as I
import Data.Array.Repa.ByteString
import Foreign

import Constants

-- Data type to represent world model
data Model
   = Model
   { densityField   :: DensityField
   , densitySource  :: Maybe (Source Float)
   , velocityField  :: VelocityField
   , velocitySource :: Maybe (Source (Float, Float))
   , clickLoc       :: Maybe (Int, Int)
   , stepsPassed    :: Int
   , currButton     :: CurrentButton
   }

type DensityField   = Field Float
type VelocityField  = Field (Float, Float)
data Source a       = Source DIM2 a
type Field a        = Array DIM2 a
data CurrentButton  = LeftButton
                    | RightButton
                    | None

-- Creates an initial blank model
model :: Model
{-# INLINE model #-}
model
   = Model
   { densityField   = A.fromList (Z:.widthI:.widthI) 
                    $ replicate (widthI*widthI) 0
   , densitySource  = Nothing
   , velocityField  = A.fromList (Z:.widthI:.widthI)
                    $ replicate (widthI*widthI) (0,0)
   , velocitySource = Nothing
   , clickLoc       = Nothing
   , stepsPassed    = 0
   , currButton     = None
   }

-- Function to convert the Model into a Bitmap for displaying in Gloss
display :: Model -> Picture
display m = {-# SCC "displaySim" #-}
            Scale scaleX scaleY $
            Bitmap width height $
            toByteString convertedM'
          where
             (Z :. width' :. height') = A.extent $ densityField m
             width      = fromIntegral width'
             height     = fromIntegral height'
             convertedM' = convertedM A.++ alpha
             convertedM = extend (Z :. All :. All :. (3::Int)) word8M
             word8M     = A.map (floatToWord8) $ densityField m

-- Converts Float value to Word8 for pixel data
floatToWord8 :: Float -> Word8
{-# INLINE floatToWord8 #-}
floatToWord8 f
   | f <  0.0  = 0
   | f >= 1.0  = 255
   | otherwise = truncate $ f * 255

-- Creates alpha values for display
alpha :: Array DIM3 Word8
{-# INLINE alpha #-}
alpha = A.fromList (Z:.widthI:.widthI:.1) 
      $ replicate (widthI*widthI) 255
