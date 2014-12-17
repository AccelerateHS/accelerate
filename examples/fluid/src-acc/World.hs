{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE PatternGuards #-}
--
-- Displaying the world state
--

module World (World(..), Source(..), initialise, render) where

import Type
import Config

import Data.Word
import Data.Label
import Graphics.Gloss.Rendering
import Graphics.Gloss.Interface.IO.Game
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Data.Array.Accelerate                            ( Z(..), (:.)(..) )
import Data.Array.Accelerate.Array.Sugar                ( Array(..) )

import qualified Data.Array.Accelerate                  as A
import qualified Data.Array.Accelerate.Array.Data       as A


data World = World
  {
    -- current state of the simulation
    densityField        :: !DensityField
  , velocityField       :: !VelocityField

    -- user input
  , densitySource       :: [(Index, Density)]
  , velocitySource      :: [(Index, Velocity)]
  , currentSource       :: Source
  , displayDensity      :: Bool
  , displayVelocity     :: Bool
  }
  deriving Show

data Source = Density  (Int, Int)
            | Velocity (Int, Int)
            | None
  deriving (Eq, Show)


-- Initialisation --------------------------------------------------------------
-- --------------                                                             --

initialise :: Config -> World
initialise opt =
  World
    { densityField      = get initialDensity opt
    , velocityField     = get initialVelocity opt
    , densitySource     = []
    , velocitySource    = []
    , currentSource     = None
    , displayDensity    = True
    , displayVelocity   = False
    }


-- Rendering -------------------------------------------------------------------
-- ---------                                                                  --

render :: Config -> World -> IO Picture
render opt world = do
  den   <- if displayDensity world
              then renderDensity   $ densityField  world
              else return blank

  vel   <- if displayVelocity world
              then renderVelocity  $ velocityField world
              else return blank
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
      in  Color red $ Line [ (x,y), (x+ 25*u, y+ 25*v) ]


-- Float to Word8 conversion because the one in the GHC libraries doesn't have
-- enough specialisations and goes via Integer.
{-# INLINE floatToWord8 #-}
floatToWord8 :: Float -> Word8
floatToWord8 f = fromIntegral (truncate f :: Int)

