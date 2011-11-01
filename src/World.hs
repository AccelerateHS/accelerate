{-# LANGUAGE BangPatterns #-}
--
-- Displaying the world state
--

module World (
  World(..),
  initialWorld, renderWorld, renderDensity
) where

import           Type
import           Config
import           Data.Word
import           Graphics.Gloss.Interface.Game
import           Foreign.Ptr
import           Foreign.ForeignPtr
import           Foreign.Storable
import           Foreign.Marshal.Alloc
import           System.IO.Unsafe
import           Data.Array.Accelerate                  ( Z(..), (:.)(..), Acc )
import           Data.Array.Accelerate.Array.Sugar      ( Array(..) )
import qualified Data.Array.Accelerate.Array.Data       as A
import qualified Data.Array.Accelerate                  as A


data World = World
  {
    -- current state of the simulation
    densityField   :: DensityField
  , velocityField  :: VelocityField
  , indexField     :: Acc IndexField
      -- ^^ because we lack functions to map with indices

    -- user input
  , densitySource  :: [(Index, Density)]
  , velocitySource :: [(Index, Velocity)]
  , currentButton  :: Maybe (MouseButton, (Int,Int))
  }

initialWorld :: Config -> World
initialWorld cfg =
  let w = simulationWidth  cfg
      h = simulationHeight cfg
  in
  World
    { densityField   = A.fromList (Z:.h:.w) (repeat 0)
    , velocityField  = A.fromList (Z:.h:.w) (repeat (0,0))
    , indexField     = A.use $ A.fromList (Z:.h:.w) [Z:.y:.x | y <- [0..h-1], x <- [0..w-1]]
    , densitySource  = []
    , velocitySource = []
    , currentButton  = Nothing
    }

renderWorld :: Config -> World -> Picture
renderWorld cfg world = Scale zoom zoom pic
  where
    zoom = fromIntegral  $ displayScale cfg
    pic  = renderDensity $ densityField world


{-# NOINLINE renderDensity #-}
renderDensity :: DensityField -> Picture
renderDensity df@(Array _ ad) = unsafePerformIO $ do
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


-- Float to Word8 conversion because the one in the GHC libraries doesn't have
-- enough specialisations and goes via Integer.
{-# INLINE floatToWord8 #-}
floatToWord8 :: Float -> Word8
floatToWord8 f = fromIntegral (truncate f :: Int)


{--
 -- We would like to be able to render the density field into an image as part
 -- of the accelerate expression, but can not at the moment because we are
 -- limited to two output arrays (a soft bug).
 --
renderDensity :: Acc DensityField -> Acc ImageRGBA
renderDensity = A.map rgba
  where
    rgba f = let r = A.truncate . (255 *) . A.max 0 . A.min 1 $ f
                 g = r * 0x100
                 b = g * 0x100
                 a = 0xFF000000
             in
             r + g + b + a
--}

