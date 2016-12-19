{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module World (

  World(..),
  updateWorld, renderWorld, initialWorld, draw, react, advance,

) where

import Mandel
import Config

import Data.Char
import Data.Int
import Data.Label
import Data.Word
import Graphics.Gloss.Accelerate.Data.Picture                       as G
import Graphics.Gloss.Interface.Pure.Game                           hiding ( translate, scale )
import System.Exit
import Prelude                                                      as P

import Data.Array.Accelerate                                        ( Array, Scalar, DIM2, Elt, Z(..), (:.)(..) )
import Data.Array.Accelerate.Data.Colour.RGB                        as A
import Data.Array.Accelerate.Examples.Internal                      as A
import qualified Data.Array.Accelerate                              as A


-- World state
-- -----------

data Precision  = Float | Double

data World where
  World :: (P.RealFloat a, A.RealFloat a) =>
    { worldDirty        :: !Bool
    , worldPrecision    :: !Precision
    , worldPicture      :: Picture
    , worldRender       :: (Scalar a -> Scalar a -> Scalar a -> Scalar Int32 -> Scalar a -> Array DIM2 Word32)
    , worldSizeX        :: !Int
    , worldSizeY        :: !Int
    , worldPosX         :: Scalar a
    , worldPosY         :: Scalar a
    , worldWidth        :: Scalar a
    , worldIters        :: Scalar Int32
    , worldRadius       :: Scalar a
    , worldPanning      :: Maybe (Float,Float)
    , worldZooming      :: Maybe Double
    }
    -> World

initialWorld :: Config -> Options -> World
initialWorld conf opts
  = setPrecision opts Float
  $ loadPreset 0
  $ World { worldDirty      = True
          , worldPrecision  = Float
          , worldPicture    = blank
          , worldSizeX      = get configWidth conf
          , worldSizeY      = get configHeight conf
          , worldPanning    = Nothing
          , worldZooming    = Nothing
          , worldPosX       = undefined :: Scalar Float
          , worldPosY       = undefined
          , worldWidth      = undefined
          , worldRadius     = undefined
          , worldIters      = undefined
          , worldRender     = undefined
          }

setPrecision :: Options -> Precision -> World -> World
setPrecision opts prec World{..} =
  let
      backend = get optBackend opts
      palette = run backend
              $ A.generate (A.constant (Z :. 2048))
                           (\ix -> packRGB (ultra (A.toFloating (A.unindex1 ix) / 2048)))
      --
      render :: (A.RealFloat a, A.FromIntegral Int a, A.ToFloating Int32 a)
             => Scalar a -> Scalar a -> Scalar a -> Scalar Int32 -> Scalar a -> Array DIM2 Word32
      render  = run5 backend $ \posX posY width limit radius
             -> A.map (escapeToRGBA limit (A.use palette))
              $ mandelbrot worldSizeX worldSizeY posX posY width limit radius
  in
  case prec of
    Float  -> let cvt :: (Elt a, P.Real a) => Scalar a -> Scalar Float
                  cvt x = unit (P.realToFrac (the x))
              in
              World { worldPrecision = Float
                    , worldPosX      = cvt worldPosX
                    , worldPosY      = cvt worldPosY
                    , worldWidth     = cvt worldWidth
                    , worldRadius    = cvt worldRadius
                    , worldRender    = render
                    , ..
                    }
    Double -> let cvt :: (Elt a, P.Real a) => Scalar a -> Scalar Double
                  cvt x = unit (P.realToFrac (the x))
              in
              World { worldPrecision = Double
                    , worldPosX      = cvt worldPosX
                    , worldPosY      = cvt worldPosY
                    , worldWidth     = cvt worldWidth
                    , worldRadius    = cvt worldRadius
                    , worldRender    = render
                    , ..
                    }

-- Draw the world
--
draw :: World -> IO Picture
draw = return . worldPicture

-- React to events
--
react :: Config -> Options -> Event -> World -> IO World
react conf opts event world@World{..} =
  case event of
    -- zooming
    EventKey (Char 'w') s _ _               -> toggle zooming 0.975 s world
    EventKey (Char 's') s _ _               -> toggle zooming 1.025 s world

    -- panning
    EventKey (MouseButton LeftButton) s _ p -> toggle panning p s world
    EventMotion (x,y)
      | Just (x0,y0) <- worldPanning
      -> let dx = (x0-x) * P.realToFrac (the worldWidth) / P.fromIntegral worldSizeX
             dy = (y0-y) * P.realToFrac (the worldWidth) / P.fromIntegral worldSizeX
         in
         return . dirty
                $ World { worldPosX    = unit (P.realToFrac dx + the worldPosX)
                        , worldPosY    = unit (P.realToFrac dy + the worldPosY)
                        , worldPanning = Just (x,y)
                        , ..
                        }

    -- algorithm
    EventKey (Char 'a') Down _ _ -> return . dirty
      $ World { worldIters = unit (P.truncate (P.fromIntegral (the worldIters) * 0.8 :: Double)), .. }
    EventKey (Char 'd') Down _ _ -> return . dirty
      $ World { worldIters = unit (P.truncate (P.fromIntegral (the worldIters) * 1.2 :: Double)), .. }

    EventKey (Char 'z') Down _ _ -> return . dirty
      $ World { worldRadius = unit (the worldRadius * 0.5), .. }
    EventKey (Char 'c') Down _ _ -> return . dirty
      $ World { worldRadius = unit (the worldRadius * 2.0), .. }

    EventKey (Char 'p') Down _ _ -> return . dirty
      $ case worldPrecision of
          Float  -> setPrecision opts Double world  -- could fail if no hardware support
          Double -> setPrecision opts Float  world

    -- presets
    EventKey (Char d) Down _ _ | isDigit d  -> return . dirty $ loadPreset (read [d]) world

    -- misc
    EventKey (Char 'r') Down _ _            -> return . dirty $ initialWorld conf opts
    EventKey (Char '.') Down _ _            -> putStrLn (showWorld world) >> return world
    EventKey (SpecialKey KeyEsc) Down _ _   -> exitSuccess

    _                                       -> return world
  where
    toggle f x Down = return . dirty . set f (Just x)
    toggle f _ Up   = return         . set f Nothing

    dirty w         = w { worldDirty = True }


-- Move and zoom the display based on the key state.
--
advance :: Float -> World -> IO World
advance _ world@World{..}
  | Just f <- worldZooming  = return $ updateWorld $ World { worldDirty = False, worldWidth = A.fromList Z [ P.realToFrac f * the worldWidth ], .. }
  | worldDirty              = return $ updateWorld $ World { worldDirty = False, .. }
  | otherwise               = return world

-- Update the picture
--
updateWorld :: World -> World
updateWorld world =
  world { worldPicture = bitmapOfArray (renderWorld world) False }

renderWorld :: World -> Array DIM2 Word32
renderWorld World{..} =
  worldRender worldPosX worldPosY worldWidth worldIters worldRadius


-- Miscellaneous
-- -------------

zooming :: World :-> Maybe Double
zooming = lens worldZooming (\f World{..} -> World { worldZooming = f worldZooming, .. })

panning :: World :-> Maybe (Float,Float)
panning = lens worldPanning (\f World{..} -> World { worldPanning = f worldPanning, .. })

the :: Elt a => Scalar a -> a
the a = a `A.indexArray` Z

unit :: Elt a => a -> Scalar a
unit a = A.fromList Z [a]

-- Presets
-- -------

showWorld :: World -> String
showWorld World{..} =
  show ( the worldPosX
       , the worldPosY
       , the worldWidth
       , the worldIters
       , the worldRadius
       )

loadWorld :: (Double,Double,Double,Double,Double) -> World -> World
loadWorld (posX, posY, width, iters, radius) World{..}
  = World { worldPosX   = unit (P.realToFrac posX) `asTypeOf` worldPosX
          , worldPosY   = unit (P.realToFrac posY)
          , worldWidth  = unit (P.realToFrac width)
          , worldIters  = unit (P.truncate iters)
          , worldRadius = unit (P.realToFrac radius)
          , ..
          }

loadPreset :: Int -> World -> World
loadPreset n = loadWorld (presets P.!! n)

presets :: [(Double,Double,Double,Double,Double)]
presets =
  [ (-0.7,                   0,                               3.067,                  100.0,   16.0)
  , (0.20508818500545423,    0.9014915666351141   * 900/1440, 6.375321937544527e-6,   629.0,   256.0)
  , (0.4510757067879078,     0.6144133202705898   * 900/1440, 7.632248223018773e-5,   399.0,   4.0)
  , (0.3469337523117071,     0.6866350870407725   * 900/1440, 3.508380713647269e-5,   505.0,   1048576.0)
  , (-0.7902001921590814,    0.24910667566731381  * 900/1440, 5.071115028132377e-4,   1176.0,  3.4359738368e10)
  , (2.3127178455019423e-2, -1.301205470975472    * 900/1440, 3.6349313304610088e-9,  566.0,   4.0)
  , (2.3127176148480418e-2, -1.3012054707668765   * 900/1440, 2.71444790387451e-10,   604.0,   4.0)
  , (2.3127176156746785e-2, -1.301205470242045    * 900/1440, 4.49615119202067e-12,   2000.0,  4.0)
  , (0.2550376327692795,     8.962363618058007e-4 * 900/1440, 7.351698819132829e-5,   1412.0,  256.0)
  , (0.25498593633806477,    8.726424280526077e-4 * 900/1440, 1.6858526052251987e-10, 10492.0, 4.0)
  ]

