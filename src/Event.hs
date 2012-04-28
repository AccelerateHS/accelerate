{-# LANGUAGE PatternGuards #-}
--
-- Event handling
--

module Event where

import Config
import World
import Data.Label
import Graphics.Gloss.Interface.Pure.Game
import Data.Array.Accelerate          ( Z(..), (:.)(..) )


-- Event locations are returned as window coordinates, where the origin is in
-- the centre of the window and increases to the right and up. If the simulation
-- size is (100,100) with scale factor of 4, then the event coordinates are
-- returned in the range [-200,200].
--
react :: Options -> Event -> World -> World
react opt event world =
  case event of
    EventKey (MouseButton LeftButton) s m uv    -> mouse m s (coord uv)
    EventMotion uv                              -> motion (coord uv)
    _                                           -> world
  where
    -- Inject a new density source when the left button is clicked.
    --
    -- If the shift key is held, remember the location and add a new velocity
    -- source between the old and new points as the mouse moves.
    --
    mouse key button xy
      | Up   <- shift key, Down <- button
      = world { currentSource = Density xy
              , densitySource = addDensity xy }

      | Down <- shift key, Down <- button
      = world { currentSource = Velocity xy }

      | Down <- shift key, Up   <- button
      = case currentSource world of
          Velocity x0y0 -> world { currentSource  = None
                                 , velocitySource = addVelocity x0y0 xy }
          _             -> world { currentSource  = None}

      | otherwise
      = world { currentSource = None }

    -- As the mouse moves, keep inserting density sources, or adding source
    -- velocities
    --
    motion xy =
      case currentSource world of
        Density _       -> world { currentSource  = Density xy
                                 , densitySource  = addDensity xy }
        Velocity x0y0   -> world { currentSource  = Velocity xy
                                 , velocitySource = addVelocity x0y0 xy }
        _               -> world
    --
    addDensity (x,y)            = (Z:.y:.x, 1) : densitySource world

    addVelocity (x0,y0) (x1,y1) = let u = fromIntegral (x1-x0)
                                      v = fromIntegral (y1-y0)
                                  in  (Z:.y0:.x0, (u * width, v * height)) : velocitySource world
    --
    zoom        = fromIntegral $ get displayScale opt
    width       = fromIntegral $ get simulationWidth  opt
    height      = fromIntegral $ get simulationHeight opt
    scaleX      = width  / (width  * zoom + 1)
    scaleY      = height / (height * zoom + 1)
    coord (u,v) = ( truncate $ u * scaleX + width /2
                  , truncate $ v * scaleY + height/2)

