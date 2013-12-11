
module Gloss.Event
  where

-- friends
import Scene.State

-- library
import Graphics.Gloss                                           as G
import Graphics.Gloss.Interface.Pure.Game                       as G


handleEvent :: Event -> State -> State
handleEvent event state
  -- Start translation
  | EventKey (MouseButton LeftButton) Down _ (x, y)     <- event
  = state { stateLeftClick = Just (x, y)}

  -- End transation
  | EventKey (MouseButton LeftButton) Up _ _            <- event
  = state { stateLeftClick = Nothing }

  -- Moving forward
  | EventKey (Char 'w') Down _ _                        <- event
  = state { stateMovingForward  = True }

  | EventKey (Char 'w') Up   _ _                        <- event
  = state { stateMovingForward  = False }

  -- Moving backward
  | EventKey (Char 's') Down _ _                        <- event
  = state { stateMovingBackward = True }

  | EventKey (Char 's') Up   _ _                        <- event
  = state { stateMovingBackward = False }

  -- Moving left
  | EventKey (Char 'a') Down _ _                        <- event
  = state { stateMovingLeft = True }

  | EventKey (Char 'a') Up   _ _                        <- event
  = state { stateMovingLeft = False }

  -- Moving right
  | EventKey (Char 'd') Down _ _                        <- event
  = state { stateMovingRight = True }

  | EventKey (Char 'd') Up   _ _                        <- event
  = state { stateMovingRight = False }

  -- Translate the world
--  | EventMotion (x, y)  <- event
--  , Just (oX, oY)         <- stateLeftClick state
--  , XYZ eyeX eyeY eyeZ   <- stateEyeLoc    state
--  = let   eyeX'   = eyeX + (x - oX)
--          eyeY'   = eyeY
--          eyeZ'   = eyeZ + (y - oY)
--
--    in    setEyeLoc (XYZ eyeX' eyeY' eyeZ')
--           $ state { stateLeftClick  = Just (x, y) }
--
  | otherwise
  = state

