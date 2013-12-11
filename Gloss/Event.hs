
module Gloss.Event
  where

-- friends
import Vec3
import Scene.State

-- library
import Data.Char
import Data.Label
import Graphics.Gloss.Interface.Pure.Game                       as G


handleEvent :: Event -> State -> State
handleEvent event state
  = case event of
      EventKey (Char c) s _ _           -> char (toLower c) s state
      EventKey (MouseButton b) s _ l    -> click b s l state
      EventMotion p                     -> motion p
      _                                 -> state
  where
    toggle f Up         = set f False
    toggle f Down       = set f True

    toggle' f Down x    = set f (Just x)
    toggle' f Up   _    = set f Nothing

    char 'w'            = toggle stateMovingForward
    char ','            = toggle stateMovingForward
    char 's'            = toggle stateMovingBackward
    char 'o'            = toggle stateMovingBackward
    char 'a'            = toggle stateMovingLeft
    char 'd'            = toggle stateMovingRight
    char 'e'            = toggle stateMovingRight
    char _              = const id

    click LeftButton    = toggle' stateLeftClick
    click _             = const (const id)

    motion (x,y)
      | Just (oX, oY)           <- get stateLeftClick state
      , XYZ eyeX eyeY eyeZ      <- get stateEyeLoc    state
      = let eyeX'       = eyeX + (x - oX)
            eyeY'       = eyeY
            eyeZ'       = eyeZ + (y - oY)
        in
        set stateEyeLoc (XYZ eyeX' eyeY' eyeZ')
          $ set stateLeftClick  (Just (x, y))
          $ state

    motion _
      = state

