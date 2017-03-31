{-# LANGUAGE PatternGuards #-}

module Gloss.Event
  where

-- friends
import Common.Type
import Scene.State

-- library
import Data.Char
import Data.Label
import Graphics.Gloss.Interface.Pure.Game                       as G


handleEvent :: Event -> State -> State
handleEvent event state
  = case event of
      EventKey (Char c) s _ _           -> char (toLower c) s state
      EventKey (SpecialKey c) s _ _     -> special c s state
      EventKey (MouseButton b) s _ l    -> click b l s state
      EventMotion p                     -> motion p
      _                                 -> state
  where
    toggle f x Down     = set f (Just x)
    toggle f _ Up       = set f Nothing

    char 'w'            = toggle stateEyeVert Fwd
    char ','            = toggle stateEyeVert Fwd
    char 's'            = toggle stateEyeVert Rev
    char 'o'            = toggle stateEyeVert Rev
    char 'a'            = toggle stateEyeHoriz Rev
    char 'd'            = toggle stateEyeHoriz Fwd
    char 'e'            = toggle stateEyeHoriz Fwd
    char _              = const id

    click LeftButton    = toggle stateLeftClick
    click _             = const (const id)

    special KeyUp       = toggle stateLightVert Fwd
    special KeyDown     = toggle stateLightVert Rev
    special KeyRight    = toggle stateLightHoriz Fwd
    special KeyLeft     = toggle stateLightHoriz Rev
    special _           = const id

    motion (x,y)
      | Just (oX, oY)           <- get stateLeftClick state
      , V3 eyeX eyeY eyeZ       <- get stateEyeDelta  state
      = let eyeX'       = eyeX + (x - oX)
            eyeY'       = eyeY
            eyeZ'       = eyeZ + (y - oY)
        in
        set stateEyeDelta (V3 eyeX' eyeY' eyeZ')
          $ set stateLeftClick  (Just (x, y))
          $ state

    motion _
      = state

