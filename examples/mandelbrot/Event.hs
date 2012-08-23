{-# LANGUAGE TemplateHaskell #-}
--
-- Event handling
--

module Event (

  -- types
  View, World,

  -- updating the viewport
  viewport, frame, refocus, react

) where

import Config
import Data.Char
import Data.Label
import Graphics.Gloss.Interface.Pure.Game       hiding ( translate, scale )


type View       = (Float,Float,Float,Float)

data Zoom       = In  | Out
data Move       = Fwd | Rev

data World      = World
  { _viewport   :: View

  , _zooming    :: Maybe Zoom
  , _horizontal :: Maybe Move
  , _vertical   :: Maybe Move
  }

$(mkLabels [''World])


-- Initialise the viewport
--
frame :: View -> World
frame v = World v Nothing Nothing Nothing


-- Refocus the viewport by adjusting the limits of the x- and y- range of the
-- display, based on the current key state.
--
refocus :: World -> World
refocus = move . zoom
  where
    -- translate the display
    --
    move v = modify viewport (translate (y,x)) v
      where
        x = case get horizontal v of
              Nothing   ->  0
              Just Fwd  ->  0.025
              Just Rev  -> -0.025

        y = case get vertical v of
              Nothing   ->  0
              Just Fwd  ->  0.025
              Just Rev  -> -0.025

    -- zoom the display in or out
    --
    zoom v = modify viewport (scale s) v
      where
        s = case get zooming v of
              Nothing   -> 1
              Just In   -> 0.975
              Just Out  -> 1.025

    -- viewport translation and scaling
    --
    translate (u,v) (x,y,x',y') =
      let sizex = x' - x
          sizey = y' - y
      in (x+u*sizex, y+v*sizey, x'+u*sizex, y'+v*sizey)

    scale alpha (x,y,x',y') =
      let dx    = sizex * alpha / 2
          dy    = sizey * alpha / 2
          sizex = x' - x
          sizey = y' - y
          midx  = x + sizex / 2
          midy  = y + sizey / 2
      in (midx - dx, midy - dy, midx + dx, midy + dy)



-- Event locations are returned as window coordinates, where the origin is in
-- the centre of the window and increases to the right and up. If the simulation
-- size is (100,100) with scale factor of 4, then the event coordinates are
-- returned in the range [-200,200].
--
react :: Options -> Event -> World -> World
react _opt event view
  = case event of
      EventKey (Char c) s _ _                   -> char (toLower c) s view
      EventKey (SpecialKey c) s _ _             -> special c s view
      _                                         -> view
  where
    char ';'            = toggle zooming In
    char 'z'            = toggle zooming In
    char 'q'            = toggle zooming Out
    char 'x'            = toggle zooming Out
    char _              = const id

    special KeyUp       = toggle vertical Fwd
    special KeyDown     = toggle vertical Rev
    special KeyRight    = toggle horizontal Fwd
    special KeyLeft     = toggle horizontal Rev
    special _           = const id

    toggle f x Down     = set f (Just x)
    toggle f _ Up       = set f Nothing

