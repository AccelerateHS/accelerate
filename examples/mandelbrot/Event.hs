--
-- Event handling
--

module Event where

import Config
import Data.Char
import Graphics.Gloss.Interface.Pure.Game       hiding ( translate, scale )


type View = (Float,Float,Float,Float)

-- Event locations are returned as window coordinates, where the origin is in
-- the centre of the window and increases to the right and up. If the simulation
-- size is (100,100) with scale factor of 4, then the event coordinates are
-- returned in the range [-200,200].
--
react :: Options -> Event -> View -> View
react _opt event view
  = case event of
      EventKey (Char c) s m _                   -> zoom (toLower c) s m view
      EventKey (SpecialKey c) s m _             -> move c s m view
      _                                         -> view
  where
    -- translate the display
    --
    move KeyLeft  Down _ = translate (0, -0.1)
    move KeyRight Down _ = translate (0,  0.1)
    move KeyUp    Down _ = translate ( 0.1, 0)
    move KeyDown  Down _ = translate (-0.1, 0)
    move _        _    _ = id

    -- zoom the display in or out
    --
    zoom ';' Down _     = scale 0.45
    zoom 'z' Down _     = scale 0.45
    zoom 'q' Down _     = scale 0.55
    zoom 'x' Down _     = scale 0.55
    zoom _   _    _     = id

    translate (u,v) (x,y,x',y') =
      let sizex = x' - x
          sizey = y' - y
      in (x+u*sizex, y+v*sizey, x'+u*sizex, y'+v*sizey)

    scale alpha (x,y,x',y') =
      let dx    = sizex * alpha
          dy    = sizey * alpha
          sizex = x' - x
          sizey = y' - y
          midx  = x + sizex / 2
          midy  = y + sizey / 2
      in (midx - dx, midy - dy, midx + dx, midy + dy)

