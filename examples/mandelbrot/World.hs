{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}

module World (

  -- Types
  World,

  -- Updating the World state
  renderWorld, initialWorld, refocus, react

) where

import Mandel
import Config

import Data.Char
import Data.Label
import Data.Array.Accelerate                    as A hiding ( size )
import Graphics.Gloss.Interface.Pure.Game       hiding ( translate, scale )


-- World state
-- -----------

data Zoom       = In  | Out
data Move       = Fwd | Rev

data Precision  = Float | Double

data World where
  World :: (Elt a, RealFloat a)
        => View a
        -> Render a
        -> Maybe Zoom
        -> Maybe Move   -- horizontal movement
        -> Maybe Move   -- vertical movement
        -> World


-- Render the picture
--
renderWorld :: World -> Bitmap
renderWorld (World view render _ _ _) = render $ A.fromList Z [view]


-- Initialise the World state
--
initialWorld :: Options -> View Float -> World
initialWorld config view
  = setPrecisionOfWorld Float config
  $ World view undefined Nothing Nothing Nothing


-- Reset the rendering routines to compute with the specified precision
--
setPrecisionOfWorld :: Precision -> Options -> World -> World
setPrecisionOfWorld f config (World p _ z h v)
  = let
        size    = get optSize config
        limit   = get optLimit config

        render :: (Elt a, IsFloating a) => Render a
        render  = run1 config
                $ A.map (prettyRGBA (constant limit))
                . mandelbrot size size limit

    in case f of
         Float  -> World (convertView p :: View Float)  render z h v
         Double -> World (convertView p :: View Double) render z h v


-- Event handling
-- --------------

-- Refocus the viewport by adjusting the limits of the x- and y- range of the
-- display, based on the current key state.
--
refocus :: World -> World
refocus = move . zoom
  where
    -- translate the display
    --
    move :: World -> World
    move world@(World viewport r z h v)
      = World (translate (dy,dx) viewport) r z h v
      where
        dx = case get horizontal world of
               Nothing   ->  0
               Just Fwd  ->  0.025
               Just Rev  -> -0.025

        dy = case get vertical world of
               Nothing   ->  0
               Just Fwd  ->  0.025
               Just Rev  -> -0.025

        translate (j,i) (x,y,x',y') =
          let sizex = x' - x
              sizey = y' - y
          in (x+i*sizex, y+j*sizey, x'+i*sizex, y'+j*sizey)

    -- zoom the display in or out
    --
    zoom :: World -> World
    zoom world@(World viewport r z h v)
      = World (scale s viewport) r z h v
      where
        s = case get zooming world of
              Nothing   -> 1
              Just In   -> 0.975
              Just Out  -> 1.025

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
react opt event world
  = case event of
      EventKey (Char c) s _ _           -> char (toLower c) s world
      EventKey (SpecialKey c) s _ _     -> special c s world
      _                                 -> world
  where
    char ';'            = toggle zooming In
    char 'z'            = toggle zooming In
    char 'q'            = toggle zooming Out
    char 'x'            = toggle zooming Out
    char 'd'            = precision Double
    char 'f'            = precision Float
    char _              = const id

    special KeyUp       = toggle vertical Fwd
    special KeyDown     = toggle vertical Rev
    special KeyRight    = toggle horizontal Fwd
    special KeyLeft     = toggle horizontal Rev
    special _           = const id

    toggle f x Down     = set f (Just x)
    toggle f _ Up       = set f Nothing

    precision f Down    = setPrecisionOfWorld f opt
    precision _ _       = id


-- Miscellaneous
-- -------------

zooming :: World :-> Maybe Zoom
zooming = lens (\(World _ _ z _ _)   -> z) (\z (World p r _ h v) -> World p r z h v)

horizontal :: World :-> Maybe Move
horizontal = lens (\(World _ _ _ h _)   -> h) (\h (World p r z _ v) -> World p r z h v)

vertical :: World :-> Maybe Move
vertical = lens (\(World _ _ _ _ v)   -> v) (\v (World p r z h _) -> World p r z h v)

convertView :: (Real a, Fractional b) => View a -> View b
convertView (x,y,x',y') = (realToFrac x, realToFrac y, realToFrac x', realToFrac y')

