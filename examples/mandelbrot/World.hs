{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module World (

  -- Types
  World,

  -- Updating the World state
  renderWorld, initialWorld, refocus, react

) where

import Mandel
import Config
import ParseArgs

import Prelude                                  as P
import Data.Char
import Data.Label
import Data.Array.Accelerate                    as A
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
        width   = get optWidth config
        height  = get optHeight config
        limit   = get optLimit config
        backend = get optBackend config

        render :: (Elt a, IsFloating a) => Render a
        render  = run1 backend
                $ A.map (prettyRGBA (constant (P.fromIntegral limit)))
                . mandelbrot width height limit

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
    char n | isDigit n  = preset n
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

    preset n Down       = loadPreset (read [n])
    preset _ Up         = id


-- Miscellaneous
-- -------------

zooming :: World :-> Maybe Zoom
zooming = lens (\(World _ _ z _ _) -> z) (\f (World p r z h v) -> World p r (f z) h v)

horizontal :: World :-> Maybe Move
horizontal = lens (\(World _ _ _ h _) -> h) (\f (World p r z h v) -> World p r z (f h) v)

vertical :: World :-> Maybe Move
vertical = lens (\(World _ _ _ _ v) -> v) (\f (World p r z h v) -> World p r z h (f v))

convertView :: (Real a, Fractional b) => View a -> View b
convertView (x,y,x',y') = (realToFrac x, realToFrac y, realToFrac x', realToFrac y')

-- Presets
-- -------

loadPreset :: Int -> World -> World
loadPreset n (World _ r z h v) = load (table P.!! n)
  where
    load (posX, posY, zoom, _iters :: Double, _radius :: Double)        -- type signature only to suppress a warning
      = let x' = zoom / 2
            y' = x' * 0.75
        in  World (posX-x', posY-y', posX+x', posY+y') r z h v

    table =
      [ (-0.7,                   0,                               3.067,                  100.0,               2.0)
      , (0.20508818500545423,    0.9014915666351141   * 900/1440, 6.375321937544527e-6,   629.3354966759534,   16.0)
      , (0.4510757067879078,     0.6144133202705898   * 900/1440, 7.632248223018773e-5,   253.61352386150395,  2.0)
      , (0.3469337523117071,     0.6866350870407725   * 900/1440, 3.508380713647269e-5,   168.61054759193718,  1024.0)
      , (-0.7902001921590814,    0.24910667566731381  * 900/1440, 5.071115028132377e-4,   1176.757810813391,   3.4359738368e10)
      , (2.3127178455019423e-2, -1.301205470975472    * 900/1440, 3.6349313304610088e-9,  343.0390372557315,   2.0)
      , (2.3127176148480418e-2, -1.3012054707668765   * 900/1440, 2.71444790387451e-10,   604.1620768089155,   2.0)
      , (2.3127176156746785e-2, -1.301205470242045    * 900/1440, 4.49615119202067e-12,   1731.8575629678642,  2.0)
      , (0.2550376327692795,     8.962363618058007e-4 * 900/1440, 7.351698819132829e-5,   1412.1093729760698,  16.0)
      , (0.25498593633806477,    8.726424280526077e-4 * 900/1440, 1.6858526052251987e-10, 10492.090844482025,  2.0)
      ]

