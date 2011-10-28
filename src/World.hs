--
-- Displaying the world state
--

module World where

import Config
import Field

import Data.Array.Accelerate
import Graphics.Gloss.Interface.Game    hiding ( Point )


data World = World
  {
    -- current state of the simulation
    densityField   :: Acc DensityField
  , velocityField  :: Acc VelocityField
  , indexField     :: Acc IndexField
      -- ^^ because we lack functions to map with indices

    -- user inputs
  , densitySource  :: [(Point, Density)]
  , velocitySource :: [(Point, Velocity)]
  , currentButton  :: Maybe (MouseButton, (Int, Int))
  }

initialWorld :: Config -> World
initialWorld cfg =
  let w = simulationWidth  cfg
      h = simulationHeight cfg
  in
  World
    { densityField   = use $ fromList (Z:.h:.w) (repeat 0)
    , velocityField  = use $ fromList (Z:.h:.w) (repeat (0,0))
    , indexField     = use $ fromList (Z:.h:.w) [Z:.y:.x | y <- [0..h-1], x <- [0..w-1]]
    , densitySource  = []
    , velocitySource = []
    , currentButton  = Nothing
    }

renderWorld :: Config -> World -> Picture
renderWorld _ _ = error "TODO: renderWorld"

