{-# LANGUAGE TemplateHaskell #-}

module Scene.State
  where

-- friends
import Vec3
import Scene.Light
import Scene.Object
import Scene.World

-- frenemies
import Data.Array.Accelerate                                    as A

-- library
import Prelude                                                  hiding ( (.), id )
import Data.Label
import Control.Category
import qualified Graphics.Gloss                                 as G


data Move  = Fwd | Rev
  deriving Show

data State = State
  { _stateTime                  :: !Float
  , _stateEyePos                :: !Position
  , _stateEyeLoc                :: !Position

  , _stateLeftClick             :: !(Maybe G.Point)

  , _stateMoveSpeed             :: !Float
  , _stateEyeHoriz              :: !(Maybe Move)
  , _stateEyeVert               :: !(Maybe Move)

  , _stateObjects               :: !Objects
  , _stateLights                :: !Lights
  }
  deriving Show

mkLabels [''State]


-- | Initialise the world and interface state
--
initState :: Float -> State
initState time
  = State
      { _stateTime              = time
      , _stateEyePos            = XYZ 50    (-100) (-700)
      , _stateEyeLoc            = XYZ (-50) 200   1296

      , _stateLeftClick         = Nothing

      , _stateMoveSpeed         = 400
      , _stateEyeHoriz          = Nothing
      , _stateEyeVert           = Nothing

      , _stateObjects           = makeObjects time
      , _stateLights            = makeLights  time
      }


-- | Advance the world forward in time
--
advanceState :: Float -> State -> State
advanceState dt state
  = move stateEyeVert  zz
  $ move stateEyeHoriz xx
  $ setTime (get stateTime state + dt) state
  where
    speed       = get stateMoveSpeed state
    move f d    = case get f state of
                    Nothing     -> id
                    Just Fwd    -> moveEyeLoc (set d ( speed * dt) (XYZ 0 0 0))
                    Just Rev    -> moveEyeLoc (set d (-speed * dt) (XYZ 0 0 0))

    zz          = lens (\(XYZ _ _ z) -> z) (\f (XYZ x y z) -> XYZ x y (f z))
    xx          = lens (\(XYZ x _ _) -> x) (\f (XYZ x y z) -> XYZ (f x) y z)


-- | Set the location of the eye
--
moveEyeLoc :: Position -> State -> State
moveEyeLoc v state
  = modify stateEyeLoc (+ v) state

-- | Set the time of the world
--
setTime :: Float -> State -> State
setTime time state
  = let
        objects = makeObjects time
        lights  = makeLights  time
        eyeLoc  = get stateEyeLoc state
    in
    set stateTime time
      $ set stateObjects (translateObjects eyeLoc objects)
      $ set stateLights  (translateLights  eyeLoc lights)
      $ state


translateObjects :: Position -> Objects -> Objects
translateObjects v (spheres, planes)
  = ( fromList (arrayShape spheres) [ Sphere (p + v) r c s | Sphere p r c s <- toList spheres ]
    , fromList (arrayShape planes)  [ Plane  (p + v) d c s | Plane  p d c s <- toList planes  ]
    )

translateLights :: Position -> Lights -> Lights
translateLights v lights
  = fromList (arrayShape lights) [ Light (p + v) c | Light p c <- toList lights ]

