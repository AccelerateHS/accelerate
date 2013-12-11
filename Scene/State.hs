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


data State = State
  { _stateTime                  :: !Float
  , _stateEyePos                :: !Position
  , _stateEyeLoc                :: !Position

  , _stateLeftClick             :: !(Maybe G.Point)

  , _stateMoveSpeed             :: !Float
  , _stateMovingForward         :: !Bool
  , _stateMovingBackward        :: !Bool
  , _stateMovingLeft            :: !Bool
  , _stateMovingRight           :: !Bool

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
      , _stateMovingForward     = False
      , _stateMovingBackward    = False
      , _stateMovingLeft        = False
      , _stateMovingRight       = False

      , _stateObjects           = makeObjects time
      , _stateLights            = makeLights  time
      }


-- | Advance the world forward in time
--
advanceState :: Float -> State -> State
advanceState dt state
  = move stateMovingForward  (XYZ 0 0 ( speed * dt))
  $ move stateMovingBackward (XYZ 0 0 (-speed * dt))
  $ move stateMovingLeft     (XYZ (-speed * dt) 0 0)
  $ move stateMovingRight    (XYZ ( speed * dt) 0 0)
  $ setTime (get stateTime state + dt) state
  where
    speed       = get stateMoveSpeed state
    move f x    = if get f state then moveEyeLoc x else id


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

