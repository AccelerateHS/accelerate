
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
import qualified Graphics.Gloss                                 as G


data State = State
  { stateTime                   :: !Float
  , stateEyePos                 :: !Position
  , stateEyeLoc                 :: !Position

  , stateLeftClick              :: !(Maybe G.Point)

  , stateMoveSpeed              :: !Float
  , stateMovingForward          :: !Bool
  , stateMovingBackward         :: !Bool
  , stateMovingLeft             :: !Bool
  , stateMovingRight            :: !Bool

  , stateObjects                :: !Objects
  , stateObjectsView            :: !Objects

  , stateLights                 :: !Lights
  , stateLightsView             :: !Lights
  }
  deriving Show


-- | Initialise the world and interface state
--
initState :: Float -> State
initState time
  = State
      { stateTime               = time
      , stateEyePos             = XYZ 50    (-100) (-700)
      , stateEyeLoc             = XYZ (-50) 200   1296

      , stateLeftClick          = Nothing

      , stateMoveSpeed          = 400
      , stateMovingForward      = False
      , stateMovingBackward     = False
      , stateMovingLeft         = False
      , stateMovingRight        = False

      , stateObjects            = makeObjects time
      , stateObjectsView        = makeObjects time

      , stateLights             = makeLights time
      , stateLightsView         = makeLights time
      }


-- | Advance the world forward in time
--
advanceState :: Float -> State -> State
advanceState dt state
  = let
        time'   = stateTime state + dt

        speed   = stateMoveSpeed state
        move    = (if stateMovingForward state
                        then moveEyeLoc (XYZ 0 0 (-speed * dt))
                        else id)
                . (if stateMovingBackward state
                        then moveEyeLoc (XYZ 0 0 (speed * dt))
                        else id)
                . (if stateMovingLeft state
                        then moveEyeLoc (XYZ (speed * dt) 0 0)
                        else id)
                . (if stateMovingRight state
                        then moveEyeLoc (XYZ (-speed * dt) 0 0)
                        else id)
    in
    setTime time' $ move state


-- | Set the location of the eye
--
moveEyeLoc :: Position -> State -> State
moveEyeLoc v state = state { stateEyeLoc = stateEyeLoc state + v }

-- | Set the time of the world
--
setTime :: Float -> State -> State
setTime time state
  = let
        objects = makeObjects time
        lights  = makeLights  time
        eyeLoc  = stateEyeLoc state
    in
    state { stateTime             = time
          , stateObjectsView      = translateObjects eyeLoc objects
          , stateLightsView       = translateLights  eyeLoc lights
          }


translateObjects :: Position -> Objects -> Objects
translateObjects v (spheres, planes)
  = ( fromList (arrayShape spheres) [ Sphere (p + v) r c s | Sphere p r c s <- toList spheres ]
    , fromList (arrayShape planes)  [ Plane  (p + v) d c s | Plane  p d c s <- toList planes  ]
    )

translateLights :: Position -> Lights -> Lights
translateLights v lights
  = fromList (arrayShape lights) [ Light (p + v) c | Light p c <- toList lights ]

