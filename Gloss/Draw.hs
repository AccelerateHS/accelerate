
module Gloss.Draw
  where

-- friends
import Vec3
import Scene.Light
import Scene.Object
import Scene.State
import Ray.Trace

-- frenemies
import Data.Array.Accelerate                                    as A
import Graphics.Gloss.Accelerate.Data.Color.RGB
import qualified Graphics.Gloss.Accelerate.Raster.Field         as G

--library
import Prelude                                                  as P


-- | Prepare the state for rendering
--
prepareState :: State -> (Objects, Lights, Scalar Position)
prepareState state
  = let
        objects         = stateObjectsView state
        lights          = stateLightsView  state
        eyePos          = fromList Z [stateEyePos state]
    in
    (objects, lights, eyePos)


-- | Render a single pixel of the image
--
tracePixel
    :: Int
    -> Int
    -> Int
    -> Int
    -> Acc (Objects, Lights, Scalar Position)
    -> Exp G.Point
    -> Exp G.Color
tracePixel sizeX sizeY fov bounces state point
  = let
        sizeX'          = P.fromIntegral sizeX
        sizeY'          = P.fromIntegral sizeY
        aspect          = sizeX' / sizeY'
        fov'            = P.fromIntegral fov
        fovX            = fov' * aspect
        fovY            = fov'

        (x,y)           = G.xyOfPoint point

        ambient         = rawColor 0.3 0.3 0.3

        eyeDir          = normalise $ makeVec3 (x * fovX) ((-y) * fovY) 0 - eyePos
        eyePos          = the eyePos'
        (objects, lights, eyePos')
                        = unlift state

        (r,g,b)         = rgbOfColor $ traceRay bounces objects lights ambient eyePos eyeDir
    in
    G.rawColor r g b 1

