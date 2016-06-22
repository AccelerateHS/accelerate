
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
import Data.Array.Accelerate.Data.Colour.RGB                    as RGB
import qualified Data.Array.Accelerate.Data.Colour.RGBA         as RGBA

import Graphics.Gloss.Accelerate.Data.Point

--library
import Prelude                                                  as P
import Data.Label                                               ( get )


-- | Prepare the state for rendering
--
prepareState :: State -> (Objects, Lights, Scalar Position)
prepareState state
  = let
        objects         = get stateObjects state
        lights          = get stateLights  state
        eyePos          = fromList Z [get stateEyePos state]
    in
    (objects, lights, eyePos)


-- | Render a single pixel of the image
--
tracePixel
    :: Int
    -> Int
    -> Int
    -> Int
    -> Exp Colour
    -> Acc (Objects, Lights, Scalar Position)
    -> Exp Point
    -> Exp RGBA.Colour
tracePixel sizeX sizeY fov bounces ambient state point
  = let
        sizeX'          = P.fromIntegral sizeX
        sizeY'          = P.fromIntegral sizeY
        aspect          = sizeX' / sizeY'
        fov'            = P.fromIntegral fov
        fovX            = fov' * aspect
        fovY            = fov'

        (x,y)           = xyOfPoint point

        eyeDir          = normalise $ makeVec3 (x * fovX) ((-y) * fovY) 0 - eyePos
        eyePos          = the eyePos'
        (objects, lights, eyePos')
                        = unlift state

        RGB r g b       = unlift $ traceRay bounces objects lights ambient eyePos eyeDir
    in
    RGBA.rgba r g b 1

