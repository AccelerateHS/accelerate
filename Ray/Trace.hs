
module Ray.Trace
  where

-- friends
import Vec3

-- frenemies
import Data.Array.Accelerate                                    as A
import Graphics.Gloss.Accelerate.Data.Point

-- standard library
import Prelude                                                  as P


-- Generate all of the rays that will be cast from the given eye position to
-- cover the entire field of view.
--
castViewRays
    :: Int                              -- width of the display
    -> Int                              -- height
    -> Int                              -- field of view
    -> Position                         -- eye position
    -> Acc (Array DIM2 Direction)       -- all rays originating from the eye position
castViewRays sizeX sizeY fov eyePos
  = let
        sizeX'          = P.fromIntegral sizeX
        sizeY'          = P.fromIntegral sizeY
        aspect          = sizeX' / sizeY'
        fov'            = P.fromIntegral fov
        fovX            = fov' * aspect
        fovY            = fov'
    in
    A.generate (constant (Z :. sizeY :. sizeX))
               (\ix -> let (x, y) = xyOfPoint $ pointOfIndex sizeX sizeY ix
                       in  normalise $ makeVec3 (x * fovX) (-y * fovY) 0 - constant eyePos)

