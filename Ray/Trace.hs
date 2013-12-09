{-# LANGUAGE ScopedTypeVariables #-}

module Ray.Trace
  where

-- friends
import Vec3
import Scene.Object
import Scene.Light
import Ray.Intersect

-- frenemies
import Data.Array.Accelerate                                    as A
import Graphics.Gloss.Accelerate.Data.Color.RGB
import Graphics.Gloss.Accelerate.Data.Point

-- standard library
import Prelude                                                  as P


-- | Generate all of the rays that will be cast from the given eye position to
--   cover the entire field of view.
--
castViewRays
    :: Int                              -- width of the display
    -> Int                              -- height
    -> Int                              -- field of view
    -> Exp Position                     -- eye position
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
                       in  normalise $ makeVec3 (x * fovX) ((-y) * fovY) 0 - eyePos)


-- | Cast rays into the scene
--
traceRays
    :: Int                              -- ^ maximum reflection count
    -> Exp Color                        -- ^ ambient light in the scene
    -> Acc Objects                      -- ^ objects in scene
    -> Acc Lights                       -- ^ lights in scene
    -> Acc (Array DIM2 Position)        -- ^ positions in the scene under consideration
    -> Acc (Array DIM2 Direction)       -- ^ surface normal at each point under consideration
    -> Acc (Array DIM2 Color)           -- ^ resultant colour at each point
traceRays lIMIT ambient objects lights = go 0
  where
    (spheres, planes)   = unlift objects

    -- The maximum number of reflections that we consider. Don't go overboard
    -- because (a) no (fine-grained) iteration, and (b) give up in case we've
    -- found two parallel mirrors.
    --
    go bounces points _ | bounces >= lIMIT
      = A.fill (shape points) black

    -- See if this bounce intersects any objects. If so, add the contribution to
    -- colour of that object and continue in the new direction, otherwise this
    -- ray contributes black.
    --
    go bounces points directions
      = let
            isect_sph   = intersectRays distanceToSphere spheres points directions
            isect_pln   = intersectRays distanceToPlane  planes  points directions

            -- If the ray hits an object, get the new surface normal, point of
            -- intersection, colour and shine of the object. If we did not get a
            -- hit, return the old values.
            --
            (hits, normals, colors, shines, points', directions')
              = A.unzip6
              $ A.zipWith4 (\orig dir sph pln ->
                    let (h1, d1, i1)            = unlift sph
                        (h2, d2, i2)            = unlift pln

                        defaults                = lift (dir, black, constant 0)
                        hit_sph                 = h1 ? (hitSphere     (spheres ! index1 i1) d1 orig dir, defaults)
                        hit_pln                 = h2 ? (hitPlaneCheck (planes  ! index1 i2) d2 orig dir, defaults)

                        (hit, dist, x)          = unlift $ nearest' (lift (h1, d1, hit_sph)) (lift (h2, d2, hit_pln))
                        (normal, color, shine)  = unlift x

                        dir'                    = hit ? ( dir - (2.0 * normal `dot` dir) .* normal , dir )
                        point'                  = orig + dist .* dir

                        -- help the type checker...
                        x     :: Exp (Direction, Color, Float)
                        shine :: Exp Float
                        color :: Exp Color
                    in
                    lift ( hit, normal, color, shine, point', dir' ))
                  points directions isect_sph isect_pln

            -- Determine the direct lighting at this point
            directs     = applyLighting objects lights points' normals

            -- See if the ray hits anything else
            refls       = go (bounces + 1) points' directions'

            -- The outgoing light is the incoming light modified by surface
            -- color. This is also clipped in case the sum of all incoming
            -- lights is too bright to display.
            light_outs  = mask hits (A.fill (shape points) black)
                        $ A.zipWith4 (\direct refl color shine ->
                                         let lighting    = direct + ambient
                                             light_in    = mixColors shine (1 - shine) refl lighting
                                         in  clampColor (light_in * color))
                            directs refls colors shines
        in
        light_outs


mask :: Elt a
     => Acc (Array DIM2 Bool)
     -> Acc (Array DIM2 a)              -- default values
     -> Acc (Array DIM2 a)              -- values if there was a hit
     -> Acc (Array DIM2 a)
mask = A.zipWith3 (\hit y x -> hit ? (x, y))


hitSphere :: Exp Sphere -> Exp Float -> Exp Position -> Exp Direction -> Exp (Direction, Color, Float)
hitSphere sph dist orig dir
  = let
        point   = orig + dist .* dir
        normal  = sphereNormal sph point
        color   = sphereColor sph
        shine   = sphereShine sph
    in
    lift (normal, color, shine)

hitPlane :: Exp Plane -> Exp Float -> Exp Position -> Exp Direction -> Exp (Direction, Color, Float)
hitPlane pln dist orig dir
  = let
        _point  = orig + dist .* dir
        normal  = planeNormal pln
        color   = planeColor pln
        shine   = planeShine pln
    in
    lift (normal, color, shine)

hitPlaneCheck :: Exp PlaneCheck -> Exp Float -> Exp Position -> Exp Direction -> Exp (Direction, Color, Float)
hitPlaneCheck pln dist orig dir
  = let
        point   = orig + dist .* dir
        normal  = planeCheckNormal pln
        color   = checkers point
        shine   = planeCheckShine pln
    in
    lift (normal, color, shine)



