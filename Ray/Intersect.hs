{-# LANGUAGE TypeOperators #-}

module Ray.Intersect
  where

-- friends
import Vec3
import Scene.Object

-- frenemies
import Data.Array.Accelerate                                    as A

-- standard library
import Prelude                                                  as P


-- Determine the closest intersection point (if any) for a gang of rays.
--
-- This tests all objects for each ray and just takes the minimum. To scale to
-- larger scenes, this should use a spatial decomposition technique like a
-- bounding volume hierarchy to reduce the search space.
--
-- https://developer.nvidia.com/content/thinking-parallel-part-ii-tree-traversal-gpu
--
-- https://developer.nvidia.com/content/thinking-parallel-part-iii-tree-construction-gpu
--
intersectRays
    :: Acc Objects                              -- objects in the scene
    -> Acc (Array DIM2 Position)                -- source of each ray
    -> Acc (Array DIM2 Direction)               -- direction of each ray
    -> Acc (Array DIM2 (Bool, Float))           -- distance to the closest object
intersectRays objects points directions
  = let
        sh                      = shape points

        (spheres, planes)       = unlift objects
        n_sph                   = unindex1 (shape spheres)
        n_pln                   = unindex1 (shape planes)
        miss                    = constant (False, 0)

        -- Determine the nearest of two object intersections
        nearest i1 i2
          = let (h1, d1)        = unlift i1     -- hit test and distance for each object
                (h2, d2)        = unlift i2     :: (Exp Bool, Exp Float)
            in
            h1 &&* h2 ? ( lift (h1, min d1 d2), -- both hit, select the closest
            h1        ? ( i1, i2 ) )            -- just the one that intersects

        -- Intersections of all rays with the spheres and planes in the scene
        intersect_sph
          = A.zipWith3 distanceToSphere
                       (A.replicate (lift (sh  :. All))   spheres)
                       (A.replicate (lift (Any :. n_sph)) points)
                       (A.replicate (lift (Any :. n_sph)) directions)

        intersect_pln
          = A.zipWith3 distanceToPlane
                       (A.replicate (lift (sh  :. All))   planes)
                       (A.replicate (lift (Any :. n_pln)) points)
                       (A.replicate (lift (Any :. n_pln)) directions)
    in
    A.fold nearest miss (intersect_sph A.++ intersect_pln)

