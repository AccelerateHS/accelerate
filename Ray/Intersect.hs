{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Ray.Intersect
  where

-- friends
import Vec3

-- frenemies
import Data.Array.Accelerate                                    as A

-- standard library
import Prelude                                                  as P


-- | Of two intersection tests, take the nearest
--
nearest :: Exp (Bool, Float) -> Exp (Bool, Float) -> Exp (Bool, Float)
nearest x y
  = let
        -- hit test and distance for each object
        (h1, d1)        = unlift x
        (h2, d2)        = unlift y      :: (Exp Bool, Exp Float)
    in
    h1 &&* h2 ? ( lift (h1, min d1 d2), -- both hit, select the closest
    h1        ? ( x, y ) )              -- just the one that intersects


nearest' :: forall a. Elt a
         => Exp (Bool, Float, a)
         -> Exp (Bool, Float, a)
         -> Exp (Bool, Float, a)
nearest' x y
  = let
        (h1, d1, _ :: Exp a) = unlift x
        (h2, d2, _ :: Exp a) = unlift y
    in
    h1 &&* h2 ? ( d1 <* d2 ? (x, y)     -- both objects intersect; take the nearest
                , h1 ?       (x, y) )   -- only one object intersects


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
    :: Elt object
    => (Exp object -> Exp Position -> Exp Direction -> Exp (Bool, Float))
    -> Acc (Array DIM1 object)          -- objects to test
    -> Acc (Array DIM2 Position)        -- source of each ray
    -> Acc (Array DIM2 Direction)       -- direction of each ray
    -> Acc (Array DIM2 ( Bool           -- was there was an intersection?
                       , Float          -- distance to object from the point in the direction of the normal
                       , Int))          -- array index of the nearest intersected object
intersectRays distanceToObject objects points normals
  = let
        sh              = shape points
        n_obj           = unindex1 (shape objects)
        miss            = constant (False, 0, 0)

        intersects
          = let objs    = A.replicate (lift (sh  :. All))   objects
                pts     = A.replicate (lift (Any :. n_obj)) points
                dirs    = A.replicate (lift (Any :. n_obj)) normals
            in
            A.generate (lift (sh :. n_obj))
                       (\ix -> let _ :. i       = unlift ix :: Exp DIM2 :. Exp Int
                                   obj          = objs ! ix
                                   pt           = pts  ! ix
                                   dir          = dirs ! ix
                                   (hit, dist)  = unlift $ distanceToObject obj pt dir :: (Exp Bool, Exp Float)
                               in
                               lift (hit, dist, i))
     in
     A.fold nearest' miss intersects

