{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Ray.Intersect
  where

-- friends
import Common.Type
import Scene.Object

-- frenemies
import Data.Array.Accelerate                                    as A


-- | Of two intersection tests, take the nearest.
--
nearest :: forall a. Elt a
        => Exp (Bool, Float, a)
        -> Exp (Bool, Float, a)
        -> Exp (Bool, Float, a)
nearest x y
  = let
        (h1, d1, _ :: Exp a) = unlift x
        (h2, d2, _ :: Exp a) = unlift y
    in
    h1 && h2 ? ( d1 < d2 ? (x, y)         -- both objects intersect; take the nearest
               , h1 ?      (x, y) )       -- only one object intersects


-- | Find the nearest point of intersection for a ray. If there is a hit, then
-- return the origin and normal of the new reflected ray, as well as the colour
-- and shine of the surface that was hit.
--
-- This tests all objects for each ray and just takes the minimum. To scale to
-- larger scenes, this should use a spatial decomposition technique like a
-- bounding volume hierarchy to reduce the search space.
--
-- https://developer.nvidia.com/content/thinking-parallel-part-ii-tree-traversal-gpu
--
-- https://developer.nvidia.com/content/thinking-parallel-part-iii-tree-construction-gpu
--
castRay
    :: forall object. Elt object
    => (Exp object -> Exp Position -> Exp Direction -> Exp (Bool, Float))
    -> Exp object                       -- dummy object (because we have no Maybe types)
    -> Acc (Vector object)              -- objects to test
    -> Exp Position                     -- ray origin
    -> Exp Direction                    -- ray direction
    -> Exp (Bool, Float, object)
castRay distanceTo  dummy objects orig dir
  = sfoldl (\s o -> let (_,   dist, _)  = unlift s      :: (Exp Bool, Exp Float, Exp object)
                        (hit, dist')    = unlift $ distanceTo o orig dir
                    in
                    hit && dist' < dist ? (lift (hit, dist', o), s))
           (lift (False, infinity, dummy))
           (constant Z)
           objects


-- | Check where there is some object closer than a given minimum distance. We
--   stop as soon as there is an intersection.
--
checkRay
    :: Elt object
    => (Exp object -> Exp Position -> Exp Direction -> Exp (Bool, Float))
    -> Acc (Vector object)              -- objects to test
    -> Exp Position                     -- ray origin
    -> Exp Direction                    -- ray direction
    -> Exp Float                        -- minimum distance
    -> Exp Bool
checkRay distanceTo objs orig dir dist
  = fst $ while (\s -> let (hit, i) = unlift s in not hit && i < unindex1 (shape objs))
                (\s -> let i        = snd s
                           (hit, dist') = unlift $ distanceTo (objs ! index1 i) orig dir
                       in  hit && dist' < dist ? (lift (True, i), lift (False, i+1)))
                (constant (False, 0))

