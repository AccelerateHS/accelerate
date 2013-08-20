{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Scene.Light
  where

-- friends
import Vec3
import Ray.Intersect
import Scene.Object

-- frenemies
import Data.Array.Accelerate                                    as A
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Array.Sugar                        ( Elt(..), EltRepr, EltRepr' )
import Graphics.Gloss.Accelerate.Data.Color

-- standard library
import Data.Typeable


-- | An omnidirectional point light source
--
data Light = Light Position Color
  deriving (Eq, Show, Typeable)

lightPos   :: Exp Light -> Exp Position
lightColor :: Exp Light -> Exp Color


-- | Compute the direct lighting at a particular point for a single light
--
colorOfLight
    :: Exp Light
    -> Exp Position
    -> Exp Direction
    -> Exp Color
colorOfLight light point normal
  = let
        (r,g,b,a)       = rgbaOfColor $ lightColor light
        lpoint          = lightPos light

        lp_l    = lpoint - point
        dir     = normalise lp_l                        -- vector from the light to the surface point
        dist    = magnitude lp_l                        -- distance from light to surface
        mag     = (normal `dot` dir) / (dist * dist)    -- magnitude of the reflection
    in
    rawColor (r * mag) (g * mag) (b * mag) a


-- | Get the colour at each position as a function of all lights acting on the
--   ray along the given direction.
--
applyLighting
    :: Acc Objects
    -> Acc (Vector Light)
    -> Acc (Array DIM2 Position)
    -> Acc (Array DIM2 Direction)
    -> Acc (Array DIM2 Color)
applyLighting objects lights points normals
  = let
        (spheres, planes)       = unlift objects
        sh                      = shape points
        n_l                     = unindex1 (shape lights)
        n_sph                   = unindex1 (shape spheres)
        n_pln                   = unindex1 (shape planes)
        miss                    = constant (False, 0)

        -- For all lights, what is the direction and distance from the light
        -- source to the surface?
        --
        -- TLM: we have multiple uses of this term, which means this replicated
        --      array will be created as a manifest array in memory. Disaster!
        --
        (dirLights, distLights)
          = A.unzip
          $ A.zipWith (\light point -> let lp_p = lightPos light - point
                                       in  lift (normalise lp_p, magnitude lp_p))
                      (A.replicate (lift (sh  :. All)) lights)
                      (A.replicate (lift (Any :. n_l)) points)

        -- For all lights, find the closest object to that light in the
        -- direction of the vector from the light to the point.
        obstructions
          = let dist_sph = A.zipWith3 distanceToSphere
                              (A.replicate (lift (sh  :. n_l :. All))   spheres)
                              (A.replicate (lift (Any :. n_l :. n_sph)) points)
                              (A.replicate (lift (Any        :. n_sph)) dirLights)

                dist_pln = A.zipWith3 distanceToPlane
                              (A.replicate (lift (sh  :. n_l :. All))   planes)
                              (A.replicate (lift (Any :. n_l :. n_pln)) points)
                              (A.replicate (lift (Any        :. n_pln)) dirLights)
            in
            A.fold nearest miss (dist_sph A.++ dist_pln)

        -- Check if there are any occluding objects between the light and the
        -- surface. If so that light does not contribute, otherwise calculate
        -- the magnitude of the reflected light.
        illumination
          = A.zipWith5 (\distLight obj light point normal ->
                          let (hits, distObj)   = unlift obj
                              occluded          = hits &&* distObj <* distLight
                          in
                          occluded ? ( black, colorOfLight light point normal ))
                       distLights
                       obstructions
                       (A.replicate (lift (sh  :. All)) lights)
                       (A.replicate (lift (Any :. n_l)) points)
                       (A.replicate (lift (Any :. n_l)) normals)
    in
    A.fold addColors black illumination


-- Get Lights into Accelerate --------------------------------------------------

lightPos l   = Exp $ SuccTupIdx ZeroTupIdx `Prj` l
lightColor l = Exp $ ZeroTupIdx `Prj` l

type instance EltRepr Light  = EltRepr (Position, Color)
type instance EltRepr' Light = EltRepr' (Position, Color)

instance Elt Light where
  eltType (_ :: Light)  = eltType (undefined :: (Position, Color))
  toElt light           = let (p,c) = toElt light in Light p c
  fromElt (Light p c)   = fromElt (p,c)

  eltType' (_ :: Light) = eltType' (undefined :: (Position, Color))
  toElt' light          = let (p,c) = toElt' light in Light p c
  fromElt' (Light p c)  = fromElt' (p,c)

instance IsTuple Light where
  type TupleRepr Light = TupleRepr (Position, Color)
  fromTuple (Light p c) = fromTuple (p,c)
  toTuple t             = let (p,c) = toTuple t in Light p c

instance Lift Exp Light where
  type Plain Light = Light
  lift (Light p c) = Exp . Tuple $ NilTup `SnocTup` lift p `SnocTup` lift c


