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
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Array.Sugar                        ( Elt(..), EltRepr, Tuple(..), fromTuple, toTuple )
import Graphics.Gloss.Accelerate.Data.Color.RGB

-- standard library
import Data.Typeable


-- | An omnidirectional point light source, whose intensity drops off with
--   distance from the source.
--
data Light = Light Position Color
  deriving (Eq, Show, Typeable)

type Lights = Array DIM1 Light

lightPos   :: Exp Light -> Exp Position
lightColor :: Exp Light -> Exp Color


-- | Compute the direct lighting contribution of all lights acting on a point on
-- a surface at a given normal direction.
--
--    normal
--         ^           x light
--         |        .
--         |     . objects?
--         |  .
--   ______x______ surface
--       point
--
applyLights
    :: Acc Objects
    -> Acc Lights
    -> Exp Position
    -> Exp Direction
    -> Exp Color
applyLights objects lights point normal
  = sfoldl (\c l -> c + applyLight objects point normal l) black (constant Z) lights


-- | Compute the direct lighting at a particular point for a single light
--
applyLight
    :: Acc Objects              -- possible occluding objects, used for shadows
    -> Exp Position             -- point which is being lit
    -> Exp Direction            -- surface normal at this point
    -> Exp Light                -- does this light contribute colour to this point?
    -> Exp Color
applyLight objects point normal light
  = let
        (spheres, planes)       = unlift objects

        -- What is the direction and distance from the light source to the point
        -- on the surface?
        --
        lp_p                    = lightPos light - point
        dist                    = magnitude lp_p
        dir                     = (1.0 / dist) .* lp_p

        -- Calculate the magnitude of the reflected light, if there are no
        -- occluding objects between the light and the surface point.
        --
        mag                     = (normal `dot` dir) / (dist * dist)
        (r, g, b)               = rgbOfColor (lightColor light)
        refl                    = rawColor (r * mag) (g * mag) (b * mag)
    in
    checkRay distanceToSphere spheres point dir dist ||* checkRay distanceToPlane planes point dir dist
      ? ( black, refl )



-- Get Lights into Accelerate --------------------------------------------------

lightPos l   = Exp $ SuccTupIdx ZeroTupIdx `Prj` l
lightColor l = Exp $ ZeroTupIdx `Prj` l

type instance EltRepr Light = EltRepr (Position, Color)

instance Elt Light where
  eltType (_ :: Light)  = eltType (undefined :: (Position, Color))
  toElt light           = let (p,c) = toElt light in Light p c
  fromElt (Light p c)   = fromElt (p,c)

instance IsProduct Elt Light where
  type ProdRepr Light = ProdRepr (Position, Color)
  fromProd _ (Light p c) = fromTuple (p,c)
  toProd _ t             = let (p,c) = toTuple t in Light p c
  prod cst _             = prod cst (undefined :: (Position, Color))

instance Lift Exp Light where
  type Plain Light = Light
  lift (Light p c) = Exp . Tuple $ NilTup `SnocTup` lift p `SnocTup` lift c

