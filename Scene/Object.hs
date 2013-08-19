{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Scene.Object
  where

-- friends
import Vec3

-- frenemies
import Data.Array.Accelerate                                    hiding ( Vector )
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Array.Sugar                        hiding ( Vector )
import Graphics.Gloss.Accelerate.Data.Color

-- standard library
import Data.Typeable


-- | Objects in the world. Accelerate does not have sum types, so define each
--   object separately (and hope this works out...)
--
data Sphere = Sphere Point Float Color Float
  deriving (Eq, Show, Typeable)

{--
data Plane
  = Plane
      { planePos                :: Point
      , planeNormal             :: Vector
      , planeColor              :: Color
      , planeShine              :: Float
      }
  deriving (Eq, Show, Typeable)

data PlaneCheck
  = PlaneCheck
      { planeCheckPos           :: Point
      , planeCheckNormal        :: Vector
      , planeCheckShine         :: Float
      }
  deriving (Eq, Show, Typeable)
--}

-- | Compute the distance to the surface of various objects
--
distanceToSphere
    :: Exp Sphere               -- ^ Object to intersect
    -> Exp Point                -- ^ Ray cast from this point...
    -> Exp Vector               -- ^ ...along this direction
    -> Exp (Bool, Float)        -- ^ Distance to intersection, if there is one
distanceToSphere sphere origin direction
  = let
        pos     = spherePos sphere
        radius  = sphereRadius sphere

        p       = origin + ((pos - origin) `dot` direction) .* direction
        d_cp    = magnitude (p - pos)
        sep     = p - origin
        miss    = d_cp >=* radius ||* sep `dot` direction <=* 0
    in
    miss ? ( lift (False, constant 0)
           , lift (True,  magnitude sep - sqrt (radius * radius - d_cp * d_cp)) )


-- Get Objects into Accelerate -------------------------------------------------

type instance EltRepr Sphere  = EltRepr (Point, Float, Color, Float)
type instance EltRepr' Sphere = EltRepr' (Point, Float, Color, Float)

instance Elt Sphere where
  eltType (_ :: Sphere)         = eltType (undefined :: (Point, Float, Color, Float))
  toElt sphere                  = let (p,r,c,s) = toElt sphere in Sphere p r c s
  fromElt (Sphere p r c s)      = fromElt (p, r, c, s)

  eltType' (_ :: Sphere)        = eltType' (undefined :: (Point, Float, Color, Float))
  toElt' sphere                 = let (p,r,c,s) = toElt' sphere in Sphere p r c s
  fromElt' (Sphere p r c s)     = fromElt' (p, r, c, s)

instance IsTuple Sphere where
  type TupleRepr Sphere = TupleRepr (Point, Float, Color, Float)
  fromTuple (Sphere p r c s)    = fromTuple (p, r, c, s)
  toTuple t                     = let (p, r, c, s) = toTuple t in Sphere p r c s

instance Lift Exp Sphere where
  type Plain Sphere = Sphere
  lift (Sphere p r c s)
      = Exp . Tuple
      $ NilTup `SnocTup` lift p `SnocTup` lift r `SnocTup` lift c `SnocTup` lift s

-- Because the Sphere type isn't parameterised, the type of the individual
-- fields are fixed, we can't unlift from (Exp Sphere) to a Sphere of Exp
-- things. Poop.
--
spherePos :: Exp Sphere -> Exp Point
spherePos s = Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` s

sphereRadius :: Exp Sphere -> Exp Float
sphereRadius s = Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` s

sphereColor :: Exp Sphere -> Exp Color
sphereColor s = Exp $ SuccTupIdx ZeroTupIdx `Prj` s

sphereShine :: Exp Sphere -> Exp Float
sphereShine s = Exp $ ZeroTupIdx `Prj` s

