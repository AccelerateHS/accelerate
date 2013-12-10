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
import Data.Array.Accelerate                                    as A
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Array.Sugar                        ( Elt(..), EltRepr, EltRepr' )
import Graphics.Gloss.Accelerate.Data.Color.RGB

-- standard library
import Prelude                                                  as P
import Data.Bits                                                ( xor )
import Data.Typeable


-- | All objects in the scene
--
type Objects = (Array DIM1 Sphere, Array DIM1 Plane)


-- | Objects in the world. Accelerate does not have sum types, so define each
--   object separately (and hope this works out...)
--
data Sphere = Sphere Position Float Color Float
  deriving (Eq, Show, Typeable)

spherePos    :: Exp Sphere -> Exp Position
sphereColor  :: Exp Sphere -> Exp Color
sphereShine  :: Exp Sphere -> Exp Float
sphereRadius :: Exp Sphere -> Exp Float


data Plane = Plane Position Direction Color Float
  deriving (Eq, Show, Typeable)

planePos    :: Exp Plane -> Exp Position
planeNormal :: Exp Plane -> Exp Direction
planeColor  :: Exp Plane -> Exp Color
planeShine  :: Exp Plane -> Exp Float


type PlaneCheck = Plane

planeCheckPos    :: Exp PlaneCheck -> Exp Position
planeCheckNormal :: Exp PlaneCheck -> Exp Direction
planeCheckShine  :: Exp PlaneCheck -> Exp Float


-- | Compute the distance to the surface of a sphere
--
distanceToSphere
    :: Exp Sphere               -- ^ Object to intersect
    -> Exp Position             -- ^ Ray cast from this point...
    -> Exp Direction            -- ^ ...along this direction
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
    miss ? ( lift (False, infinity)
           , lift (True,  magnitude sep - sqrt (radius * radius - d_cp * d_cp)) )


-- | Compute the distance to the surface of a Plane
--
distanceToPlane
    :: Exp Plane                -- ^ Plane to intersect
    -> Exp Position             -- ^ Ray cast from this point
    -> Exp Direction            -- ^ ...along this direction
    -> Exp (Bool, Float)        -- ^ Distance to intersection, if there is one
distanceToPlane plane origin direction
  = let
        pos             = planePos plane
        normal          = planeNormal plane
        theta           = direction `dot` normal        -- TLM: name?
    in
    theta >=* 0 ? ( lift (False, infinity)
                  , lift (True,  ((pos - origin) `dot` normal) / theta) )


-- | The maximum representable floating point value
--
infinity :: Exp Float
infinity = constant (encodeFloat m n)
  where
    a           = undefined :: Float
    b           = floatRadix a
    e           = floatDigits a
    (_, e')     = floatRange a
    m           = b ^ e - 1
    n           = e' - e


-- | Compute the surface normal of a sphere at a point
--
sphereNormal
    :: Exp Sphere
    -> Exp Position             -- ^ A point on the surface of the sphere
    -> Exp Direction            -- ^ Normal at that point
sphereNormal sphere point
  = normalise (point - spherePos sphere)


-- | A checkerboard pattern along the x/z axis
--
checkers :: Exp Position -> Exp Color
checkers pos
  = let
        (x,_,z) = xyzOfVec pos

        v1      = (A.truncate (x / 100) :: Exp Int32) `mod` 2
        v2      = (A.truncate (z / 100) :: Exp Int32) `mod` 2
        v3      = A.fromIntegral . boolToInt $ x <* 0.0
        v4      = A.fromIntegral . boolToInt $ z <* 0.0
    in
    v1 `xor` v2 `xor` v3 `xor` v4 ==* 1 {- True -}
      ? ( rawColor 1.0 1.0 1.0
        , rawColor 0.4 0.4 0.4 )


-- Get Objects into Accelerate -------------------------------------------------

-- Because the Object types aren't parameterised, the type of the individual
-- fields are fixed. Thus we can't unlift from (Exp Sphere) to a Sphere of Exp
-- things. Poop.
--
spherePos s    = Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` s
sphereRadius s = Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` s
sphereColor s  = Exp $ SuccTupIdx ZeroTupIdx `Prj` s
sphereShine s  = Exp $ ZeroTupIdx `Prj` s

planePos p    = Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` p
planeNormal p = Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` p
planeColor p  = Exp $ SuccTupIdx ZeroTupIdx `Prj` p
planeShine p  = Exp $ ZeroTupIdx `Prj` p

planeCheckPos    = planePos
planeCheckNormal = planeNormal
planeCheckShine  = planeShine


-- Sphere
-- ------

type instance EltRepr Sphere  = EltRepr (Position, Float, Color, Float)
type instance EltRepr' Sphere = EltRepr' (Position, Float, Color, Float)

instance Elt Sphere where
  eltType (_ :: Sphere)         = eltType (undefined :: (Position, Float, Color, Float))
  toElt sphere                  = let (p,r,c,s) = toElt sphere in Sphere p r c s
  fromElt (Sphere p r c s)      = fromElt (p, r, c, s)

  eltType' (_ :: Sphere)        = eltType' (undefined :: (Position, Float, Color, Float))
  toElt' sphere                 = let (p,r,c,s) = toElt' sphere in Sphere p r c s
  fromElt' (Sphere p r c s)     = fromElt' (p, r, c, s)

instance IsTuple Sphere where
  type TupleRepr Sphere = TupleRepr (Position, Float, Color, Float)
  fromTuple (Sphere p r c s)    = fromTuple (p, r, c, s)
  toTuple t                     = let (p, r, c, s) = toTuple t in Sphere p r c s

instance Lift Exp Sphere where
  type Plain Sphere = Sphere
  lift (Sphere p r c s)
      = Exp . Tuple
      $ NilTup `SnocTup` lift p `SnocTup` lift r `SnocTup` lift c `SnocTup` lift s


-- Plane
-- -----

type instance EltRepr Plane  = EltRepr (Position, Direction, Color, Float)
type instance EltRepr' Plane = EltRepr' (Position, Direction, Color, Float)

instance Elt Plane where
  eltType (_ :: Plane)          = eltType (undefined :: (Position, Direction, Color, Float))
  toElt plane                   = let (p,n,c,s) = toElt plane in Plane p n c s
  fromElt (Plane p n c s)       = fromElt (p, n, c, s)

  eltType' (_ :: Plane)         = eltType' (undefined :: (Position, Direction, Color, Float))
  toElt' plane                  = let (p,n,c,s) = toElt' plane in Plane p n c s
  fromElt' (Plane p n c s)      = fromElt' (p, n, c, s)

instance IsTuple Plane where
  type TupleRepr Plane = TupleRepr (Position, Direction, Color, Float)
  fromTuple (Plane p n c s)     = fromTuple (p, n, c, s)
  toTuple t                     = let (p, n, c, s) = toTuple t in Plane p n c s

instance Lift Exp Plane where
  type Plain Plane = Plane
  lift (Plane p n c s)
      = Exp . Tuple
      $ NilTup `SnocTup` lift p `SnocTup` lift n `SnocTup` lift c `SnocTup` lift s


{--
-- Checkered Plane
-- ---------------

type instance EltRepr PlaneCheck  = EltRepr (Position, Direction, Float)
type instance EltRepr' PlaneCheck = EltRepr' (Position, Direction, Float)

instance Elt PlaneCheck where
  eltType (_ :: PlaneCheck)     = eltType (undefined :: (Position, Direction, Float))
  toElt plane                   = let (p,n,s) = toElt plane in PlaneCheck p n s
  fromElt (PlaneCheck p n s)    = fromElt (p, n, s)

  eltType' (_ :: PlaneCheck)    = eltType' (undefined :: (Position, Direction, Float))
  toElt' plane                  = let (p,n,s) = toElt' plane in PlaneCheck p n s
  fromElt' (PlaneCheck p n s)   = fromElt' (p, n, s)

instance IsTuple PlaneCheck where
  type TupleRepr PlaneCheck = TupleRepr (Position, Direction, Float)
  fromTuple (PlaneCheck p n s)  = fromTuple (p, n, s)
  toTuple t                     = let (p, n, s) = toTuple t in PlaneCheck p n s

instance Lift Exp PlaneCheck where
  type Plain PlaneCheck = PlaneCheck
  lift (PlaneCheck p n s)
      = Exp . Tuple
      $ NilTup `SnocTup` lift p `SnocTup` lift n `SnocTup` lift s
--}

