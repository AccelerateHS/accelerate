{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -fno-warn-missing-methods #-}

module Vec3
  where

import Prelude
import Data.Typeable
import Data.Array.Accelerate
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Array.Sugar


-- | Points and vectors in 3D Space
--
type Position   = Vec3
type Direction  = Vec3
type Normal     = (Position, Direction)

-- | A parameterised point in XYZ space.
--
type Vec3       = XYZ Float

-- | Make a Vec3
--
makeVec3 :: Exp Float -> Exp Float -> Exp Float -> Exp Vec3
makeVec3 x y z = lift (XYZ x y z)


-- | Extract the components of the Vec3
--
xyzOfVec :: Exp Vec3 -> (Exp Float, Exp Float, Exp Float)
xyzOfVec v
  = let XYZ x y z       = unlift v
    in  (x, y, z)


-- | Yield the magnitude of a vector.
--
magnitude :: Exp Vec3 -> Exp Float
magnitude v
  = let XYZ x y z       = unlift v
    in
    sqrt (x * x + y * y + z * z)


-- | Normalise a vector to have unit length.
--
normalise :: Exp Vec3 -> Exp Vec3
normalise v = (1.0 / magnitude v) .* v


-- | Component-wise multiply a vector by a scalar.
--
infixl 7 .*
(.*) :: Exp Float -> Exp Vec3 -> Exp Vec3
(.*) s v
  = let XYZ x y z       = unlift v
    in
    makeVec3 (s * x) (s * y) (s * z)


-- | Compute the dot product of two vectors.
--
dot :: Exp Vec3 -> Exp Vec3 -> Exp Float
dot v1 v2
  = let XYZ x1 y1 z1    = unlift v1
        XYZ x2 y2 z2    = unlift v2
    in
    x1 * x2 + y1 * y2 + z1 * z2


-- | Clamp a vectors components to some minimum and maximum values.
--
clamp :: Exp Vec3 -> Exp Float -> Exp Float -> Exp Vec3
clamp v minVal maxVal =
  makeVec3 (go x) (go y) (go z)
  where
    XYZ x y z   = unlift v
    go u        = minVal `max` u `min` maxVal


-- | Clip a vector's components to some maximum value.
--
clip :: Exp Vec3 -> Exp Float -> Exp Vec3
clip v maxVal =
  makeVec3 (go x) (go y) (go z)
  where
    XYZ x y z   = unlift v
    go u        = u `min` maxVal


-- Get a Vec3 into Accelerate --------------------------------------------------

data XYZ a = XYZ a a a
  deriving (Eq, Show, Typeable)

type instance EltRepr  (XYZ a)  = EltRepr  (a, a, a)
type instance EltRepr' (XYZ a)  = EltRepr' (a, a, a)

instance Elt a => Elt (XYZ a) where
  eltType (_ :: XYZ a)  = eltType (undefined :: (a, a, a))
  toElt p               = let (x,y,z) = toElt p in XYZ x y z
  fromElt (XYZ x y z)   = fromElt (x, y, z)

  eltType' (_ :: XYZ a) = eltType (undefined :: (a, a, a))
  toElt' p              = let (x,y,z) = toElt p in XYZ x y z
  fromElt' (XYZ x y z)  = fromElt (x, y, z)

instance Elt a => IsProduct Elt (XYZ a) where
  type ProdRepr (XYZ a)  = ProdRepr (a, a, a)
  fromProd cst (XYZ x y z) = fromProd cst (x, y, z)
  toProd _ t               = let (x,y,z) = toTuple t in XYZ x y z

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (XYZ a) where
  type Plain (XYZ a) = XYZ (Plain a)
  lift (XYZ x y z)      = Exp . Tuple $ NilTup `SnocTup` lift x `SnocTup` lift y `SnocTup` lift z

instance Elt a => Unlift Exp (XYZ (Exp a)) where
  unlift t      = let x = Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t
                      y = Exp $ SuccTupIdx ZeroTupIdx `Prj` t
                      z = Exp $ ZeroTupIdx `Prj` t
                  in XYZ x y z

-- | Pretend a Vec3 is a number
--
instance Num a => Num (XYZ a) where
 (+) (XYZ x1 x2 x3) (XYZ y1 y2 y3)
        = XYZ (x1 + y1) (x2 + y2) (x3 + y3)

 (-) (XYZ x1 x2 x3) (XYZ y1 y2 y3)
        = XYZ (x1 - y1) (x2 - y2) (x3 - y3)

 (*) (XYZ x1 x2 x3) (XYZ y1 y2 y3)
        = XYZ (x1 * y1) (x2 * y2) (x3 * y3)


instance (Elt a, IsNum a) => Num (Exp (XYZ a)) where
  (+)   = lift2 ((+) :: XYZ (Exp a) -> XYZ (Exp a) -> XYZ (Exp a))
  (-)   = lift2 ((-) :: XYZ (Exp a) -> XYZ (Exp a) -> XYZ (Exp a))
  (*)   = lift2 ((*) :: XYZ (Exp a) -> XYZ (Exp a) -> XYZ (Exp a))

