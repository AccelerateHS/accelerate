
module Common.Util (

  magnitude, dot, normalise, vec,

  (.+.), (.-.), (.*.), (+.), (-.), (*.),

) where

import Common.Type
import Data.Array.Accelerate            as A

-- | The magnitude of a vector.
--
magnitude :: (Elt a, IsFloating a) => Exp (Vec a) -> Exp a
magnitude v = sqrt (dot v v)


-- | Dot product of a vector
--
dot :: (Elt a, IsNum a) => Exp (Vec a) -> Exp (Vec a) -> Exp a
dot v1 v2
  = let (x1,y1,z1) = unlift v1
        (x2,y2,z2) = unlift v2
    in
    x1 * x2 + y1 * y2 + z1 * z2


-- | Normalise a vector, so it has a magnitude of 1.
--
normalise :: (Elt a, IsFloating a) => Exp (Vec a) -> Exp (Vec a)
normalise v = (1 / magnitude v) *. v

-- | Replicate a value into a vector
--
vec :: Elt a => Exp a -> Exp (Vec a)
vec x = lift (x,x,x)

-- | Basic arithmetic component-wise
--
infixl 7 .*.
infixl 6 .+.
infixl 6 .-.

(.+.), (.-.), (.*.) :: (Elt a, IsNum a) => Exp (Vec a) -> Exp (Vec a) -> Exp (Vec a)
(.+.) = vzipWith (+)
(.-.) = vzipWith (-)
(.*.) = vzipWith (*)

-- | Apply a scalar value component-wise to each element of the vector
--
infixl 7 *.
infixl 6 +.
infixl 6 -.

(+.), (-.), (*.) :: (Elt a, IsNum a) => Exp a -> Exp (Vec a) -> Exp (Vec a)
(+.) c = vmap (c+)
(-.) c = vmap (c-)
(*.) c = vmap (c*)

-- | Arithmetic lifted to our vector type. As far as possible, want to gloss
--   over whether we are calculating in 2D or 3D.
--
vmap :: (Elt a, Elt b) => (Exp a -> Exp b) -> Exp (Vec a) -> Exp (Vec b)
vmap f v
  = let (x1,y1,z1) = unlift v
    in
    lift (f x1, f y1, f z1)

vzipWith :: (Elt a, Elt b, Elt c) => (Exp a -> Exp b -> Exp c) -> Exp (Vec a) -> Exp (Vec b) -> Exp (Vec c)
vzipWith f v1 v2
  = let (x1,y1,z1) = unlift v1
        (x2,y2,z2) = unlift v2
    in
    lift (f x1 x2, f y1 y2, f z1 z2)

