
module Common.Util (

  magV,
  plusV,
  mulSV,
  normaliseV

) where

import Data.Array.Accelerate            as A

-- | The magnitude of a vector.
--
magV :: (Elt a, IsFloating a) => Exp (a, a) -> Exp a
magV v =
  let (x, y) = unlift v
  in  sqrt (x * x + y * y)


-- | Add two vectors component-wise
--
plusV :: (Elt a, IsNum a) => Exp (a, a) -> Exp (a, a) -> Exp (a, a)
plusV u v = lift ( A.fst u + A.fst v
                 , A.snd u + A.snd v )


-- | Multiply a vector by a scalar.
--
mulSV :: (Elt a, IsNum a) => Exp a -> Exp (a, a) -> Exp (a, a)
mulSV s v = lift ( s * A.fst v
                 , s * A.fst v )


-- | Normalise a vector, so it has a magnitude of 1.
--
normaliseV :: (Elt a, IsFloating a) => Exp (a, a) -> Exp (a, a)
normaliseV v = mulSV (1 / magV v) v

