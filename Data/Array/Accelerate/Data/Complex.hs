{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE IncoherentInstances   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS -fno-warn-orphans #-}

module Data.Array.Accelerate.Data.Complex (

  -- * Rectangular from
  Complex(..),
  real,
  imag,

  -- * Polar form
  mkPolar,
  cis,
  polar,
  magnitude,
  phase,

  -- * Conjugate
  conjugate,

) where

import Prelude
import Data.Complex                             ( Complex(..) )
import Data.Array.Accelerate
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Array.Sugar


type instance EltRepr  (Complex a) = (EltRepr a, EltRepr' a)
type instance EltRepr' (Complex a) = (EltRepr a, EltRepr' a)

instance Elt a => Elt (Complex a) where
  eltType (_::Complex a)        = eltType (undefined :: (a,a))
  toElt (a,b)                   = toElt a :+ toElt' b
  fromElt (a :+ b)              = (fromElt a, fromElt' b)

  eltType' (_::Complex a)       = eltType' (undefined :: (a,a))
  toElt' (a,b)                  = toElt a :+ toElt' b
  fromElt' (a :+ b)             = (fromElt a, fromElt' b)

  eltRep (_::Complex a) = error "Complex is a custom type, not supported by closed eltRep"

instance IsTuple (Complex a) where
  type TupleRepr (Complex a) = (((), a), a)
  fromTuple (x :+ y)    = (((), x), y)
  toTuple (((), x), y)  = (x :+ y)

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Complex a) where
  type Plain (Complex a) = Complex (Plain a)
  lift (x1 :+ x2)       = Exp $ Tuple (NilTup `SnocTup` lift x1 `SnocTup` lift x2)

instance Elt a => Unlift Exp (Complex (Exp a)) where
  unlift e
    = let x     = Exp $ SuccTupIdx ZeroTupIdx `Prj` e
          y     = Exp $ ZeroTupIdx `Prj` e
      in
      x :+ y

instance (Elt a, IsFloating a) => Num (Exp (Complex a)) where
  (+)           = lift2 ((+) :: Complex (Exp a) -> Complex (Exp a) -> Complex (Exp a))
  (-)           = lift2 ((-) :: Complex (Exp a) -> Complex (Exp a) -> Complex (Exp a))
  (*)           = lift2 ((*) :: Complex (Exp a) -> Complex (Exp a) -> Complex (Exp a))
  negate        = lift1 (negate :: Complex (Exp a) -> Complex (Exp a))
  signum        = lift1 (signum :: Complex (Exp a) -> Complex (Exp a))
  abs           = lift1 (abs :: Complex (Exp a) -> Complex (Exp a))
  fromInteger n = lift (constant (fromInteger n) :+ 0)


instance (Elt a, IsFloating a) => Fractional (Exp (Complex a)) where
  c / c'
    = let x  :+ y       = unlift c
          x' :+ y'      = unlift c'     :: Complex (Exp a)
          den           = x'^(2 :: Int) + y'^(2 :: Int)
          re            = (x * x' + y * y') / den
          im            = (y * x' - x * y') / den
      in
      lift (re :+ im)

  fromRational x
    = lift (constant (fromRational x) :+ constant 0)


instance (Elt a, IsFloating a, RealFloat a) => Floating (Exp (Complex a)) where
  sqrt z
    = let
          x :+ y        = unlift z
          v'            = abs y / (u'*2)
          u'            = sqrt ((magnitude z + abs x) / 2)
          (u, v)        = unlift ( x <* 0 ? ( lift (v',u'), lift (u',v') ) )
      in
      x ==* 0 &&* y ==* 0 ?
        {- then -} ( 0
        {- else -} , lift (u :+ (y <* 0 ? (-v,v))) )

  pi            = lift (pi :+ constant 0)
  log z         = lift (log (magnitude z) :+ phase z)
  exp           = lift1 (exp :: Complex (Exp a) -> Complex (Exp a))
  sin           = lift1 (sin :: Complex (Exp a) -> Complex (Exp a))
  cos           = lift1 (cos :: Complex (Exp a) -> Complex (Exp a))
  tan           = lift1 (tan :: Complex (Exp a) -> Complex (Exp a))
  sinh          = lift1 (sinh :: Complex (Exp a) -> Complex (Exp a))
  cosh          = lift1 (cosh :: Complex (Exp a) -> Complex (Exp a))
  tanh          = lift1 (tanh :: Complex (Exp a) -> Complex (Exp a))
  asin          = lift1 (asin :: Complex (Exp a) -> Complex (Exp a))
  acos          = lift1 (acos :: Complex (Exp a) -> Complex (Exp a))
  atan          = lift1 (atan :: Complex (Exp a) -> Complex (Exp a))
  asinh         = lift1 (asinh :: Complex (Exp a) -> Complex (Exp a))
  acosh         = lift1 (acosh :: Complex (Exp a) -> Complex (Exp a))
  atanh         = lift1 (atanh :: Complex (Exp a) -> Complex (Exp a))


-- | The non-negative magnitude of a complex number
--
magnitude :: (Elt a, IsFloating a) => Exp (Complex a) -> Exp a
magnitude c =
  let r :+ i    = unlift c
  in sqrt (r*r + i*i)

-- | The phase of a complex number, in the range @(-'pi', 'pi']@. If the
-- magnitude is zero, then so is the phase.
--
phase :: (Elt a, IsFloating a) => Exp (Complex a) -> Exp a
phase c =
  let x :+ y    = unlift c
  in atan2 y x

-- | The function 'polar' takes a complex number and returns a (magnitude,
-- phase) pair in canonical form: the magnitude is non-negative, and the phase
-- in the range @(-'pi', 'pi']@; if the magnitude is zero, then so is the phase.
--
polar :: (Elt a, IsFloating a) => Exp (Complex a) -> Exp (a,a)
polar z =  lift (magnitude z, phase z)

-- | Form a complex number from polar components of magnitude and phase.
--
mkPolar :: (Elt a, IsFloating a) => Exp a -> Exp a -> Exp (Complex a)
mkPolar r theta  =  lift $ r * cos theta :+ r * sin theta

-- | @'cis' t@ is a complex value with magnitude @1@ and phase @t@ (modulo
-- @2*'pi'@).
--
cis :: (Elt a, IsFloating a) => Exp a -> Exp (Complex a)
cis theta = lift $ cos theta :+ sin theta

-- | Return the real part of a complex number
--
real :: Elt a => Exp (Complex a) -> Exp a
real c =
  let r :+ _    = unlift c
  in  r

-- | Return the imaginary part of a complex number
--
imag :: Elt a => Exp (Complex a) -> Exp a
imag c =
  let _ :+ i    = unlift c
  in  i

-- | Return the complex conjugate of a complex number, defined as
--
-- > conjugate(Z) = X - iY
--
conjugate :: (Elt a, IsNum a) => Exp (Complex a) -> Exp (Complex a)
conjugate z = lift $ real z :+ (- imag z)

