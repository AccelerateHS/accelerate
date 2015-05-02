{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE IncoherentInstances   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Data.Complex
-- Copyright   : [2015] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Complex numbers
--
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
import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Array.Sugar
import qualified Data.Complex                   as C


type instance EltRepr (Complex a) = EltRepr (a, a)

instance Elt a => Elt (Complex a) where
  eltType _             = eltType (undefined :: (a,a))
  toElt p               = let (a, b) = toElt p in a :+ b
  fromElt (a :+ b)      = fromElt (a, b)

instance cst a => IsProduct cst (Complex a) where
  type ProdRepr (Complex a) = ProdRepr (a, a)
  fromProd cst (x :+ y) = fromProd cst (x, y)
  toProd cst p          = let (x, y) = toProd cst p in (x :+ y)
  prod cst _            = prod cst (undefined :: (a, a))

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
          (u, v)        = unlift ( x A.<* 0 ? ( lift (v',u'), lift (u',v') ) )
      in
      x ==* 0 &&* y ==* 0 ?
        {- then -} ( 0
        {- else -} , lift (u :+ (y A.<* 0 ? (-v,v))) )

  pi            = lift (pi :: Complex (Exp a))
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
mkPolar :: forall a. (Elt a, IsFloating a) => Exp a -> Exp a -> Exp (Complex a)
mkPolar = lift2 (C.mkPolar :: Exp a -> Exp a -> Complex (Exp a))

-- | @'cis' t@ is a complex value with magnitude @1@ and phase @t@ (modulo
-- @2*'pi'@).
--
cis :: forall a. (Elt a, IsFloating a) => Exp a -> Exp (Complex a)
cis = lift1 (C.cis :: Exp a -> Complex (Exp a))

-- | Return the real part of a complex number
--
real :: forall a. Elt a => Exp (Complex a) -> Exp a
real = lift1 (C.realPart :: Complex (Exp a) -> Exp a)

-- | Return the imaginary part of a complex number
--
imag :: forall a. Elt a => Exp (Complex a) -> Exp a
imag = lift1 (C.imagPart :: Complex (Exp a) -> Exp a)

-- | Return the complex conjugate of a complex number, defined as
--
-- > conjugate(Z) = X - iY
--
conjugate :: forall a. (Elt a, IsNum a) => Exp (Complex a) -> Exp (Complex a)
conjugate = lift1 (C.conjugate :: Complex (Exp a) -> Complex (Exp a))

