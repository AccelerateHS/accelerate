{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
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

import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Classes                                as A
import Data.Array.Accelerate.Language
import Data.Array.Accelerate.Lift
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type

import Prelude                                                      ( ($), undefined, fromInteger )
import Data.Complex                                                 ( Complex(..) )
import qualified Data.Complex                                       as C
import qualified Prelude                                            as P


type instance EltRepr (Complex a) = EltRepr (a, a)

instance Elt a => Elt (Complex a) where
  eltType _             = eltType (undefined :: (a,a))
  toElt p               = let (a, b) = toElt p in a :+ b
  fromElt (a :+ b)      = fromElt (a, b)
  eltFlavour _          = EltTuple

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

instance A.Eq a => A.Eq (Complex a) where
  x == y = let r1 :+ c1 = unlift x
               r2 :+ c2 = unlift y
           in  r1 == r2 && c1 == c2
  x /= y = let r1 :+ c1 = unlift x
               r2 :+ c2 = unlift y
           in  r1 /= r2 || c1 /= c2

instance A.RealFloat a => P.Num (Exp (Complex a)) where
  (+)           = lift2 ((+) :: Complex (Exp a) -> Complex (Exp a) -> Complex (Exp a))
  (-)           = lift2 ((-) :: Complex (Exp a) -> Complex (Exp a) -> Complex (Exp a))
  (*)           = lift2 ((*) :: Complex (Exp a) -> Complex (Exp a) -> Complex (Exp a))
  negate        = lift1 (negate :: Complex (Exp a) -> Complex (Exp a))
  signum        = lift1 (signum :: Complex (Exp a) -> Complex (Exp a))
  abs           = lift1 (abs :: Complex (Exp a) -> Complex (Exp a))
  fromInteger n = lift ((fromInteger n :: Exp a) :+ 0)

instance A.RealFloat a => P.Fractional (Exp (Complex a)) where
  c / c'
    = let x  :+ y       = unlift c
          x' :+ y'      = unlift c'     :: Complex (Exp a)
          den           = x' P.^ (2 :: Int) + y' P.^ (2 :: Int)
          re            = (x * x' + y * y') / den
          im            = (y * x' - x * y') / den
      in
      lift (re :+ im)

  fromRational x
    = lift ((fromRational x :: Exp a) :+ 0)

instance A.RealFloat a => P.Floating (Exp (Complex a)) where
  sqrt z
    = let
          x :+ y        = unlift z
          v'            = abs y / (u'*2)
          u'            = sqrt ((magnitude z + abs x) / 2)
          (u, v)        = unlift $ cond (x < 0) (lift (v',u')) (lift (u',v'))
      in
      cond (x == 0 && y == 0)
        {- then -} 0
        {- else -} (lift (u :+ (cond (y < 0) (-v) v)))

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

instance (A.FromIntegral a b, A.Num b) => A.FromIntegral a (Complex b) where
  fromIntegral x = lift (fromIntegral x :+ (0 :: Exp b))


-- | The non-negative magnitude of a complex number
--
magnitude :: RealFloat a => Exp (Complex a) -> Exp a
magnitude c =
  let r :+ i    = unlift c
  in sqrt (r*r + i*i)

-- | The phase of a complex number, in the range @(-'pi', 'pi']@. If the
-- magnitude is zero, then so is the phase.
--
phase :: RealFloat a => Exp (Complex a) -> Exp a
phase c =
  let x :+ y    = unlift c
  in atan2 y x

-- | The function 'polar' takes a complex number and returns a (magnitude,
-- phase) pair in canonical form: the magnitude is non-negative, and the phase
-- in the range @(-'pi', 'pi']@; if the magnitude is zero, then so is the phase.
--
polar :: RealFloat a => Exp (Complex a) -> Exp (a,a)
polar z =  lift (magnitude z, phase z)

-- | Form a complex number from polar components of magnitude and phase.
--
#if __GLASGOW_HASKELL__ <= 708
mkPolar :: forall a. RealFloat a => Exp a -> Exp a -> Exp (Complex a)
#else
mkPolar :: forall a. Floating a  => Exp a -> Exp a -> Exp (Complex a)
#endif
mkPolar = lift2 (C.mkPolar :: Exp a -> Exp a -> Complex (Exp a))

-- | @'cis' t@ is a complex value with magnitude @1@ and phase @t@ (modulo
-- @2*'pi'@).
--
#if __GLASGOW_HASKELL__ <= 708
cis :: forall a. RealFloat a => Exp a -> Exp (Complex a)
#else
cis :: forall a. Floating a  => Exp a -> Exp (Complex a)
#endif
cis = lift1 (C.cis :: Exp a -> Complex (Exp a))

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
conjugate :: Num a => Exp (Complex a) -> Exp (Complex a)
conjugate z = lift $ real z :+ (- imag z)

