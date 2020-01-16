{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Data.Complex
-- Copyright   : [2015..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Complex numbers, stored in the usual C-style array-of-struct representation,
-- for easy interoperability.
--
module Data.Array.Accelerate.Data.Complex (

  -- * Rectangular from
  Complex(..), pattern (::+),
  real,
  imag,

  -- * Polar form
  mkPolar,
  cis,
  polar,
  magnitude, magnitude',
  phase,

  -- * Conjugate
  conjugate,

) where

import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Classes
import Data.Array.Accelerate.Data.Functor
import Data.Array.Accelerate.Pattern
import Data.Array.Accelerate.Prelude
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type

import Data.Complex                                                 ( Complex(..) )
import qualified Data.Complex                                       as C
import qualified Prelude                                            as P


infix 6 ::+
pattern (::+) :: (Elt a, Elt (Complex a)) => Exp a -> Exp a -> Exp (Complex a)
pattern r ::+ c = Pattern (r, c)
{-# COMPLETE (::+) #-}


-- Use an array-of-structs representation for complex numbers. This matches the
-- standard C-style layout, but means that we can define instances only at
-- specific types (not for any type 'a') as we can only have vectors of
-- primitive type.
--

instance Elt (Complex Half) where
  type EltRepr (Complex Half) = V2 Half
  {-# INLINE eltType     #-}
  {-# INLINE [1] toElt   #-}
  {-# INLINE [1] fromElt #-}
  eltType          = TypeRscalar scalarType
  toElt (V2 r i)   = r :+ i
  fromElt (r :+ i) = V2 r i

instance Elt (Complex Float) where
  type EltRepr (Complex Float) = V2 Float
  {-# INLINE eltType     #-}
  {-# INLINE [1] toElt   #-}
  {-# INLINE [1] fromElt #-}
  eltType          = TypeRscalar scalarType
  toElt (V2 r i)   = r :+ i
  fromElt (r :+ i) = V2 r i

instance Elt (Complex Double) where
  type EltRepr (Complex Double) = V2 Double
  {-# INLINE eltType     #-}
  {-# INLINE [1] toElt   #-}
  {-# INLINE [1] fromElt #-}
  eltType          = TypeRscalar scalarType
  toElt (V2 r i)   = r :+ i
  fromElt (r :+ i) = V2 r i

instance Elt (Complex CFloat) where
  type EltRepr (Complex CFloat) = V2 Float
  {-# INLINE eltType     #-}
  {-# INLINE [1] toElt   #-}
  {-# INLINE [1] fromElt #-}
  eltType                        = TypeRscalar scalarType
  toElt (V2 r i)                 = CFloat r :+ CFloat i
  fromElt (CFloat r :+ CFloat i) = V2 r i

instance Elt (Complex CDouble) where
  type EltRepr (Complex CDouble) = V2 Double
  {-# INLINE eltType     #-}
  {-# INLINE [1] toElt   #-}
  {-# INLINE [1] fromElt #-}
  eltType                          = TypeRscalar scalarType
  toElt (V2 r i)                   = CDouble r :+ CDouble i
  fromElt (CDouble r :+ CDouble i) = V2 r i

instance cst a => IsProduct cst (Complex a) where
  type ProdRepr (Complex a) = ProdRepr (a,a)
  fromProd (r :+ i) = fromProd @cst (r,i)
  toProd p          = let (r,i) = toProd @cst p in (r :+ i)
  prod              = prod @cst @(a,a)

instance (Lift Exp a, Elt (Plain a), Elt (Complex (Plain a))) => Lift Exp (Complex a) where
  type Plain (Complex a) = Complex (Plain a)
  lift (r :+ i) = lift r ::+ lift i

instance (Elt a, Elt (Complex a)) => Unlift Exp (Complex (Exp a)) where
  unlift (r ::+ i) = r :+ i


instance (Eq a, Elt (Complex a)) => Eq (Complex a) where
  r1 ::+ c1 == r2 ::+ c2 = r1 == r2 && c1 == c2
  r1 ::+ c1 /= r2 ::+ c2 = r1 /= r2 || c1 /= c2

instance (RealFloat a, Elt (Complex a)) => P.Num (Exp (Complex a)) where
  (+)    = lift2 ((+) :: Complex (Exp a) -> Complex (Exp a) -> Complex (Exp a))
  (-)    = lift2 ((-) :: Complex (Exp a) -> Complex (Exp a) -> Complex (Exp a))
  (*)    = lift2 ((*) :: Complex (Exp a) -> Complex (Exp a) -> Complex (Exp a))
  negate = lift1 (negate :: Complex (Exp a) -> Complex (Exp a))
  signum z@(x ::+ y) =
    if z == 0
       then z
       else let r = magnitude z
             in x/r ::+ y/r
  abs z         = magnitude z ::+ 0
  fromInteger n = fromInteger n ::+ 0

instance (RealFloat a, Elt (Complex a)) => P.Fractional (Exp (Complex a)) where
  fromRational x  = fromRational x ::+ 0
  z / z'          = (x*x''+y*y'') / d ::+ (y*x''-x*y'') / d
    where
      x  :+ y   = unlift z
      x' :+ y'  = unlift z'
      --
      x'' = scaleFloat k x'
      y'' = scaleFloat k y'
      k   = - max (exponent x') (exponent y')
      d   = x'*x'' + y'*y''

instance (RealFloat a, Elt (Complex a)) => P.Floating (Exp (Complex a)) where
  pi                = pi ::+ 0
  exp (x ::+ y)     = let expx = exp x
                       in expx * cos y ::+ expx * sin y
  log z             = log (magnitude z) ::+ phase z
  sqrt z@(x ::+ y)  =
    if z == 0
      then 0
      else u ::+ (y < 0 ? (-v, v))
    where
      T2 u v = x < 0 ? (T2 v' u', T2 u' v')
      v'     = abs y / (u'*2)
      u'     = sqrt ((magnitude z + abs x) / 2)

  x ** y =
    if y == 0 then 1 else
    if x == 0 then if exp_r > 0 then 0 else
                   if exp_r < 0 then inf ::+ 0
                                else nan ::+ nan
              else if isInfinite r || isInfinite i
                     then if exp_r > 0 then inf ::+ 0 else
                          if exp_r < 0 then 0
                                       else nan ::+ nan
                     else exp (log x * y)
    where
      r     ::+ i  = x
      exp_r ::+ _  = y
      --
      inf = 1 / 0
      nan = 0 / 0

  sin (x ::+ y)  = sin x * cosh y ::+ cos x * sinh y
  cos (x ::+ y)  = cos x * cosh y ::+ (- sin x * sinh y)
  tan (x ::+ y)  = (sinx*coshy ::+ cosx*sinhy) / (cosx*coshy ::+ (-sinx*sinhy))
    where
      sinx  = sin x
      cosx  = cos x
      sinhy = sinh y
      coshy = cosh y

  sinh (x ::+ y) = cos y * sinh x ::+ sin  y * cosh x
  cosh (x ::+ y) = cos y * cosh x ::+ sin y * sinh x
  tanh (x ::+ y) = (cosy*sinhx ::+ siny*coshx) / (cosy*coshx ::+ siny*sinhx)
    where
      siny  = sin y
      cosy  = cos y
      sinhx = sinh x
      coshx = cosh x

  asin z@(x ::+ y) = y' ::+ (-x')
    where
      x' ::+ y' = log (((-y) ::+ x) + sqrt (1 - z*z))

  acos z                    = y'' ::+ (-x'')
    where
      x'' ::+ y''  = log (z + ((-y') ::+ x'))
      x'  ::+ y'   = sqrt (1 - z*z)

  atan z@(x ::+ y) = y' ::+ (-x')
    where
      x' ::+ y' = log (((1-y) ::+ x) / sqrt (1+z*z))

  asinh z =  log (z + sqrt (1+z*z))
  acosh z =  log (z + (z+1) * sqrt ((z-1)/(z+1)))
  atanh z =  0.5 * log ((1.0+z) / (1.0-z))


instance (FromIntegral a b, Num b, Elt (Complex b)) => FromIntegral a (Complex b) where
  fromIntegral x = fromIntegral x ::+ 0

-- | @since 1.2.0.0
instance Functor Complex where
  fmap f (r ::+ i) = f r ::+ f i


-- | The non-negative magnitude of a complex number
--
magnitude :: (RealFloat a, Elt (Complex a)) => Exp (Complex a) -> Exp a
magnitude (r ::+ i) = scaleFloat k (sqrt (sqr (scaleFloat mk r) + sqr (scaleFloat mk i)))
  where
    k     = max (exponent r) (exponent i)
    mk    = -k
    sqr z = z * z

-- | As 'magnitude', but ignore floating point rounding and use the traditional
-- (simpler to evaluate) definition.
--
-- @since 1.3.0.0
--
magnitude' :: (RealFloat a, Elt (Complex a)) => Exp (Complex a) -> Exp a
magnitude' (r ::+ i) = sqrt (r*r + i*i)

-- | The phase of a complex number, in the range @(-'pi', 'pi']@. If the
-- magnitude is zero, then so is the phase.
--
phase :: (RealFloat a, Elt (Complex a)) => Exp (Complex a) -> Exp a
phase z@(r ::+ i) =
  if z == 0
    then 0
    else atan2 i r

-- | The function 'polar' takes a complex number and returns a (magnitude,
-- phase) pair in canonical form: the magnitude is non-negative, and the phase
-- in the range @(-'pi', 'pi']@; if the magnitude is zero, then so is the phase.
--
polar :: (RealFloat a, Elt (Complex a)) => Exp (Complex a) -> Exp (a,a)
polar z =  T2 (magnitude z) (phase z)

-- | Form a complex number from polar components of magnitude and phase.
--
#if __GLASGOW_HASKELL__ <= 708
mkPolar :: forall a. (RealFloat a, Elt (Complex a)) => Exp a -> Exp a -> Exp (Complex a)
#else
mkPolar :: forall a. (Floating a,  Elt (Complex a)) => Exp a -> Exp a -> Exp (Complex a)
#endif
mkPolar = lift2 (C.mkPolar :: Exp a -> Exp a -> Complex (Exp a))

-- | @'cis' t@ is a complex value with magnitude @1@ and phase @t@ (modulo
-- @2*'pi'@).
--
#if __GLASGOW_HASKELL__ <= 708
cis :: forall a. (RealFloat a, Elt (Complex a)) => Exp a -> Exp (Complex a)
#else
cis :: forall a. (Floating a,  Elt (Complex a)) => Exp a -> Exp (Complex a)
#endif
cis = lift1 (C.cis :: Exp a -> Complex (Exp a))

-- | Return the real part of a complex number
--
real :: (Elt a, Elt (Complex a)) => Exp (Complex a) -> Exp a
real (r ::+ _) = r

-- | Return the imaginary part of a complex number
--
imag :: (Elt a, Elt (Complex a)) => Exp (Complex a) -> Exp a
imag (_ ::+ i) = i

-- | Return the complex conjugate of a complex number, defined as
--
-- > conjugate(Z) = X - iY
--
conjugate :: (Num a, Elt (Complex a)) => Exp (Complex a) -> Exp (Complex a)
conjugate z = real z ::+ (- imag z)

