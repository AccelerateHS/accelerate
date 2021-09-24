{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MagicHash             #-}
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
-- Copyright   : [2015..2020] The Accelerate Team
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

import Data.Array.Accelerate.Annotations
import Data.Array.Accelerate.Classes.Eq
import Data.Array.Accelerate.Classes.Floating
import Data.Array.Accelerate.Classes.Fractional
import Data.Array.Accelerate.Classes.FromIntegral
import Data.Array.Accelerate.Classes.Num
import Data.Array.Accelerate.Classes.Ord
import Data.Array.Accelerate.Classes.RealFloat
import Data.Array.Accelerate.Data.Functor
import Data.Array.Accelerate.Pattern
import Data.Array.Accelerate.Prelude
import Data.Array.Accelerate.Representation.Tag
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Representation.Vec
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Sugar.Vec
import Data.Array.Accelerate.Type
import Data.Primitive.Vec

import Data.Complex                                                 ( Complex(..) )
import Prelude                                                      ( ($) )
import qualified Data.Complex                                       as C
import qualified Prelude                                            as P


infix 6 ::+
pattern (::+) :: (HasCallStack, Elt a) => Exp a -> Exp a -> Exp (Complex a)
pattern r ::+ i <- (sourceMapPattern deconstructComplex -> (r, i))
  where (::+) = sourceMapPattern constructComplex
{-# COMPLETE (::+) #-}


-- Use an array-of-structs representation for complex numbers if possible.
-- This matches the standard C-style layout, but we can use this representation only at
-- specific types (not for any type 'a') as we can only have vectors of primitive type.
-- For other types, we use a structure-of-arrays representation. This is handled by the
-- ComplexR. We use the GADT ComplexR and function complexR to reconstruct
-- information on how the elements are represented.
--
instance Elt a => Elt (Complex a) where
  type EltR (Complex a) = ComplexR (EltR a)
  eltR = let tR = eltR @a
          in case complexR tR of
               ComplexVec s -> TupRsingle $ VectorScalarType $ VectorType 2 s
               ComplexTup   -> TupRunit `TupRpair` tR `TupRpair` tR

  tagsR = let tR = eltR @a
           in case complexR tR of
               ComplexVec s -> [ TagRsingle (VectorScalarType (VectorType 2 s)) ]
               ComplexTup   -> let go :: TypeR t -> [TagR t]
                                   go TupRunit         = [TagRunit]
                                   go (TupRsingle s)   = [TagRsingle s]
                                   go (TupRpair ta tb) = [TagRpair a b | a <- go ta, b <- go tb]
                                in
                                [ TagRunit `TagRpair` ta `TagRpair` tb | ta <- go tR, tb <- go tR ]

  toElt = case complexR $ eltR @a of
    ComplexVec _ -> \(Vec2 r i)   -> toElt r :+ toElt i
    ComplexTup   -> \(((), r), i) -> toElt r :+ toElt i

  fromElt (r :+ i) = case complexR $ eltR @a of
    ComplexVec _ -> Vec2 (fromElt r) (fromElt i)
    ComplexTup   -> (((), fromElt r), fromElt i)

type family ComplexR a where
  ComplexR Half   = Vec2 Half
  ComplexR Float  = Vec2 Float
  ComplexR Double = Vec2 Double
  ComplexR Int    = Vec2 Int
  ComplexR Int8   = Vec2 Int8
  ComplexR Int16  = Vec2 Int16
  ComplexR Int32  = Vec2 Int32
  ComplexR Int64  = Vec2 Int64
  ComplexR Word   = Vec2 Word
  ComplexR Word8  = Vec2 Word8
  ComplexR Word16 = Vec2 Word16
  ComplexR Word32 = Vec2 Word32
  ComplexR Word64 = Vec2 Word64
  ComplexR a      = (((), a), a)

-- This isn't ideal because we gather the evidence based on the
-- representation type, so we really get the evidence (VecElt (EltR a)),
-- which is not very useful...
--    - TLM 2020-07-16
data ComplexType a c where
  ComplexVec :: VecElt a => SingleType a -> ComplexType a (Vec2 a)
  ComplexTup ::                             ComplexType a (((), a), a)

complexR :: TypeR a -> ComplexType a (ComplexR a)
complexR = tuple
  where
    tuple :: TypeR a -> ComplexType a (ComplexR a)
    tuple TupRunit       = ComplexTup
    tuple TupRpair{}     = ComplexTup
    tuple (TupRsingle s) = scalar s

    scalar :: ScalarType a -> ComplexType a (ComplexR a)
    scalar (SingleScalarType t) = single t
    scalar VectorScalarType{}   = ComplexTup

    single :: SingleType a -> ComplexType a (ComplexR a)
    single (NumSingleType t) = num t

    num :: NumType a -> ComplexType a (ComplexR a)
    num (IntegralNumType t) = integral t
    num (FloatingNumType t) = floating t

    integral :: IntegralType a -> ComplexType a (ComplexR a)
    integral TypeInt    = ComplexVec singleType
    integral TypeInt8   = ComplexVec singleType
    integral TypeInt16  = ComplexVec singleType
    integral TypeInt32  = ComplexVec singleType
    integral TypeInt64  = ComplexVec singleType
    integral TypeWord   = ComplexVec singleType
    integral TypeWord8  = ComplexVec singleType
    integral TypeWord16 = ComplexVec singleType
    integral TypeWord32 = ComplexVec singleType
    integral TypeWord64 = ComplexVec singleType

    floating :: FloatingType a -> ComplexType a (ComplexR a)
    floating TypeHalf   = ComplexVec singleType
    floating TypeFloat  = ComplexVec singleType
    floating TypeDouble = ComplexVec singleType


constructComplex :: forall a. (SourceMapped, Elt a) => Exp a -> Exp a -> Exp (Complex a)
constructComplex r i =
  case complexR (eltR @a) of
    ComplexTup   -> coerce $ T2 r i
    ComplexVec _ -> V2 (coerce @a @(EltR a) r) (coerce @a @(EltR a) i)

deconstructComplex :: forall a. (SourceMapped, Elt a) => Exp (Complex a) -> (Exp a, Exp a)
deconstructComplex c@(Exp c') =
  case complexR (eltR @a) of
    ComplexTup   -> let T2 r i = coerce c in (r, i)
    ComplexVec t -> let T2 r i = Exp (SmartExp (VecUnpack mkAnn (VecRsucc (VecRsucc (VecRnil t))) c'))
                     in (r, i)

coerce :: EltR a ~ EltR b => Exp a -> Exp b
coerce (Exp e) = Exp e

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Complex a) where
  type Plain (Complex a) = Complex (Plain a)
  lift (r :+ i) = sourceMap $ lift r ::+ lift i

instance Elt a => Unlift Exp (Complex (Exp a)) where
  unlift = sourceMap $ \(r ::+ i) -> r :+ i


instance Eq a => Eq (Complex a) where
  (==) = sourceMap $ \(r1 ::+ c1) (r2 ::+ c2) -> r1 == r2 && c1 == c2
  (/=) = sourceMap $ \(r1 ::+ c1) (r2 ::+ c2) -> r1 /= r2 || c1 /= c2

instance RealFloat a => P.Num (Exp (Complex a)) where
  (+)    = sourceMapRuntime $ lift2 ((+)    :: Complex (Exp a) -> Complex (Exp a) -> Complex (Exp a))
  (-)    = sourceMapRuntime $ lift2 ((-)    :: Complex (Exp a) -> Complex (Exp a) -> Complex (Exp a))
  (*)    = sourceMapRuntime $ lift2 ((*)    :: Complex (Exp a) -> Complex (Exp a) -> Complex (Exp a))
  negate = sourceMapRuntime $ lift1 (negate :: Complex (Exp a) -> Complex (Exp a))
  signum = sourceMapRuntime $ \z@(x ::+ y) ->
    if z == 0
       then z
       else let r = magnitude z
             in x/r ::+ y/r
  abs z         = sourceMapRuntime $ magnitude z ::+ 0
  fromInteger n = sourceMapRuntime $ fromInteger n ::+ 0

instance RealFloat a => P.Fractional (Exp (Complex a)) where
  fromRational x  = sourceMapRuntime $ fromRational x ::+ 0
  z / z' = sourceMapRuntime
    $ let x  :+ y   = unlift z
          x' :+ y'  = unlift z'
          --
          x'' = scaleFloat k x'
          y'' = scaleFloat k y'
          k   = - max (exponent x') (exponent y')
          d   = x'*x'' + y'*y''
       in (x*x''+y*y'') / d ::+ (y*x''-x*y'') / d

instance RealFloat a => P.Floating (Exp (Complex a)) where
  pi    = sourceMapRuntime $ pi ::+ 0
  exp   = sourceMapRuntime $ \(x ::+ y) ->
    let expx = exp x
    in  expx * cos y ::+ expx * sin y
  log z = sourceMapRuntime $ log (magnitude z) ::+ phase z
  sqrt  = sourceMapRuntime $ \z@(x ::+ y) ->
    let T2 u v = x < 0 ? (T2 v' u', T2 u' v')
        v'     = abs y / (u'*2)
        u'     = sqrt ((magnitude z + abs x) / 2)
     in if z == 0
          then 0
          else u ::+ (y < 0 ? (-v, v))

  x ** y = sourceMapRuntime
    $ let r     ::+ i  = x
          exp_r ::+ _  = y
          --
          inf = 1 / 0
          nan = 0 / 0
       in if y == 0 then 1 else
            if x == 0 then if exp_r > 0 then 0 else
                          if exp_r < 0 then inf ::+ 0
                                        else nan ::+ nan
                      else if isInfinite r || isInfinite i
                            then if exp_r > 0 then inf ::+ 0 else
                                  if exp_r < 0 then 0
                                              else nan ::+ nan
                            else exp (log x * y)

  sin = sourceMapRuntime $ \(x ::+ y) ->
    sin x * cosh y ::+ cos x * sinh y
  cos = sourceMapRuntime $ \(x ::+ y) ->
    cos x * cosh y ::+ (- sin x * sinh y)
  tan = sourceMapRuntime $ \(x ::+ y) ->
    let sinx  = sin x
        cosx  = cos x
        sinhy = sinh y
        coshy = cosh y
     in (sinx*coshy ::+ cosx*sinhy) / (cosx*coshy ::+ (-sinx*sinhy))

  sinh = sourceMapRuntime $ \(x ::+ y) ->
    cos y * sinh x ::+ sin  y * cosh x
  cosh = sourceMapRuntime $ \(x ::+ y) ->
    cos y * cosh x ::+ sin y * sinh x
  tanh = sourceMapRuntime $ \(x ::+ y) ->
    let siny  = sin y
        cosy  = cos y
        sinhx = sinh x
        coshx = cosh x
     in (cosy*sinhx ::+ siny*coshx) / (cosy*coshx ::+ siny*sinhx)

  asin = sourceMapRuntime $ \z@(x ::+ y) ->
    let x' ::+ y' = log (((-y) ::+ x) + sqrt (1 - z*z))
     in y' ::+ (-x')

  acos z = sourceMapRuntime
    $ let x'' ::+ y''  = log (z + ((-y') ::+ x'))
          x'  ::+ y'   = sqrt (1 - z*z)
       in y'' ::+ (-x'')

  atan = sourceMapRuntime $ \z@(x ::+ y) ->
    let x' ::+ y' = log (((1-y) ::+ x) / sqrt (1+z*z))
     in y' ::+ (-x')

  asinh z = sourceMapRuntime $ log (z + sqrt (1+z*z))
  acosh z = sourceMapRuntime $ log (z + (z+1) * sqrt ((z-1)/(z+1)))
  atanh z = sourceMapRuntime $ 0.5 * log ((1.0+z) / (1.0-z))


instance (FromIntegral a b, Num b, Elt (Complex b)) => FromIntegral a (Complex b) where
  fromIntegral x = sourceMap $ fromIntegral x ::+ 0

-- | @since 1.2.0.0
--
instance Functor Complex where
  fmap f = sourceMap $ \(r ::+ i) -> f r ::+ f i


-- | The non-negative magnitude of a complex number
--
magnitude :: (HasCallStack, RealFloat a) => Exp (Complex a) -> Exp a
magnitude = sourceMap $ \(r ::+ i) ->
    let k  = max (exponent r) (exponent i)
        mk = -k
        sqr z = z * z
    in  scaleFloat k (sqrt (sqr (scaleFloat mk r) + sqr (scaleFloat mk i)))

-- | As 'magnitude', but ignore floating point rounding and use the traditional
-- (simpler to evaluate) definition.
--
-- @since 1.3.0.0
--
magnitude' :: (HasCallStack, RealFloat a) => Exp (Complex a) -> Exp a
magnitude' = sourceMap $ \(r ::+ i) -> sqrt (r*r + i*i)

-- | The phase of a complex number, in the range @(-'pi', 'pi']@. If the
-- magnitude is zero, then so is the phase.
--
phase :: (HasCallStack, RealFloat a) => Exp (Complex a) -> Exp a
phase = sourceMap $ \z@(r ::+ i) ->
    if z == 0
      then 0
      else atan2 i r

-- | The function 'polar' takes a complex number and returns a (magnitude,
-- phase) pair in canonical form: the magnitude is non-negative, and the phase
-- in the range @(-'pi', 'pi']@; if the magnitude is zero, then so is the phase.
--
polar :: (HasCallStack, RealFloat a) => Exp (Complex a) -> Exp (a,a)
polar z = sourceMap $ T2 (magnitude z) (phase z)

-- | Form a complex number from polar components of magnitude and phase.
--
mkPolar :: forall a. (HasCallStack, Floating a) => Exp a -> Exp a -> Exp (Complex a)
mkPolar = sourceMap $ lift2 (C.mkPolar :: Exp a -> Exp a -> Complex (Exp a))

-- | @'cis' t@ is a complex value with magnitude @1@ and phase @t@ (modulo
-- @2*'pi'@).
--
cis :: forall a. (HasCallStack, Floating a) => Exp a -> Exp (Complex a)
cis = sourceMap $ lift1 (C.cis :: Exp a -> Complex (Exp a))

-- | Return the real part of a complex number
--
real :: (HasCallStack, Elt a) => Exp (Complex a) -> Exp a
real = sourceMap $ \(r ::+ _) -> r

-- | Return the imaginary part of a complex number
--
imag :: (HasCallStack, Elt a) => Exp (Complex a) -> Exp a
imag = sourceMap $ \(_ ::+ i) -> i

-- | Return the complex conjugate of a complex number, defined as
--
-- > conjugate(Z) = X - iY
--
conjugate :: (HasCallStack, Num a) => Exp (Complex a) -> Exp (Complex a)
conjugate z = sourceMap $ real z ::+ (- imag z)
