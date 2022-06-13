{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
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

import Data.Array.Accelerate.AST                                    ( PrimFun(..) )
import Data.Array.Accelerate.AST.Idx
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
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Type
import qualified Data.Primitive.Vec                                 as Prim

import Data.Complex                                                 ( Complex(..) )
import Data.Primitive.Types
import Prelude                                                      ( ($) )
import qualified Data.Complex                                       as C
import qualified Prelude                                            as P


infix 6 ::+
pattern (::+) :: Elt a => Exp a -> Exp a -> Exp (Complex a)
pattern r ::+ i <- (deconstructComplex -> (r, i))
  where (::+) = constructComplex
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
               ComplexVec t -> TupRsingle (NumScalarType t)
               ComplexTup   -> TupRunit `TupRpair` tR `TupRpair` tR

  tagsR = let tR = eltR @a
           in case complexR tR of
               ComplexVec t -> [ TagRsingle (NumScalarType t) ]
               ComplexTup   -> let go :: TypeR t -> [TagR t]
                                   go TupRunit         = [TagRunit]
                                   go (TupRsingle s)   = [TagRsingle s]
                                   go (TupRpair ta tb) = [TagRpair a b | a <- go ta, b <- go tb]
                                in
                                [ TagRunit `TagRpair` ta `TagRpair` tb | ta <- go tR, tb <- go tR ]

  toElt = case complexR $ eltR @a of
    ComplexVec _ -> \(Prim.Vec2 r i) -> toElt r :+ toElt i
    ComplexTup   -> \(((), r), i)    -> toElt r :+ toElt i

  fromElt (r :+ i) = case complexR $ eltR @a of
    ComplexVec _ -> Prim.Vec2 (fromElt r) (fromElt i)
    ComplexTup   -> (((), fromElt r), fromElt i)

type family ComplexR a where
  ComplexR Half     = Prim.Vec2 Float16
  ComplexR Float    = Prim.Vec2 Float32
  ComplexR Double   = Prim.Vec2 Float64
  ComplexR Float128 = Prim.Vec2 Float128
  ComplexR Int8     = Prim.Vec2 Int8
  ComplexR Int16    = Prim.Vec2 Int16
  ComplexR Int32    = Prim.Vec2 Int32
  ComplexR Int64    = Prim.Vec2 Int64
  ComplexR Int128   = Prim.Vec2 Int128
  ComplexR Word8    = Prim.Vec2 Word8
  ComplexR Word16   = Prim.Vec2 Word16
  ComplexR Word32   = Prim.Vec2 Word32
  ComplexR Word64   = Prim.Vec2 Word64
  ComplexR Word128  = Prim.Vec2 Word128
  ComplexR a        = (((), a), a)

data ComplexType a c where
  ComplexVec :: Prim a => NumType (Prim.Vec2 a) -> ComplexType a (Prim.Vec2 a)
  ComplexTup ::                                    ComplexType a (((), a), a)

complexR :: TypeR a -> ComplexType a (ComplexR a)
complexR = tuple
  where
    tuple :: TypeR a -> ComplexType a (ComplexR a)
    tuple TupRunit       = ComplexTup
    tuple TupRpair{}     = ComplexTup
    tuple (TupRsingle s) = scalar s

    scalar :: ScalarType a -> ComplexType a (ComplexR a)
    scalar (NumScalarType t) = num t
    scalar (BitScalarType t) = bit t

    bit :: BitType t -> ComplexType t (ComplexR t)
    bit TypeBit    = ComplexTup
    bit TypeMask{} = ComplexTup

    num :: NumType a -> ComplexType a (ComplexR a)
    num (IntegralNumType t) = integral t
    num (FloatingNumType t) = floating t

    integral :: IntegralType a -> ComplexType a (ComplexR a)
    integral = \case
      VectorIntegralType{} -> ComplexTup
      SingleIntegralType t -> case t of
        TypeInt8    -> ComplexVec numType
        TypeInt16   -> ComplexVec numType
        TypeInt32   -> ComplexVec numType
        TypeInt64   -> ComplexVec numType
        TypeInt128  -> ComplexVec numType
        TypeWord8   -> ComplexVec numType
        TypeWord16  -> ComplexVec numType
        TypeWord32  -> ComplexVec numType
        TypeWord64  -> ComplexVec numType
        TypeWord128 -> ComplexVec numType

    floating :: FloatingType a -> ComplexType a (ComplexR a)
    floating = \case
      VectorFloatingType{} -> ComplexTup
      SingleFloatingType t -> case t of
        TypeFloat16  -> ComplexVec numType
        TypeFloat32  -> ComplexVec numType
        TypeFloat64  -> ComplexVec numType
        TypeFloat128 -> ComplexVec numType

constructComplex :: forall a. Elt a => Exp a -> Exp a -> Exp (Complex a)
constructComplex r@(Exp r') i@(Exp i') =
  case complexR (eltR @a) of
    ComplexTup   -> Pattern (r,i)
    ComplexVec t -> Exp $ num t r' i'
    where
      num :: NumType (Prim.Vec2 t) -> SmartExp t -> SmartExp t -> SmartExp (ComplexR t)
      num (IntegralNumType t) = integral t
      num (FloatingNumType t) = floating t

      integral :: IntegralType (Prim.Vec2 t) -> SmartExp t -> SmartExp t -> SmartExp (ComplexR t)
      integral (SingleIntegralType   t) = case t of
      integral (VectorIntegralType n t) =
        let v = NumScalarType (IntegralNumType (VectorIntegralType n t))
         in case t of
              TypeInt8    -> pack v
              TypeInt16   -> pack v
              TypeInt32   -> pack v
              TypeInt64   -> pack v
              TypeInt128  -> pack v
              TypeWord8   -> pack v
              TypeWord16  -> pack v
              TypeWord32  -> pack v
              TypeWord64  -> pack v
              TypeWord128 -> pack v

      floating :: FloatingType (Prim.Vec2 t) -> SmartExp t -> SmartExp t -> SmartExp (ComplexR t)
      floating (SingleFloatingType   t) = case t of
      floating (VectorFloatingType n t) =
        let v = NumScalarType (FloatingNumType (VectorFloatingType n t))
         in case t of
               TypeFloat16  -> pack v
               TypeFloat32  -> pack v
               TypeFloat64  -> pack v
               TypeFloat128 -> pack v

      pack :: ScalarType (Prim.Vec 2 t) -> SmartExp t -> SmartExp t -> SmartExp (Prim.Vec 2 t)
      pack v x y
        = SmartExp (Insert v TypeWord8
            (SmartExp (Insert v TypeWord8 (SmartExp (Undef v)) (SmartExp (Const scalarType 0)) x))
            (SmartExp (Const scalarType 1)) y)

deconstructComplex :: forall a. Elt a => Exp (Complex a) -> (Exp a, Exp a)
deconstructComplex (Exp c) =
  case complexR (eltR @a) of
    ComplexTup   ->
      let i = SmartExp (Prj PairIdxRight c)
          r = SmartExp (Prj PairIdxRight (SmartExp (Prj PairIdxLeft c)))
       in (Exp r, Exp i)
    ComplexVec t ->
      let (r, i) = num t c
       in (Exp r, Exp i)
    where
      num :: NumType (Prim.Vec2 t) -> SmartExp (ComplexR t) -> (SmartExp t, SmartExp t)
      num (IntegralNumType t) = integral t
      num (FloatingNumType t) = floating t

      integral :: IntegralType (Prim.Vec2 t) -> SmartExp (ComplexR t) -> (SmartExp t, SmartExp t)
      integral (SingleIntegralType   t) = case t of
      integral (VectorIntegralType n t) =
        let v = NumScalarType (IntegralNumType (VectorIntegralType n t))
         in case t of
              TypeInt8    -> unpack v
              TypeInt16   -> unpack v
              TypeInt32   -> unpack v
              TypeInt64   -> unpack v
              TypeInt128  -> unpack v
              TypeWord8   -> unpack v
              TypeWord16  -> unpack v
              TypeWord32  -> unpack v
              TypeWord64  -> unpack v
              TypeWord128 -> unpack v

      floating :: FloatingType (Prim.Vec2 t) -> SmartExp (ComplexR t) -> (SmartExp t, SmartExp t)
      floating (SingleFloatingType   t) = case t of
      floating (VectorFloatingType n t) =
        let v = NumScalarType (FloatingNumType (VectorFloatingType n t))
         in case t of
               TypeFloat16  -> unpack v
               TypeFloat32  -> unpack v
               TypeFloat64  -> unpack v
               TypeFloat128 -> unpack v

      unpack :: ScalarType (Prim.Vec 2 t) -> SmartExp (Prim.Vec 2 t) -> (SmartExp t, SmartExp t)
      unpack v x =
        let r = SmartExp (Extract v TypeWord8 x (SmartExp (Const scalarType 0)))
            i = SmartExp (Extract v TypeWord8 x (SmartExp (Const scalarType 1)))
        in
        (r, i)


instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Complex a) where
  type Plain (Complex a) = Complex (Plain a)
  lift (r :+ i) = lift r ::+ lift i

instance Elt a => Unlift Exp (Complex (Exp a)) where
  unlift (r ::+ i) = r :+ i


instance Eq a => Eq (Complex a) where
  r1 ::+ c1 == r2 ::+ c2 = r1 == r2 && c1 == c2
  r1 ::+ c1 /= r2 ::+ c2 = r1 /= r2 || c1 /= c2

instance RealFloat a => P.Num (Exp (Complex a)) where
  (+) = case complexR (eltR @a) of
          ComplexTup   -> lift2 ((+) :: Complex (Exp a) -> Complex (Exp a) -> Complex (Exp a))
          ComplexVec t -> mkPrimBinary $ PrimAdd t
  (-) = case complexR (eltR @a) of
          ComplexTup   -> lift2 ((-) :: Complex (Exp a) -> Complex (Exp a) -> Complex (Exp a))
          ComplexVec t -> mkPrimBinary $ PrimSub t
  (*) = lift2 ((*) :: Complex (Exp a) -> Complex (Exp a) -> Complex (Exp a))
  negate = case complexR (eltR @a) of
             ComplexTup   -> lift1 (negate :: Complex (Exp a) -> Complex (Exp a))
             ComplexVec t -> mkPrimUnary $ PrimNeg t
  signum z@(x ::+ y) =
    if z == 0
       then z
       else let r = magnitude z
             in x/r ::+ y/r
  abs z         = magnitude z ::+ 0
  fromInteger n = fromInteger n ::+ 0

instance RealFloat a => P.Fractional (Exp (Complex a)) where
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

instance RealFloat a => P.Floating (Exp (Complex a)) where
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
--
instance Functor Complex where
  fmap f (r ::+ i) = f r ::+ f i


-- | The non-negative magnitude of a complex number
--
magnitude :: RealFloat a => Exp (Complex a) -> Exp a
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
magnitude' :: RealFloat a => Exp (Complex a) -> Exp a
magnitude' (r ::+ i) = sqrt (r*r + i*i)

-- | The phase of a complex number, in the range @(-'pi', 'pi']@. If the
-- magnitude is zero, then so is the phase.
--
phase :: RealFloat a => Exp (Complex a) -> Exp a
phase z@(r ::+ i) =
  if z == 0
    then 0
    else atan2 i r

-- | The function 'polar' takes a complex number and returns a (magnitude,
-- phase) pair in canonical form: the magnitude is non-negative, and the phase
-- in the range @(-'pi', 'pi']@; if the magnitude is zero, then so is the phase.
--
polar :: RealFloat a => Exp (Complex a) -> Exp (a,a)
polar z =  T2 (magnitude z) (phase z)

-- | Form a complex number from polar components of magnitude and phase.
--
mkPolar :: forall a. Floating a => Exp a -> Exp a -> Exp (Complex a)
mkPolar = lift2 (C.mkPolar :: Exp a -> Exp a -> Complex (Exp a))

-- | @'cis' t@ is a complex value with magnitude @1@ and phase @t@ (modulo
-- @2*'pi'@).
--
cis :: forall a. Floating a => Exp a -> Exp (Complex a)
cis = lift1 (C.cis :: Exp a -> Complex (Exp a))

-- | Return the real part of a complex number
--
real :: Elt a => Exp (Complex a) -> Exp a
real (r ::+ _) = r

-- | Return the imaginary part of a complex number
--
imag :: Elt a => Exp (Complex a) -> Exp a
imag (_ ::+ i) = i

-- | Return the complex conjugate of a complex number, defined as
--
-- > conjugate(Z) = X - iY
--
conjugate :: Num a => Exp (Complex a) -> Exp (Complex a)
conjugate z = real z ::+ (- imag z)

