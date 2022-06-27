{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.RealFrac
-- Copyright   : [2016..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Classes.RealFrac (

  RealFrac(..),
  div', mod', divMod',

) where

import Data.Array.Accelerate.Language                               ( cond, even )
import Data.Array.Accelerate.Pattern.Tuple
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.Classes.Eq
import Data.Array.Accelerate.Classes.Floating
import Data.Array.Accelerate.Classes.Fractional
import Data.Array.Accelerate.Classes.FromIntegral
import Data.Array.Accelerate.Classes.Integral
import Data.Array.Accelerate.Classes.Num
import Data.Array.Accelerate.Classes.Ord
import Data.Array.Accelerate.Classes.ToFloating

import Data.Maybe
import Data.Kind
import Prelude                                                      ( ($), String, error, otherwise, unlines )
import Text.Printf
import qualified Prelude                                            as P


-- | Generalisation of 'P.div' to any instance of 'RealFrac'
--
div' :: (RealFrac a, FromIntegral (Significand a) b, Integral b) => Exp a -> Exp a -> Exp b
div' n d = floor (n / d)

-- | Generalisation of 'P.mod' to any instance of 'RealFrac'
--
mod' :: forall a. (Floating a, RealFrac a, Integral (Significand a), ToFloating (Significand a) a, FromIntegral (Significand a) (Significand a))
     => Exp a
     -> Exp a
     -> Exp a
mod' n d = n - (toFloating f) * d
  where
    f :: Exp (Significand a)
    f = div' n d

-- | Generalisation of 'P.divMod' to any instance of 'RealFrac'
--
divMod'
    :: (Floating a, RealFrac a, Integral b, FromIntegral (Significand a) b, ToFloating b a)
    => Exp a
    -> Exp a
    -> (Exp b, Exp a)
divMod' n d = (f, n - (toFloating f) * d)
  where
    f = div' n d


-- | Extracting components of fractions.
--
class (Ord a, Fractional a, Integral (Significand a)) => RealFrac a where
  -- | The significand (also known as the mantissa) is the part of a number in
  -- floating point representation consisting of the significant digits.
  -- Generally speaking, this is the integral part of a fractional number.
  --
  type Significand a :: Type

  {-# MINIMAL properFraction #-}

  -- | The function 'properFraction' takes a real fractional number @x@ and
  -- returns a pair @(n,f)@ such that @x = n+f@, and:
  --
  -- * @n@ is an integral number with the same sign as @x@; and
  --
  -- * @f@ is a fraction with the same type and sign as @x@,
  --   and with absolute value less than @1@.
  --
  -- The default definitions of the 'ceiling', 'floor', 'truncate'
  -- and 'round' functions are in terms of 'properFraction'.
  properFraction :: (FromIntegral (Significand a) b, Integral b) => Exp a -> Exp (b, a)

  -- The function 'splitFraction' takes a real fractional number @x@ and
  -- returns a pair @(n,f)@ such that @x = n+f@, and:
  --
  -- * @n@ is an integral number with the same sign as @x@; and
  --
  -- * @f@ is a fraction with the same type as @x@ in the range [0,1). Note that
  -- this differs from 'Prelude.properFraction'.
  --
  -- splitFraction :: (Elt b, IsIntegral b) => Exp a -> (Exp b, Exp a)

  -- @fraction x@ returns @x@ with the integer part removed.
  -- fraction       :: Exp a -> Exp a

  -- properFraction is part of the standard Haskell'98 RealFrac type classes
  -- splitFraction / fraction are from numeric-prelude Algebra.RealRing

  -- | @truncate x@ returns the integer nearest @x@ between zero and @x@
  truncate :: (Integral b, FromIntegral (Significand a) b) => Exp a -> Exp b
  truncate = defaultTruncate

  -- | @'round' x@ returns the nearest integer to @x@; the even integer if @x@
  -- is equidistant between two integers
  round :: (Integral b, FromIntegral (Significand a) b) => Exp a -> Exp b
  round = defaultRound

  -- | @'ceiling' x@ returns the least integer not less than @x@
  ceiling :: (Integral b, FromIntegral (Significand a) b) => Exp a -> Exp b
  ceiling = defaultCeiling

  -- | @'floor' x@ returns the greatest integer not greater than @x@
  floor :: (Integral b, FromIntegral (Significand a) b) => Exp a -> Exp b
  floor = defaultFloor


defaultTruncate :: forall a b. (RealFrac a, Integral b, FromIntegral (Significand a) b) => Exp a -> Exp b
defaultTruncate x
  | Just FloatingDict <- floatingDict @a
  , Just IntegralDict <- integralDict @b
  = mkTruncate x
  --
  | otherwise
  = let T2 n _ = properFraction x in n

defaultCeiling :: forall a b. (RealFrac a, Integral b, FromIntegral (Significand a) b) => Exp a -> Exp b
defaultCeiling x
  | Just FloatingDict <- floatingDict @a
  , Just IntegralDict <- integralDict @b
  = mkCeiling x
  --
  | otherwise
  = let T2 n r = properFraction x in cond (r > 0) (n+1) n

defaultFloor :: forall a b. (RealFrac a, Integral b, FromIntegral (Significand a) b) => Exp a -> Exp b
defaultFloor x
  | Just FloatingDict <- floatingDict @a
  , Just IntegralDict <- integralDict @b
  = mkCeiling x
  --
  | otherwise
  = let T2 n r = properFraction x in cond (r < 0) (n-1) n

defaultRound :: forall a b. (RealFrac a, Integral b, FromIntegral (Significand a) b) => Exp a -> Exp b
defaultRound x
  | Just FloatingDict <- floatingDict @a
  , Just IntegralDict <- integralDict @b
  = mkCeiling x
  --
  | otherwise
  = let T2 n r    = properFraction x
        m         = cond (r < 0.0) (n-1) (n+1)
        half_down = abs r - 0.5
        p         = compare half_down 0.0
    in
    cond (constant LT == p) n                   $
    cond (constant EQ == p) (cond (even n) n m) $
            {- otherwise -} m


data FloatingDict a where
  FloatingDict :: IsFloating a => FloatingDict a

data IntegralDict a where
  IntegralDict :: IsIntegral a => IntegralDict a

floatingDict :: forall a. Elt a => Maybe (FloatingDict (EltR a))
floatingDict = go (eltR @a)
  where
    go :: TypeR t -> Maybe (FloatingDict t)
    go (TupRsingle t) = scalar t
    go _              = Nothing

    scalar :: ScalarType t -> Maybe (FloatingDict t)
    scalar (NumScalarType t) = num t
    scalar _                 = Nothing

    num :: NumType t -> Maybe (FloatingDict t)
    num (FloatingNumType t) = floating t
    num _                   = Nothing

    floating :: forall t. FloatingType t -> Maybe (FloatingDict t)
    floating (SingleFloatingType   t) =
      case t of
        TypeFloat16  -> Just FloatingDict
        TypeFloat32  -> Just FloatingDict
        TypeFloat64  -> Just FloatingDict
        TypeFloat128 -> Just FloatingDict
    floating (VectorFloatingType _ t) =
      case t of
        TypeFloat16  -> Just FloatingDict
        TypeFloat32  -> Just FloatingDict
        TypeFloat64  -> Just FloatingDict
        TypeFloat128 -> Just FloatingDict

integralDict :: forall a. Elt a => Maybe (IntegralDict (EltR a))
integralDict = go (eltR @a)
  where
    go :: TypeR t -> Maybe (IntegralDict t)
    go (TupRsingle t) = scalar t
    go _              = Nothing

    scalar :: ScalarType t -> Maybe (IntegralDict t)
    scalar (NumScalarType t) = num t
    scalar _                 = Nothing

    num :: NumType t -> Maybe (IntegralDict t)
    num (IntegralNumType t) = integral t
    num _                   = Nothing

    integral :: forall t. IntegralType t -> Maybe (IntegralDict t)
    integral (SingleIntegralType   t) =
      case t of
        TypeInt8    -> Just IntegralDict
        TypeInt16   -> Just IntegralDict
        TypeInt32   -> Just IntegralDict
        TypeInt64   -> Just IntegralDict
        TypeInt128  -> Just IntegralDict
        TypeWord8   -> Just IntegralDict
        TypeWord16  -> Just IntegralDict
        TypeWord32  -> Just IntegralDict
        TypeWord64  -> Just IntegralDict
        TypeWord128 -> Just IntegralDict
    integral (VectorIntegralType _ t) =
      case t of
        TypeInt8    -> Just IntegralDict
        TypeInt16   -> Just IntegralDict
        TypeInt32   -> Just IntegralDict
        TypeInt64   -> Just IntegralDict
        TypeInt128  -> Just IntegralDict
        TypeWord8   -> Just IntegralDict
        TypeWord16  -> Just IntegralDict
        TypeWord32  -> Just IntegralDict
        TypeWord64  -> Just IntegralDict
        TypeWord128 -> Just IntegralDict


-- To satisfy superclass constraints
--
instance RealFrac a => P.RealFrac (Exp a) where
  properFraction = preludeError "properFraction"
  truncate       = preludeError "truncate"
  round          = preludeError "round"
  ceiling        = preludeError "ceiling"
  floor          = preludeError "floor"

preludeError :: String -> a
preludeError x
  = error
  $ unlines [ printf "Prelude.%s applied to EDSL types: use Data.Array.Accelerate.%s instead" x x
            , ""
            , "These Prelude.RealFrac instances are present only to fulfil superclass"
            , "constraints for subsequent classes in the standard Haskell numeric hierarchy."
            ]

-- Instances declared in Data.Array.Accelerate.Classes.RealFloat to avoid
-- recursive modules

