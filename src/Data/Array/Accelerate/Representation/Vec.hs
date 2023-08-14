{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Representation.Vec
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Representation.Vec
  where

import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Type

import Data.Primitive.Bit                                           as Prim

import qualified GHC.Exts                                           as GHC


toList :: TypeR v -> TypeR a -> v -> [a]
toList = go
  where
    go :: TypeR v -> TypeR t -> v -> [t]
    go TupRunit         TupRunit         _      = repeat ()
    go (TupRpair va vb) (TupRpair ta tb) (a, b) = zip (go va ta a) (go vb tb b)
    go (TupRsingle v)   (TupRsingle t)   xs     = scalar v t xs
    go _                _                _      = internalError "unexpected vector encoding"

    scalar :: ScalarType v -> ScalarType t -> v -> [t]
    scalar (NumScalarType v) (NumScalarType t) = num v t
    scalar (BitScalarType v) (BitScalarType t) = bit v t
    scalar _ _ = internalError "unexpected vector encoding"

    bit :: BitType v -> BitType t -> v -> [t]
    bit (TypeMask _) TypeBit = GHC.toList . Prim.BitMask
    bit _ _ = internalError "unexpected vector encoding"

    num :: NumType v -> NumType t -> v -> [t]
    num (IntegralNumType v) (IntegralNumType t) = integral v t
    num (FloatingNumType v) (FloatingNumType t) = floating v t
    num _ _ = internalError "unexpected vector encoding"

    integral :: IntegralType v -> IntegralType t -> v -> [t]
    integral (VectorIntegralType _ TypeInt8)    (SingleIntegralType TypeInt8)    = GHC.toList
    integral (VectorIntegralType _ TypeInt16)   (SingleIntegralType TypeInt16)   = GHC.toList
    integral (VectorIntegralType _ TypeInt32)   (SingleIntegralType TypeInt32)   = GHC.toList
    integral (VectorIntegralType _ TypeInt64)   (SingleIntegralType TypeInt64)   = GHC.toList
    integral (VectorIntegralType _ TypeInt128)  (SingleIntegralType TypeInt128)  = GHC.toList
    integral (VectorIntegralType _ TypeWord8)   (SingleIntegralType TypeWord8)   = GHC.toList
    integral (VectorIntegralType _ TypeWord16)  (SingleIntegralType TypeWord16)  = GHC.toList
    integral (VectorIntegralType _ TypeWord32)  (SingleIntegralType TypeWord32)  = GHC.toList
    integral (VectorIntegralType _ TypeWord64)  (SingleIntegralType TypeWord64)  = GHC.toList
    integral (VectorIntegralType _ TypeWord128) (SingleIntegralType TypeWord128) = GHC.toList
    integral _ _ = internalError "unexpected vector encoding"

    floating :: FloatingType v -> FloatingType t -> v -> [t]
    floating (VectorFloatingType _ TypeFloat16)  (SingleFloatingType TypeFloat16)  = GHC.toList
    floating (VectorFloatingType _ TypeFloat32)  (SingleFloatingType TypeFloat32)  = GHC.toList
    floating (VectorFloatingType _ TypeFloat64)  (SingleFloatingType TypeFloat64)  = GHC.toList
    floating (VectorFloatingType _ TypeFloat128) (SingleFloatingType TypeFloat128) = GHC.toList
    floating _ _ = internalError "unexpected vector encoding"


fromList :: TypeR v -> TypeR a -> [a] -> v
fromList = go
  where
    go :: TypeR v -> TypeR t -> [t] -> v
    go TupRunit         TupRunit         _  = ()
    go (TupRpair va vb) (TupRpair ta tb) xs = let (as, bs) = unzip xs in (go va ta as, go vb tb bs)
    go (TupRsingle v)   (TupRsingle t)   xs = scalar v t xs
    go _                _                _  = error "unexpected vector encoding"

    scalar :: ScalarType v -> ScalarType t -> [t] -> v
    scalar (NumScalarType v) (NumScalarType t) = num v t
    scalar (BitScalarType v) (BitScalarType t) = bit v t
    scalar _ _ = internalError "unexpected vector encoding"

    bit :: BitType v -> BitType t -> [t] -> v
    bit (TypeMask _) TypeBit = Prim.unMask . GHC.fromList
    bit _ _ = internalError "unexpected vector encoding"

    num :: NumType v -> NumType t -> [t] -> v
    num (IntegralNumType v) (IntegralNumType t) = integral v t
    num (FloatingNumType v) (FloatingNumType t) = floating v t
    num _ _ = internalError "unexpected vector encoding"

    integral :: IntegralType v -> IntegralType t -> [t] -> v
    integral (VectorIntegralType _ TypeInt8)    (SingleIntegralType TypeInt8)    = GHC.fromList
    integral (VectorIntegralType _ TypeInt16)   (SingleIntegralType TypeInt16)   = GHC.fromList
    integral (VectorIntegralType _ TypeInt32)   (SingleIntegralType TypeInt32)   = GHC.fromList
    integral (VectorIntegralType _ TypeInt64)   (SingleIntegralType TypeInt64)   = GHC.fromList
    integral (VectorIntegralType _ TypeInt128)  (SingleIntegralType TypeInt128)  = GHC.fromList
    integral (VectorIntegralType _ TypeWord8)   (SingleIntegralType TypeWord8)   = GHC.fromList
    integral (VectorIntegralType _ TypeWord16)  (SingleIntegralType TypeWord16)  = GHC.fromList
    integral (VectorIntegralType _ TypeWord32)  (SingleIntegralType TypeWord32)  = GHC.fromList
    integral (VectorIntegralType _ TypeWord64)  (SingleIntegralType TypeWord64)  = GHC.fromList
    integral (VectorIntegralType _ TypeWord128) (SingleIntegralType TypeWord128) = GHC.fromList
    integral _ _ = internalError "unexpected vector encoding"

    floating :: FloatingType v -> FloatingType t -> [t] -> v
    floating (VectorFloatingType _ TypeFloat16)  (SingleFloatingType TypeFloat16)  = GHC.fromList
    floating (VectorFloatingType _ TypeFloat32)  (SingleFloatingType TypeFloat32)  = GHC.fromList
    floating (VectorFloatingType _ TypeFloat64)  (SingleFloatingType TypeFloat64)  = GHC.fromList
    floating (VectorFloatingType _ TypeFloat128) (SingleFloatingType TypeFloat128) = GHC.fromList
    floating _ _ = internalError "unexpected vector encoding"

