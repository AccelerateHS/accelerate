{-# LANGUAGE GADTs           #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Representation.Elt
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Representation.Elt
  where

import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Type
import Data.Primitive.Bit
import Data.Primitive.Vec

import Control.Monad.ST
import Data.Primitive.ByteArray
import Language.Haskell.TH.Extra

import GHC.TypeLits
import GHC.Base


undefElt :: TypeR t -> t
undefElt = tuple
  where
    tuple :: TypeR t -> t
    tuple TupRunit         = ()
    tuple (TupRpair ta tb) = (tuple ta, tuple tb)
    tuple (TupRsingle t)   = scalar t

    scalar :: ScalarType t -> t
    scalar (NumScalarType t) = num t
    scalar (BitScalarType t) = bit t

    bit :: BitType t -> t
    bit TypeBit      = Bit False
    bit (TypeMask n) =
      let bytes = quot (fromInteger (natVal' n) + 7) 8
       in runST $ do
            mba           <- newByteArray bytes
            ByteArray ba# <- unsafeFreezeByteArray mba
            return $! Vec ba#

    num :: NumType t -> t
    num (IntegralNumType t) = integral t
    num (FloatingNumType t) = floating t

    integral :: IntegralType t -> t
    integral = \case
      SingleIntegralType t   -> single t
      VectorIntegralType n t -> vector n t
      where
        single :: SingleIntegralType t -> t
        single TypeInt8    = 0
        single TypeInt16   = 0
        single TypeInt32   = 0
        single TypeInt64   = 0
        single TypeInt128  = 0
        single TypeWord8   = 0
        single TypeWord16  = 0
        single TypeWord32  = 0
        single TypeWord64  = 0
        single TypeWord128 = 0

        vector :: KnownNat n => Proxy# n -> SingleIntegralType t -> Vec n t
        vector n t = runST $ do
          let bits  = case t of
                        TypeInt  w -> w
                        TypeWord w -> w
              bytes = max 1 (quot (fromInteger (natVal' n) * bits) 8)
          mba           <- newAlignedPinnedByteArray bytes 16
          ByteArray ba# <- unsafeFreezeByteArray mba
          return $! Vec ba#

    floating :: FloatingType t -> t
    floating = \case
      SingleFloatingType t   -> single t
      VectorFloatingType n t -> vector n t
      where
        single :: SingleFloatingType t -> t
        single TypeFloat16  = 0
        single TypeFloat32  = 0
        single TypeFloat64  = 0
        single TypeFloat128 = 0

        vector :: KnownNat n => Proxy# n -> SingleFloatingType t -> Vec n t
        vector n t = runST $ do
          let bits  = case t of
                        TypeFloat16  -> 16
                        TypeFloat32  -> 32
                        TypeFloat64  -> 64
                        TypeFloat128 -> 128
              bytes = max 1 (quot (fromInteger (natVal' n) * bits) 8)
          mba           <- newAlignedPinnedByteArray bytes 16
          ByteArray ba# <- unsafeFreezeByteArray mba
          return $! Vec ba#


showElt :: TypeR e -> e -> String
showElt t v = showsElt t v ""

showsElt :: TypeR e -> e -> ShowS
showsElt = tuple
  where
    tuple :: TypeR e -> e -> ShowS
    tuple TupRunit         ()       = showString "()"
    tuple (TupRpair t1 t2) (e1, e2) = showString "(" . tuple t1 e1 . showString ", " . tuple t2 e2 . showString ")"
    tuple (TupRsingle tp)  val      = scalar tp val

    scalar :: ScalarType e -> e -> ShowS
    scalar (NumScalarType t) = num t
    scalar (BitScalarType t) = bit t

    bit :: BitType e -> e -> ShowS
    bit TypeBit    = shows
    bit TypeMask{} = shows . BitMask

    num :: NumType e -> e -> ShowS
    num (IntegralNumType t) = integral t
    num (FloatingNumType t) = floating t

    integral :: IntegralType e -> e -> ShowS
    integral = \case
      SingleIntegralType t   -> single t
      VectorIntegralType _ t -> vector t
      where
        single :: SingleIntegralType t -> t -> ShowS
        single TypeInt8    = shows
        single TypeInt16   = shows
        single TypeInt32   = shows
        single TypeInt64   = shows
        single TypeInt128  = shows
        single TypeWord8   = shows
        single TypeWord16  = shows
        single TypeWord32  = shows
        single TypeWord64  = shows
        single TypeWord128 = shows

        vector :: KnownNat n => SingleIntegralType t -> Vec n t -> ShowS
        vector TypeInt8    = shows
        vector TypeInt16   = shows
        vector TypeInt32   = shows
        vector TypeInt64   = shows
        vector TypeInt128  = shows
        vector TypeWord8   = shows
        vector TypeWord16  = shows
        vector TypeWord32  = shows
        vector TypeWord64  = shows
        vector TypeWord128 = shows

    floating :: FloatingType e -> e -> ShowS
    floating = \case
      SingleFloatingType t   -> single t
      VectorFloatingType _ t -> vector t
      where
        single :: SingleFloatingType t -> t -> ShowS
        single TypeFloat16  = shows
        single TypeFloat32  = shows
        single TypeFloat64  = shows
        single TypeFloat128 = shows

        vector :: KnownNat n => SingleFloatingType t -> Vec n t -> ShowS
        vector TypeFloat16  = shows
        vector TypeFloat32  = shows
        vector TypeFloat64  = shows
        vector TypeFloat128 = shows

liftElt :: TypeR t -> t -> CodeQ t
liftElt TupRunit         ()    = [|| () ||]
liftElt (TupRsingle t)   x     = [|| $$(liftScalar t x) ||]
liftElt (TupRpair ta tb) (a,b) = [|| ($$(liftElt ta a), $$(liftElt tb b)) ||]

