{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Sugar.Vec
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Sugar.Vec
  where

import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Representation.Tag
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Type
import Data.Primitive.Types
import Data.Primitive.Vec

import GHC.TypeLits
import GHC.Prim


type VecElt a = (Elt a, Prim a, IsSingle a, EltR a ~ a)

instance (KnownNat n, VecElt a) => Elt (Vec n a) where
  type EltR (Vec n a) = Vec n a
  eltR    = TupRsingle (VectorScalarType (VectorType (fromIntegral (natVal' (proxy# :: Proxy# n))) singleType))
  tagsR   = [TagRsingle (VectorScalarType (VectorType (fromIntegral (natVal' (proxy# :: Proxy# n))) singleType))]
  toElt   = id
  fromElt = id

