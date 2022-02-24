{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
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
import Data.Array.Accelerate.Representation.POS
import Data.Array.Accelerate.Type
import Data.Primitive.Types
import Data.Primitive.Vec
import Data.Kind

import GHC.TypeLits
import GHC.Prim


type VecElt a = (Elt a, Prim a, IsSingle a)

instance GroundType (Vec n a)

instance (KnownNat n, VecElt a, Num a) => POSable (Vec n a) where
    type Choices (Vec n a) = 1

    choices _ = 0

    emptyChoices = 0

    fromPOSable 0 (Cons (Pick x) Nil) = x

    type Fields (Vec n a) = '[ '[Vec n a]]
    fields x = Cons (Pick x) Nil

    emptyFields = PTCons (STSucc (replicateVecN 0) STZero) PTNil


-- Elt instance automatically derived from POSable instance
instance (KnownNat n, VecElt a, Num a) => (Elt (Vec n a))
