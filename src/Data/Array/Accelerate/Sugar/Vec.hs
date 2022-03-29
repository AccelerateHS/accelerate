{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Data.Array.Accelerate.Representation.POS
import Data.Array.Accelerate.Type
import Data.Primitive.Types
import Data.Primitive.Vec


type VecElt a = (Elt a, Prim a, IsSingle a, GroundType a, Num a)

instance VecElt a => POSable (Vec2 a) where
    type Choices (Vec2 a) = 1

    choices _ = 0

    emptyChoices = 0

    fromPOSable 0 (Cons (Pick a) (Cons (Pick b) Nil)) = Vec2 a b

    type Fields (Vec2 a) = '[ '[a], '[a]]
    fields (Vec2 a b) = Cons (Pick a) (Cons (Pick b) Nil)

    emptyFields = PTCons (STSucc (mkTypeRep @a) STZero) (PTCons (STSucc (mkTypeRep @a) STZero) PTNil)

    type OuterChoices (Vec2 a) = 1
    outerChoice _ = 0

-- Elt instance automatically derived from POSable instance
instance VecElt a => Elt (Vec2 a)


instance VecElt a => POSable (Vec4 a) where
    type Choices (Vec4 a) = 1

    choices _ = 0

    emptyChoices = 0

    fromPOSable 0 ( Cons (Pick a) (Cons (Pick b) (Cons (Pick c) (Cons (Pick d) Nil)))) = Vec4 a b c d

    type Fields (Vec4 a) = '[ '[a], '[a], '[a], '[a]]
    fields (Vec4 a b c d) = Cons (Pick a) (Cons (Pick b) (Cons (Pick c) (Cons (Pick d) Nil)))

    emptyFields = PTCons (STSucc (mkTypeRep @a) STZero) (PTCons (STSucc (mkTypeRep @a) STZero) (PTCons (STSucc (mkTypeRep @a) STZero) (PTCons (STSucc (mkTypeRep @a) STZero) PTNil)))

    type OuterChoices (Vec4 a) = 1
    outerChoice _ = 0


-- Elt instance automatically derived from POSable instance
instance VecElt a => Elt (Vec4 a)

-- TODO: instances for 8 and 16, probably with some TH
