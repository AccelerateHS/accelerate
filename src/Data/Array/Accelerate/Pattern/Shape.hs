{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE ViewPatterns           #-}
-- |
-- Module      : Data.Array.Accelerate.Pattern.Shape
-- Copyright   : [2018..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Pattern.Shape (

  pattern Z, Z,
  pattern (:.), (:.),
  pattern All, All,
  pattern Any, Any,

) where

import Data.Array.Accelerate.AST.Idx
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Sugar.Shape                            ( (:.), Z, All, Any )
import qualified Data.Array.Accelerate.Sugar.Shape                  as Sugar


pattern Z :: IsShapeZ z => z
pattern Z <- (z_matcher -> True)
  where Z = z_builder
{-# COMPLETE Z #-}

pattern All :: IsShapeAll all => all
pattern All <- (all_matcher -> True)
  where All = all_builder
{-# COMPLETE All #-}

pattern Any :: IsShapeAny any => any
pattern Any <- (any_matcher -> True)
  where Any = any_builder
{-# COMPLETE Any #-}

infixl 3 :.
pattern (:.) :: IsShapeSnoc t h s => t -> h -> s
pattern t :. h <- (snoc_matcher -> (t Sugar.:. h))
  where t :. h = snoc_builder t h
{-# COMPLETE (:.) #-}


class IsShapeZ z where
  z_matcher :: z -> Bool
  z_builder :: z

instance IsShapeZ Z where
  z_matcher _ = True
  z_builder   = Sugar.Z

instance IsShapeZ (Exp Z) where
  z_matcher _ = True
  z_builder = constant Sugar.Z

class IsShapeAll all where
  all_matcher :: all -> Bool
  all_builder :: all

instance IsShapeAll All where
  all_matcher _ = True
  all_builder   = Sugar.All

instance IsShapeAll (Exp All) where
  all_matcher _ = True
  all_builder = constant Sugar.All

class IsShapeAny any where
  any_matcher :: any -> Bool
  any_builder :: any

instance IsShapeAny (Any sh) where
  any_matcher _ = True
  any_builder   = Sugar.Any

instance Elt (Any sh) => IsShapeAny (Exp (Any sh)) where
  any_matcher _ = True
  any_builder = constant Sugar.Any

class IsShapeSnoc t h s | s -> t, s -> h where
  snoc_matcher :: s -> (t :. h)
  snoc_builder :: t -> h -> s

instance IsShapeSnoc (Exp t) (Exp h) (Exp (t :. h)) where
  snoc_builder (Exp a) (Exp b) = Exp $ SmartExp $ Pair a b
  snoc_matcher (Exp t)         = Exp (SmartExp $ Prj PairIdxLeft t) Sugar.:. Exp (SmartExp $ Prj PairIdxRight t)

instance IsShapeSnoc t h (t :. h) where
  snoc_builder = (Sugar.:.)
  snoc_matcher = id

