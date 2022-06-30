{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE DataKinds #-}
-- |
-- Module      : Data.Array.Accelerate.Pattern.Either
-- Copyright   : [2018..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Pattern.Either (

  Either, pattern Left_, pattern Right_,

) where

import           Data.Array.Accelerate.Smart as Smart
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Pattern.Matchable
import           Generics.SOP as SOP
import Data.Array.Accelerate.Representation.POS as POS

{-# COMPLETE Left_, Right_ #-}
pattern Left_ ::
  forall a b .
  ( Elt a
  , POSable a
  , POSable b
  , Matchable a
  ) => Exp a -> Exp (Either a b)
pattern Left_ x <- (matchLeft -> Just x) where
  Left_ = buildLeft

matchLeft :: forall a b . (POSable a, Elt a, POSable b) => Exp (Either a b) -> Maybe (Exp a)
matchLeft x = case match (Proxy @0) x of
  Just (x' :* SOP.Nil) -> Just x'
  Nothing -> Nothing

buildLeft :: forall a b . (Elt a, POSable a, POSable b) => Exp a -> Exp (Either a b)
buildLeft x = build (Proxy @0) (x :* SOP.Nil)

pattern Right_ ::
  forall a b .
  ( Elt a
  , POSable a
  , POSable b
  , Matchable a
  ) => Exp b -> Exp (Either a b)
pattern Right_ x <- (matchRight -> Just x) where
  Right_ = buildRight

matchRight :: forall a b . (Elt a, POSable a, POSable b) => Exp (Either a b) -> Maybe (Exp b)
matchRight x = case match (Proxy @1) x of
  Just (x' :* SOP.Nil) -> Just x'
  Nothing -> Nothing

buildRight :: forall a b . (Elt a, POSable a, POSable b) => Exp b -> Exp (Either a b)
buildRight x = build (Proxy @1) (x :* SOP.Nil)
