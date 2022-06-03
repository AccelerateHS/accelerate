{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE DataKinds #-}
-- |
-- Module      : Data.Array.Accelerate.Pattern.Maybe
-- Copyright   : [2018..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Pattern.Maybe (

  Maybe, pattern Nothing_, pattern Just_,

) where

import           Data.Array.Accelerate.Smart as Smart
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Pattern.Matchable
import           Generics.SOP as SOP
import Data.Array.Accelerate.Representation.POS as POS

{-# COMPLETE Nothing_, Just_ #-}
pattern Nothing_ ::
  forall a .
  ( Elt a
  , POSable a
  , Matchable a
  ) => Exp (Maybe a)
pattern Nothing_ <- (matchNothing -> Just ()) where
  Nothing_ = buildNothing

matchNothing :: forall a . (POSable a, Elt a) => Exp (Maybe a) -> Maybe ()
matchNothing x = case match (Proxy @0) x of
  Just SOP.Nil -> Just ()
  Nothing -> Nothing

buildNothing :: forall a . (Elt a, POSable a) => Exp (Maybe a)
buildNothing = build (Proxy @0) SOP.Nil

pattern Just_ ::
  forall a .
  ( Elt a
  , POSable a
  , Matchable a
  ) => Exp a -> Exp (Maybe a)
pattern Just_ x <- (matchJust -> Just x) where
  Just_ = buildJust

matchJust :: forall a . (Elt a, POSable a) => Exp (Maybe a) -> Maybe (Exp a)
matchJust x = case match (Proxy @1) x of
  Just (x' :* SOP.Nil) -> Just x'
  Nothing -> Nothing

buildJust :: forall a . (Elt a, POSable a) => Exp a -> Exp (Maybe a)
buildJust x = build (Proxy @1) (x :* SOP.Nil)
