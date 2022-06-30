{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE DataKinds #-}
-- |
-- Module      : Data.Array.Accelerate.Pattern.Bool
-- Copyright   : [2018..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Pattern.Bool (

  Bool, pattern True_, pattern False_,

) where

import           Data.Array.Accelerate.Smart as Smart
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Pattern.Matchable
import           Generics.SOP as SOP
import Data.Array.Accelerate.Representation.POS as POS

{-# COMPLETE False_, True_ #-}
pattern False_ :: Exp Bool
pattern False_ <- (matchFalse -> Just ()) where
  False_ = buildFalse

matchFalse :: Exp Bool -> Maybe ()
matchFalse x = case match (Proxy @0) x of
  Just SOP.Nil -> Just ()
  Nothing -> Nothing

buildFalse :: Exp Bool
buildFalse = build (Proxy @0) SOP.Nil

pattern True_ :: Exp Bool
pattern True_ <- (matchTrue -> Just x) where
  True_ = buildTrue

matchTrue :: Exp Bool -> Maybe ()
matchTrue x = case match (Proxy @1) x of
  Just SOP.Nil -> Just ()
  Nothing -> Nothing

buildTrue :: Exp Bool
buildTrue = build (Proxy @1) SOP.Nil
