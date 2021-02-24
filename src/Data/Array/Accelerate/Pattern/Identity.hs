{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveAnyClass      #-}
-- |
-- Module      : Data.Array.Accelerate.Pattern.Either
-- Copyright   : [2018..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Pattern.Identity (

  Identity(..), pattern Identity_, runIdentity_

) where
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Pattern.TH
import GHC.Generics

data Identity a = Identity {runIdentity :: a}
  deriving (Show, Generic, Elt)

mkPattern ''Identity

