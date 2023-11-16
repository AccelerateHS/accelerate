{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.VOrd
-- Copyright   : [2016..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Classes.Ord (

  Ord(..),
  Ordering,

) where

import Data.Array.Accelerate.Classes.Eq
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Pattern.Ordering


class Eq a => Ord a where
  {-# MINIMAL (<=) | compare #-}
  (<)     :: Exp a -> Exp a -> Exp Bool
  (>)     :: Exp a -> Exp a -> Exp Bool
  (<=)    :: Exp a -> Exp a -> Exp Bool
  (>=)    :: Exp a -> Exp a -> Exp Bool
  min     :: Exp a -> Exp a -> Exp a
  max     :: Exp a -> Exp a -> Exp a
  compare :: Exp a -> Exp a -> Exp Ordering

  (<)  = undefined
  (<=) = undefined
  (>)  = undefined
  (>=) = undefined

  min = undefined
  max = undefined

  compare = undefined

