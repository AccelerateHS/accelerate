{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.VOrd
-- Copyright   : [2016..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Classes.VOrd (

  VOrd(..),

) where

import Data.Array.Accelerate.Classes.VEq
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Vec
import {-# SOURCE #-} Data.Array.Accelerate.Classes.Ord

import Prelude                                                      hiding ( Ord(..), Ordering(..), (<*) )


class VEq n a => VOrd n a where
  {-# MINIMAL (<=*) | vcompare #-}
  (<*)     :: Exp (Vec n a) -> Exp (Vec n a) -> Exp (Vec n Bool)
  (>*)     :: Exp (Vec n a) -> Exp (Vec n a) -> Exp (Vec n Bool)
  (<=*)    :: Exp (Vec n a) -> Exp (Vec n a) -> Exp (Vec n Bool)
  (>=*)    :: Exp (Vec n a) -> Exp (Vec n a) -> Exp (Vec n Bool)
  vmin     :: Exp (Vec n a) -> Exp (Vec n a) -> Exp (Vec n a)
  vmax     :: Exp (Vec n a) -> Exp (Vec n a) -> Exp (Vec n a)
  vminimum :: Exp (Vec n a) -> Exp a
  vmaximum :: Exp (Vec n a) -> Exp a
  vcompare :: Exp (Vec n a) -> Exp (Vec n a) -> Exp (Vec n Ordering)

  (<*)  = undefined
  (<=*) = undefined
  (>*)  = undefined
  (>=*) = undefined

  vmin = undefined
  vmax = undefined

  default vminimum :: Ord a => Exp (Vec n a) -> Exp a
  default vmaximum :: Ord a => Exp (Vec n a) -> Exp a
  vminimum = undefined
  vmaximum = undefined

  vcompare = undefined

