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

import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Vec
import Data.Array.Accelerate.Classes.VEq


class VEq n a => VOrd n a where
  {-# MINIMAL (<=*) | vcompare #-}
  (<*)     :: Exp (Vec n a) -> Exp (Vec n a) -> Exp (Vec n Bool)
  (>*)     :: Exp (Vec n a) -> Exp (Vec n a) -> Exp (Vec n Bool)
  (<=*)    :: Exp (Vec n a) -> Exp (Vec n a) -> Exp (Vec n Bool)
  (>=*)    :: Exp (Vec n a) -> Exp (Vec n a) -> Exp (Vec n Bool)
  vmin     :: Exp (Vec n a) -> Exp (Vec n a) -> Exp (Vec n a)
  vmax     :: Exp (Vec n a) -> Exp (Vec n a) -> Exp (Vec n a)
  vcompare :: Exp (Vec n a) -> Exp (Vec n a) -> Exp (Vec n Ordering)

  x <*  y  = select (vcompare x y ==* vlt) vtrue vfalse
  x <=* y  = select (vcompare x y ==* vgt) vfalse vtrue
  x >*  y  = select (vcompare x y ==* vgt) vtrue vfalse
  x >=* y  = select (vcompare x y ==* vlt) vfalse vtrue

  vmin x y = select (x <=* y) x y
  vmax x y = select (x <=* y) y x

  vcompare x y
    = select (x ==* y) veq
    $ select (x <=* y) vlt vgt

vtrue, vfalse :: KnownNat n => Exp (Vec n Bool)

