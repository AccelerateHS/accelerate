{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.VEq
-- Copyright   : [2016..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Classes.VEq
  where

import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Vec

class SIMD n a => VEq n a where
  (==*) :: Exp (Vec n a) -> Exp (Vec n a) -> Exp (Vec n Bool)
  (/=*) :: Exp (Vec n a) -> Exp (Vec n a) -> Exp (Vec n Bool)
  {-# MINIMAL (==*) | (/=*) #-}
  x ==* y = vnot (x /=* y)
  x /=* y = vnot (x ==* y)

vnot :: KnownNat n => Exp (Vec n Bool) -> Exp (Vec n Bool)

