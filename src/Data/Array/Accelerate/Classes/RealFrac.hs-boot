{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.RealFrac
-- Copyright   : [2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Classes.RealFrac
  where

import Data.Array.Accelerate.Type

class RealFrac a

instance RealFrac Half
instance RealFrac Float
instance RealFrac Double
instance RealFrac CFloat
instance RealFrac CDouble

