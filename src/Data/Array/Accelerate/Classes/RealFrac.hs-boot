{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.RealFrac
-- Copyright   : [2019] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
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

