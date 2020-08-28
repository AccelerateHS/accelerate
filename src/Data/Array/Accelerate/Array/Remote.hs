{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Array.Remote
-- Copyright   : [2015..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Umbrella module for the remote memory management facilities. To
-- implement an LRU cache for your backend, provide an instance of the
-- 'RemoteMemory' class, and, if required, specialise or overload the LRU
-- functions to your particular memory table types.
--

module Data.Array.Accelerate.Array.Remote (

  module Remote

) where

import Data.Array.Accelerate.Array.Remote.Class         as Remote
import Data.Array.Accelerate.Array.Remote.LRU           as Remote

