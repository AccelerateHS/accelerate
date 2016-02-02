{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Array.Remote
-- Copyright   : [2015..2016] Manuel M T Chakravarty, Gabriele Keller, Robert Clifton-Everest
--               [2016]              Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Robert Clifton-Everest <robertce@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Umbrella module for the remote memory management facilities. To implement an
-- LRU cache for your backend, provide an instance of the 'RemoteMemory' class,
-- and, if required, specialise or overload the LRU functions to your particular
-- memory table types.
--

module Data.Array.Accelerate.Array.Remote (

  module Remote

) where

import Data.Array.Accelerate.Array.Remote.Class         as Remote
import Data.Array.Accelerate.Array.Remote.LRU           as Remote

