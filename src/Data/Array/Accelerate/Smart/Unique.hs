{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Smart.Unique
-- Copyright   : [2026] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Smart.Unique (

  withUnique,

) where

import Data.Unique ( newUnique, Unique ) 
import System.IO.Unsafe ( unsafePerformIO )


-- | Build a value with a fresh unique identity.
--
-- Keeping the builder inside the IO action,
-- disabling CSE and full laziness, and preventing inlining
-- keep this operation safe.
--
withUnique :: (Unique -> a) -> a
withUnique f = unsafePerformIO $ f <$> newUnique
{-# NOINLINE withUnique #-}
