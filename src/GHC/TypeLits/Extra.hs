{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module      : GHC.TypeLits.Extra
-- Copyright   : [2012..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module GHC.TypeLits.Extra
  where

import Data.Typeable

import GHC.Exts
import GHC.TypeLits
import Unsafe.Coerce


-- | We either get evidence that this function was instantiated with the same
-- type-level numbers, or 'Nothing'.
--
{-# INLINEABLE sameNat' #-}
sameNat' :: (KnownNat n, KnownNat m) => Proxy# n -> Proxy# m -> Maybe (n :~: m)
sameNat' n m
  | natVal' n == natVal' m = Just (unsafeCoerce Refl) -- same as 'GHC.TypeLits.sameNat' but for 'Proxy#'
  | otherwise              = Nothing

