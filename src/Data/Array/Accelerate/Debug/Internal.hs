{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Debug.Internal
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Embedded array processing language: internal debugging support. This module
-- provides functionality that is useful for developers of the library. It is
-- not meant for library users.
--

module Data.Array.Accelerate.Debug.Internal (

  debuggingIsEnabled,
  tracyIsEnabled,
  boundsChecksAreEnabled,
  -- unsafeChecksAreEnabled,
  internalChecksAreEnabled,

  module Debug,

) where

import Data.Array.Accelerate.Debug.Internal.Flags                   as Debug
import Data.Array.Accelerate.Debug.Internal.Graph                   as Debug
import Data.Array.Accelerate.Debug.Internal.Profile                 as Debug
import Data.Array.Accelerate.Debug.Internal.Stats                   as Debug
import Data.Array.Accelerate.Debug.Internal.Timed                   as Debug
import Data.Array.Accelerate.Debug.Internal.Trace                   as Debug
import Data.Array.Accelerate.Debug.Internal.Tracy                   as Debug


{-# INLINE debuggingIsEnabled #-}
debuggingIsEnabled :: Bool
#ifdef ACCELERATE_DEBUG
debuggingIsEnabled = True
#else
debuggingIsEnabled = False
#endif

{-# INLINE tracyIsEnabled #-}
tracyIsEnabled :: Bool
#ifdef ACCELERATE_TRACY
tracyIsEnabled = True
#else
tracyIsEnabled = False
#endif

{-# INLINE boundsChecksAreEnabled #-}
boundsChecksAreEnabled :: Bool
#ifdef ACCELERATE_BOUNDS_CHECKS
boundsChecksAreEnabled = True
#else
boundsChecksAreEnabled = False
#endif

-- {-# INLINE unsafeChecksAreEnabled #-}
-- unsafeChecksAreEnabled :: Bool
-- #ifdef ACCELERATE_UNSAFE_CHECKS
-- unsafeChecksAreEnabled = True
-- #else
-- unsafeChecksAreEnabled = False
-- #endif

{-# INLINE internalChecksAreEnabled #-}
internalChecksAreEnabled :: Bool
#ifdef ACCELERATE_INTERNAL_CHECKS
internalChecksAreEnabled = True
#else
internalChecksAreEnabled = False
#endif

