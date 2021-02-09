-- |
-- Module      : Data.Array.Accelerate.Debug.Trace
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Functions for tracing and monitoring execution. These are useful for
-- investigating bugs.
--
-- @since 1.4.0.0
--

module Data.Array.Accelerate.Debug.Trace (

  -- * Tracing
  -- $tracing
  --
  atrace, atraceArray, atraceId, atraceExp,

) where

import Data.Array.Accelerate.Language

-- $tracing
--
-- The 'atrace', 'atraceArray', 'atraceId', and 'atraceExp' functions print
-- messages to an output stream. They are intended for \"printf
-- debugging\", that is: tracing the flow of execution and printing
-- interesting values.
--
-- Note that arrays are printed in their internal representation (using
-- 'Data.Array.Accelerate.Sugar.Array.ArraysR'), which causes that tuples
-- or custom data types are shown differently.
--
-- These functions have the same caveats as those defined in "Debug.Trace".
--
