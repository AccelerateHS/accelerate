{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Data.Array.Accelerate.Debug.Internal.Trace
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Functions for tracing and monitoring execution. These are useful for
-- investigating bugs and performance problems, but by default are not enabled
-- in performance code.
--

module Data.Array.Accelerate.Debug.Internal.Trace (

  showFFloatSIBase,

  putTraceMsg,
  trace, traceIO,

) where

import Data.Array.Accelerate.Debug.Internal.Flags

#ifdef ACCELERATE_DEBUG
import Data.Array.Accelerate.Debug.Internal.Clock
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.RealFloat
import Data.Text.Format
import System.IO
import System.IO.Unsafe
#endif


-- | Show a signed 'RealFloat' value using SI unit prefixes. In the call to:
--
-- > showFFloatSIBase prec base val
--
-- If @prec@ is @'Nothing'@ the value is shown to full precision, and if @prec@
-- is @'Just' d@, then at most @d@ digits are shown after the decimal place.
-- Here @base@ represents the increment size between multiples of the original
-- unit. For measures in base-10 this will be 1000 and for values in base-2 this
-- is usually 1024, for example when measuring seconds versus bytes,
-- respectively.
--
{-# NOINLINE[0] showFFloatSIBase #-}
{-# SPECIALISE showFFloatSIBase :: Maybe Int -> Double -> Double -> Builder -> Builder #-}
showFFloatSIBase :: RealFloat a => Maybe Int -> a -> a -> Builder -> Builder
showFFloatSIBase mp !base !k !t
  = case pow of
      4   -> with "T"
      3   -> with "G"
      2   -> with "M"
      1   -> with "k"
      -1  -> with "m"
      -2  -> with "µ"
      -3  -> with "n"
      -4  -> with "p"
      _   -> case mp of       -- no unit or unhandled SI prefix
               Nothing -> realFloat k <> singleton ' '
               Just p  -> prec p k    <> singleton ' '
  where
    with unit   = kb <> singleton ' ' <> unit <> t
    !k'         = k / (base ^^ pow)
    !pow        = floor (logBase base k) :: Int
    kb          = case mp of
                    Nothing -> realFloat k'
                    Just p  -> fixed p k'


-- | The 'trace' function outputs the message given as its second argument when
-- the debug mode indicated by the first argument is enabled, before returning
-- the third argument as its result. The message is prefixed with a time stamp.
--
trace :: Flag -> Builder -> a -> a
#ifdef ACCELERATE_DEBUG
{-# NOINLINE trace #-}
trace f msg expr = unsafePerformIO $ do
  traceIO f msg
  return expr
#else
{-# INLINE trace #-}
trace _ _ expr = expr
#endif


-- | The 'traceIO' function outputs the trace message together with a time stamp
-- from the IO monad. This sequences the output with respect to other IO
-- actions.

-- TLM: Perhaps we should automatically format the log messages. Namely:
--        * prefix with a description of the mode (e.g. "gc: foo")
--        * align multi-line messages
--
{-# INLINE traceIO #-}
traceIO :: Flag -> Builder -> IO ()
#ifdef ACCELERATE_DEBUG
traceIO f msg = when f $ putTraceMsg msg
#else
traceIO _ _   = return ()
#endif


-- | Print a message prefixed with the current elapsed wall-clock time.
--
{-# INLINE putTraceMsg #-}
putTraceMsg :: Builder -> IO ()
#ifdef ACCELERATE_DEBUG
putTraceMsg msg = do
  timestamp <- getProgramTime
  hprint stderr "[{}] {}\n" (left 8 ' ' (fixed 3 timestamp), msg)
  hFlush stderr
#else
putTraceMsg _   = return ()
#endif

