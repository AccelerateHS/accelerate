{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- |
-- Module      : Data.Array.Accelerate.Debug.Trace
-- Copyright   : [2008..2014] Manuel M T Chakravarty, Gabriele Keller
--               [2009..2014] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Functions for tracing and monitoring execution. These are useful for
-- investigating bugs and performance problems, but by default are not enabled
-- in performance code.
--

module Data.Array.Accelerate.Debug.Trace
  where

import Data.Array.Accelerate.Debug.Flags

import Numeric
import System.CPUTime
import System.IO.Unsafe
import Text.Printf
import qualified Debug.Trace                            as D


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
showFFloatSIBase :: RealFloat a => Maybe Int -> a -> a -> ShowS
showFFloatSIBase prec !base !k
  = showString
  $ case pow of
      4   -> with "T"
      3   -> with "G"
      2   -> with "M"
      1   -> with "k"
      -1  -> with "m"
      -2  -> with "µ"
      -3  -> with "n"
      -4  -> with "p"
      _   -> showGFloat prec k " "      -- no unit or unhandled SI prefix
  where
    !k'         = k / (base ^^ pow)
    !pow        = floor (logBase base k) :: Int
    with unit   = showFFloat prec k' (' ':unit)


-- | The 'trace' function outputs the message given as its second argument when
-- the debug mode indicated by the first argument is enabled, before returning
-- the third argument as its result. The message is prefixed with a time stamp.
--
trace :: Mode -> String -> a -> a
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
traceIO :: Mode -> String -> IO ()
#ifdef ACCELERATE_DEBUG
traceIO f msg = when f $ putTraceMsg msg
#else
{-# INLINE traceIO #-}
traceIO _ _   = return ()
#endif


-- | The 'traceEvent' function behaves like 'trace' with the difference that the
-- message is emitted to the eventlog, if eventlog profiling is enabled at
-- runtime.
--
traceEvent :: Mode -> String -> a -> a
#ifdef ACCELERATE_DEBUG
{-# NOINLINE traceEvent #-}
traceEvent f msg expr = unsafePerformIO $ do
  traceEventIO f msg
  return expr
#else
{-# INLINE traceEvent #-}
traceEvent _ _ expr = expr
#endif


-- | Print a message prefixed with the current CPU time.
--
putTraceMsg :: String -> IO ()
#ifdef ACCELERATE_DEBUG
putTraceMsg msg = do
  psec        <- getCPUTime
  let secs    = fromIntegral psec * 1E-12 :: Double
  D.traceIO   $ printf "[%8.3f] %s" secs msg
#else
putTraceMsg _   = return ()
#endif


-- | The 'traceEventIO' function emits a message to the eventlog, if eventlog
-- profiling is available and enabled at runtime.
--
-- Compared to 'traceEvent', 'traceEventIO' sequences the event with respect to
-- other IO actions.
--
traceEventIO :: Mode -> String -> IO ()
#ifdef ACCELERATE_DEBUG
traceEventIO f msg = do
  when f $ D.traceEventIO msg
#else
{-# INLINE traceEventIO #-}
traceEventIO _ _ = return ()
#endif

