{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
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
  formatSIBase,

  putTraceMsg, dprint,
  trace, traceIO,

) where

import Data.Array.Accelerate.Debug.Internal.Flags

import Formatting
import Data.Text.Lazy.Builder

#ifdef ACCELERATE_DEBUG
import Data.Array.Accelerate.Debug.Internal.Clock
import System.IO                                                    hiding ( stderr )
import System.IO.Unsafe
import qualified Data.Text.IO                                       as T

import GHC.MVar
import GHC.IO.Encoding
import GHC.IO.Handle.Types
import GHC.IO.Handle.Internals
import qualified GHC.IO.FD                                          as FD
#if defined(mingw32_HOST_OS)
import Foreign.C.Types
#endif
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
{-# INLINEABLE showFFloatSIBase #-}
showFFloatSIBase :: RealFloat a => Maybe Int -> a -> a -> Builder -> Builder
showFFloatSIBase mp !b !k !t = bformat (formatSIBase mp b % builder) k t

{-# INLINEABLE formatSIBase #-}
formatSIBase :: RealFloat a => Maybe Int -> a -> Format r (a -> r)
formatSIBase mp !b = later go
  where
    go k =
      let !pow = floor (logBase b k) :: Int
          !k'  = k / (b ^^ pow)
          unit = case pow of
                   4 -> "T"
                   3 -> "G"
                   2 -> "M"
                   1 -> "k"
                   -1 -> "m"
                   -2 -> "µ"
                   -3 -> "n"
                   -4 -> "p"
                   _  -> ""
      in
      bformat (maybe float fixed mp % " " % builder) k' unit


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
putTraceMsg msg = dprint builder msg
#else
putTraceMsg _   = return ()
#endif


-- | Run the formatter and print out the text to stderr prefixed with the
-- current elapsed wall-clock time
--
{-# INLINE dprint #-}
dprint :: Format (IO ()) a -> a
dprint m =
  runFormat m $ \_k -> do
#ifdef ACCELERATE_DEBUG
    timestamp <- getProgramTime
    T.hPutStr stderr . sformat (squared (rfixed 8 ' ' (fixed 3)) % " " % builder % "\n") timestamp $ _k
#endif
    return ()


#ifdef ACCELERATE_DEBUG
-- | A handle managing output to the Haskell program's standard output
-- channnel.
--
-- In contrast to 'System.IO.stderr' this output handle is (line) buffered,
-- which prevents garbled output when used my multiple threads. Stolen from
-- 'GHC.IO.Handle.FD.stderr'.
--
{-# NOINLINE stderr #-}
stderr :: Handle
stderr = unsafePerformIO $ do
  -- TODO: acquire lock
  setBinaryMode FD.stderr
  enc <- getLocaleEncoding
  mkHandle FD.stderr "<stderr>" WriteHandle True -- this stderr IS buffered
               (Just enc)
               nativeNewlineMode -- translate newlines
               (Just stdHandleFinalizer) Nothing

stdHandleFinalizer :: FilePath -> MVar Handle__ -> IO ()
stdHandleFinalizer fp m = do
  h_ <- takeMVar m
  flushWriteBuffer h_
  case haType h_ of
    ClosedHandle -> return ()
    _            -> closeTextCodecs h_
  putMVar m (ioe_finalizedHandle fp)

-- We have to put the FDs into binary mode on Windows to avoid the newline
-- translation that the CRT IO library does.
setBinaryMode :: FD.FD -> IO ()
#if defined(mingw32_HOST_OS)
setBinaryMode fd = do _ <- setmode (FD.fdFD fd) True
                      return ()

foreign import ccall unsafe "__hscore_setmode" setmode :: CInt -> Bool -> IO CInt
#else
setBinaryMode _ = return ()
#endif
#endif

