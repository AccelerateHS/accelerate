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

  putTraceMsg,
  trace, traceM,

) where

import Data.Array.Accelerate.Debug.Internal.Flags

import Control.Monad.Trans
import Data.Double.Conversion.Text
import Data.Text.Lazy.Builder
import Formatting

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
      let !pow  = floor (logBase b k) :: Int
          !k'   = k / (b ^^ pow)
          !unit =
            case pow of
              4  -> Just "T"
              3  -> Just "G"
              2  -> Just "M"
              1  -> Just "k"
              -1 -> Just "m"
              -2 -> Just "µ"
              -3 -> Just "n"
              -4 -> Just "p"
              _  -> Nothing
      in
      case unit of
        Nothing -> bformat (maybe float prec  mp % " ") k
        Just t  -> bformat (maybe float fixed mp % " " % builder) k' t

{-# INLINE prec #-}
prec :: Real a => Int -> Format r (a -> r)
prec digits = later (fromText . toPrecision digits . realToFrac)


-- | The 'trace' function outputs the message given as its second argument when
-- the debug mode indicated by the first argument is enabled, before returning
-- the third argument as its result. The message is prefixed with a time stamp.
--
trace :: Flag -> Builder -> a -> a
#ifdef ACCELERATE_DEBUG
{-# NOINLINE trace #-}
trace f msg expr = unsafePerformIO $ do
  traceM f builder msg
  return expr
#else
{-# INLINE trace #-}
trace _ _ expr = expr
#endif


-- | The 'traceM' function outputs the trace message together with a time
-- stamp from the MonadIO monad. This sequences the output with respect to
-- other IO actions.

-- TLM: Perhaps we should automatically format the log messages. Namely:
--        * prefix with a description of the mode (e.g. "gc: foo")
--        * align multi-line messages
--
{-# INLINE traceM #-}
traceM :: MonadIO m => Flag -> Format (m ()) a -> a
traceM _f m =
  runFormat m $ \_k -> do
#ifdef ACCELERATE_DEBUG
    when _f $ putTraceMsg (now _k)
#endif
    return ()


-- | Print a message prefixed with the current elapsed wall-clock time.
--
{-# INLINE putTraceMsg #-}
putTraceMsg :: MonadIO m => Format (m ()) a -> a
putTraceMsg m =
  runFormat m $ \_k -> liftIO $ do
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

