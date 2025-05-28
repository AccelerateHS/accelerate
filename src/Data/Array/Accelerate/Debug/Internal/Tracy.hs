{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# OPTIONS_GHC -fobject-code #-}
-- |
-- Module      : Data.Array.Accelerate.Debug.Internal.Tracy
-- Copyright   : [2016..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Debug.Internal.Tracy
  where

import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

#if defined(ACCELERATE_TRACY) && !defined(__GHCIDE__)
import Language.Haskell.TH.Syntax
#endif

type Zone   = Word64
type SrcLoc = Word64

-- SEE: [HLS and GHC IDE]
--
#if defined(ACCELERATE_TRACY) && !defined(__GHCIDE__)

#ifdef TRACY_MANUAL_LIFETIME
foreign import ccall unsafe "___tracy_startup_profiler" startup_profiler :: IO ()
foreign import ccall unsafe "___tracy_shutdown_profiler" shutdown_profiler :: IO ()
#else
startup_profiler :: IO ()
startup_profiler = return ()

shutdown_profiler :: IO ()
shutdown_profiler = return ()
#endif

foreign import ccall unsafe "___tracy_connected" tracy_connected :: IO CInt

foreign import ccall unsafe "___tracy_alloc_srcloc" alloc_srcloc :: Word32 -> CString -> CSize -> CString -> CSize -> IO SrcLoc
foreign import ccall unsafe "___tracy_alloc_srcloc_name" alloc_srcloc_name :: Word32 -> CString -> CSize -> CString -> CSize -> CString -> CSize -> IO SrcLoc

foreign import ccall unsafe "___tracy_emit_zone_begin_alloc" emit_zone_begin :: SrcLoc -> CInt -> IO Zone
foreign import ccall unsafe "___tracy_emit_zone_end" emit_zone_end :: Zone -> IO ()
foreign import ccall unsafe "___tracy_emit_zone_text" emit_zone_text :: Zone -> CString -> CSize -> IO ()
foreign import ccall unsafe "___tracy_emit_zone_name" emit_zone_name :: Zone -> CString -> CSize -> IO ()
foreign import ccall unsafe "___tracy_emit_zone_color" emit_zone_color :: Zone -> Word32 -> IO ()
foreign import ccall unsafe "___tracy_emit_zone_value" emit_zone_value :: Zone -> Word64 -> IO ()

foreign import ccall unsafe "___tracy_emit_memory_alloc" emit_memory_alloc :: Ptr a -> CSize -> CInt -> IO ()
foreign import ccall unsafe "___tracy_emit_memory_free" emit_memory_free :: Ptr a -> CInt -> IO ()
foreign import ccall unsafe "___tracy_emit_memory_alloc_named" emit_memory_alloc_named :: Ptr a -> CSize -> CInt -> CString -> IO ()
foreign import ccall unsafe "___tracy_emit_memory_free_named" emit_memory_free_named :: Ptr a -> CInt -> CString -> IO ()

foreign import ccall unsafe "___tracy_emit_message" emit_message :: CString -> CSize -> CInt -> IO ()
foreign import ccall unsafe "___tracy_emit_messageC" emit_message_colour :: CString -> CSize -> Word32 -> CInt -> IO ()
foreign import ccall unsafe "___tracy_emit_messageL" emit_message_literal :: CString -> CInt -> IO ()
foreign import ccall unsafe "___tracy_emit_messageLC" emit_message_literal_colour :: CString -> Word32 -> CInt -> IO ()

foreign import ccall unsafe "___tracy_emit_frame_mark" emit_frame_mark :: CString -> IO ()
foreign import ccall unsafe "___tracy_emit_frame_mark_start" emit_frame_mark_start :: CString -> IO ()
foreign import ccall unsafe "___tracy_emit_frame_mark_end" emit_frame_mark_end :: CString -> IO ()
foreign import ccall unsafe "___tracy_emit_frame_image" emit_frame_image :: Ptr Word32 -> Word16 -> Word16 -> Word8 -> CInt -> IO () -- width height offset flip; dimensions must be divisible by 4!

foreign import ccall unsafe "___tracy_emit_plot" emit_plot :: CString -> Double -> IO ()
foreign import ccall unsafe "___tracy_emit_message_appinfo" emit_message_appinfo :: CString -> CSize -> IO ()

#ifdef TRACY_FIBERS
foreign import ccall unsafe "___tracy_fiber_enter" fiber_enter :: CString -> IO ()
foreign import ccall unsafe "___tracy_fiber_leave" fiber_leave :: IO ()
#else
fiber_enter :: CString -> IO ()
fiber_enter _ = return ()

fiber_leave :: IO ()
fiber_leave   = return ()
#endif

-- SEE: [linking to .c files]
--
runQ $ do
  addForeignFilePath LangCxx "cbits/tracy/public/TracyClient.cpp"
  return []

#else

startup_profiler :: IO ()
startup_profiler = return ()

shutdown_profiler :: IO ()
shutdown_profiler = return ()

tracy_connected :: IO CInt
tracy_connected = return 0

alloc_srcloc :: Word32 -> CString -> CSize -> CString -> CSize -> IO SrcLoc
alloc_srcloc _ _ _ _ _ = return 0

alloc_srcloc_name :: Word32 -> CString -> CSize -> CString -> CSize -> CString -> CSize -> IO SrcLoc
alloc_srcloc_name _ _ _ _ _ _ _ = return 0

emit_zone_begin :: SrcLoc -> CInt -> IO Zone
emit_zone_begin _ _ = return 0

emit_zone_end :: Zone -> IO ()
emit_zone_end _ = return ()

emit_zone_text :: Zone -> CString -> CSize -> IO ()
emit_zone_text _ _ _ = return ()

emit_zone_name :: Zone -> CString -> CSize -> IO ()
emit_zone_name _ _ _ = return ()

emit_zone_color :: Zone -> Word32 -> IO ()
emit_zone_color _ _ = return ()

emit_zone_value :: Zone -> Word64 -> IO ()
emit_zone_value _ _ = return ()

emit_memory_alloc :: Ptr a -> CSize -> CInt -> IO ()
emit_memory_alloc _ _ _ = return ()

emit_memory_free :: Ptr a -> CInt -> IO ()
emit_memory_free _ _ = return ()

emit_memory_alloc_named :: Ptr a -> CSize -> CInt -> CString -> IO ()
emit_memory_alloc_named _ _ _ _ = return ()

emit_memory_free_named :: Ptr a -> CInt -> CString -> IO ()
emit_memory_free_named _ _ _ = return ()

emit_message :: CString -> CSize -> CInt -> IO ()
emit_message _ _ _ = return ()

emit_message_colour :: CString -> CSize -> Word32 -> CInt -> IO ()
emit_message_colour _ _ _ _ = return ()

emit_message_literal :: CString -> CInt -> IO ()
emit_message_literal _ _ = return ()

emit_message_literal_colour :: CString -> Word32 -> CInt -> IO ()
emit_message_literal_colour _ _ _ = return ()

emit_frame_mark :: CString -> IO ()
emit_frame_mark _ = return ()

emit_frame_mark_start :: CString -> IO ()
emit_frame_mark_start _ = return ()

emit_frame_mark_end :: CString -> IO ()
emit_frame_mark_end _ = return ()

emit_frame_image :: Ptr Word32 -> Word16 -> Word16 -> Word8 -> CInt -> IO ()
emit_frame_image _ _ _ _ _ = return ()

emit_plot :: CString -> Double -> IO ()
emit_plot _ _ = return ()

emit_message_appinfo :: CString -> CSize -> IO ()
emit_message_appinfo _ _ = return ()

fiber_enter :: CString -> IO ()
fiber_enter _ = return ()

fiber_leave :: IO ()
fiber_leave = return ()

#endif

