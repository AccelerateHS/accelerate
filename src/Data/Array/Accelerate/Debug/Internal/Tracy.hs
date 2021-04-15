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
import Language.Haskell.TH.Syntax


type TracyCZoneCtx = Word64
type TracyCSrcLoc  = Word64

foreign import ccall unsafe "___tracy_init_thread" ___tracy_init_thread :: IO ()
foreign import ccall unsafe "___tracy_set_thread_name" ___tracy_set_thread_name :: CString -> IO ()

foreign import ccall unsafe "___tracy_alloc_srcloc" ___tracy_alloc_srcloc :: Word32 -> CString -> CSize -> CString -> CSize -> IO TracyCSrcLoc
foreign import ccall unsafe "___tracy_alloc_srcloc_name" ___tracy_alloc_srcloc_name :: Word32 -> CString -> CSize -> CString -> CSize -> CString -> CSize -> IO TracyCSrcLoc

foreign import ccall unsafe "___tracy_emit_zone_begin_alloc" ___tracy_emit_zone_begin_alloc :: TracyCSrcLoc -> CInt -> IO TracyCZoneCtx
foreign import ccall unsafe "___tracy_emit_zone_end" ___tracy_emit_zone_end :: TracyCZoneCtx -> IO ()
foreign import ccall unsafe "___tracy_emit_zone_text" ___tracy_emit_zone_text :: TracyCZoneCtx -> CString -> CSize -> IO ()
foreign import ccall unsafe "___tracy_emit_zone_name" ___tracy_emit_zone_name :: TracyCZoneCtx -> CString -> CSize -> IO ()
foreign import ccall unsafe "___tracy_emit_zone_color" ___tracy_emit_zone_color :: TracyCZoneCtx -> Word32 -> IO ()
foreign import ccall unsafe "___tracy_emit_zone_value" ___tracy_emit_zone_value :: TracyCZoneCtx -> Word64 -> IO ()

foreign import ccall unsafe "___tracy_emit_memory_alloc" ___tracy_emit_memory_alloc :: Ptr () -> CSize -> CInt -> IO ()
foreign import ccall unsafe "___tracy_emit_memory_free" ___tracy_emit_memory_free :: Ptr () -> CInt -> IO ()
foreign import ccall unsafe "___tracy_emit_memory_alloc_named" ___tracy_emit_memory_alloc_named :: Ptr () -> CSize -> CInt -> CString -> IO ()
foreign import ccall unsafe "___tracy_emit_memory_free_named" ___tracy_emit_memory_free_named :: Ptr () -> CInt -> CString -> IO ()

foreign import ccall unsafe "___tracy_emit_message"  ___tracy_emit_message :: CString -> CSize -> CInt -> IO ()
foreign import ccall unsafe "___tracy_emit_messageC" ___tracy_emit_message_colour :: CString -> CSize -> Word32 -> CInt -> IO ()
foreign import ccall unsafe "___tracy_emit_messageL"  ___tracy_emit_message_literal :: CString -> CInt -> IO ()
foreign import ccall unsafe "___tracy_emit_messageLC" ___tracy_emit_message_literal_colour :: CString -> Word32 -> CInt -> IO ()

foreign import ccall unsafe "___tracy_emit_frame_mark" ___tracy_emit_frame_mark :: CString -> IO ()
foreign import ccall unsafe "___tracy_emit_frame_mark_start" ___tracy_emit_frame_mark_start :: CString -> IO ()
foreign import ccall unsafe "___tracy_emit_frame_mark_end" ___tracy_emit_frame_mark_end :: CString -> IO ()
foreign import ccall unsafe "___tracy_emit_frame_image" ___tracy_emit_frame_image :: Ptr () -> Word16 -> Word16 -> Word8 -> CInt -> IO () -- width height offset flip

foreign import ccall unsafe "___tracy_emit_plot" ___tracy_emit_plot :: CString -> Double -> IO ()
foreign import ccall unsafe "___tracy_emit_message_appinfo" ___tracy_emit_message_appinfo :: CString -> CSize -> IO ()

-- SEE: [linking to .c files]
--
runQ $ do
  addForeignFilePath LangCxx "cbits/tracy/TracyClient.cpp"
  return []

