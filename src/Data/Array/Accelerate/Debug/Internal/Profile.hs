{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# OPTIONS_GHC -fobject-code #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Debug.Internal.Profile
-- Copyright   : [2016..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Debug.Internal.Profile (

  local_memory_alloc,
  local_memory_free,

  remote_memory_alloc, remote_memory_alloc_nursery,
  remote_memory_free,  remote_memory_free_nursery,
  remote_memory_evict,

  memcpy_to_remote,
  memcpy_from_remote,

  emit_remote_gc,

) where

#ifdef ACCELERATE_DEBUG
import Control.Monad
import qualified Data.Array.Accelerate.Debug.Internal.Tracy         as Tracy
#endif

import Data.Atomic                                                  ( Atomic )
import qualified Data.Atomic                                        as Atomic

import Data.Char
import Foreign.C.String
import Foreign.Ptr
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import GHC.Ptr


-- Embed some string data into the constant section and grab a pointer
-- directly to it.
--
-- XXX: This only allows us to track a single nursery, but in reality it is
-- one per remote memory space. May or may not be useful to separate this.
--
runQ $ sequence
  [ sigD (mkName "___nursery") (conT ''CString)
  , valD (varP (mkName "___nursery")) (normalB (conE 'Ptr `appE` litE (stringPrimL (map (fromIntegral . ord) "nursery\0")))) []
  ]

-- Allocations in the local memory space
--
{-# INLINE local_memory_alloc #-}
{-# INLINE local_memory_free  #-}
local_memory_alloc :: Ptr a -> Int -> IO ()
local_memory_free  :: Ptr a -> IO ()
#ifndef ACCELERATE_DEBUG
local_memory_alloc _ _ = return ()
local_memory_free _    = return ()
#else

local_memory_alloc _p n = do
  Tracy.emit_memory_alloc _p (fromIntegral n) 0
  void $ Atomic.add __total_bytes_allocated_local (fromIntegral n)

local_memory_free _p =
  Tracy.emit_memory_free _p 0
#endif


-- Allocations in the remote memory space
--
{-# INLINE remote_memory_alloc #-}
{-# INLINE remote_memory_free  #-}
{-# INLINE remote_memory_evict #-}
remote_memory_alloc :: CString -> Ptr a -> Int -> IO ()
remote_memory_free  :: CString -> Ptr a -> IO ()
remote_memory_evict :: CString -> Ptr a -> Int -> IO ()
#ifndef ACCELERATE_DEBUG
remote_memory_alloc _ _ _ = return ()
remote_memory_free _ _    = return ()
remote_memory_evict _ _ _ = return ()
#else
remote_memory_alloc _name _ptr bytes = do
  Tracy.emit_memory_alloc_named _ptr (fromIntegral bytes) 0 _name
  void $ Atomic.add __total_bytes_allocated_remote (fromIntegral bytes)

remote_memory_free _name _ptr = do
  Tracy.emit_memory_free_named _ptr 0 _name

remote_memory_evict name ptr bytes = do
  void $ Atomic.add __num_evictions 1
  void $ Atomic.add __total_bytes_evicted_from_remote (fromIntegral bytes)
  remote_memory_free name ptr
  memcpy_from_remote bytes
#endif

remote_memory_alloc_nursery :: Ptr a -> Int -> IO ()
remote_memory_free_nursery  :: Ptr a -> IO ()
#ifndef ACCELERATE_DEBUG
remote_memory_alloc_nursery _ _ = return ()
remote_memory_free_nursery _    = return ()
#else
remote_memory_alloc_nursery p n = Tracy.emit_memory_alloc_named p (fromIntegral n) 0 ___nursery
remote_memory_free_nursery p    = Tracy.emit_memory_free_named p 0 ___nursery
#endif


-- Data transfer between memory spaces
--
{-# INLINE memcpy_to_remote   #-}
{-# INLINE memcpy_from_remote #-}
memcpy_to_remote   :: Int -> IO ()
memcpy_from_remote :: Int -> IO ()
#ifndef ACCELERATE_DEBUG
memcpy_to_remote   _ = return ()
memcpy_from_remote _ = return ()
#else
memcpy_to_remote   n = void $ Atomic.add __total_bytes_copied_to_remote (fromIntegral n)
memcpy_from_remote n = void $ Atomic.add __total_bytes_copied_from_remote (fromIntegral n)
#endif


-- Performed a major GC of the remote memory space
--
{-# INLINE emit_remote_gc #-}
emit_remote_gc :: IO ()
#ifndef ACCELERATE_DEBUG
emit_remote_gc = return ()
#else
emit_remote_gc = void $ Atomic.add __num_remote_gcs 1
#endif


-- Monitoring variables
-- --------------------

-- SEE: [HLS and GHC IDE]
--
#ifndef __GHCIDE__

foreign import ccall "&__total_bytes_allocated_local"     __total_bytes_allocated_local     :: Atomic -- bytes allocated in the local (CPU) memory space
foreign import ccall "&__total_bytes_allocated_remote"    __total_bytes_allocated_remote    :: Atomic -- bytes allocated in the remote memory space (if it is separate, e.g. GPU)
foreign import ccall "&__total_bytes_copied_to_remote"    __total_bytes_copied_to_remote    :: Atomic -- bytes copied to the remote memory space
foreign import ccall "&__total_bytes_copied_from_remote"  __total_bytes_copied_from_remote  :: Atomic -- bytes copied from the remote memory space
foreign import ccall "&__total_bytes_evicted_from_remote" __total_bytes_evicted_from_remote :: Atomic -- total bytes copied from the remote due to evictions
foreign import ccall "&__num_remote_gcs"                  __num_remote_gcs                  :: Atomic -- number of times the remote memory space was forcibly garbage collected
foreign import ccall "&__num_evictions"                   __num_evictions                   :: Atomic -- number of LRU eviction events

#else

__total_bytes_allocated_local :: Atomic
__total_bytes_allocated_local = undefined

__total_bytes_allocated_remote :: Atomic
__total_bytes_allocated_remote = undefined

__total_bytes_copied_to_remote :: Atomic
__total_bytes_copied_to_remote = undefined

__total_bytes_copied_from_remote :: Atomic
__total_bytes_copied_from_remote = undefined

__total_bytes_evicted_from_remote :: Atomic
__total_bytes_evicted_from_remote = undefined

__num_remote_gcs :: Atomic
__num_remote_gcs = undefined

__num_evictions :: Atomic
__num_evictions = undefined

#endif

-- SEE: [linking to .c files]
--
runQ $ do
  addForeignFilePath LangC "cbits/monitoring.c"
  return []

