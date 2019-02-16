{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE NoImplicitPrelude        #-}
{-# LANGUAGE UnboxedTuples            #-}
-- |
-- Module      : Data.Atomic
-- Copyright   : [2016..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Atomic integer values. All operations are thread safe.
--

module Data.Atomic (

  Atomic(..),
  read, write, add, and, subtract,

) where

import Data.Int

import GHC.Ptr
import GHC.Base


-- | A mutable atomic integer
--
newtype Atomic = Atomic ( Ptr Int64 )

-- -- | Create a new atomic variable initialised to the given value
-- --
-- -- TLM: This is not correct because we need to keep the MutableByteArray#
-- --      around so that it does not get GC'ed. This would have been stored as
-- --      the ForeignPtrContents. Since we don't use this function at the
-- --      moment (all of the atomics we care about are defined in C code) we
-- --      just drop this function for now.
-- --
-- new :: Int64 -> IO Atomic
-- new v = do
--   a <- IO $ \s -> case newPinnedByteArray# 8# s of
--                     (# s', mbarr# #) -> (# s', Atomic (Ptr (byteArrayContents# (unsafeCoerce# mbarr#))) #)
--   write a v
--   return a

-- | Get the current value.
--
foreign import ccall unsafe "atomic_read_64" read :: Atomic -> IO Int64

-- | Set the atomic to the given value.
--
foreign import ccall unsafe "atomic_write_64" write :: Atomic -> Int64 -> IO ()

-- | Increase the atomic by the given amount. Returns the old value.
--
foreign import ccall unsafe "atomic_fetch_and_add_64" add :: Atomic -> Int64 -> IO Int64

-- | Bitwise AND the atomic with the given value. Return the old value.
--
foreign import ccall unsafe "atomic_fetch_and_and_64" and :: Atomic -> Int64 -> IO Int64

-- | Decrement the atomic value by the given amount. Return the old value.
--
foreign import ccall unsafe "atomic_fetch_and_sub_64" subtract :: Atomic -> Int64 -> IO Int64

