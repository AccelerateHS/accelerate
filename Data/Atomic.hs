{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      : Data.Atomic
-- Copyright   : [2016] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Atomic integer values. All operations are thread safe.
--

module Data.Atomic (

  Atomic,
  new,
  add, and,

) where

import Data.Int
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Prelude                                                      ( ($), IO, return )


-- | A mutable atomic integer
--
newtype Atomic = Atomic ( ForeignPtr Int64 )

-- | Create a new atomic variable initialised to the given value
--
new :: Int64 -> IO Atomic
new v = do
  fp <- mallocForeignPtr
  withForeignPtr fp $ \p -> poke p v
  return $ Atomic fp

-- | Increase the atomic by the given amount. Returns the old value.
--
add :: Atomic -> Int64 -> IO Int64
add (Atomic fp) v =
  withForeignPtr fp $ \p -> atomic_fetch_and_add_64 p v

-- | Bitwise AND the atomic with the given value. Return the old value.
--
and :: Atomic -> Int64 -> IO Int64
and (Atomic fp) v =
  withForeignPtr fp $ \p -> atomic_fetch_and_and_64 p v


-- Perform the operation suggested by the name and return the old value
--
-- > { tmp = *ptr; *ptr op= value; return tmp; }
--
foreign import ccall unsafe "hs_atomic_fetch_and_add_64" atomic_fetch_and_add_64 :: Ptr Int64 -> Int64 -> IO Int64
foreign import ccall unsafe "hs_atomic_fetch_and_and_64" atomic_fetch_and_and_64 :: Ptr Int64 -> Int64 -> IO Int64

