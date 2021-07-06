{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Array.Unique
-- Copyright   : [2016..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Array.Unique
  where

import Data.Array.Accelerate.Lifetime

import Control.Applicative
import Control.Concurrent.Unique
import Control.DeepSeq
import Data.Word
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Language.Haskell.TH.Extra
import System.IO.Unsafe
import Prelude


-- | A uniquely identifiable array.
--
-- For the purposes of memory management, we use arrays as keys in a table. For
-- this reason we need a way to uniquely identify each array we create. We do
-- this by attaching a unique identifier to each array.
--
-- Note: [Unique array strictness]
--
-- The actual array data is in many cases unnecessary. For discrete memory
-- backends such as for GPUs, we require the unique identifier to track the data
-- in the remote memory space, but the data will in most cases never be copied
-- back to the host. Thus, the array payload field is only lazily allocated, and
-- we should be careful not to make this field overly strict.
--
data UniqueArray e = UniqueArray
    { uniqueArrayId   :: {-# UNPACK #-} !Unique
    , uniqueArrayData :: {-# UNPACK #-} !(Lifetime (ForeignPtr e))
    }

instance NFData (UniqueArray e) where
  rnf = rnfUniqueArray

-- | Create a new UniqueArray
--
{-# INLINE newUniqueArray #-}
newUniqueArray :: ForeignPtr e -> IO (UniqueArray e)
newUniqueArray fp = UniqueArray <$> newUnique <*> newLifetime fp

-- | Access the pointer backing the unique array.
--
-- The array data is kept alive at least during the whole action, even if it is
-- not directly used inside. Note that it is not safe to return the pointer from
-- the action and use it after the action completes. All uses of the pointer
-- should be inside the bracketed function.
--
{-# INLINE withUniqueArrayPtr #-}
withUniqueArrayPtr :: UniqueArray a -> (Ptr a -> IO b) -> IO b
withUniqueArrayPtr ua go =
  withLifetime (uniqueArrayData ua) $ \fp -> withForeignPtr fp go

-- | Returns the element of an immutable array at the specified index. This
-- does no bounds checking.
--
{-# INLINE unsafeIndexArray #-}
unsafeIndexArray :: Storable e => UniqueArray e -> Int -> e
unsafeIndexArray !ua !i =
  unsafePerformIO $! unsafeReadArray ua i

-- | Read an element from a mutable array at the given index. This does no
-- bounds checking.
--
{-# INLINE unsafeReadArray #-}
unsafeReadArray :: Storable e => UniqueArray e -> Int -> IO e
unsafeReadArray !ua !i =
  withUniqueArrayPtr ua $ \ptr -> peekElemOff ptr i

-- | Write an element into a mutable array at the given index. This does no
-- bounds checking.
--
{-# INLINE unsafeWriteArray #-}
unsafeWriteArray :: Storable e => UniqueArray e -> Int -> e -> IO ()
unsafeWriteArray !ua !i !e =
  withUniqueArrayPtr ua $ \ptr -> pokeElemOff ptr i e


-- | Extract the pointer backing the unique array.
--
-- This is potentially unsafe, as if the argument is the last occurrence of this
-- unique array then the finalisers will be run, potentially invalidating the
-- plain pointer just obtained.
--
-- See also: 'unsafeGetValue', 'unsafeForeignPtrToPtr'.
--
{-# INLINE unsafeUniqueArrayPtr #-}
unsafeUniqueArrayPtr :: UniqueArray a -> Ptr a
unsafeUniqueArrayPtr = unsafeForeignPtrToPtr . unsafeGetValue . uniqueArrayData


-- | Ensure that the unique array is alive at the given place in a sequence of
-- IO actions. Note that this does not force the actual array payload.
--
-- See: [Unique array strictness]
--
{-# INLINE touchUniqueArray #-}
touchUniqueArray :: UniqueArray a -> IO ()
touchUniqueArray = touchLifetime . uniqueArrayData


rnfUniqueArray :: UniqueArray a -> ()
rnfUniqueArray (UniqueArray _ ad) = unsafeGetValue ad `seq` ()

-- TODO: Make sure that the data is correctly aligned...
--
liftUniqueArray :: forall a. Storable a => Int -> UniqueArray a -> CodeQ (UniqueArray a)
liftUniqueArray sz ua = unsafeCodeCoerce $ do
  bytes <- runIO $ peekArray (sizeOf (undefined::a) * sz) (castPtr (unsafeUniqueArrayPtr ua) :: Ptr Word8)
  [| unsafePerformIO $ do
       fp  <- newForeignPtr_ (Ptr $(litE (StringPrimL bytes)))
       ua' <- newUniqueArray (castForeignPtr fp)
       return ua'
   |]

