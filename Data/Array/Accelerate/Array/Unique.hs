-- |
-- Module      : Data.Array.Accelerate.Array.Unique
-- Copyright   : [2016] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Array.Unique (

  UniqueArray(..),
  newUniqueArray,
  withUniqueArrayPtr,
  unsafeUniqueArrayPtr,
  touchUniqueArray,

) where

-- library
import Control.Applicative
import Control.Concurrent.Unique
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Prelude

-- friends
import Data.Array.Accelerate.Lifetime


-- | A uniquely identifiable array.
--
-- For the purposes of memory management, we use arrays as keys in a table. For
-- this reason we need a way to uniquely identify each array we create. We do
-- this by attaching a unique identifier to each array.
--
data UniqueArray e = UniqueArray
    { uniqueArrayId   :: {-# UNPACK #-} !Unique
    , uniqueArrayData :: {-# UNPACK #-} !(Lifetime (ForeignPtr e))
    }

-- | Create a new UniqueArray
--
newUniqueArray :: ForeignPtr e -> IO (UniqueArray e)
newUniqueArray fp = UniqueArray <$> newUnique <*> newLifetime fp

-- | Access the pointer backing the unique array.
--
-- The array data is kept alive at least during the whole action, even if it is
-- not directly used inside. Note that it is not safe to return the pointer from
-- the action and use it after the action completes. All uses of the pointer
-- should be inside the bracketed function.
--
withUniqueArrayPtr :: UniqueArray a -> (Ptr a -> IO b) -> IO b
withUniqueArrayPtr ua go =
  withLifetime (uniqueArrayData ua) $ \fp -> withForeignPtr fp go


-- | Extract the pointer backing the unique array.
--
-- This is potentially unsafe, as if the argument is the last occurrence of this
-- unique array then the finalisers will be run, potentially invalidating the
-- plain pointer just obtained.
--
-- See also: 'unsafeGetValue', 'unsafeForeignPtrToPtr'.
--
unsafeUniqueArrayPtr :: UniqueArray a -> Ptr a
unsafeUniqueArrayPtr = unsafeForeignPtrToPtr . unsafeGetValue . uniqueArrayData


-- | Ensure that the unique array is alive at the given place in a sequence of
-- IO actions. Note that this does not force the actual array payload.
--
touchUniqueArray :: UniqueArray a -> IO ()
touchUniqueArray = touchLifetime . uniqueArrayData

