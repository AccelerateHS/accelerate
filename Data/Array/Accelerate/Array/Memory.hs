{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Data.Array.Accelerate.Array.Memory
-- Copyright   : [2015] Manuel M T Chakravarty, Gabriele Keller, Robert Clifton-Everest
-- License     : BSD3
--
-- Maintainer  : Robert Clifton-Everest <robertce@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Array.Memory (
  RemoteMemory(..)
  ) where

import Data.Array.Accelerate.Array.Data

import Data.Int
import Foreign.Ptr
import Foreign.Storable

-- |Pointers into a remote heap that supports allocation, freeing and transfers
-- to/from the main memory.
--
class RemoteMemory p where

  -- | Allocate into the remote memory. Returns Nothing if out of memory.
  malloc :: Storable e => Int -> IO (Maybe (p e))

  -- | Free memory previously allocated with `malloc`.
  free :: p e -> IO ()

  -- | Copy from host array to remote memory.
  poke :: (ArrayElt e, Storable a, ArrayPtrs e ~ Ptr a) => Int -> p a -> ArrayData e -> IO ()

  -- | Copy from remote memory to host array.
  peek :: (ArrayElt e, Storable a, ArrayPtrs e ~ Ptr a) => Int -> p a -> MutableArrayData e -> IO ()

  -- | Cast the remote pointer.
  castPtr :: p a -> p b

  -- | Returns the total remote memory available in bytes.
  totalMem :: proxy p -> IO Int64

  -- | Returns, in bytes, the available remote memory.
  availableMem :: proxy p -> IO Int64

  -- | Some remote memories allocate in chunks of a certain size. Memory
  -- managers can take advantage of this information to minimise the total
  -- number of allocations.
  chunkSize :: proxy p -> Int
  chunkSize _ = 1
