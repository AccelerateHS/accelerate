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

-- |Monads that have access to a remote memory.
--
-- Acclerate backends can provide an instance of this class in order to take
-- advantage of the automated memory managers we provide as part of the base
-- package.
--
class Monad m => RemoteMemory m where

  -- | Pointers into this particalur remote memory.
  type RemotePointer m :: * -> *

  -- | Allocate into the remote memory. Returns Nothing if out of memory.
  malloc :: Storable e => Int -> m (Maybe (RemotePointer m e))

  -- | Copy from host array to remote memory.
  poke :: (ArrayElt e, Storable a, ArrayPtrs e ~ Ptr a) => Int -> RemotePointer m a -> ArrayData e -> m ()

  -- | Copy from remote memory to host array.
  peek :: (ArrayElt e, Storable a, ArrayPtrs e ~ Ptr a) => Int -> RemotePointer m a -> MutableArrayData e -> m ()

  -- | Free memory previously allocated with `malloc`.
  --
  free :: RemotePointer m e -> m ()

  -- | Cast a remote pointer.
  castPtr :: proxy m -> RemotePointer m a -> RemotePointer m b

  -- | Returns the total remote memory available in bytes.
  totalMem :: m Int64

  -- | Returns, in bytes, the available remote memory.
  availableMem :: m Int64

  -- | Some remote memories allocate in chunks of a certain size. Memory
  -- managers can take advantage of this information to minimise the total
  -- number of allocations.
  chunkSize :: m Int
  chunkSize = return 1
