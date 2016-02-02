{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies    #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Array.Remote.Class
-- Copyright   : [2015] Manuel M T Chakravarty, Gabriele Keller, Robert Clifton-Everest
--               [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Robert Clifton-Everest <robertce@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module define a class of operations over pointers in a remote memory
-- space. Backends provide an instance of this class, and then can use (or
-- extend upon) one of the following modules in order to manage the remote
-- memory:
--
-- * 'Data.Array.Accelerate.Array.Remote.Table': basic, unmanaged memory tables,
--   mapping accelerate arrays on the host to the corresponding array in the
--   remote memory space.
--
-- * 'Data.Array.Accelerate.Array.Remote.LRU': managed memory tables which
--   additionally evict old entries from the device if the remote memory is
--   exhausted.
--

module Data.Array.Accelerate.Array.Remote.Class (

  RemoteMemory(..), PrimElt

) where

import Data.Array.Accelerate.Array.Data

import Control.Monad.Catch
import Data.Int
import Data.Typeable
import Foreign.Ptr
import Foreign.Storable

-- | Matches array element types to primitive types.
--
type PrimElt e a = (ArrayElt e, Storable a, ArrayPtrs e ~ Ptr a, Typeable e, Typeable a)

-- | Monads that have access to a remote memory.
--
-- Accelerate backends can provide an instance of this class in order to take
-- advantage of the automated memory managers we provide as part of the base
-- package.
--
class (Monad m, MonadCatch m, MonadMask m) => RemoteMemory m where

  -- | Pointers into this particular remote memory.
  type RemotePtr m :: * -> *

  -- | Allocate into the remote memory. Returns Nothing if out of memory.
  mallocRemote :: Storable e => Int -> m (Maybe (RemotePtr m e))

  -- | Copy from host array to remote memory.
  pokeRemote :: PrimElt e a => Int -> RemotePtr m a -> ArrayData e -> m ()

  -- | Copy from remote memory to host array.
  peekRemote :: PrimElt e a => Int -> RemotePtr m a -> MutableArrayData e -> m ()

  -- | Free memory previously allocated with `mallocRemote`.
  freeRemote :: RemotePtr m e -> m ()

  -- | Cast a remote pointer.
  castRemotePtr :: proxy m -> RemotePtr m a -> RemotePtr m b

  -- | Advance the remote pointer address by the given offset in bytes
  plusRemotePtr :: proxy m -> RemotePtr m a -> Int -> RemotePtr m a

  -- | Returns the total remote memory available in bytes.
  totalRemoteMem :: m Int64

  -- | Returns, in bytes, the available remote memory.
  availableRemoteMem :: m Int64

  -- | Some remote memories allocate in chunks of a certain size. Memory
  -- managers can take advantage of this information to minimise the total
  -- number of allocations.
  remoteAllocationSize :: m Int
  remoteAllocationSize = return 1

