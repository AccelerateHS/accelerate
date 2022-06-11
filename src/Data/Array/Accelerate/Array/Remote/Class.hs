{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Array.Remote.Class
-- Copyright   : [2015..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
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

  RemoteMemory(..)

) where

import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Type

import Control.Applicative
import Control.Monad.Catch
import Data.Kind
import Prelude


-- | Accelerate backends can provide an instance of this class in order to take
-- advantage of the automated memory managers we provide as part of the base
-- package.
--
class (Applicative m, Monad m, MonadCatch m, MonadMask m) => RemoteMemory m where

  -- | Pointers into this particular remote memory.
  type RemotePtr m :: Type -> Type

  -- | Attempt to allocate the given number of bytes in the remote memory space.
  -- Returns Nothing on failure.
  mallocRemote :: Int -> m (Maybe (RemotePtr m Word8))

  -- | Copy the given number of elements from the host array into remote memory.
  pokeRemote :: ScalarType e -> Int -> RemotePtr m (ScalarArrayDataR e) -> ArrayData e -> m ()

  -- | Copy the given number of elements from remote memory to the host array.
  peekRemote :: ScalarType e -> Int -> RemotePtr m (ScalarArrayDataR e) -> MutableArrayData e -> m ()

  -- | Cast a remote pointer.
  castRemotePtr :: RemotePtr m a -> RemotePtr m b

  -- | Returns the total remote memory available in bytes.
  totalRemoteMem :: m Int64

  -- | Returns, in bytes, the available remote memory.
  availableRemoteMem :: m Int64

  -- | The chunk allocation size (bytes).
  remoteAllocationSize :: m Int
  remoteAllocationSize = return 1024

