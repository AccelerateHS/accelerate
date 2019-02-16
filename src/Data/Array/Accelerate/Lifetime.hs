{-# LANGUAGE CPP           #-}
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Lifetime
-- Copyright   : [2015..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Lifetime (

  Lifetime(..),
  newLifetime, withLifetime, touchLifetime,
  addFinalizer, finalize, mkWeak, mkWeakPtr,

  unsafeGetValue,

) where

import Data.Function                ( on )
import Data.IORef                   ( mkWeakIORef, atomicModifyIORef' )
import Prelude

import GHC.Base                     ( touch#, IO(..))
import GHC.IORef                    ( IORef(.. ), newIORef )
import GHC.Prim                     ( mkWeak# )
import GHC.STRef                    ( STRef(..) )
import GHC.Weak                     ( Weak(..) )


-- | A lifetime represents a value with attached finalizers. This is similar to
-- the functionality provided by "System.Mem.Weak", but has the following
-- stronger properties:
--
-- * Unless explicitly forced, finalizers will not fire until after the
--   'Lifetime' has become unreachable, where \"reachability\" is the same as
--   defined in "System.Mem.Weak". That is to say, there is no issue with
--   creating a 'Lifetime' for a non-primitve type and finalizers firing while
--   an object is still reachable.
--
-- * Finalizers are fired sequentially in reverse of the order in which they
--   were attached.
--
-- * As the finalizers are attached to the 'Lifetime' and not the underlying
--   value, there is no danger in storing it UNPACKED as part of another
--   structure.
--
type LTF        = IORef [IO ()]
data Lifetime a = Lifetime {-# UNPACK #-} !LTF
                           {-# UNPACK #-} !(Weak LTF)
                           {- LAZY -}     a

instance Eq a => Eq (Lifetime a) where
  (==) = (==) `on` unsafeGetValue

-- | Construct a new 'Lifetime' from the given value.
--
{-# INLINE newLifetime #-}
newLifetime :: a -> IO (Lifetime a)
newLifetime a = do
  ref  <- newIORef []
  weak <- mkWeakIORef ref (finalizer ref)
  return $! Lifetime ref weak a

-- | This provides a way of looking at the value inside a 'Lifetime'. The
-- supplied function is executed immediately and the 'Lifetime' kept alive
-- throughout its execution. It is important to not let the value /leak/ outside
-- the function, either by returning it or by lazy IO.
--
{-# INLINE withLifetime #-}
withLifetime :: Lifetime a -> (a -> IO b) -> IO b
withLifetime (Lifetime ref _ a) f = do
  r <- f a
  touchIORef ref
  return r

-- | Ensure that the lifetime is alive at the given place in a sequence of IO
-- actions. Does not force the payload.
--
{-# INLINE touchLifetime #-}
touchLifetime :: Lifetime a -> IO ()
touchLifetime (Lifetime ref _ _) = touchIORef ref

-- | Attaches a finalizer to a 'Lifetime'. Like in "System.Mem.Weak", there is
-- no guarantee that the finalizers will eventually run. If they do run,
-- they will be executed in the order in which they were supplied.
--
addFinalizer :: Lifetime a -> IO () -> IO ()
addFinalizer (Lifetime ref _ _) f =
  atomicModifyIORef' ref (\fs -> (f:fs,()))

-- | Causes any finalizers associated with the given lifetime to be run
-- immediately on the calling thread.
--
-- Because the finalizer is run on the calling thread. Care should be taken to
-- ensure that the it does not try to acquire any locks the calling thread might
-- already possess. This can result in deadlock and is in contrast to calling
-- 'System.Mem.Weak.finalize' on 'System.Mem.Weak.Weak'.
--
finalize :: Lifetime a -> IO ()
finalize (Lifetime ref _ _) = finalizer ref

-- | Create a weak pointer from a 'Lifetime' to the supplied value.
--
-- Because weak pointers have their own concept of finalizers, it is important
-- to note these behaviours:
--
-- * Calling 'System.Mem.Weak.finalize' causes the finalizers attached to the
--   lifetime to be scheduled, and run in the correct order, but does not
--   guarantee they will execute on the calling thread.
--
-- * If 'deRefWeak' returns Nothing, there is no guarantee that the finalizers
--   have already run.
--
mkWeak :: Lifetime k -> v -> IO (Weak v)
mkWeak (Lifetime ref@(IORef (STRef r#)) _ _) v = go (finalizer ref)
  where
#if __GLASGOW_HASKELL__ >= 800
    go (IO f)  =  -- GHC-8.x
#else
    go f       =  -- GHC-7.x
#endif
      IO $ \s -> case mkWeak# r# v f s of
                   (# s', w# #) -> (# s', Weak w# #)

-- A specialised version of 'mkWeak' where the key and value are the same
-- 'Lifetime'.
--
-- > mkWeakPtr key = mkWeak key key
--
mkWeakPtr :: Lifetime a -> IO (Weak (Lifetime a))
mkWeakPtr l = mkWeak l l

-- | Retrieve the value from a lifetime. This is unsafe because, unless the
-- 'Lifetime' is still reachable, the finalizers may fire, potentially
-- invalidating the value.
--
{-# INLINE unsafeGetValue #-}
unsafeGetValue :: Lifetime a -> a
unsafeGetValue (Lifetime _ _ a) = a

-- The actual finalizer for 'Lifetime's.
--
finalizer :: IORef [IO ()] -> IO ()
finalizer ref = do
  fins <- atomicModifyIORef' ref ([],)
  sequence_ fins

-- Touch an 'IORef', ensuring that it is alive at this point in a sequence of IO
-- actions.
--
{-# INLINE touchIORef #-}
touchIORef :: IORef a -> IO ()
touchIORef r = IO $ \s -> case touch# r s of s' -> (# s', () #)

