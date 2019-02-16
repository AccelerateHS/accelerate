{-# LANGUAGE CPP           #-}
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Async
-- Copyright   : [2009..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Async (

  -- * Asynchronous actions
  Async,

  -- ** Spawning
  async, asyncOn, asyncBound,

  -- ** Querying 'Async's
  wait, poll, cancel,

) where

import Control.Exception
import Control.Concurrent

import GHC.Exts
import GHC.Conc
import GHC.IO


-- We need to execute the main thread asynchronously to give finalisers a chance
-- to run. Make sure to catch exceptions to avoid "blocked indefinitely on MVar"
-- errors.
--
data Async a = Async {-# UNPACK #-} !ThreadId
                     {-# UNPACK #-} !(MVar (Either SomeException a))

-- | Spawn an asynchronous action in a separate thread.
--
async :: IO a -> IO (Async a)
async = inline asyncUsing rawForkIO

-- | Like 'async', but using 'forkOn' internally.
--
asyncOn :: Int -> IO a -> IO (Async a)
asyncOn cpu = inline asyncUsing (rawForkOn cpu)

-- | Like 'async', but using 'forkOS' internally.
--
asyncBound :: IO a -> IO (Async a)
asyncBound = inline asyncUsing forkOS

asyncUsing :: (IO () -> IO ThreadId) -> IO a -> IO (Async a)
asyncUsing fork action = do
   var <- newEmptyMVar
   tid <- mask $ \restore ->
            fork $ try (restore action) >>= putMVar var
   return (Async tid var)

-- | Block the calling thread until the computation completes, then return the
-- result.
--
{-# INLINE wait #-}
wait :: Async a -> IO a
wait (Async _ var) = either throwIO return =<< readMVar var

-- | Test whether the asynchronous computation has already completed. If so,
-- return the result, else 'Nothing'.
--
{-# INLINE poll #-}
poll :: Async a -> IO (Maybe a)
poll (Async _ var) =
  maybe (return Nothing) (either throwIO (return . Just)) =<< tryReadMVar var

-- | Cancel a running asynchronous computation.
--
{-# INLINE cancel #-}
cancel :: Async a -> IO ()
cancel (Async tid _) = throwTo tid ThreadKilled


-- A version of 'forkIO' that does not include the outer exception handler. This
-- saves a bit of time when we will be installing our own exception handler.
--
-- Stolen from Simon Marlow's 'async' package (BSD).
--
{-# INLINE rawForkIO #-}
rawForkIO :: IO () -> IO ThreadId
rawForkIO action = IO $ \s ->
  case fork# action s of
    (# s', tid #) -> (# s', ThreadId tid #)

{-# INLINE rawForkOn #-}
rawForkOn :: Int -> IO () -> IO ThreadId
rawForkOn (I# cpu) action = IO $ \s ->
  case forkOn# cpu action s of
    (# s', tid #) -> (# s', ThreadId tid #)

