{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Data.Array.Accelerate.Array.Remote.Nursery
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Array.Remote.Nursery (

  Nursery(..), NRS, new, lookup, insert, cleanup, size

) where

import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Debug.Internal.Flags                   as Debug
import Data.Array.Accelerate.Debug.Internal.Trace                   as Debug

import Control.Concurrent.MVar
import Data.Int
import Data.Sequence                                                ( Seq )
import Data.Word
import Formatting
import System.Mem.Weak                                              ( Weak )
import Prelude                                                      hiding ( lookup )
import qualified Data.HashTable.IO                                  as HT
import qualified Data.Sequence                                      as Seq
import qualified Data.Traversable                                   as Seq


-- The nursery is a place to store remote memory arrays that are no longer
-- needed. Often it is quicker to reuse an existing array, rather than call out
-- to the external API to allocate fresh memory.
--
-- The nursery is wrapped in an MVar so that several threads may safely access
-- it concurrently.
--
type HashTable key val  = HT.CuckooHashTable key val
type NRS ptr            = MVar ( HashTable Int (Seq (ptr Word8)) )  -- #bytes -> available memory
data Nursery ptr        = Nursery {-# UNPACK #-} !(NRS ptr)
                                  {-# UNPACK #-} !(Weak (NRS ptr))


-- | Create a fresh nursery.
--
-- When the nursery is garbage collected, the provided function will be run on
-- each value to free the retained memory.
--
{-# INLINEABLE new #-}
new :: (ptr Word8 -> IO ()) -> IO (Nursery ptr)
new delete = do
  message "initialise nursery"
  nrs    <- HT.new
  ref    <- newMVar nrs
  weak   <- mkWeakMVar ref (cleanup delete ref)
  return $! Nursery ref weak


-- | Look for an entry with the requested size.
--
{-# INLINEABLE lookup #-}
lookup :: HasCallStack => Int -> Nursery ptr -> IO (Maybe (ptr Word8))
lookup !key (Nursery !ref !_) =
  withMVar ref $ \nrs ->
    HT.mutateIO nrs key $ \case
      Nothing -> return (Nothing, Nothing)
      Just r  ->
        case Seq.viewl r of
          v Seq.:< vs -> do
            -- Debug.remote_memory_free_nursery v
            if Seq.null vs
              then return (Nothing, Just v)   -- delete this entry from the map
              else return (Just vs, Just v)   -- re-insert the tail
          --
          Seq.EmptyL  -> internalError "expected non-empty sequence"


-- | Add an entry to the nursery
--
{-# INLINEABLE insert #-}
insert :: Int -> ptr Word8 -> Nursery ptr -> IO ()
insert !key !val (Nursery !ref _) =
  withMVar ref $ \nrs -> do
    -- Debug.remote_memory_alloc_nursery val key
    HT.mutate nrs key $ \case
      Nothing -> (Just (Seq.singleton val), ())
      Just vs -> (Just (vs Seq.|> val),     ())


-- | Delete all entries from the nursery
--
{-# INLINEABLE cleanup #-}
cleanup :: (ptr Word8 -> IO ()) -> NRS ptr -> IO ()
cleanup delete !ref = do
  message "nursery cleanup"
  modifyMVar_ ref $ \nrs -> do
    HT.mapM_ (Seq.mapM delete . snd) nrs
    nrs' <- HT.new
    return nrs'


-- | The total number of bytes retained by the nursery
--
{-# INLINEABLE size #-}
size :: Nursery ptr -> IO Int64
size (Nursery ref _)
  = withMVar ref
  $ HT.foldM (\s (k,v) -> return $ s + fromIntegral (k * (Seq.length v))) 0


-- Debug
-- -----

{-# INLINE message #-}
message :: Format (IO ()) a -> a
message fmt = Debug.traceM Debug.dump_gc ("gc: " % fmt)

