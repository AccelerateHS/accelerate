{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      : Data.Array.Accelerate.Array.Remote.Nursery
-- Copyright   : [2008..2014] Manuel M T Chakravarty, Gabriele Keller
--               [2009..2016] Trevor L. McDonell
--               [2015..2015] Robert Clifton-Everest
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Array.Remote.Nursery (

  Nursery(..), NRS, new, lookup, insert, cleanup, size

) where

-- friends
import Data.Array.Accelerate.Error
import qualified Data.Array.Accelerate.Debug                    as D

-- libraries
import Prelude                                                  hiding ( lookup )
import Control.Concurrent.MVar
import Data.Int
import Data.IntMap                                              ( IntMap )
import Data.Sequence                                            ( Seq )
import Data.Word
import System.Mem.Weak                                          ( Weak )
import qualified Data.IntMap.Strict                             as IM
import qualified Data.Sequence                                  as Seq
import qualified Data.Traversable                               as Seq


-- The nursery is a place to store remote memory arrays that are no longer
-- needed. Often it is quicker to reuse an existing array, rather than call out
-- to the external API to allocate fresh memory.
--
-- The nursery is wrapped in an MVar so that several threads may safely access
-- it concurrently.
--
data Nursery ptr        = Nursery {-# UNPACK #-} !(NRS ptr)
                                  {-# UNPACK #-} !(Weak (NRS ptr))
type NRS ptr            = MVar (N ptr)

data N ptr              = N !(IntMap (Seq (ptr Word8)))       -- #bytes -> ptr
                            {-# UNPACK #-} !Int64             -- total allocated bytes


-- | Create a fresh nursery.
--
-- When the nursery is garbage collected, the provided function will be run on
-- each value to free the retained memory.
--
{-# INLINEABLE new #-}
new :: (ptr Word8 -> IO ()) -> IO (Nursery ptr)
new delete = do
  message "initialise nursery"
  ref    <- newMVar ( N IM.empty 0 )
  weak   <- mkWeakMVar ref (cleanup delete ref)
  return $! Nursery ref weak


-- | Look for an entry with the requested size.
--
{-# INLINEABLE lookup #-}
lookup :: Int -> Nursery ptr -> IO (Maybe (ptr Word8))
lookup !key (Nursery !ref !_) =
  modifyMVar ref $ \nrs@( N im sz ) ->
    let
        (mv, nrs')  = IM.updateLookupWithKey f key im         -- returns _original_ value, if located
        f _k v      =
          case Seq.viewl v of
            Seq.EmptyL  -> $internalError "lookup" "expected non-empty sequence"
            _ Seq.:< vs -> if Seq.null vs then Nothing        -- delete this entry in the map
                                          else Just vs        -- re-insert the tail
    in
    case fmap Seq.viewl mv of
      Just (v Seq.:< _) -> return ( N nrs' (sz - fromIntegral key) , Just v  )
      _                 -> return ( nrs,                             Nothing )


-- | Add an entry to the nursery
--
{-# INLINEABLE insert #-}
insert :: Int -> ptr Word8 -> Nursery ptr -> IO ()
insert !key !val (Nursery !ref _) =
  let
      f Nothing   = Just (Seq.singleton val)
      f (Just vs) = Just (vs Seq.|> val)
  in
  modifyMVar_ ref $ \(N im sz) ->
    return $! N (IM.alter f key im) (sz + fromIntegral key)


-- | Delete all entries from the nursery
--
{-# INLINEABLE cleanup #-}
cleanup :: (ptr Word8 -> IO ()) -> NRS ptr -> IO ()
cleanup delete !ref = do
  message "nursery cleanup"
  modifyMVar_ ref $ \(N nrs _) -> do mapM_ (Seq.mapM delete) (IM.elems nrs)
                                     return ( N IM.empty 0 )


-- | The total number of bytes retained by the nursery
--
{-# INLINEABLE size #-}
size :: Nursery ptr -> IO Int64
size (Nursery ref _) = withMVar ref $ \(N _ sz) -> return sz


-- Debug
-- -----

{-# INLINE message #-}
message :: String -> IO ()
message msg = D.traceIO D.dump_gc ("gc: " ++ msg)

