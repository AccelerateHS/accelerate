{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# OPTIONS_GHC -fobject-code #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Debug.Internal.Monitoring
-- Copyright   : [2016..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Debug.Internal.Monitoring (

  beginMonitoring,
  initAccMetrics,

  -- Load monitoring
  Processor(..),
  withProcessor, addProcessorTime,

  -- GC subsystem monitoring
  didAllocateBytesLocal, didAllocateBytesRemote,
  didCopyBytesToRemote, didCopyBytesFromRemote,
  increaseCurrentBytesRemote, decreaseCurrentBytesRemote,
  increaseCurrentBytesNursery, decreaseCurrentBytesNursery, getCurrentBytesNursery, setCurrentBytesNursery,
  didRemoteGC,
  didEvictBytes,

) where

#ifdef ACCELERATE_MONITORING
import Data.Array.Accelerate.Debug.Internal.Clock

import System.Metrics
import System.Remote.Monitoring

import Control.Concurrent
import Control.Concurrent.Async
import Data.IORef
import Data.Text                                                    ( Text )
import Text.Printf
import qualified Data.HashMap.Strict                                as Map
#endif

#if defined(ACCELERATE_MONITORING) || defined(ACCELERATE_DEBUG)
import Control.Monad
#endif

import Data.Atomic                                                  ( Atomic )
import qualified Data.Atomic                                        as Atomic

import Data.Int
import Language.Haskell.TH.Syntax


-- | Launch a monitoring server that will collect statistics on the running
-- application. This should be called as soon as the application starts. The
-- program will need to be run with the RTS option -T.
--
beginMonitoring :: IO ()
#ifdef ACCELERATE_MONITORING
beginMonitoring = do
  store <- initAccMetrics
  registerGcMetrics store
  r     <- withAsync (forkServerWith store "localhost" 8000 >> threadDelay 10000) waitCatch
  case r of
    Right _ -> printf "EKG monitor started at: http://localhost:8000\n"
    Left _  -> printf "Failed to start EKG monitor\n"
#else
beginMonitoring = return ()
#endif


-- | Initialise and return the Accelerate monitoring store. To enable monitoring
-- of your application:
--
-- > import Data.Array.Accelerate.Debug
-- >
-- > import System.Metrics
-- > import System.Remote.Monitoring
-- >
-- > main :: IO ()
-- > main = do
-- >   store  <- initAccMetrics
-- >   registerGcMetrics store      -- optional
-- >
-- >   server <- forkServerWith store "localhost" 8000
-- >
-- >   ...
--
-- Note that aside from the processor load metrics, counters are shared between
-- all active backends.
--
-- Registered rates:
--
-- [@acc.load.llvm_native@] Current processor load (%) of the LLVM CPU backend.
-- This only includes time spent executing Accelerate functions; compare this to
-- the total processor load (e.g. via top) to estimate the productivity of the
-- Accelerate program.
--
-- [@acc.load.llvm_ptx@] Current processor load (%) of the GPU in the LLVM PTX
-- backend. This only takes into account how much time the GPU spent executing
-- Accelerate code, and does not consider the number of active cores during that
-- time.
--
-- Registered gauges:
--
-- [@acc.gc.current_bytes_remote@] Total number of bytes currently considered
-- live in the remote address space.
--
-- [@acc.gc.current_bytes_nursery@] Total number of bytes allocated in the
-- remote address space but not currently live (available for reallocation).
--
-- Registered counters:
--
-- [@acc.gc.bytes_allocated_local@] Total number of bytes allocated in the local
-- address space.
--
-- [@acc.gc.bytes_allocated_remote@] Total number of bytes allocated in the
-- remote address space.
--
-- [@acc.gc.bytes_copied_to_remote@] Total number of bytes copied from the host
-- to the remote address space (e.g. from the CPU to the GPU).
--
-- [@acc.gc.bytes_copied_from_remote@] Total number of bytes copied from the
-- remote address space back to the host (e.g. from the GPU back to the CPU).
--
-- [@acc.gc.bytes_evicted_from_remote@] Total number of bytes evicted from the
-- remote address space by the LRU memory manager, in order to make space for
-- new allocations. A subset of __acc.gc.bytes_copied_from_remote__.
--
-- [@acc.gc.num_gcs@] Number of garbage collections of the remote address space
-- performed.
--
-- [@acc.gc.num_lru_evict@] Total number of evictions from the remote address
-- space performed.
--
#ifndef ACCELERATE_MONITORING
initAccMetrics :: IO a
initAccMetrics = error $ unlines [ "Data.Array.Accelerate: Monitoring is disabled."
                                 , "Reinstall package 'accelerate' with '-fekg' to enable it." ]
#else
initAccMetrics :: IO Store
initAccMetrics = do
  store <- newStore

  registerRate    "acc.load.llvm_native"             (estimateProcessorLoad __active_ns_llvm_native) store
  registerRate    "acc.load.llvm_ptx"                (estimateProcessorLoad __active_ns_llvm_ptx)    store
  registerGauge   "acc.gc.current_bytes_remote"      (Atomic.read __current_bytes_remote)            store
  registerGauge   "acc.gc.current_bytes_nursery"     (Atomic.read __current_bytes_nursery)           store
  registerCounter "acc.gc.bytes_allocated_local"     (Atomic.read __total_bytes_allocated_local)     store
  registerCounter "acc.gc.bytes_allocated_remote"    (Atomic.read __total_bytes_allocated_remote)    store
  registerCounter "acc.gc.bytes_copied_to_remote"    (Atomic.read __total_bytes_copied_to_remote)    store
  registerCounter "acc.gc.bytes_copied_from_remote"  (Atomic.read __total_bytes_copied_from_remote)  store
  registerCounter "acc.gc.bytes_evicted_from_remote" (Atomic.read __total_bytes_evicted_from_remote) store
  registerCounter "acc.gc.num_gcs"                   (Atomic.read __num_remote_gcs)                  store
  registerCounter "acc.gc.num_lru_evict"             (Atomic.read __num_evictions)                   store

  return store


-- Abusing 'registerGroup' to perform the rate calculation on every wake-up.
--
registerRate :: Text -> (IORef EMAState -> IO Int64) -> Store -> IO ()
registerRate name sample store = do
  now <- getMonotonicTime
  st  <- newIORef (ES now 0 0)
  registerGroup (Map.singleton name Gauge) (sample st) store
#endif


-- Recording metrics
-- -----------------

data Processor = Native | PTX

-- | Execute the given action and assign the elapsed wall-clock time as active
-- time for the given processing element.
--
{-# INLINE withProcessor #-}
withProcessor :: Processor -> IO a -> IO a
#ifndef ACCELERATE_MONITORING
withProcessor _      = id
#else
withProcessor Native = withProcessor' __active_ns_llvm_native
withProcessor PTX    = withProcessor' __active_ns_llvm_ptx

withProcessor' :: Atomic -> IO a -> IO a
withProcessor' var action = do
  wall0 <- getMonotonicTime
  !r    <- action
  wall1 <- getMonotonicTime
  addProcessorTime' var (wall1 - wall0)
  return r
#endif

-- | Record the given number of seconds as active processing time for the given
-- processing element.
--
{-# INLINE addProcessorTime #-}
addProcessorTime :: Processor -> Double -> IO ()
#ifndef ACCELERATE_MONITORING
addProcessorTime _ _    = return ()
#else
addProcessorTime Native = addProcessorTime' __active_ns_llvm_native
addProcessorTime PTX    = addProcessorTime' __active_ns_llvm_ptx

addProcessorTime' :: Atomic -> Double -> IO ()
addProcessorTime' var secs =
  let ns   = round (secs * 1.0E9)
  in  void $ Atomic.add var ns
#endif


-- | Allocated the number of bytes in the local memory space
--
{-# INLINE didAllocateBytesLocal #-}
didAllocateBytesLocal :: Int64 -> IO ()
#ifndef ACCELERATE_DEBUG
didAllocateBytesLocal _ = return ()
#else
didAllocateBytesLocal n = do
  -- void $ Atomic.add __active_bytes_allocated_local n
  void $ Atomic.add __total_bytes_allocated_local n
#endif

-- Allocations in the number of bytes of /new/ memory in the remote memory space
--
{-# INLINE didAllocateBytesRemote     #-}
{-# INLINE increaseCurrentBytesRemote #-}
{-# INLINE decreaseCurrentBytesRemote #-}
didAllocateBytesRemote     :: Int64 -> IO ()
decreaseCurrentBytesRemote :: Int64 -> IO ()
increaseCurrentBytesRemote :: Int64 -> IO ()
#ifndef ACCELERATE_DEBUG
didAllocateBytesRemote     _ = return ()
increaseCurrentBytesRemote _ = return ()
decreaseCurrentBytesRemote _ = return ()
#else
didAllocateBytesRemote n = do
 -- void $ Atomic.add __active_bytes_allocated_remote n
 void $ Atomic.add __total_bytes_allocated_remote n

increaseCurrentBytesRemote n = void $ Atomic.add __current_bytes_remote n
decreaseCurrentBytesRemote n = void $ Atomic.subtract __current_bytes_remote n
#endif


-- | Copied data between the local and remote memory spaces
--
{-# INLINE didCopyBytesToRemote   #-}
{-# INLINE didCopyBytesFromRemote #-}
didCopyBytesFromRemote :: Int64 -> IO ()
didCopyBytesToRemote   :: Int64 -> IO ()
#ifndef ACCELERATE_DEBUG
didCopyBytesToRemote   _ = return ()
didCopyBytesFromRemote _ = return ()
#else
didCopyBytesToRemote   n = void $ Atomic.add __total_bytes_copied_to_remote n
didCopyBytesFromRemote n = void $ Atomic.add __total_bytes_copied_from_remote n
#endif


-- Allocations in the nursery
--
{-# INLINE increaseCurrentBytesNursery #-}
{-# INLINE decreaseCurrentBytesNursery #-}
{-# INLINE setCurrentBytesNursery      #-}
increaseCurrentBytesNursery :: Int64 -> IO ()
decreaseCurrentBytesNursery :: Int64 -> IO ()
setCurrentBytesNursery      :: Int64 -> IO ()
getCurrentBytesNursery      :: IO Int64
#ifndef ACCELERATE_DEBUG
increaseCurrentBytesNursery _ = return ()
decreaseCurrentBytesNursery _ = return ()
setCurrentBytesNursery      _ = return ()
getCurrentBytesNursery        = return 0
#else
increaseCurrentBytesNursery n = void $ Atomic.add      __current_bytes_nursery n
decreaseCurrentBytesNursery n = void $ Atomic.subtract __current_bytes_nursery n
setCurrentBytesNursery      n =        Atomic.write    __current_bytes_nursery n
getCurrentBytesNursery        =        Atomic.read     __current_bytes_nursery
#endif


-- | Performed a major GC of the remote memory space
--
{-# INLINE didRemoteGC #-}
didRemoteGC :: IO ()
#ifndef ACCELERATE_DEBUG
didRemoteGC = return ()
#else
didRemoteGC = void $ Atomic.add __num_remote_gcs 1
#endif

-- | Performed an eviction of a remote array of the given number of bytes
--
{-# INLINE didEvictBytes #-}
didEvictBytes :: Int64 -> IO ()
#ifndef ACCELERATE_DEBUG
didEvictBytes _ = return ()
#else
didEvictBytes n = do
  void $ Atomic.add __num_evictions 1
  void $ Atomic.add __total_bytes_evicted_from_remote n
#endif


-- Implementation
-- --------------

#ifdef ACCELERATE_MONITORING

-- In order to calculate the processor load we need to remember the previous
-- values. Storing this state in an IORef has a bit of extra overhead (as
-- indirection) compared to the rest of the monitoring counters (which are
-- unboxed values on the heap manipulated directly with atomic primops), but
-- since 'calculateProcessorLoad' will only be called by the EKG monitor
-- whenever it refreshes the value for display, rather than running continuously
-- in the background, we should be okay.
--
data EMAState = ES
  { old_time  :: {-# UNPACK #-} !Double
  , old_inst  :: {-# UNPACK #-} !Double
  , old_avg   :: {-# UNPACK #-} !Double
  }

-- Estimate the load on the processor as a moving exponential average
-- (weight of previous measurement = 0.2).
--
estimateProcessorLoad :: Atomic -> IORef EMAState -> IO Int64
estimateProcessorLoad !var !ref = do
  ES{..} <- readIORef ref
  time   <- getMonotonicTime
  sample <- Atomic.and var 0
  --
  let
      active_ns   = fromIntegral sample
      elapsed_s   = old_time - time
      elapsed_ns  = 1.0E9 * elapsed_s
      --
      new_inst    = 100 * (active_ns / elapsed_ns)                -- instantaneous load
      new_avg     = ema 0.2 elapsed_s old_avg old_inst new_inst   -- moving average load
  --
  writeIORef ref (ES time new_inst new_avg)
  return (round new_avg)

{--
-- Compute the current load on a processor as a percentage of time spent working
-- over the elapsed time. This is meant to run continuously by a background
-- thread, updating the gauge each time it wakes up.
--
monitorProcessorLoad :: Gauge -> Atomic -> UTCTime -> Double -> Double -> IO ()
monitorProcessorLoad !gauge !var !old_time !old_inst !old_avg = do
  time   <- getCurrentTime
  sample <- Atomic.and var 0

  let
      active_ns   = fromIntegral sample
      elapsed_s   = realToFrac (diffUTCTime time old_time)
      elapsed_ns  = 1.0E9 * elapsed_s

      load_inst   = 100 * (active_ns / elapsed_ns)                -- instantaneous load
      load_avg    = ema 0.2 elapsed_s old_avg old_inst load_inst  -- moving average load

  -- Set what we thing the processor load over the previous interval should be
  Gauge.set gauge (round load_avg)

  -- Sleep for a bit, then do it all again
  threadDelay 500000    -- 500 ms
  monitorProcessorLoad gauge var time load_inst load_avg
--}

-- Exponential moving average for irregular time series
--
ema :: Double -> Double -> Double -> Double -> Double -> Double
ema !alpha !dt !old_ema !old_sample !new_sample =
  let
      a = dt / alpha
      u = exp ( -a )
      v = ( 1 - u ) / a
  in
  (u * old_ema) + ((v-u) * old_sample) + ((1-v) * new_sample)

#endif


-- Monitoring variables
-- --------------------

-- Number of nanoseconds a backend has spent doing real work since the last
-- check. This is an integer amount because there are no built-in functions for
-- atomic memory access on double precision (as specified by the Intel docs).
--
foreign import ccall "&__active_ns_llvm_native"           __active_ns_llvm_native           :: Atomic
foreign import ccall "&__active_ns_llvm_ptx"              __active_ns_llvm_ptx              :: Atomic

foreign import ccall "&__current_bytes_remote"            __current_bytes_remote            :: Atomic -- current working size of the remote memory space (active memory)
foreign import ccall "&__current_bytes_nursery"           __current_bytes_nursery           :: Atomic -- current size of the remote nursery (inactive memory)

foreign import ccall "&__total_bytes_allocated_local"     __total_bytes_allocated_local     :: Atomic -- bytes allocated in the local (CPU) memory space
foreign import ccall "&__total_bytes_allocated_remote"    __total_bytes_allocated_remote    :: Atomic -- bytes allocated in the remote memory space (if it is separate, e.g. GPU)
foreign import ccall "&__total_bytes_copied_to_remote"    __total_bytes_copied_to_remote    :: Atomic -- bytes copied to the remote memory space
foreign import ccall "&__total_bytes_copied_from_remote"  __total_bytes_copied_from_remote  :: Atomic -- bytes copied from the remote memory space
foreign import ccall "&__total_bytes_evicted_from_remote" __total_bytes_evicted_from_remote :: Atomic -- total bytes copied from the remote due to evictions
foreign import ccall "&__num_remote_gcs"                  __num_remote_gcs                  :: Atomic -- number of times the remote memory space was forcibly garbage collected
foreign import ccall "&__num_evictions"                   __num_evictions                   :: Atomic -- number of LRU eviction events

-- SEE: [linking to .c files]
--
runQ $ do
  addForeignFilePath LangC "cbits/monitoring.c"
  return []

