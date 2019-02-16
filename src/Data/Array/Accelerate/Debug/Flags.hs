{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeOperators            #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
#endif
-- |
-- Module      : Data.Array.Accelerate.Debug.Flags
-- Copyright   : [2008..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Option parsing for debug flags
--

module Data.Array.Accelerate.Debug.Flags (

  Value,
  unfolding_use_threshold,
  getValue,
  setValue,

  Flag,
  acc_sharing, exp_sharing, fusion, simplify, flush_cache, force_recomp,
  fast_math, debug, verbose, dump_phases, dump_sharing, dump_fusion,
  dump_simpl_stats, dump_simpl_iterations, dump_vectorisation, dump_dot,
  dump_simpl_dot, dump_gc, dump_gc_stats, dump_cc, dump_ld, dump_asm, dump_exec,
  dump_sched,

  getFlag,
  setFlag, setFlags,
  clearFlag, clearFlags,

  when,
  unless,

) where


import Data.Int
import Foreign.Ptr
import Foreign.Storable

import Control.Monad.IO.Class                                       ( MonadIO, liftIO )
import qualified Control.Monad                                      as M

newtype Flag  = Flag  (Ptr Int32)
newtype Value = Value (Ptr Int32)


-- | Conditional execution of a monadic debugging expression.
--
-- This does nothing unless the program is compiled in debug mode.
--
{-# INLINEABLE when #-}
when :: MonadIO m => Flag -> m () -> m ()
#if ACCELERATE_DEBUG
when f action = do
  yes <- liftIO $ getFlag f
  M.when yes action
#else
when _ _ = return ()
#endif


-- | The opposite of 'when'.
--
-- This does nothing unless the program is compiled in debug mode.
--
{-# INLINEABLE unless #-}
unless :: MonadIO m => Flag -> m () -> m ()
#ifdef ACCELERATE_DEBUG
unless f action = do
  yes <- liftIO $ getFlag f
  M.unless yes action
#else
unless _ _ = return ()
#endif


setValue   :: Value -> Int -> IO ()
#ifdef ACCELERATE_DEBUG
setValue (Value f) v = poke f (fromIntegral v)
#else
setValue _         _ = notEnabled
#endif

getValue   :: Value -> IO Int
#ifdef ACCELERATE_DEBUG
getValue (Value f) = fromIntegral `fmap` peek f
#else
getValue _         = notEnabled
#endif

getFlag    :: Flag -> IO Bool
#ifdef ACCELERATE_DEBUG
getFlag (Flag f) = toBool `fmap` peek f
#else
getFlag _        = notEnabled
#endif

setFlag    :: Flag -> IO ()
#ifdef ACCELERATE_DEBUG
setFlag (Flag f) = poke f (fromBool True)
#else
setFlag _        = notEnabled
#endif

clearFlag  :: Flag -> IO ()
#ifdef ACCELERATE_DEBUG
clearFlag (Flag f) = poke f (fromBool False)
#else
clearFlag _        = notEnabled
#endif

setFlags   :: [Flag] -> IO ()
setFlags = mapM_ setFlag

clearFlags :: [Flag] -> IO ()
clearFlags = mapM_ clearFlag

notEnabled :: a
notEnabled = error $ unlines [ "Data.Array.Accelerate: Debugging options are disabled."
                             , "Reinstall package 'accelerate' with '-fdebug' to enable them." ]

toBool :: Int32 -> Bool
toBool 0 = False
toBool _ = True

fromBool :: Bool -> Int32
fromBool False = 0
fromBool True  = 1


-- Import the underlying flag variables. These are defined in the file
-- cbits/flags.c and initialised at program initialisation.

-- These @-f<blah>=INT@ values are used by the compiler
--
foreign import ccall "&__unfolding_use_threshold" unfolding_use_threshold :: Value  -- the magic cut-off figure for inlining

-- These @-f<blah>@ flags can be reversed with @-fno-<blah>@
--
foreign import ccall "&__acc_sharing"             acc_sharing             :: Flag   -- recover sharing of array computations
foreign import ccall "&__exp_sharing"             exp_sharing             :: Flag   -- recover sharing of scalar expressions
foreign import ccall "&__fusion"                  fusion                  :: Flag   -- fuse array expressions
foreign import ccall "&__simplify"                simplify                :: Flag   -- simplify scalar expressions
foreign import ccall "&__fast_math"               fast_math               :: Flag   -- delete persistent compilation cache(s)
foreign import ccall "&__flush_cache"             flush_cache             :: Flag   -- force recompilation of array programs
foreign import ccall "&__force_recomp"            force_recomp            :: Flag   -- use faster, less precise math library operations
foreign import ccall "&__debug"                   debug                   :: Flag   -- compile code with debugging symbols (-g)

-- These debugging flags are disable by default and are enabled with @-d<blah>@
--
foreign import ccall "&__verbose"                 verbose                 :: Flag   -- be very chatty
foreign import ccall "&__dump_phases"             dump_phases             :: Flag   -- print information about each phase of the compiler
foreign import ccall "&__dump_sharing"            dump_sharing            :: Flag   -- sharing recovery phase
foreign import ccall "&__dump_fusion"             dump_fusion             :: Flag   -- array fusion phase
foreign import ccall "&__dump_simpl_stats"        dump_simpl_stats        :: Flag   -- statistics form fusion/simplification
foreign import ccall "&__dump_simpl_iterations"   dump_simpl_iterations   :: Flag   -- output from each simplifier iteration
foreign import ccall "&__dump_vectorisation"      dump_vectorisation      :: Flag   -- output from the vectoriser
foreign import ccall "&__dump_dot"                dump_dot                :: Flag   -- generate dot output of the program
foreign import ccall "&__dump_simpl_dot"          dump_simpl_dot          :: Flag   -- generate simplified dot output
foreign import ccall "&__dump_gc"                 dump_gc                 :: Flag   -- trace garbage collector
foreign import ccall "&__dump_gc_stats"           dump_gc_stats           :: Flag   -- print final GC statistics
foreign import ccall "&__dump_cc"                 dump_cc                 :: Flag   -- trace code generation & compilation
foreign import ccall "&__dump_ld"                 dump_ld                 :: Flag   -- trace runtime linker
foreign import ccall "&__dump_asm"                dump_asm                :: Flag   -- trace assembler
foreign import ccall "&__dump_exec"               dump_exec               :: Flag   -- trace execution
foreign import ccall "&__dump_sched"              dump_sched              :: Flag   -- trace scheduler

