{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeOperators            #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports     #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds   #-}
{-# OPTIONS_GHC -fobject-code                #-} -- SEE: [linking to .c files]
-- |
-- Module      : Data.Array.Accelerate.Debug.Internal.Flags
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Option parsing for debug flags
--

module Data.Array.Accelerate.Debug.Internal.Flags (

  Value,
  getValue,
  setValue,

  Flag(..),
  seq_sharing, acc_sharing, exp_sharing, array_fusion, inplace, force_recomp,
  fast_math, fast_permute_const, debug, verbose, dump_phases, dump_sharing, dump_fusion,
  dump_simpl_stats, dump_simpl_iterations, dump_vectorisation, dump_dot,
  dump_simpl_dot, dump_gc, dump_gc_stats, dump_cc, dump_ld, dump_asm, dump_exec,
  dump_sched,

  getFlag,
  setFlag, setFlags,
  clearFlag, clearFlags,

  when,
  unless,

  __cmd_line_flags,

) where


import Control.Monad.IO.Class                                       ( MonadIO, liftIO )
import Data.Bits
import Data.Int
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import Language.Haskell.TH.Syntax
import System.Directory
import System.FilePath
import qualified Control.Monad                                      as M

newtype Flag  = Flag  Int
newtype Value = Value (Ptr Word32)    -- see flags.c

-- We aren't using a "real" enum so that we can make use of the unused top
-- bits for other configuration options, not controlled by the command line
-- flags.
--
-- However, as there are currently no such special configuration options, the
-- bit hack complexity here is unnecessary. It's kept as an easter egg for
-- future maintainers.
--
instance Enum Flag where
  toEnum            = Flag
  fromEnum (Flag x) = x

-- SEE: [layout of command line options bitfield]
instance Show Flag where
  show (Flag x) =
    case x of
      0  -> "seq-sharing"
      1  -> "acc-sharing"
      2  -> "exp-sharing"
      3  -> "fusion"
      4  -> "inplace"
      5  -> "fast-math"
      6  -> "fast-permute-const"
      7  -> "force-recomp"
      8 -> "debug"
      9 -> "verbose"
      10 -> "dump-phases"
      11 -> "dump-sharing"
      12 -> "dump-fusion"
      13 -> "dump-simpl-stats"
      14 -> "dump-simpl-iterations"
      15 -> "dump-vectorisation"
      16 -> "dump-dot"
      17 -> "dump-simpl-dot"
      18 -> "dump-gc"
      19 -> "dump-gc-stats"
      20 -> "dump-cc"
      21 -> "dump-ld"
      22 -> "dump-asm"
      23 -> "dump-exec"
      24 -> "dump-sched"
      _  -> show x

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


setValue :: Value -> Word32 -> IO ()
setValue (Value f) v = poke f v

getValue :: Value -> IO Word32
getValue (Value f) = peek f

getFlag :: Flag -> IO Bool
getFlag (Flag i) = do
  flags  <- peek __cmd_line_flags
  return $! testBit flags i

setFlag :: Flag -> IO ()
setFlag (Flag i) = do
  flags <- peek __cmd_line_flags
  poke __cmd_line_flags (setBit flags i)

clearFlag :: Flag -> IO ()
clearFlag (Flag i) = do
  flags <- peek __cmd_line_flags
  poke __cmd_line_flags (clearBit flags i)

setFlags :: [Flag] -> IO ()
setFlags = mapM_ setFlag

clearFlags :: [Flag] -> IO ()
clearFlags = mapM_ clearFlag

-- notEnabled :: a
-- notEnabled = error $ unlines [ "Data.Array.Accelerate: Debugging options are disabled."
--                              , "Reinstall package 'accelerate' with '-fdebug' to enable them." ]

-- Note: [HLS and GHC IDE]
--
-- HLS requires stubs because it does not process the 'addForeignFilePath'
-- calls when evaluating Template Haskell
--
-- > https://github.com/haskell/haskell-language-server/issues/365
--
#ifndef __GHCIDE__

-- Import the underlying flag variables. These are defined in the file
-- cbits/flags.h as a bitfield and initialised at program initialisation.
--
-- SEE: [layout of command line options bitfield]
-- SEE: [linking to .c files]
--
foreign import ccall "&__cmd_line_flags" __cmd_line_flags :: Ptr Word32

#else

__cmd_line_flags :: Ptr Word32
__cmd_line_flags = undefined

#endif

-- These @-f<blah>@ flags can be reversed with @-fno-<blah>@
--
-- SEE: [layout of command line options bitfield]
seq_sharing           = Flag  0 -- recover sharing of sequence expressions
acc_sharing           = Flag  1 -- recover sharing of array computations
exp_sharing           = Flag  2 -- recover sharing of scalar expressions
array_fusion          = Flag  3 -- fuse array expressions
inplace               = Flag  4 -- allow (safe) in-place array updates
fast_math             = Flag  5 -- use faster, less precise math library operations
fast_permute_const    = Flag  6 -- allow non-atomic permute const for product types
force_recomp          = Flag  7 -- force recompilation of array programs

-- These debugging flags are disable by default and are enabled with @-d<blah>@
--
debug                 = Flag  8 -- compile code with debugging symbols (-g)
verbose               = Flag  9 -- be very chatty
dump_phases           = Flag 10 -- print information about each phase of the compiler
dump_sharing          = Flag 11 -- sharing recovery phase
dump_fusion           = Flag 12 -- array fusion phase
dump_simpl_stats      = Flag 13 -- statistics form fusion/simplification
dump_simpl_iterations = Flag 14 -- output from each simplifier iteration
dump_vectorisation    = Flag 15 -- output from the vectoriser
dump_dot              = Flag 16 -- generate dot output of the program
dump_simpl_dot        = Flag 17 -- generate simplified dot output
dump_gc               = Flag 18 -- trace garbage collector
dump_gc_stats         = Flag 19 -- print final GC statistics
dump_cc               = Flag 20 -- trace code generation & compilation
dump_ld               = Flag 21 -- trace runtime linker
dump_asm              = Flag 22 -- trace assembler
dump_exec             = Flag 23 -- trace execution
dump_sched            = Flag 24 -- trace scheduler


-- Note: [linking to .c files]
--
-- We use Template Haskell to tell GHC which .c files need to be compiled
-- for a particular module, rather than relying on Cabal as is traditional.
-- Using Cabal:
--
--  * loading Accelerate into GHCi only works _after_ compiling the entire
--    package (which defeats the purpose), presumably because the .c files
--    are compiled last. This would often lead to errors such "can not find
--    symbol __cmd_line_flags" etc.
--
--  * Cabal would refuse to re-compile .c files when changing command
--    line flags, see: https://github.com/haskell/cabal/issues/4937
--
--  * Linking problems also prevented us from using Template Haskell in
--    some locations, because GHC was unable to load the project into the
--    interpreter to run the splices.
--
-- Note that for this fix to work in GHCi we also require modules using it
-- to be loaded as object code.
--
runQ $ do
  addForeignFilePath LangC "cbits/flags.c"
  addForeignFilePath LangC "cbits/getopt_long.c"
  return []

