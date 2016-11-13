{-# LANGUAGE CPP           #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module:      : Data.Array.Accelerate.Examples.Internal.Backend
-- Copyright    : [2014] Trevor L. McDonell
-- License      : BSD3
--
-- Maintainer   : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--
-- This file is all that should need editing in order to add support for new
-- backends to the accelerate-examples package.
--

module Data.Array.Accelerate.Examples.Internal.Backend
  where

import Prelude                                          as P
import Data.Label
import System.Console.GetOpt

import Data.Array.Accelerate
import qualified Data.Array.Accelerate                  as A
import qualified Data.Array.Accelerate.Interpreter      as Interp
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
import qualified Data.Array.Accelerate.LLVM.Native      as CPU
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
import qualified Data.Array.Accelerate.LLVM.PTX         as PTX
#endif
#ifdef ACCELERATE_CUDA_BACKEND
import qualified Data.Array.Accelerate.CUDA             as CUDA
#endif
#ifdef ACCELERATE_CILK_BACKEND
import qualified Data.Array.Accelerate.Cilk             as Cilk
#endif


-- | Execute Accelerate expressions
--
run :: Arrays a => Backend -> Acc a -> a
run Interpreter = Interp.run
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
run CPU         = CPU.run
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
run PTX         = PTX.run
#endif
#ifdef ACCELERATE_CUDA_BACKEND
run CUDA        = CUDA.run
#endif
#ifdef ACCELERATE_CILK_BACKEND
run Cilk        = Cilk.run
#endif


run1 :: (Arrays a, Arrays b) => Backend -> (Acc a -> Acc b) -> a -> b
run1 Interpreter f = Interp.run1 f
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
run1 CPU         f = CPU.run1 f
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
run1 PTX         f = PTX.run1 f
#endif
#ifdef ACCELERATE_CUDA_BACKEND
run1 CUDA        f = CUDA.run1 f
#endif
#ifdef ACCELERATE_CILK_BACKEND
run1 Cilk        f = Cilk.run . f . use
#endif

run2 :: (Arrays a, Arrays b, Arrays c) => Backend -> (Acc a -> Acc b -> Acc c) -> a -> b -> c
run2 backend f x y = run1 backend (A.uncurry f) (x,y)


-- | The set of backends available to execute the program.
--
data Backend = Interpreter
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
             | CPU
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
             | PTX
#endif
#ifdef ACCELERATE_CUDA_BACKEND
             | CUDA
#endif
#ifdef ACCELERATE_LLVM_MULTIDEV_BACKEND
             | Multi
#endif
#ifdef ACCELERATE_CILK_BACKEND
             | Cilk
#endif
  deriving (P.Eq, P.Enum, P.Bounded)


-- The choice of show instance is important because this will be used to
-- generate the command line flag.
--
instance Show Backend where
  show Interpreter      = "interpreter"
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
  show CPU              = "llvm-cpu"
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
  show PTX              = "llvm-ptx"
#endif
#ifdef ACCELERATE_CUDA_BACKEND
  show CUDA             = "cuda"
#endif
#ifdef ACCELERATE_LLVM_MULTIDEV_BACKEND
  show Multi            = "llvm-multi"
#endif
#ifdef ACCELERATE_CILK_BACKEND
  show Cilk             = "cilk"
#endif


-- The default backend to use. Use one of the accelerated backends whenever
-- available.
--
defaultBackend :: Backend
defaultBackend
  | maxBound == Interpreter = Interpreter
  | otherwise               = succ Interpreter


-- The set of available backnds. This will be used for both the command line
-- options as well as the fancy header generation.
--
availableBackends :: (options :-> Backend) -> [OptDescr (options -> options)]
availableBackends optBackend =
  [ Option  [] [show Interpreter]
            (NoArg (set optBackend Interpreter))
            "reference implementation (sequential)"

#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
  , Option  [] [show CPU]
            (NoArg (set optBackend CPU))
            "LLVM based implementation for multicore CPUs (parallel)"
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
  , Option  [] [show PTX]
            (NoArg (set optBackend PTX))
            "LLVM based implementation for NVIDIA GPUs (parallel)"
#endif
#ifdef ACCELERATE_CUDA_BACKEND
  , Option  [] [show CUDA]
            (NoArg (set optBackend CUDA))
            "CUDA based implementation for NVIDIA GPUs (parallel)"
#endif
#ifdef ACCELERATE_LLVM_MULTIDEV_BACKEND
  , Option  [] [show Multi]
            (NoArg (set optBackend Multi))
            "LLVM based multi-device implementation using CPUs and GPUs (parallel)"
#endif
#ifdef ACCELERATE_CILK_BACKEND
  , Option  [] [show Cilk]
            (NoArg (set optBackend Cilk))
            "Cilk based implementation for multicore CPUs (parallel)"
#endif
  ]


-- By default, how many instances of the backend can be run concurrently?
--
-- If this returns `Nothing` then we make no changes to the configuration and
-- inherit the RTS options. If this is `Just n`, then by default only `n`
-- threads will be used to run tests at once. This option can still be
-- overridden via the command line flags, it just sets a default.
--
-- e.g.
--   * A multicore CPU backend may specify `Just 1`, so that only one instance
--     runs at once, and it inherits all threads specified via `+RTS -Nn`
--
--   * A thread-safe accelerator backend can specify `Nothing`, while a
--     non-thread-safe backend (perhaps it requires exclusive access to the
--     accelerator board) should specify `Just 1`.
--
concurrentBackends :: Backend -> Maybe Int
concurrentBackends Interpreter  = Nothing
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
concurrentBackends CPU          = Just 1
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
concurrentBackends PTX          = Nothing       -- ???
#endif
#ifdef ACCELERATE_CUDA_BACKEND
concurrentBackends CUDA         = Nothing       -- not quite true! D:
#endif
#ifdef ACCELERATE_CILK_BACKEND
concurrentBackends Cilk         = Just 1
#endif

