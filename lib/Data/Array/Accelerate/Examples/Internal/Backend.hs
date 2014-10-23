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

import Data.Label
import System.Console.GetOpt

import Data.Array.Accelerate                            ( Arrays, Acc )
import qualified Data.Array.Accelerate                  as A
import qualified Data.Array.Accelerate.Interpreter      as Interp
#ifdef ACCELERATE_CUDA_BACKEND
import qualified Data.Array.Accelerate.CUDA             as CUDA
#endif
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
import qualified Data.Array.Accelerate.LLVM.Native      as CPU
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
import qualified Data.Array.Accelerate.LLVM.PTX         as PTX
#endif


-- | Execute Accelerate expressions
--
run :: Arrays a => Backend -> Acc a -> a
run Interpreter = Interp.run
#ifdef ACCELERATE_CUDA_BACKEND
run CUDA        = CUDA.run
#endif
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
run CPU         = CPU.run
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
run PTX         = PTX.run
#endif


run1 :: (Arrays a, Arrays b) => Backend -> (Acc a -> Acc b) -> a -> b
run1 Interpreter f = Interp.run1 f
#ifdef ACCELERATE_CUDA_BACKEND
run1 CUDA        f = CUDA.run1 f
#endif
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
run1 CPU         f = CPU.run1 f
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
run1 PTX         f = PTX.run1 f
#endif

run2 :: (Arrays a, Arrays b, Arrays c) => Backend -> (Acc a -> Acc b -> Acc c) -> a -> b -> c
run2 backend f x y = run1 backend (A.uncurry f) (x,y)


-- | The set of backends available to execute the program. The example programs
--   all choose 'maxBound' as the default, so there should be some honesty in
--   how this list is sorted.
--
data Backend = Interpreter
#ifdef ACCELERATE_CUDA_BACKEND
             | CUDA
#endif
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
             | CPU
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
             | PTX
#endif
#ifdef ACCELERATE_LLVM_MULTIDEV_BACKEND
             | Multi
#endif
  deriving (Eq, Bounded)


-- The choice of show instance is important because this will be used to
-- generate the command line flag.
--
instance Show Backend where
  show Interpreter      = "interpreter"
#ifdef ACCELERATE_CUDA_BACKEND
  show CUDA             = "cuda"
#endif
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
  show CPU              = "llvm-cpu"
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
  show PTX              = "llvm-gpu"
#endif
#ifdef ACCELERATE_LLVM_MULTIDEV_BACKEND
  show Multi            = "llvm-multi"
#endif


-- The set of available backnds. This will be used for both the command line
-- options as well as the fancy header generation.
--
availableBackends :: (options :-> Backend) -> [OptDescr (options -> options)]
availableBackends optBackend =
  [ Option  [] [show Interpreter]
            (NoArg (set optBackend Interpreter))
            "reference implementation (sequential)"

#ifdef ACCELERATE_CUDA_BACKEND
  , Option  [] [show CUDA]
            (NoArg (set optBackend CUDA))
            "implementation for NVIDIA GPUs (parallel)"
#endif
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
#ifdef ACCELERATE_LLVM_MULTIDEV_BACKEND
  , Option  [] [show Multi]
            (NoArg (set optBackend Multi))
            "LLVM based multi-device implementation using CPUs and GPUs (parallel)"
#endif
  ]

