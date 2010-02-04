-- This module defines a state monad which keeps track of the code generator
-- state, including the unique suffix used in the code generator.

module Data.Array.Accelerate.CUDA.Monad (
  CGIO, CGState(..)
) where

import Control.Monad.State

data CGState = CGState { uniqueID :: Int }

type CGIO = StateT CGState IO
