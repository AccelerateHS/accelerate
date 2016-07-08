{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module:      : Data.Array.Accelerate.Examples.Internal
-- Copyright    : [2014] Trevor L. McDonell
-- License      : BSD3
--
-- Maintainer   : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--
-- This module exposes some internal infrastructure that is useful and common to
-- the accelerate-examples test and benchmark program suite. This is not
-- intended for general consumption.
--

module Data.Array.Accelerate.Examples.Internal (

  module Data.Array.Accelerate.Examples.Internal.ParseArgs,
  module Data.Array.Accelerate.Examples.Internal.Criterion,
  module Data.Array.Accelerate.Examples.Internal.Interactive,
  module Data.Array.Accelerate.Examples.Internal.TestFramework,
  module Data.Array.Accelerate.Examples.Internal.Monitoring,
  module Data.Array.Accelerate.Examples.Internal.Random.Array,
  module Data.Array.Accelerate.Examples.Internal.Util,

) where

import Data.Array.Accelerate.Examples.Internal.Criterion
import Data.Array.Accelerate.Examples.Internal.Interactive
import Data.Array.Accelerate.Examples.Internal.Monitoring
import Data.Array.Accelerate.Examples.Internal.ParseArgs
import Data.Array.Accelerate.Examples.Internal.Random.Array
import Data.Array.Accelerate.Examples.Internal.Similar
import Data.Array.Accelerate.Examples.Internal.TestFramework
import Data.Array.Accelerate.Examples.Internal.Util

