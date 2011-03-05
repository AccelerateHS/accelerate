-- |
-- Module      : Data.Array.Accelerate.IO
-- Copyright   : [2010..2011] Sean Seefried, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module provides functions for efficient block copies of primitive arrays
-- (i.e. one dimensional, in row-major order in contiguous memory) to Accelerate
-- Arrays.
--
-- You should only use this module if you really know what you are doing.
-- Potential pitfalls include:
--
--   * copying from memory your program doesn't have access to (e.g. it may be
--     unallocated or not enough memory is allocated)
--
--   * memory alignment errors
--

module Data.Array.Accelerate.IO (

  module Data.Array.Accelerate.IO.Ptr,
  module Data.Array.Accelerate.IO.ByteString

) where

import Data.Array.Accelerate.IO.Ptr
import Data.Array.Accelerate.IO.ByteString

