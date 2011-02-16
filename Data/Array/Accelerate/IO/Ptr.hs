-- |
-- Module      : Data.Array.Accelerate.IO.Ptr
-- Copyright   : [2010..2011] Sean Seefried, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.IO.Ptr (

  -- * Copying to/from raw pointers
  BlockPtrs, fromPtr, toPtr,

  -- * Direct copying into/from an Accelerate array
  BlockCopyFun, BlockCopyFuns, fromArray, toArray

) where

import Data.Array.Accelerate.Array.Sugar        (Array, Shape, Elt, EltRepr)
import Data.Array.Accelerate.Array.BlockCopy


fromPtr :: (Shape sh, Elt e) => sh -> BlockPtrs (EltRepr e) -> IO (Array sh e)
fromPtr =  blockCopyToArray

toPtr :: (Shape sh, Elt e) => Array sh e -> BlockPtrs (EltRepr e) -> IO ()
toPtr = blockCopyFromArray


fromArray :: (Shape sh, Elt e) => Array sh e -> BlockCopyFuns (EltRepr e) -> IO ()
fromArray = blockCopyFromArrayWithFunctions

toArray :: (Shape sh, Elt e) => sh -> BlockCopyFuns (EltRepr e) -> IO (Array sh e)
toArray = blockCopyToArrayWithFunctions


