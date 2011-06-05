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

import Data.Array.Accelerate.IO.BlockCopy
import Data.Array.Accelerate.Array.Sugar



-- | Block copy regions of memory into a freshly allocated Accelerate array. The
--   type of elements (@e@) in the output Accelerate array determines the
--   structure of the collection of pointers that will be required as the second
--   argument to this function. See 'BlockPtrs'
--
--   Each one of these pointers points to a block of memory that is the source
--   of data for the Accelerate array (unlike function 'toArray' where one
--   passes in function which copies data to a destination address.).
--
fromPtr :: (Shape sh, Elt e) => sh -> BlockPtrs (EltRepr e) -> IO (Array sh e)
fromPtr sh blkPtrs = do
  let arr    = allocateArray sh
      copier = let ((f,_),_,_) = blockCopyFunGenerator arr in f
  copier blkPtrs
  return arr


-- | Block copy from Accelerate array to pre-allocated regions of memory. The
--   type of element of the input Accelerate array (@e@) determines the
--   structure of the collection of pointers that will be required as the second
--   argument to this function. See 'BlockPtrs'
--
--   The memory associated with the pointers must have already been allocated.
--
toPtr :: (Shape sh, Elt e) => Array sh e -> BlockPtrs (EltRepr e) -> IO ()
toPtr arr blockPtrs = do
  let copier = let (_,(f,_),_) = blockCopyFunGenerator arr in f
  copier blockPtrs
  return ()


-- | Copy values from an Accelerate array using a collection of functions that
--   have type 'BlockCopyFun'. The argument of type @Ptr e@ in each of these
--   functions refers to the address of the /source/ block of memory in the
--   Accelerate Array. The /destination/ address is implicit. e.g. the
--   'BlockCopyFun' could be the result of partially application to a @Ptr e@
--   pointing to the destination block.
--
--   The structure of this collection of functions depends on the elemente type
--   @e@. Each function (of type 'BlockCopyFun') copies data to a destination
--   address (pointed to by the argument of type @Ptr ()@).
--
--   Unless there is a particularly pressing reason to use this function, the
--   'fromPtr' function is sufficient as it uses an efficient low-level call to
--   libc's @memcpy@ to perform the copy.
--
fromArray :: (Shape sh, Elt e) => Array sh e -> BlockCopyFuns (EltRepr e) -> IO ()
fromArray arr blockCopyFuns = do
   let copier = let (_,_,f) = blockCopyFunGenerator arr in f
   copier blockCopyFuns
   return ()


-- | Copy values to a freshly allocated Accelerate array using a collection of
--   functions that have type 'BlockCopyFun'. The argument of type @Ptr e@ in
--   each of these functions refers to the address of the /destination/ block of
--   memory in the Accelerate Array. The /source/ address is implicit. e.g. the
--   'BlockCopyFun' could be the result of a partial application to a @Ptr e@
--   pointing to the source block.
--
--   The structure of this collection of functions depends on the elemente type
--   @e@. Each function (of type 'BlockCopyFun') copies data to a destination
--   address (pointed to by the argument of type @Ptr ()@).
--
--   Unless there is a particularly pressing reason to use this function, the
--   'fromPtr' function is sufficient as it uses an efficient low-level call to
--   libc's @memcpy@ to perform the copy.
--
toArray :: (Shape sh, Elt e) => sh -> BlockCopyFuns (EltRepr e) -> IO (Array sh e)
toArray sh blockCopyFuns = do
  let arr    = allocateArray sh
      copier = let (_,_,f) = blockCopyFunGenerator arr in f
  copier blockCopyFuns
  return arr

