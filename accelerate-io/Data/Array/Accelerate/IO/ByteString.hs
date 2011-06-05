-- |
-- Module      : Data.Array.Accelerate.IO.ByteString
-- Copyright   : [2010..2011] Sean Seefried, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.IO.ByteString (

  -- * Copy to/from (strict) ByteString`s
  ByteStrings, fromByteString, toByteString

) where

import Data.Array.Accelerate.IO.BlockCopy
import Data.Array.Accelerate.Array.Sugar



-- | Block copies bytes from a collection of 'ByteString's to freshly allocated
--   Accelerate array.
--
--   The type of elements (@e@) in the output Accelerate array determines the
--   structure of the collection of 'ByteString's that will be required as the
--   second argument to this function. See 'ByteStrings'
--
fromByteString :: (Shape sh, Elt e) => sh -> ByteStrings (EltRepr e) -> IO (Array sh e)
fromByteString sh byteStrings = do
  let arr    = allocateArray sh
      copier = let ((_,f),_,_) = blockCopyFunGenerator arr in f
  copier byteStrings
  return arr


-- | Block copy from an Accelerate array to a collection of freshly allocated
--   'ByteString's.
--
--   The type of elements (@e@) in the input Accelerate array determines the
--   structure of the collection of 'ByteString's that will be output. See
--   'ByteStrings'
--
toByteString :: (Shape sh, Elt e) => Array sh e -> IO (ByteStrings (EltRepr e))
toByteString arr = do
  let copier = let (_,(_,f),_) = blockCopyFunGenerator arr in f
  copier

