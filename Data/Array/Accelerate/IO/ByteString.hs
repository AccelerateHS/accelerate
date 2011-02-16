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

import Data.Array.Accelerate.Array.Sugar        (Array, Shape, Elt, EltRepr)
import Data.Array.Accelerate.Array.BlockCopy

fromByteString :: (Shape sh, Elt e) => sh -> ByteStrings (EltRepr e) -> IO (Array sh e)
fromByteString = byteStringsToArray

toByteString :: (Shape sh, Elt e) => Array sh e -> IO (ByteStrings (EltRepr e))
toByteString = arrayToByteStrings

