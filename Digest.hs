{-# LANGUAGE BangPatterns #-}

module Digest where

import MD5
import Config

import Prelude
import Data.List                                ( foldl' )
import Data.Label
import Data.Word
import Control.Monad
import Data.ByteString.Internal                 ( w2c )
import qualified Data.Serialize                 as S
import qualified Data.ByteString                as S
import qualified Data.ByteString.Lazy.Char8     as L

import Data.Array.Accelerate.Array.Sugar        as A
import qualified Data.Array.Accelerate          as A
import qualified Data.Array.Accelerate.IO       as A


-- MD5 block sizes
--
blockSizeBits, blockSizeBytes, blockSizeWords :: Int
blockSizeBits  = 512
blockSizeBytes = blockSizeBits `div` 8
blockSizeWords = blockSizeBytes `div` 4


-- Create an MD5 block from the given message. This appends The '1' bit to the
-- message, pads the block with zeros until the length in bits is 448, and
-- finally appends the length in bits (mod 2^64).
--
md5block :: L.ByteString -> S.ByteString
md5block msg = do
  let
      len               = fromIntegral (L.length msg)
      lenBits           = 8 * fromIntegral len
      lenZeroPad
        | len + 1 <= blockSizeBytes - 8 = (blockSizeBytes - 8) - (len + 1)
        | otherwise                     = (2 * blockSizeBytes - 8) - (len + 1)
  --
  S.runPut $! do
    S.putLazyByteString msg
    S.putWord8 0x80
    mapM_ S.putWord8 (replicate lenZeroPad 0)
    S.putWord64le lenBits


-- Create a dictionary of blocks ready to digest from the given bytestring. This
-- reads one entry per line. Because we only do a single MD5 chunk, we discard
-- any entries with (length > blockSizeBytes - 8 = 55) characters.
--
data Digest = Digest {-# UNPACK #-} !Int {-# UNPACK #-} !S.ByteString

transpose :: Array DIM2 Word32 -> Array DIM2 Word32
transpose arr = newArray (Z:.w:.h) swap
  where
    Z:.h:.w        = A.arrayShape arr
    swap (Z:.i:.j) = arr `A.indexArray` (Z:.j:.i)

runDigest :: Digest -> IO Dictionary
runDigest (Digest entries bs) = do
  blocks <- A.fromByteString (Z :. entries :. blockSizeWords) ((), bs)
  return $! transpose blocks

digestLazy :: Config -> L.ByteString -> Digest
digestLazy c
  = finalise
  . foldl' (\(!i,!rest) next -> (i+1, md5block next : rest)) (0, [])
  . maybe id take (get configMaxWords c)
  . drop (get configSkipWords c)
  . filter (\w -> fromIntegral (L.length w) < blockSizeBytes - 8)
  . L.lines
  where
    finalise (n,bs) = Digest n (S.concat bs)

digestFile :: Config -> FilePath -> IO Digest
digestFile c fp = digestLazy c `fmap` L.readFile fp


-- Extract a word from the dictionary at a given index
--
extract :: Dictionary -> Int -> L.ByteString
extract dict i
  | i > n       = error "extract: index too large"
  | otherwise   = L.takeWhile (/= w2c 0x80) word
  where
    Z :. _ :. n = A.arrayShape dict
    word        = S.runPutLazy $
      forM_ [0 .. blockSizeWords-1] $ \c -> S.putWord32le (dict `A.indexArray` (Z:.c:.i))

