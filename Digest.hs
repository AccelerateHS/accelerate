{-# LANGUAGE BangPatterns #-}

module Digest where

import Config
import MD5

import Prelude
import Data.List
import Data.Monoid
import Control.Monad
import Data.ByteString.Internal                 ( w2c )
import qualified Data.ByteString.Lazy.Char8     as L
import qualified Data.Serialize                 as S
import qualified Data.Serialize.Builder         as B

import Data.Array.Accelerate                    ( Z(..), (:.)(..) )
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
md5block :: L.ByteString -> B.Builder
md5block msg = do
  let
      len               = fromIntegral (L.length msg)
      lenBits           = 8 * fromIntegral len
      lenZeroPad
        | len + 1 <= blockSizeBytes - 8 = (blockSizeBytes - 8) - (len + 1)
        | otherwise                     = (2 * blockSizeBytes - 8) - (len + 1)
  --
  S.execPut $! do
    S.putLazyByteString msg
    S.putWord8 0x80
    mapM_ S.putWord8 (replicate lenZeroPad 0)
    S.putWord64le lenBits


-- Create a dictionary of blocks ready to digest from the given bytestring. This
-- reads one entry per line. Because we only do a single MD5 chunk, we discard
-- any entries with (length > blockSizeBytes - 8 = 55) characters.
--
data Digest = Digest {-# UNPACK #-} !Int !B.Builder

instance Monoid Digest where
  mempty                                = Digest 0 B.empty
  mappend (Digest n1 b1) (Digest n2 b2) = Digest (n1+n2) (b1 <> b2)


runDigest :: Config -> Digest -> IO Dictionary
runDigest c (Digest entries builder) = do
  let lbs = B.toLazyByteString builder
  --
  blocks <- A.fromByteString (Z :. entries :. blockSizeWords) ((), L.toStrict lbs)
  return $! run c $ A.transpose (A.use blocks)


digestLazy :: L.ByteString -> Digest
digestLazy
  = foldl' (\rest next -> rest <> Digest 1 (md5block next)) mempty
  . filter (\w -> fromIntegral (L.length w) < blockSizeBytes - 8)
  . L.lines

digestFile :: FilePath -> IO Digest
digestFile fp = digestLazy `fmap` L.readFile fp

digestFiles :: [FilePath] -> IO Digest
digestFiles = foldM (\rest fp -> mappend rest `fmap` digestFile fp) mempty


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

