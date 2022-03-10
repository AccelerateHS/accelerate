{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE UnliftedFFITypes         #-}
{-# OPTIONS_GHC -fobject-code #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Crypto.Hash.XKCP
-- Copyright   : [2016..2022] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Crypto.Hash.XKCP (

  SHA3_256,
  hash, hashlazy,

) where

import Control.Monad
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Language.Haskell.TH.Syntax
import System.IO.Unsafe

import qualified Data.ByteString                                    as S
import qualified Data.ByteString.Lazy                               as L
import qualified Data.ByteString.Lazy.Internal                      as L
import qualified Data.ByteString.Unsafe                             as B

import GHC.Exts
import GHC.Base
import GHC.Word
import GHC.Show


-- | SHA3 (256 bits) cryptographic hash digest
--
data SHA3_256 = SHA3_256 ByteArray#

instance Show SHA3_256 where
  show (SHA3_256 ba#) =
    let go !i# =
          case i# <# 32# of
            0# -> []
            _  -> let w8# = indexWord8Array# ba# i#
                      w#  = word8ToWord# w8#
                      n#  = quotWord# w# 16##
                      d#  = remWord# w# 16##
                      x   = intToDigit (I# (word2Int# n#))
                      y   = intToDigit (I# (word2Int# d#))
                  in
                  x : y : go (i# +# 1#)
    in
    go 0#

instance Eq SHA3_256 where
  SHA3_256 ba1# == SHA3_256 ba2# =
    case reallyUnsafePtrEquality# (unsafeCoerce# ba1#) (unsafeCoerce# ba2#) of
      1# -> True
      _  -> case (compareByteArrays# ba1# 0# ba2# 0# 32#) of
              0# -> True
              _  -> False

instance Ord SHA3_256 where
  compare (SHA3_256 ba1#) (SHA3_256 ba2#) =
    let go !i# =
          case i# <# 32# of
            0# -> EQ
            _  -> case W8# (indexWord8Array# ba1# i#) `compare` W8# (indexWord8Array# ba2# i#) of
                    EQ -> go (i# +# 1#)
                    r  -> r
    in
    go 0#

#if !MIN_VERSION_base(4,16,0)
{-# INLINE word8ToWord# #-}
word8ToWord# :: Word# -> Word#
word8ToWord# w# = w#
#endif

-- | Hash a strict 'S.ByteString' into a digest
--
hash :: S.ByteString -> SHA3_256
hash bs = unsafePerformIO $!
  B.unsafeUseAsCStringLen bs $ \(p, n) -> keccak_Hash_SHA3_256 (castPtr p) n


-- | Hash a lazy 'L.ByteString' into a digest
--
hashlazy :: L.ByteString -> SHA3_256
hashlazy lbs = unsafePerformIO $! do
  s <- keccak_HashInitialize_SHA3_256
  let go L.Empty        = keccak_HashFinal_SHA3_256 s
      go (L.Chunk c cs) = do
        B.unsafeUseAsCStringLen c $ \(p, n) -> keccak_HashUpdate_SHA3_256 s (castPtr p) n
        go cs
  go lbs


-- Internals
-- -----------------------------------------------------------------------------

keccak_Hash_SHA3_256 :: Ptr Word8 -> Int -> IO SHA3_256
keccak_Hash_SHA3_256 ptr len =
  IO $ \s0 ->
    case newByteArray# 32# s0                   of { (# s1, mba# #) ->
    case c_sha3_256 mba# ptr (fromIntegral len) of { IO c_sha3_256# ->
    case c_sha3_256# s1                         of { (# s2, _ #) ->
    case unsafeFreezeByteArray# mba# s2         of { (# s3, hash_val# #) ->
      (# s3, SHA3_256 hash_val# #)
    }}}}


data Keccak_HashInstance = Keccak_HashInstance (MutableByteArray# RealWorld)

-- See: KeccakHash.h for magic numbers
--
keccak_HashInitialize_SHA3_256 :: IO Keccak_HashInstance
keccak_HashInitialize_SHA3_256 =
  IO $ \s0 ->
    case newByteArray# 224# s0                                      of { (# s1, hash_instance# #) ->
    case c_keccak_hash_initialise hash_instance# 1088 512 256 0x06  of { IO c_keccak_hash_initialise# ->
    case c_keccak_hash_initialise# s1                               of { (# s2, _ #) ->
      (# s2, Keccak_HashInstance hash_instance# #)
  }}}

keccak_HashUpdate_SHA3_256 :: Keccak_HashInstance -> Ptr Word8 -> Int -> IO ()
keccak_HashUpdate_SHA3_256 (Keccak_HashInstance hash_instance#) ptr len =
  void $ c_keccak_hash_update hash_instance# ptr (fromIntegral len * 8)

keccak_HashFinal_SHA3_256 :: Keccak_HashInstance -> IO SHA3_256
keccak_HashFinal_SHA3_256 (Keccak_HashInstance hash_instance#) =
  IO $ \s0 ->
    case newByteArray# 32# s0                    of { (# s1, mba# #) ->
    case c_keccak_hash_final hash_instance# mba# of { IO c_keccak_hash_final# ->
    case c_keccak_hash_final# s1                 of { (# s2, _ #) ->
    case unsafeFreezeByteArray# mba# s2          of { (# s3, hash_val# #) ->
      (# s3, SHA3_256 hash_val# #)
    }}}}

-- SEE: [HLS and GHC IDE]
--
#ifndef __GHCIDE__

foreign import ccall unsafe "SHA3_256" c_sha3_256 :: MutableByteArray# RealWorld -> Ptr Word8 -> CSize -> IO CInt
foreign import ccall unsafe "Keccak_HashInitialize" c_keccak_hash_initialise :: MutableByteArray# RealWorld -> CUInt -> CUInt -> CUInt -> CUChar -> IO CInt
foreign import ccall unsafe "Keccak_HashUpdate" c_keccak_hash_update :: MutableByteArray# RealWorld -> Ptr Word8 -> CSize -> IO CInt
foreign import ccall unsafe "Keccak_HashFinal" c_keccak_hash_final :: MutableByteArray# RealWorld -> MutableByteArray# RealWorld -> IO CInt

#else

c_sha3_256 :: Ptr Word8 -> Ptr Word8 -> CSize -> IO CInt
c_sha3_256 = undefined

c_keccak_hash_initialise :: MutableByteArray# RealWorld -> CUInt -> CUInt -> CUInt -> CUChar -> IO CInt
c_keccak_hash_initialise = undefined

c_keccak_hash_update :: MutableByteArray# RealWorld -> Ptr Word8 -> CSize -> IO CInt
c_keccak_hash_update = undefined

c_keccak_hash_final :: MutableByteArray# RealWorld -> MutableByteArray# RealWorld -> IO CInt
c_keccak_hash_final = undefined

#endif


-- SEE: [linking to .c files]
--
runQ $ do
  addForeignFilePath LangC "cbits/xkcp/KeccakHash.c"
  addForeignFilePath LangC "cbits/xkcp/KeccakSponge.c"
  addForeignFilePath LangC "cbits/xkcp/SimpleFIPS202.c"
  addForeignFilePath LangC "cbits/xkcp/KeccakP-1600-opt64.c"
  return []

