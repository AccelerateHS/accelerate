{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UnboxedTuples              #-}
-- |
-- Module      : Data.Primitive.Bit
-- Copyright   : [2008..2022] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Primitive.Bit (

  Bit(..),
  BitMask(..),
  toList, fromList,
  extract, insert, zeros, ones,

) where

import Data.Array.Accelerate.Error

import Data.Bits
import Data.Typeable
import Control.Monad.ST
import Control.Exception
import qualified Foreign.Storable                                   as Foreign

import Data.Primitive.ByteArray
import Data.Primitive.Vec                                           ( Vec(..) )

import GHC.Base                                                     ( isTrue# )
import GHC.Generics
import GHC.Int
import GHC.Prim
import GHC.Ptr
import GHC.TypeLits
import GHC.Types                                                    ( IO(..) )
import GHC.Word
import qualified GHC.Exts                                           as GHC


-- | A newtype wrapper over 'Bool' whose instances pack bits as efficiently
-- as possible (8 values per byte). Arrays of 'Bit' use 8x less memory than
-- arrays of 'Bool' (which stores one value per byte). However, (parallel)
-- random writes are slower.
--
newtype Bit = Bit { unBit :: Bool }
  deriving (Eq, Ord, Bounded, Enum, FiniteBits, Bits, Typeable, Generic)

instance Show Bit where
  showsPrec _ (Bit False) = showString "0"
  showsPrec _ (Bit True)  = showString "1"

instance Read Bit where
  readsPrec p = \case
    ' ':rest -> readsPrec p rest
    '0':rest -> [(Bit False, rest)]
    '1':rest -> [(Bit True,  rest)]
    _        -> []

instance Num Bit where
  Bit a * Bit b = Bit (a && b)
  Bit a + Bit b = Bit (a /= b)
  Bit a - Bit b = Bit (a /= b)
  negate = id
  abs    = id
  signum = id
  fromInteger = Bit . odd

instance Real Bit where
  toRational = fromIntegral

instance Integral Bit where
  quotRem _ (Bit False) = throw DivideByZero
  quotRem x (Bit True)  = (x, Bit False)
  toInteger (Bit False) = 0
  toInteger (Bit True)  = 1


-- | A SIMD vector of 'Bit's
--
newtype BitMask n = BitMask { unMask :: Vec n Bit }
  deriving Eq
  -- XXX: We should mask off the unused bits before testing for equality,
  -- otherwise we are including junk in the test. TLM 2022-06-07

instance KnownNat n => Show (BitMask n) where
  show = bin . toList
    where
      bin :: [Bit] -> String
      bin bs = '0':'b': go bs
      --
      go []               = []
      go (Bit True :rest) = '1' : go rest
      go (Bit False:rest) = '0' : go rest


instance KnownNat n => GHC.IsList (BitMask n) where
  type Item (BitMask n) = Bit
  {-# INLINE toList   #-}
  {-# INLINE fromList #-}
  toList   = toList
  fromList = fromList

instance KnownNat n => Foreign.Storable (BitMask n) where
  {-# INLINE sizeOf    #-}
  {-# INLINE alignment #-}
  {-# INLINE peek      #-}
  {-# INLINE poke      #-}

  alignment _ = 1
  sizeOf _ =
    let k     = fromIntegral (natVal' (proxy# :: Proxy# n))
        (q,r) = quotRem k 8
     in if r == 0
          then q
          else q+1

  peek (Ptr addr#) =
    IO $ \s0 ->
      case Foreign.sizeOf (undefined :: BitMask n)      of { I# bytes#      ->
      case newByteArray# bytes# s0                      of { (# s1, mba# #) ->
      case copyAddrToByteArray# addr# mba# 0# bytes# s1 of { s2             ->
      case unsafeFreezeByteArray# mba# s2               of { (# s3, ba# #)  ->
        (# s3, BitMask (Vec ba#) #)
      }}}}

  poke (Ptr addr#) (BitMask (Vec ba#)) =
    IO $ \s0 ->
      case Foreign.sizeOf (undefined :: BitMask n)     of { I# bytes# ->
      case copyByteArrayToAddr# ba# 0# addr# bytes# s0 of {
        s1 -> (# s1, () #)
    }}

{-# INLINE toList #-}
toList :: forall n. KnownNat n => BitMask n -> [Bit]
toList (BitMask (Vec ba#)) = concat (unpack 0# [])
  where
    !(I# n#) = fromInteger (natVal' (proxy# :: Proxy# n))

    unpack :: Int# -> [[Bit]] -> [[Bit]]
    unpack i# acc
      | isTrue# (i# <# n#) =
          let q#    = quotInt# i# 8#
              w#    = indexWord8Array# ba# q#
              lim#  = minInt# 8# (n# -# i#)
              w8 j# = if isTrue# (j# <# lim#)
                         then let b# = testBitWord8# w# (7# -# j#)
                               in Bit (isTrue# b#) : w8 (j# +# 1#)
                         else []
          in
          unpack (i# +# 8#) (w8 0# : acc)
      | otherwise = acc

{-# INLINE fromList #-}
fromList :: forall n. KnownNat n => [Bit] -> BitMask n
fromList bits = case byteArrayFromListN bytes (pack bits') of
                  ByteArray ba# -> BitMask (Vec ba#)
  where
    bits' = take (fromInteger (natVal' (proxy# :: Proxy# n))) bits
    bytes = Foreign.sizeOf (undefined :: BitMask n)

    pack :: [Bit] -> [Word8]
    pack xs =
      let (h,t) = splitAt 8 xs
          w     = w8 7 0 h
      in if null t
           then [w]
           else w : pack t

    w8 :: Int -> Word8 -> [Bit] -> Word8
    w8 !_ !w []             = w
    w8 !i !w (Bit True :bs) = w8 (i-1) (setBit w i) bs
    w8 !i !w (Bit False:bs) = w8 (i-1) w            bs

{-# INLINE extract #-}
extract :: forall n. KnownNat n => BitMask n -> Int -> Bit
extract (BitMask (Vec ba#)) i@(I# i#) =
  let n             = fromInteger (natVal' (proxy# :: Proxy# n))
      !(# q#, r# #) = quotRemInt# i# 8#
      w#            = indexWord8Array# ba# q#
      b#            = testBitWord8# w# r#
  in
  boundsCheck "out of range" (i >= 0 && i < n) (Bit (isTrue# b#))

{-# INLINE insert #-}
insert :: forall n. KnownNat n => BitMask n -> Int -> Bit -> BitMask n
insert (BitMask (Vec ba#)) i (Bit b) = runST $ do
  let n     = fromInteger (natVal' (proxy# :: Proxy# n))
      (u,v) = quotRem i 8
      (q,r) = quotRem n 8
      bytes = if r == 0
                 then q
                 else q + 1
  --
  mba <- newByteArray n
  copyByteArray mba 0 (ByteArray ba#) 0 bytes
  x :: Word8 <- readByteArray mba u
  writeByteArray mba u $ if b then setBit   x v
                              else clearBit x v
  ByteArray ba'# <- unsafeFreezeByteArray mba
  return (BitMask (Vec ba'#))

{-# INLINE zeros #-}
zeros :: forall n. KnownNat n => BitMask n
zeros =
  let n     = fromInteger (natVal' (proxy# :: Proxy# n))
      (q,r) = quotRem n 8
      l     = if r == 0
                 then q
                 else q + 1
  in
  case byteArrayFromListN l (replicate l (0 :: Word8)) of
    ByteArray ba# -> BitMask (Vec ba#)

{-# INLINE ones #-}
ones :: forall n. KnownNat n => BitMask n
ones =
  let n     = fromInteger (natVal' (proxy# :: Proxy# n))
      (q,r) = quotRem n 8
      l     = if r == 0
                 then q
                 else q + 1
  in
  case byteArrayFromListN l (replicate l (0xff :: Word8)) of
    ByteArray ba# -> BitMask (Vec ba#)


minInt# :: Int# -> Int# -> Int#
minInt# a# b# =
  case a# <# b# of
    0# -> b#
    _  -> a#

#if __GLASGOW_HASKELL__ < 902
testBitWord8# :: Word# -> Int# -> Int#
testBitWord8# x# i# = (x# `and#` bitWord8# i#) `neWord#` 0##

bitWord8# :: Int# -> Word#
bitWord8# i# = narrow8Word# (1## `uncheckedShiftL#` i#)

#else
testBitWord8# :: Word8# -> Int# -> Int#
testBitWord8# x# i# = (x# `andWord8#` bitWord8# i#) `neWord8#` (wordToWord8# 0##)

bitWord8# :: Int# -> Word8#
bitWord8# i# = (wordToWord8# 1##) `uncheckedShiftLWord8#` i#
#endif

