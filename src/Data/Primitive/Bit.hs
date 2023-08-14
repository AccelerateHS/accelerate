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
{-# OPTIONS_HADDOCK hide #-}
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

import Control.Exception
import Control.Monad.ST
import Data.Bits
import Data.Typeable
import qualified Foreign.Storable                                   as Foreign

import Data.Primitive.ByteArray
import Data.Primitive.Vec                                           ( Vec(..) )

import GHC.Base                                                     ( isTrue# )
import GHC.Generics
import GHC.Int
import GHC.Ptr
import GHC.TypeLits
import GHC.Types                                                    ( IO(..) )
import GHC.Word
import qualified GHC.Exts                                           as GHC

#if __GLASGOW_HASKELL__ < 902
import GHC.Prim                                                     hiding ( subWord8# )
#else
import GHC.Prim
#endif


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
  {-# INLINE sizeOf      #-}
  {-# INLINE alignment   #-}
  {-# INLINE peek        #-}
  {-# INLINE poke        #-}
  {-# INLINE peekElemOff #-}
  {-# INLINE pokeElemOff #-}

  alignment _ = 1
  sizeOf _ =
    let k = fromIntegral (natVal' (proxy# :: Proxy# n))
     in quot (k + 7) 8

  peek (Ptr addr#) =
    let k = natVal' (proxy# :: Proxy# n)
     in if k `rem` 8 /= 0
           then error "TODO: use BitMask.peekElemOff for non-multiple-of-8 sized bit-masks"
           else IO $ \s0 ->
                  case Foreign.sizeOf (undefined :: BitMask n)      of { I# bytes#      ->
                  case newByteArray# bytes# s0                      of { (# s1, mba# #) ->
                  case copyAddrToByteArray# addr# mba# 0# bytes# s1 of { s2             ->
                  case unsafeFreezeByteArray# mba# s2               of { (# s3, ba# #)  ->
                    (# s3, BitMask (Vec ba#) #)
                  }}}}

  peekElemOff (Ptr addr#) (I# i#) =
    let !(I# k#)      = fromInteger (natVal' (proxy# :: Proxy# n))
        !ki#          = i# *# k#
     in
     if isTrue# (k# <=# 8#)
        then let !(# q#, r# #) = quotRemInt# ki# 8#
                 !mask#        = ((wordToWord8# 1## `uncheckedShiftLWord8#` k#) `subWord8#` wordToWord8# 1##) `uncheckedShiftLWord8#` (8# -# k#)
                 combine u# v# = (uncheckedShiftLWord8# u# r# `orWord8#` uncheckedShiftRLWord8# v# (8# -# r#)) `andWord8#` mask#
              in
              if isTrue# (r# +# k# <=# 8#)
                 -- This element does not not cross the byte boundary
                 then IO $ \s0 ->
                   case newByteArray# 1# s0                                                         of { (# s1, mba# #) ->
                   case readWord8OffAddr# addr# q# s1                                               of { (# s2, w# #)   ->
                   case writeWord8Array# mba# 0# (uncheckedShiftLWord8# w# r# `andWord8#` mask#) s2 of { s3             ->
                   case unsafeFreezeByteArray# mba# s3                                              of { (# s4, ba# #)  ->
                     (# s4, BitMask (Vec ba#) #)
                   }}}}
                 -- This element crosses the byte boundary. Read two successive
                 -- bytes (note that on little-endian we can't just treat this
                 -- as a 16-bit load) to combine and extract the bits we need.
                 else IO $ \s0 ->
                   case newByteArray# 1# s0                           of { (# s1, mba# #) ->
                   case readWord8OffAddr# addr# q# s1                 of { (# s2, w0# #)  ->
                   case readWord8OffAddr# addr# (q# +# 1#) s2         of { (# s3, w1# #)  ->
                   case writeWord8Array# mba# 0# (combine w0# w1#) s3 of { s4             ->
                   case unsafeFreezeByteArray# mba# s4                of { (# s5, ba# #)  ->
                     (# s5, BitMask (Vec ba#) #)
                   }}}}}
        else
     if isTrue# (k# <=# 16#)
        then error "TODO: BitMask (8..16]"
        else
     if isTrue# (k# <=# 32#)
        then error "TODO: BitMask (16..32]"
        else
     if isTrue# (k# <=# 64#)
        then error "TODO: BitMask (32..64]"
        else
     error "TODO: BitMask.peekElemOff not yet supported at this size"

  poke (Ptr addr#) (BitMask (Vec ba#)) =
    let k = natVal' (proxy# :: Proxy# n)
     in if k `rem` 8 /= 0
           then error "TODO: use BitMask.pokeElemOff for non-multiple-of-8 sized bit-masks"
           else IO $ \s0 ->
                  case Foreign.sizeOf (undefined :: BitMask n)     of { I# bytes# ->
                  case copyByteArrayToAddr# ba# 0# addr# bytes# s0 of {
                    s1 -> (# s1, () #)
                }}

  pokeElemOff (Ptr addr#) (I# i#) (BitMask (Vec ba#)) =
    let !(I# k#)      = fromInteger (natVal' (proxy# :: Proxy# n))
        !ki#          = i# *# k#
     in
     if isTrue# (k# <=# 8#)
        then let !(# q#, r# #) = quotRemInt# ki# 8#
                 !rk#          = r# +# k#
              in
              if isTrue# (rk# <=# 8#)
                 -- This element does not cross the byte boundary
                 then let !w#     = uncheckedShiftRLWord8# (indexWord8Array# ba# 0#) r#
                          !mask#  = ((wordToWord8# 1## `uncheckedShiftLWord8#` k#) `subWord8#` wordToWord8# 1##) `uncheckedShiftLWord8#` (8# -# rk#)
                       in IO $ \s0 ->
                            case readWord8OffAddr# addr# q# s0                                                                              of { (# s1, v# #) ->
                            case writeWord8OffAddr# addr# q# ((v# `andWord8#` complementWord8# mask#) `orWord8#` (w# `andWord8#` mask#)) s1 of { s2           ->
                              (# s2, () #)
                            }}
                 -- This element crosses the byte boundary
                 else let !w#     = indexWord8Array# ba# 0#
                          !w0#    = w# `uncheckedShiftRLWord8#` r#
                          !w1#    = w# `uncheckedShiftLWord8#`  (8# -# r#)
                          !mask#  = ((wordToWord8# 1## `uncheckedShiftLWord8#` k#) `subWord8#` wordToWord8# 1##)
                          !mask0# = mask# `uncheckedShiftRLWord8#` (rk# -# 8#)
                          !mask1# = mask# `uncheckedShiftLWord8#` (16# -# rk#)
                       in IO $ \s0 ->
                            case readWord8OffAddr# addr# q# s0                                                                                          of { (# s1, v0# #) ->
                            case readWord8OffAddr# addr# (q# +# 1#) s1                                                                                  of { (# s2, v1# #) ->
                            case writeWord8OffAddr# addr# q#         ((v0# `andWord8#` complementWord8# mask0#) `orWord8#` (w0# `andWord8#` mask0#)) s2 of { s3            ->
                            case writeWord8OffAddr# addr# (q# +# 1#) ((v1# `andWord8#` complementWord8# mask1#) `orWord8#` (w1# `andWord8#` mask1#)) s3 of { s4            ->
                              (# s4, () #)
                            }}}}
        else
     error "TODO: BitMask.pokeElemOff not yet supported at this size"

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
fromList bits = case byteArrayFromListN bytes (pack bits' []) of
                  ByteArray ba# -> BitMask (Vec ba#)
  where
    bits' = take (fromInteger (natVal' (proxy# :: Proxy# n))) bits
    bytes = Foreign.sizeOf (undefined :: BitMask n)

    pack :: [Bit] -> [Word8] -> [Word8]
    pack [] acc = acc
    pack xs acc =
      let (h,t) = splitAt 8 xs
          w     = w8 7 0 h
       in
       pack t (w : acc)

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
      bytes = quot (n+7) 8
      (u,v) = quotRem i 8
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
  let n = fromInteger (natVal' (proxy# :: Proxy# n))
      l = quot (n+7) 8
  in
  case byteArrayFromListN l (replicate l (0 :: Word8)) of
    ByteArray ba# -> BitMask (Vec ba#)

{-# INLINE ones #-}
ones :: forall n. KnownNat n => BitMask n
ones =
  let n = fromInteger (natVal' (proxy# :: Proxy# n))
      l = quot (n+7) 8
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

orWord8# :: Word# -> Word# -> Word#
orWord8# = or#

andWord8# :: Word# -> Word# -> Word#
andWord8# = and#

complementWord8# :: Word# -> Word#
complementWord8# x# = x# `xor#` 0xff##

subWord8# :: Word# -> Word# -> Word#
subWord8# = minusWord#

uncheckedShiftLWord8# :: Word# -> Int# -> Word#
uncheckedShiftLWord8# = uncheckedShiftL#

uncheckedShiftRLWord8# :: Word# -> Int# -> Word#
uncheckedShiftRLWord8# = uncheckedShiftRL#

wordToWord8# :: Word# -> Word#
wordToWord8# x = x

#else
testBitWord8# :: Word8# -> Int# -> Int#
testBitWord8# x# i# = (x# `andWord8#` bitWord8# i#) `neWord8#` wordToWord8# 0##

bitWord8# :: Int# -> Word8#
bitWord8# i# = (wordToWord8# 1##) `uncheckedShiftLWord8#` i#

complementWord8# :: Word8# -> Word8#
complementWord8# x# = x# `xorWord8#` wordToWord8# 0xff##
#endif

