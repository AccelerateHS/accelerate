{-# LANGUAGE GADTs, TypeFamilies, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE RankNTypes, MagicHash, UnboxedTuples #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
-- |
-- Module      : Data.Array.Accelerate.Array.Data
-- Copyright   : [2009..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module fixes the concrete representation of Accelerate arrays.  We
-- allocate all arrays using pinned memory to enable safe direct-access by
-- non-Haskell code in multi-threaded code.  In particular, we can safely pass
-- pointers to an array's payload to foreign code.
--

module Data.Array.Accelerate.Array.Data (

  -- * Array operations and representations
  ArrayElt(..), ArrayData, MutableArrayData, runArrayData,
  ArrayEltR(..), GArrayData(..),

  -- * Array tuple operations
  fstArrayData, sndArrayData, pairArrayData

) where

-- standard libraries
import Foreign            (Ptr)
import GHC.Base           (Int(..))
import GHC.Prim           (newPinnedByteArray#, byteArrayContents#,
                           unsafeFreezeByteArray#, Int#, (*#))
import GHC.Ptr            (Ptr(Ptr))
import GHC.ST             (ST(ST))
import Control.Monad
import Control.Monad.ST
import qualified Data.Array.IArray  as IArray
import qualified Data.Array.MArray  as MArray hiding (newArray)
import Data.Array.ST      (STUArray)
import Data.Array.Unboxed (UArray)
import Data.Array.Base    (UArray(UArray), STUArray(STUArray), bOOL_SCALE,
                           wORD_SCALE, fLOAT_SCALE, dOUBLE_SCALE)

-- friends
import Data.Array.Accelerate.Type


-- Array representation
-- --------------------

-- |Immutable array representation
--
type ArrayData e = GArrayData (UArray Int) e

-- |Mutable array representation
--
type MutableArrayData s e = GArrayData (STUArray s Int) e

-- Array representation in dependence on the element type, but abstracting
-- over the basic array type (in particular, abstracting over mutability)
--
data family GArrayData ba e
data instance GArrayData ba ()      = AD_Unit
data instance GArrayData ba Int     = AD_Int     (ba Int)
data instance GArrayData ba Int8    = AD_Int8    (ba Int8)
data instance GArrayData ba Int16   = AD_Int16   (ba Int16)
data instance GArrayData ba Int32   = AD_Int32   (ba Int32)
data instance GArrayData ba Int64   = AD_Int64   (ba Int64)
data instance GArrayData ba Word    = AD_Word    (ba Word)
data instance GArrayData ba Word8   = AD_Word8   (ba Word8)
data instance GArrayData ba Word16  = AD_Word16  (ba Word16)
data instance GArrayData ba Word32  = AD_Word32  (ba Word32)
data instance GArrayData ba Word64  = AD_Word64  (ba Word64)
-- data instance GArrayData ba CShort  = AD_CShort  (ba CShort)
-- data instance GArrayData ba CUShort = AD_CUShort (ba CUShort)
-- data instance GArrayData ba CInt    = AD_CInt    (ba CInt)
-- data instance GArrayData ba CUInt   = AD_CUInt   (ba CUInt)
-- data instance GArrayData ba CLong   = AD_CLong   (ba CLong)
-- data instance GArrayData ba CULong  = AD_CULong  (ba CULong)
-- data instance GArrayData ba CLLong  = AD_CLLong  (ba CLLong)
-- data instance GArrayData ba CULLong = AD_CULLong (ba CULLong)
data instance GArrayData ba Float   = AD_Float   (ba Float)
data instance GArrayData ba Double  = AD_Double  (ba Double)
-- data instance GArrayData ba CFloat  = AD_CFloat  (ba CFloat)
-- data instance GArrayData ba CDouble = AD_CDouble (ba CDouble)
data instance GArrayData ba Bool    = AD_Bool    (ba Bool)
data instance GArrayData ba Char    = AD_Char    (ba Char)
-- data instance GArrayData ba CChar   = AD_CChar   (ba CChar)
-- data instance GArrayData ba CSChar  = AD_CSChar  (ba CSChar)
-- data instance GArrayData ba CUChar  = AD_CUChar  (ba CUChar)
data instance GArrayData ba (a, b)  = AD_Pair (GArrayData ba a)
                                              (GArrayData ba b)

-- | GADT to reify the 'ArrayElt' class.
--
data ArrayEltR a where
  ArrayEltRunit   :: ArrayEltR ()
  ArrayEltRint    :: ArrayEltR Int
  ArrayEltRint8   :: ArrayEltR Int8
  ArrayEltRint16  :: ArrayEltR Int16
  ArrayEltRint32  :: ArrayEltR Int32
  ArrayEltRint64  :: ArrayEltR Int64
  ArrayEltRword   :: ArrayEltR Word
  ArrayEltRword8  :: ArrayEltR Word8
  ArrayEltRword16 :: ArrayEltR Word16
  ArrayEltRword32 :: ArrayEltR Word32
  ArrayEltRword64 :: ArrayEltR Word64
  ArrayEltRfloat  :: ArrayEltR Float
  ArrayEltRdouble :: ArrayEltR Double
  ArrayEltRbool   :: ArrayEltR Bool
  ArrayEltRchar   :: ArrayEltR Char
  ArrayEltRpair   :: (ArrayElt a, ArrayElt b)
                  => ArrayEltR a -> ArrayEltR b -> ArrayEltR (a,b)

-- Array operations
-- ----------------

class ArrayElt e where
  type ArrayPtrs e
  --
  indexArrayData         :: ArrayData e -> Int -> e
  ptrsOfArrayData        :: ArrayData e -> ArrayPtrs e
  --
  newArrayData           :: Int -> ST s (MutableArrayData s e)
  readArrayData          :: MutableArrayData s e -> Int      -> ST s e
  writeArrayData         :: MutableArrayData s e -> Int -> e -> ST s ()
  unsafeFreezeArrayData  :: MutableArrayData s e -> ST s (ArrayData e)
  ptrsOfMutableArrayData :: MutableArrayData s e -> ST s (ArrayPtrs e)
  --
  arrayElt               :: ArrayEltR e

instance ArrayElt () where
  type ArrayPtrs () = ()
  indexArrayData AD_Unit i = i `seq` ()
  ptrsOfArrayData AD_Unit = ()
  newArrayData size = size `seq` return AD_Unit
  readArrayData AD_Unit i = i `seq` return ()
  writeArrayData AD_Unit i () = i `seq` return ()
  unsafeFreezeArrayData AD_Unit = return AD_Unit
  ptrsOfMutableArrayData AD_Unit = return ()
  arrayElt = ArrayEltRunit

instance ArrayElt Int where
  type ArrayPtrs Int = Ptr Int
  indexArrayData (AD_Int ba) i = ba IArray.! i
  ptrsOfArrayData (AD_Int ba) = uArrayPtr ba
  newArrayData size = liftM AD_Int $ unsafeNewArray_ size wORD_SCALE
  readArrayData (AD_Int ba) i = MArray.readArray ba i
  writeArrayData (AD_Int ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Int ba) = liftM AD_Int $ MArray.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_Int ba) = sTUArrayPtr ba
  arrayElt = ArrayEltRint

instance ArrayElt Int8 where
  type ArrayPtrs Int8 = Ptr Int8
  indexArrayData (AD_Int8 ba) i = ba IArray.! i
  ptrsOfArrayData (AD_Int8 ba) = uArrayPtr ba
  newArrayData size = liftM AD_Int8 $ unsafeNewArray_ size (\x -> x)
  readArrayData (AD_Int8 ba) i = MArray.readArray ba i
  writeArrayData (AD_Int8 ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Int8 ba) = liftM AD_Int8 $ MArray.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_Int8 ba) = sTUArrayPtr ba
  arrayElt = ArrayEltRint8

instance ArrayElt Int16 where
  type ArrayPtrs Int16 = Ptr Int16
  indexArrayData (AD_Int16 ba) i = ba IArray.! i
  ptrsOfArrayData (AD_Int16 ba) = uArrayPtr ba
  newArrayData size = liftM AD_Int16 $ unsafeNewArray_ size (*# 2#)
  readArrayData (AD_Int16 ba) i = MArray.readArray ba i
  writeArrayData (AD_Int16 ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Int16 ba) = liftM AD_Int16 $ MArray.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_Int16 ba) = sTUArrayPtr ba
  arrayElt = ArrayEltRint16

instance ArrayElt Int32 where
  type ArrayPtrs Int32 = Ptr Int32
  indexArrayData (AD_Int32 ba) i = ba IArray.! i
  ptrsOfArrayData (AD_Int32 ba) = uArrayPtr ba
  newArrayData size = liftM AD_Int32 $ unsafeNewArray_ size (*# 4#)
  readArrayData (AD_Int32 ba) i = MArray.readArray ba i
  writeArrayData (AD_Int32 ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Int32 ba) = liftM AD_Int32 $ MArray.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_Int32 ba) = sTUArrayPtr ba
  arrayElt = ArrayEltRint32

instance ArrayElt Int64 where
  type ArrayPtrs Int64 = Ptr Int64
  indexArrayData (AD_Int64 ba) i = ba IArray.! i
  ptrsOfArrayData (AD_Int64 ba) = uArrayPtr ba
  newArrayData size = liftM AD_Int64 $ unsafeNewArray_ size (*# 8#)
  readArrayData (AD_Int64 ba) i = MArray.readArray ba i
  writeArrayData (AD_Int64 ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Int64 ba) = liftM AD_Int64 $ MArray.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_Int64 ba) = sTUArrayPtr ba
  arrayElt = ArrayEltRint64

instance ArrayElt Word where
  type ArrayPtrs Word = Ptr Word
  indexArrayData (AD_Word ba) i = ba IArray.! i
  ptrsOfArrayData (AD_Word ba) = uArrayPtr ba
  newArrayData size = liftM AD_Word $ unsafeNewArray_ size wORD_SCALE
  readArrayData (AD_Word ba) i = MArray.readArray ba i
  writeArrayData (AD_Word ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Word ba) = liftM AD_Word $ MArray.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_Word ba) = sTUArrayPtr ba
  arrayElt = ArrayEltRword

instance ArrayElt Word8 where
  type ArrayPtrs Word8 = Ptr Word8
  indexArrayData (AD_Word8 ba) i = ba IArray.! i
  ptrsOfArrayData (AD_Word8 ba) = uArrayPtr ba
  newArrayData size = liftM AD_Word8 $ unsafeNewArray_ size (\x -> x)
  readArrayData (AD_Word8 ba) i = MArray.readArray ba i
  writeArrayData (AD_Word8 ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Word8 ba) = liftM AD_Word8 $ MArray.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_Word8 ba) = sTUArrayPtr ba
  arrayElt = ArrayEltRword8

instance ArrayElt Word16 where
  type ArrayPtrs Word16 = Ptr Word16
  indexArrayData (AD_Word16 ba) i = ba IArray.! i
  ptrsOfArrayData (AD_Word16 ba) = uArrayPtr ba
  newArrayData size = liftM AD_Word16 $ unsafeNewArray_ size (*# 2#)
  readArrayData (AD_Word16 ba) i = MArray.readArray ba i
  writeArrayData (AD_Word16 ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Word16 ba)
    = liftM AD_Word16 $ MArray.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_Word16 ba) = sTUArrayPtr ba
  arrayElt = ArrayEltRword16

instance ArrayElt Word32 where
  type ArrayPtrs Word32 = Ptr Word32
  indexArrayData (AD_Word32 ba) i = ba IArray.! i
  ptrsOfArrayData (AD_Word32 ba) = uArrayPtr ba
  newArrayData size = liftM AD_Word32 $ unsafeNewArray_ size (*# 4#)
  readArrayData (AD_Word32 ba) i = MArray.readArray ba i
  writeArrayData (AD_Word32 ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Word32 ba)
    = liftM AD_Word32 $ MArray.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_Word32 ba) = sTUArrayPtr ba
  arrayElt = ArrayEltRword32

instance ArrayElt Word64 where
  type ArrayPtrs Word64 = Ptr Word64
  indexArrayData (AD_Word64 ba) i = ba IArray.! i
  ptrsOfArrayData (AD_Word64 ba) = uArrayPtr ba
  newArrayData size = liftM AD_Word64 $ unsafeNewArray_ size (*# 8#)
  readArrayData (AD_Word64 ba) i = MArray.readArray ba i
  writeArrayData (AD_Word64 ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Word64 ba)
    = liftM AD_Word64 $ MArray.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_Word64 ba) = sTUArrayPtr ba
  arrayElt = ArrayEltRword64

-- FIXME:
-- CShort
-- CUShort
-- CInt
-- CUInt
-- CLong
-- CULong
-- CLLong
-- CULLong

instance ArrayElt Float where
  type ArrayPtrs Float = Ptr Float
  indexArrayData (AD_Float ba) i = ba IArray.! i
  ptrsOfArrayData (AD_Float ba) = uArrayPtr ba
  newArrayData size = liftM AD_Float $ unsafeNewArray_ size fLOAT_SCALE
  readArrayData (AD_Float ba) i = MArray.readArray ba i
  writeArrayData (AD_Float ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Float ba) = liftM AD_Float $ MArray.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_Float ba) = sTUArrayPtr ba
  arrayElt = ArrayEltRfloat

instance ArrayElt Double where
  type ArrayPtrs Double = Ptr Double
  indexArrayData (AD_Double ba) i = ba IArray.! i
  ptrsOfArrayData (AD_Double ba) = uArrayPtr ba
  newArrayData size = liftM AD_Double $ unsafeNewArray_ size dOUBLE_SCALE
  readArrayData (AD_Double ba) i = MArray.readArray ba i
  writeArrayData (AD_Double ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Double ba)
    = liftM AD_Double $ MArray.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_Double ba) = sTUArrayPtr ba
  arrayElt = ArrayEltRdouble

-- FIXME:
-- CFloat
-- CDouble

instance ArrayElt Bool where
  type ArrayPtrs Bool = Ptr Word8
  indexArrayData (AD_Bool ba) i = ba IArray.! i
--  ptrsOfArrayData (AD_Bool ba) = uArrayPtr ba???currently wrong
--    Unboxed Bool arrays are stored as bit vectors, which is unsuitable
--    for parallel processing (might be useful with sequential LLVM code though)
--    - Do we want to represent 'Array sh Bool' differently or do we want to
--      copy the array on marshaling (but then we also have to copy back
--      results of the same type)
  newArrayData size = liftM AD_Bool $ unsafeNewArray_ size bOOL_SCALE
  readArrayData (AD_Bool ba) i = MArray.readArray ba i
  writeArrayData (AD_Bool ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Bool ba) = liftM AD_Bool $ MArray.unsafeFreeze ba
--  ptrsOfMutableArrayData (AD_Bool ba) = sTUArrayPtr ba???
--    see ptrsOfArrayData
  arrayElt = ArrayEltRbool

instance ArrayElt Char where
--  type ArrayPtrs Char = ???unicode???
  indexArrayData (AD_Char ba) i = ba IArray.! i
--  ptrsOfArrayData (AD_Char ba) = ???
  newArrayData size = liftM AD_Char $ unsafeNewArray_ size (*# 4#)
  readArrayData (AD_Char ba) i = MArray.readArray ba i
  writeArrayData (AD_Char ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Char ba) = liftM AD_Char $ MArray.unsafeFreeze ba
--  ptrsOfMutableArrayData (AD_Char ba) = ???
  arrayElt = ArrayEltRchar

-- FIXME:
-- CChar
-- CSChar
-- CUChar

instance (ArrayElt a, ArrayElt b) => ArrayElt (a, b) where
  type ArrayPtrs (a, b) = (ArrayPtrs a, ArrayPtrs b)
  indexArrayData (AD_Pair a b) i = (indexArrayData a i, indexArrayData b i)
  ptrsOfArrayData (AD_Pair a b) = (ptrsOfArrayData a, ptrsOfArrayData b)
  newArrayData size
    = do
        a <- newArrayData size
        b <- newArrayData size
        return $ AD_Pair a b
  readArrayData (AD_Pair a b) i
    = do
        x <- readArrayData a i
        y <- readArrayData b i
        return (x, y)
  writeArrayData (AD_Pair a b) i (x, y)
    = do
        writeArrayData a i x
        writeArrayData b i y
  unsafeFreezeArrayData (AD_Pair a b)
    = do
        a' <- unsafeFreezeArrayData a
        b' <- unsafeFreezeArrayData b
        return $ AD_Pair a' b'
  ptrsOfMutableArrayData (AD_Pair a b)
    = do
        aptr <- ptrsOfMutableArrayData a
        bptr <- ptrsOfMutableArrayData b
        return (aptr, bptr)
  arrayElt = ArrayEltRpair arrayElt arrayElt

-- |Safe combination of creating and fast freezing of array data.
--
runArrayData :: ArrayElt e
             => (forall s. ST s (MutableArrayData s e, e)) -> (ArrayData e, e)
runArrayData st = runST $ do
                    (mad, r) <- st
                    ad <- unsafeFreezeArrayData mad
                    return (ad, r)

-- Array tuple operations
-- ----------------------

fstArrayData :: ArrayData (a, b) -> ArrayData a
fstArrayData (AD_Pair x _) = x

sndArrayData :: ArrayData (a, b) -> ArrayData b
sndArrayData (AD_Pair _ y) = y

pairArrayData :: ArrayData a -> ArrayData b -> ArrayData (a, b)
pairArrayData = AD_Pair



-- Auxiliary functions
-- -------------------

-- Our own version of the 'STUArray' allocation that uses /pinned/ memory,
-- which is aligned to 16 bytes.
--
unsafeNewArray_ :: Int -> (Int# -> Int#) -> ST s (STUArray s Int e)
unsafeNewArray_ n@(I# n#) elemsToBytes
 = ST $ \s1# ->
     case newPinnedByteArray# (elemsToBytes n#) s1# of
         (# s2#, marr# #) ->
             (# s2#, STUArray 0 (n - 1) n marr# #)

-- Obtains a pointer to the payload of an unboxed array.
--
-- PRECONDITION: The unboxed array must be pinned.
--
uArrayPtr :: UArray Int a -> Ptr a
uArrayPtr (UArray _ _ _ ba) = Ptr (byteArrayContents# ba)

-- Obtains a pointer to the payload of an unboxed ST array.
--
-- PRECONDITION: The unboxed ST array must be pinned.
--
sTUArrayPtr :: STUArray s Int a -> ST s (Ptr a)
sTUArrayPtr (STUArray _ _ _ mba) = ST $ \s ->
  case unsafeFreezeByteArray# mba s of
    (# s', ba #) -> (# s', Ptr (byteArrayContents# ba) #)

