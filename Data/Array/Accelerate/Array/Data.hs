{-# LANGUAGE GADTs, TypeFamilies, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE RankNTypes, MagicHash, UnboxedTuples #-}
-- |
-- Module      : Data.Array.Accelerate.Array.Data
-- Copyright   : [2009..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
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

module Data.Array.Accelerate.Array.Data (

  -- * Array operations and representations
  ArrayElem(..), ArrayData, MutableArrayData, runArrayData,

) where

-- standard libraries
import Foreign            (Ptr)
import GHC.Prim           (ByteArray#, byteArrayContents#,
                           unsafeFreezeByteArray#)
import GHC.Ptr            (Ptr(Ptr))
import GHC.ST             (ST(ST))
import Control.Monad
import Control.Monad.ST
import qualified Data.Array.IArray  as IArray
import qualified Data.Array.MArray  as MArray
import Data.Array.ST      (STUArray)
import Data.Array.Unboxed (UArray)
import Data.Array.Base    (UArray(UArray), STUArray(STUArray))

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


-- Array operations
-- ----------------

class ArrayElem e where
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

instance ArrayElem () where
  type ArrayPtrs () = ()
  indexArrayData AD_Unit i = i `seq` ()
  ptrsOfArrayData AD_Unit = ()
  newArrayData size = size `seq` return AD_Unit
  readArrayData AD_Unit i = i `seq` return ()
  writeArrayData AD_Unit i () = i `seq` return ()
  unsafeFreezeArrayData AD_Unit = return AD_Unit
  ptrsOfMutableArrayData AD_Unit = return ()

instance ArrayElem Int where
  type ArrayPtrs Int = Ptr Int
  indexArrayData (AD_Int ba) i = ba IArray.! i
  ptrsOfArrayData (AD_Int ba) = uArrayPtr ba
  newArrayData size = liftM AD_Int $ MArray.newArray_ (0, size - 1)
  readArrayData (AD_Int ba) i = MArray.readArray ba i
  writeArrayData (AD_Int ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Int ba) = liftM AD_Int $ MArray.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_Int ba) = sTUArrayPtr ba

instance ArrayElem Int8 where
  type ArrayPtrs Int8 = Ptr Int8
  indexArrayData (AD_Int8 ba) i = ba IArray.! i
  ptrsOfArrayData (AD_Int8 ba) = uArrayPtr ba
  newArrayData size = liftM AD_Int8 $ MArray.newArray_ (0, size - 1)
  readArrayData (AD_Int8 ba) i = MArray.readArray ba i
  writeArrayData (AD_Int8 ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Int8 ba) = liftM AD_Int8 $ MArray.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_Int8 ba) = sTUArrayPtr ba

instance ArrayElem Int16 where
  type ArrayPtrs Int16 = Ptr Int16
  indexArrayData (AD_Int16 ba) i = ba IArray.! i
  ptrsOfArrayData (AD_Int16 ba) = uArrayPtr ba
  newArrayData size = liftM AD_Int16 $ MArray.newArray_ (0, size - 1)
  readArrayData (AD_Int16 ba) i = MArray.readArray ba i
  writeArrayData (AD_Int16 ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Int16 ba) = liftM AD_Int16 $ MArray.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_Int16 ba) = sTUArrayPtr ba

instance ArrayElem Int32 where
  type ArrayPtrs Int32 = Ptr Int32
  indexArrayData (AD_Int32 ba) i = ba IArray.! i
  ptrsOfArrayData (AD_Int32 ba) = uArrayPtr ba
  newArrayData size = liftM AD_Int32 $ MArray.newArray_ (0, size - 1)
  readArrayData (AD_Int32 ba) i = MArray.readArray ba i
  writeArrayData (AD_Int32 ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Int32 ba) = liftM AD_Int32 $ MArray.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_Int32 ba) = sTUArrayPtr ba

instance ArrayElem Int64 where
  type ArrayPtrs Int64 = Ptr Int64
  indexArrayData (AD_Int64 ba) i = ba IArray.! i
  ptrsOfArrayData (AD_Int64 ba) = uArrayPtr ba
  newArrayData size = liftM AD_Int64 $ MArray.newArray_ (0, size - 1)
  readArrayData (AD_Int64 ba) i = MArray.readArray ba i
  writeArrayData (AD_Int64 ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Int64 ba) = liftM AD_Int64 $ MArray.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_Int64 ba) = sTUArrayPtr ba

instance ArrayElem Word where
  type ArrayPtrs Word = Ptr Word
  indexArrayData (AD_Word ba) i = ba IArray.! i
  ptrsOfArrayData (AD_Word ba) = uArrayPtr ba
  newArrayData size = liftM AD_Word $ MArray.newArray_ (0, size - 1)
  readArrayData (AD_Word ba) i = MArray.readArray ba i
  writeArrayData (AD_Word ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Word ba) = liftM AD_Word $ MArray.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_Word ba) = sTUArrayPtr ba

instance ArrayElem Word8 where
  type ArrayPtrs Word8 = Ptr Word8
  indexArrayData (AD_Word8 ba) i = ba IArray.! i
  ptrsOfArrayData (AD_Word8 ba) = uArrayPtr ba
  newArrayData size = liftM AD_Word8 $ MArray.newArray_ (0, size - 1)
  readArrayData (AD_Word8 ba) i = MArray.readArray ba i
  writeArrayData (AD_Word8 ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Word8 ba) = liftM AD_Word8 $ MArray.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_Word8 ba) = sTUArrayPtr ba

instance ArrayElem Word16 where
  type ArrayPtrs Word16 = Ptr Word16
  indexArrayData (AD_Word16 ba) i = ba IArray.! i
  ptrsOfArrayData (AD_Word16 ba) = uArrayPtr ba
  newArrayData size = liftM AD_Word16 $ MArray.newArray_ (0, size - 1)
  readArrayData (AD_Word16 ba) i = MArray.readArray ba i
  writeArrayData (AD_Word16 ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Word16 ba) 
    = liftM AD_Word16 $ MArray.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_Word16 ba) = sTUArrayPtr ba

instance ArrayElem Word32 where
  type ArrayPtrs Word32 = Ptr Word32
  indexArrayData (AD_Word32 ba) i = ba IArray.! i
  ptrsOfArrayData (AD_Word32 ba) = uArrayPtr ba
  newArrayData size = liftM AD_Word32 $ MArray.newArray_ (0, size - 1)
  readArrayData (AD_Word32 ba) i = MArray.readArray ba i
  writeArrayData (AD_Word32 ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Word32 ba) 
    = liftM AD_Word32 $ MArray.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_Word32 ba) = sTUArrayPtr ba

instance ArrayElem Word64 where
  type ArrayPtrs Word64 = Ptr Word64
  indexArrayData (AD_Word64 ba) i = ba IArray.! i
  ptrsOfArrayData (AD_Word64 ba) = uArrayPtr ba
  newArrayData size = liftM AD_Word64 $ MArray.newArray_ (0, size - 1)
  readArrayData (AD_Word64 ba) i = MArray.readArray ba i
  writeArrayData (AD_Word64 ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Word64 ba) 
    = liftM AD_Word64 $ MArray.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_Word64 ba) = sTUArrayPtr ba
  
-- FIXME:
-- CShort
-- CUShort
-- CInt
-- CUInt
-- CLong
-- CULong
-- CLLong
-- CULLong

instance ArrayElem Float where
  type ArrayPtrs Float = Ptr Float
  indexArrayData (AD_Float ba) i = ba IArray.! i
  ptrsOfArrayData (AD_Float ba) = uArrayPtr ba
  newArrayData size = liftM AD_Float $ MArray.newArray_ (0, size - 1)
  readArrayData (AD_Float ba) i = MArray.readArray ba i
  writeArrayData (AD_Float ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Float ba) = liftM AD_Float $ MArray.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_Float ba) = sTUArrayPtr ba

instance ArrayElem Double where
  type ArrayPtrs Double = Ptr Double
  indexArrayData (AD_Double ba) i = ba IArray.! i
  ptrsOfArrayData (AD_Double ba) = uArrayPtr ba
  newArrayData size = liftM AD_Double $ MArray.newArray_ (0, size - 1)
  readArrayData (AD_Double ba) i = MArray.readArray ba i
  writeArrayData (AD_Double ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Double ba) 
    = liftM AD_Double $ MArray.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_Double ba) = sTUArrayPtr ba

-- FIXME:
-- CFloat
-- CDouble

instance ArrayElem Bool where
  type ArrayPtrs Bool = Ptr Word8
  indexArrayData (AD_Bool ba) i = ba IArray.! i
--  ptrsOfArrayData (AD_Bool ba) = uArrayPtr ba???currently wrong
--    Unboxed Bool arrays are stored as bit vectors, which is unsuitable
--    for parallel processing (might be useful with sequential LLVM code though)
--    - Do we want to represent 'Array sh Bool' differently or do we want to
--      copy the array on marshaling (but then we also have to copy back
--      results of the same type)
  newArrayData size = liftM AD_Bool $ MArray.newArray_ (0, size - 1)
  readArrayData (AD_Bool ba) i = MArray.readArray ba i
  writeArrayData (AD_Bool ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Bool ba) = liftM AD_Bool $ MArray.unsafeFreeze ba
--  ptrsOfMutableArrayData (AD_Bool ba) = sTUArrayPtr ba???
--    see ptrsOfArrayData

instance ArrayElem Char where
--  type ArrayPtrs Char = ???unicode???
  indexArrayData (AD_Char ba) i = ba IArray.! i
--  ptrsOfArrayData (AD_Char ba) = ???
  newArrayData size = liftM AD_Char $ MArray.newArray_ (0, size - 1)
  readArrayData (AD_Char ba) i = MArray.readArray ba i
  writeArrayData (AD_Char ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Char ba) = liftM AD_Char $ MArray.unsafeFreeze ba
--  ptrsOfMutableArrayData (AD_Char ba) = ???

-- FIXME:
-- CChar
-- CSChar
-- CUChar

instance (ArrayElem a, ArrayElem b) => ArrayElem (a, b) where
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

-- |Safe combination of creating and fast freezing of array data.
--
runArrayData :: ArrayElem e
             => (forall s. ST s (MutableArrayData s e, e)) -> (ArrayData e, e)
runArrayData st = runST $ do
                    (mad, r) <- st
                    ad <- unsafeFreezeArrayData mad
                    return (ad, r)


-- Auxilliary functions
-- --------------------

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
    (# s, ba #) -> (# s, Ptr (byteArrayContents# ba) #)
