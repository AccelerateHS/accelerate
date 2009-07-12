{-# LANGUAGE GADTs, TypeFamilies, FlexibleContexts, FlexibleInstances #-}

-- |Embedded array processing language: array data layout for linear arrays
--
--  Copyright (c) 2009 Manuel M T Chakravarty, Gabriele Keller, Sean Lee
--
--  License: BSD3
--
--- Description ---------------------------------------------------------------
--

module Data.Array.Accelerate.Array.Data (

  -- * Array operations and representations
  ArrayElem, ArrayData, MutableArrayData

) where

-- standard libraries
import Control.Monad
import Control.Monad.ST
import qualified Data.Array.IArray  as IArray
import qualified Data.Array.MArray  as MArray
import Data.Array.ST      (STUArray)
import Data.Array.Unboxed (UArray)

-- friends
import Data.Array.Accelerate.Type


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
data instance GArrayData ba CShort  = AD_CShort  (ba CShort)
data instance GArrayData ba CUShort = AD_CUShort (ba CUShort)
data instance GArrayData ba CInt    = AD_CInt    (ba CInt)
data instance GArrayData ba CUInt   = AD_CUInt   (ba CUInt)
data instance GArrayData ba CLong   = AD_CLong   (ba CLong)
data instance GArrayData ba CULong  = AD_CULong  (ba CULong)
data instance GArrayData ba CLLong  = AD_CLLong  (ba CLLong)
data instance GArrayData ba CULLong = AD_CULLong (ba CULLong)
data instance GArrayData ba Float   = AD_Float   (ba Float)
data instance GArrayData ba Double  = AD_Double  (ba Double)
data instance GArrayData ba CFloat  = AD_CFloat  (ba CFloat)
data instance GArrayData ba CDouble = AD_CDouble (ba CDouble)
data instance GArrayData ba Bool    = AD_Bool    (ba Bool)
data instance GArrayData ba Char    = AD_Char    (ba Char)
data instance GArrayData ba CChar   = AD_CChar   (ba CChar)
data instance GArrayData ba CSChar  = AD_CSChar  (ba CSChar)
data instance GArrayData ba CUChar  = AD_CUChar  (ba CUChar)
data instance GArrayData ba (a, b)  = AD_Pair (GArrayData ba a) 
                                              (GArrayData ba b)

class ArrayElem e where
  indexArray :: ArrayData e -> Int -> e
  --
  newArray   :: Int                              -> ST s (MutableArrayData s e)
  readArray  :: MutableArrayData s e -> Int      -> ST s e
  writeArray :: MutableArrayData s e -> Int -> e -> ST s ()

instance ArrayElem () where
  indexArray AD_Unit i = i `seq` ()
  newArray size = return AD_Unit
  readArray AD_Unit i =  i `seq` return ()
  writeArray AD_Unit i () =  return ()

instance ArrayElem Int where
  indexArray (AD_Int ba) i = ba IArray.! i
  newArray size = liftM AD_Int $ MArray.newArray_ (0, size - 1)
  readArray (AD_Int ba) i =  MArray.readArray ba i
  writeArray (AD_Int ba) i e = MArray.writeArray ba i e

instance ArrayElem Int8 where
  indexArray (AD_Int8 ba) i = ba IArray.! i
  newArray size = liftM AD_Int8 $ MArray.newArray_ (0, size - 1)
  readArray (AD_Int8 ba) i =  MArray.readArray ba i
  writeArray (AD_Int8 ba) i e = MArray.writeArray ba i e

instance ArrayElem Int16 where
  indexArray (AD_Int16 ba) i = ba IArray.! i
  newArray size = liftM AD_Int16 $ MArray.newArray_ (0, size - 1)
  readArray (AD_Int16 ba) i =  MArray.readArray ba i
  writeArray (AD_Int16 ba) i e = MArray.writeArray ba i e

instance ArrayElem Int32 where
  indexArray (AD_Int32 ba) i = ba IArray.! i
  newArray size = liftM AD_Int32 $ MArray.newArray_ (0, size - 1)
  readArray (AD_Int32 ba) i =  MArray.readArray ba i
  writeArray (AD_Int32 ba) i e = MArray.writeArray ba i e

instance ArrayElem Int64 where
  indexArray (AD_Int64 ba) i = ba IArray.! i
  newArray size = liftM AD_Int64 $ MArray.newArray_ (0, size - 1)
  readArray (AD_Int64 ba) i =  MArray.readArray ba i
  writeArray (AD_Int64 ba) i e = MArray.writeArray ba i e

instance ArrayElem Word where
  indexArray (AD_Word ba) i = ba IArray.! i
  newArray size = liftM AD_Word $ MArray.newArray_ (0, size - 1)
  readArray (AD_Word ba) i =  MArray.readArray ba i
  writeArray (AD_Word ba) i e = MArray.writeArray ba i e

instance ArrayElem Word8 where
  indexArray (AD_Word8 ba) i = ba IArray.! i
  newArray size = liftM AD_Word8 $ MArray.newArray_ (0, size - 1)
  readArray (AD_Word8 ba) i =  MArray.readArray ba i
  writeArray (AD_Word8 ba) i e = MArray.writeArray ba i e

instance ArrayElem Word16 where
  indexArray (AD_Word16 ba) i = ba IArray.! i
  newArray size = liftM AD_Word16 $ MArray.newArray_ (0, size - 1)
  readArray (AD_Word16 ba) i =  MArray.readArray ba i
  writeArray (AD_Word16 ba) i e = MArray.writeArray ba i e

instance ArrayElem Word32 where
  indexArray (AD_Word32 ba) i = ba IArray.! i
  newArray size = liftM AD_Word32 $ MArray.newArray_ (0, size - 1)
  readArray (AD_Word32 ba) i =  MArray.readArray ba i
  writeArray (AD_Word32 ba) i e = MArray.writeArray ba i e

instance ArrayElem Word64 where
  indexArray (AD_Word64 ba) i = ba IArray.! i
  newArray size = liftM AD_Word64 $ MArray.newArray_ (0, size - 1)
  readArray (AD_Word64 ba) i =  MArray.readArray ba i
  writeArray (AD_Word64 ba) i e = MArray.writeArray ba i e

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
  indexArray (AD_Float ba) i = ba IArray.! i
  newArray size = liftM AD_Float $ MArray.newArray_ (0, size - 1)
  readArray (AD_Float ba) i =  MArray.readArray ba i
  writeArray (AD_Float ba) i e = MArray.writeArray ba i e

instance ArrayElem Double where
  indexArray (AD_Double ba) i = ba IArray.! i
  newArray size = liftM AD_Double $ MArray.newArray_ (0, size - 1)
  readArray (AD_Double ba) i =  MArray.readArray ba i
  writeArray (AD_Double ba) i e = MArray.writeArray ba i e

-- FIXME:
-- CFloat
-- CDouble

instance ArrayElem Bool where
  indexArray (AD_Bool ba) i = ba IArray.! i
  newArray size = liftM AD_Bool $ MArray.newArray_ (0, size - 1)
  readArray (AD_Bool ba) i =  MArray.readArray ba i
  writeArray (AD_Bool ba) i e = MArray.writeArray ba i e

instance ArrayElem Char where
  indexArray (AD_Char ba) i = ba IArray.! i
  newArray size = liftM AD_Char $ MArray.newArray_ (0, size - 1)
  readArray (AD_Char ba) i =  MArray.readArray ba i
  writeArray (AD_Char ba) i e = MArray.writeArray ba i e

-- FIXME:
-- CChar
-- CSChar
-- CUChar

instance (ArrayElem a, ArrayElem b) => ArrayElem (a, b) where
  indexArray (AD_Pair a b) i = (indexArray a i, indexArray b i)
  newArray size 
    = do 
        a <- newArray size
        b <- newArray size
        return $ AD_Pair a b
  readArray (AD_Pair a b) i 
    = do
        x <- readArray a i
        y <- readArray b i
        return (x, y)
  writeArray (AD_Pair a b) i (x, y)
    = do
        writeArray a i x
        writeArray b i y
