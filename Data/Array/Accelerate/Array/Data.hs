{-# LANGUAGE GADTs, TypeFamilies, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

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
  ArrayElem(..), ArrayData, MutableArrayData, runArrayData

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
  indexArrayData        :: ArrayData e -> Int -> e
  --
  newArrayData          :: Int -> ST s (MutableArrayData s e)
  readArrayData         :: MutableArrayData s e -> Int      -> ST s e
  writeArrayData        :: MutableArrayData s e -> Int -> e -> ST s ()
  unsafeFreezeArrayData :: MutableArrayData s e -> ST s (ArrayData e)

instance ArrayElem () where
  indexArrayData AD_Unit i = i `seq` ()
  newArrayData size = return AD_Unit
  readArrayData AD_Unit i = i `seq` return ()
  writeArrayData AD_Unit i () = return ()
  unsafeFreezeArrayData AD_Unit = return AD_Unit

instance ArrayElem Int where
  indexArrayData (AD_Int ba) i = ba IArray.! i
  newArrayData size = liftM AD_Int $ MArray.newArray_ (0, size - 1)
  readArrayData (AD_Int ba) i = MArray.readArray ba i
  writeArrayData (AD_Int ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Int ba) = liftM AD_Int $ MArray.unsafeFreeze ba

instance ArrayElem Int8 where
  indexArrayData (AD_Int8 ba) i = ba IArray.! i
  newArrayData size = liftM AD_Int8 $ MArray.newArray_ (0, size - 1)
  readArrayData (AD_Int8 ba) i = MArray.readArray ba i
  writeArrayData (AD_Int8 ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Int8 ba) = liftM AD_Int8 $ MArray.unsafeFreeze ba

instance ArrayElem Int16 where
  indexArrayData (AD_Int16 ba) i = ba IArray.! i
  newArrayData size = liftM AD_Int16 $ MArray.newArray_ (0, size - 1)
  readArrayData (AD_Int16 ba) i = MArray.readArray ba i
  writeArrayData (AD_Int16 ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Int16 ba) = liftM AD_Int16 $ MArray.unsafeFreeze ba

instance ArrayElem Int32 where
  indexArrayData (AD_Int32 ba) i = ba IArray.! i
  newArrayData size = liftM AD_Int32 $ MArray.newArray_ (0, size - 1)
  readArrayData (AD_Int32 ba) i = MArray.readArray ba i
  writeArrayData (AD_Int32 ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Int32 ba) = liftM AD_Int32 $ MArray.unsafeFreeze ba

instance ArrayElem Int64 where
  indexArrayData (AD_Int64 ba) i = ba IArray.! i
  newArrayData size = liftM AD_Int64 $ MArray.newArray_ (0, size - 1)
  readArrayData (AD_Int64 ba) i = MArray.readArray ba i
  writeArrayData (AD_Int64 ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Int64 ba) = liftM AD_Int64 $ MArray.unsafeFreeze ba

instance ArrayElem Word where
  indexArrayData (AD_Word ba) i = ba IArray.! i
  newArrayData size = liftM AD_Word $ MArray.newArray_ (0, size - 1)
  readArrayData (AD_Word ba) i = MArray.readArray ba i
  writeArrayData (AD_Word ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Word ba) = liftM AD_Word $ MArray.unsafeFreeze ba

instance ArrayElem Word8 where
  indexArrayData (AD_Word8 ba) i = ba IArray.! i
  newArrayData size = liftM AD_Word8 $ MArray.newArray_ (0, size - 1)
  readArrayData (AD_Word8 ba) i = MArray.readArray ba i
  writeArrayData (AD_Word8 ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Word8 ba) = liftM AD_Word8 $ MArray.unsafeFreeze ba

instance ArrayElem Word16 where
  indexArrayData (AD_Word16 ba) i = ba IArray.! i
  newArrayData size = liftM AD_Word16 $ MArray.newArray_ (0, size - 1)
  readArrayData (AD_Word16 ba) i = MArray.readArray ba i
  writeArrayData (AD_Word16 ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Word16 ba) 
    = liftM AD_Word16 $ MArray.unsafeFreeze ba

instance ArrayElem Word32 where
  indexArrayData (AD_Word32 ba) i = ba IArray.! i
  newArrayData size = liftM AD_Word32 $ MArray.newArray_ (0, size - 1)
  readArrayData (AD_Word32 ba) i = MArray.readArray ba i
  writeArrayData (AD_Word32 ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Word32 ba) 
    = liftM AD_Word32 $ MArray.unsafeFreeze ba

instance ArrayElem Word64 where
  indexArrayData (AD_Word64 ba) i = ba IArray.! i
  newArrayData size = liftM AD_Word64 $ MArray.newArray_ (0, size - 1)
  readArrayData (AD_Word64 ba) i = MArray.readArray ba i
  writeArrayData (AD_Word64 ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Word64 ba) 
    = liftM AD_Word64 $ MArray.unsafeFreeze ba
  
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
  indexArrayData (AD_Float ba) i = ba IArray.! i
  newArrayData size = liftM AD_Float $ MArray.newArray_ (0, size - 1)
  readArrayData (AD_Float ba) i = MArray.readArray ba i
  writeArrayData (AD_Float ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Float ba) = liftM AD_Float $ MArray.unsafeFreeze ba

instance ArrayElem Double where
  indexArrayData (AD_Double ba) i = ba IArray.! i
  newArrayData size = liftM AD_Double $ MArray.newArray_ (0, size - 1)
  readArrayData (AD_Double ba) i = MArray.readArray ba i
  writeArrayData (AD_Double ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Double ba) 
    = liftM AD_Double $ MArray.unsafeFreeze ba

-- FIXME:
-- CFloat
-- CDouble

instance ArrayElem Bool where
  indexArrayData (AD_Bool ba) i = ba IArray.! i
  newArrayData size = liftM AD_Bool $ MArray.newArray_ (0, size - 1)
  readArrayData (AD_Bool ba) i = MArray.readArray ba i
  writeArrayData (AD_Bool ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Bool ba) = liftM AD_Bool $ MArray.unsafeFreeze ba

instance ArrayElem Char where
  indexArrayData (AD_Char ba) i = ba IArray.! i
  newArrayData size = liftM AD_Char $ MArray.newArray_ (0, size - 1)
  readArrayData (AD_Char ba) i = MArray.readArray ba i
  writeArrayData (AD_Char ba) i e = MArray.writeArray ba i e
  unsafeFreezeArrayData (AD_Char ba) = liftM AD_Char $ MArray.unsafeFreeze ba

-- FIXME:
-- CChar
-- CSChar
-- CUChar

instance (ArrayElem a, ArrayElem b) => ArrayElem (a, b) where
  indexArrayData (AD_Pair a b) i = (indexArrayData a i, indexArrayData b i)
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

-- |Safe combination of creating and fast freezing of array data.
--
runArrayData :: ArrayElem e
             => (forall s. ST s (MutableArrayData s e, e)) -> (ArrayData e, e)
runArrayData st = runST $ do
                    (mad, r) <- st
                    ad <- unsafeFreezeArrayData mad
                    return (ad, r)
