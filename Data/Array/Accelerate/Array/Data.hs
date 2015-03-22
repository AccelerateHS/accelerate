{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UnboxedTuples       #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-orphans         #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Array.Data
-- Copyright   : [2008..2014] Manuel M T Chakravarty, Gabriele Keller
--               [2008..2009] Sean Lee
--               [2009..2014] Trevor L. McDonell
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
  fstArrayData, sndArrayData, pairArrayData,

  -- * Type macros
  HTYPE_INT, HTYPE_WORD, HTYPE_LONG, HTYPE_UNSIGNED_LONG, HTYPE_CCHAR,

  -- * Unique arrays
  UniqueArray, storableFromUnique, uniqueFromStorable, getUniqueId

) where

-- standard libraries
import Foreign            (Ptr)
import Foreign.C.Types
import Data.Bits
import Data.Functor       ((<$>))
import Data.Typeable      (Typeable)
import Control.Concurrent.MVar ( MVar, newMVar, modifyMVar )
import Control.Monad
#ifdef ACCELERATE_UNSAFE_CHECKS
import qualified Data.Array.Base    as MArray (readArray, writeArray)
#else
import qualified Data.Array.Base    as MArray (unsafeRead, unsafeWrite)
#endif
import Data.Array.Storable.Internals
import Foreign.ForeignPtr.Unsafe
import Foreign.Storable
import System.IO.Unsafe
import Data.Array.MArray  (MArray(..))
import Data.Array.Base    (unsafeNewArray_)
import Language.Haskell.TH

-- friends
import Data.Array.Accelerate.Type

-- Add needed Typeable instance for StorableArray
--
deriving instance Typeable StorableArray

-- Determine the underlying type of a Haskell CLong or CULong.
--
$( runQ [d| type HTYPE_INT = $(
              case finiteBitSize (undefined::Int) of
                32 -> [t| Int32 |]
                64 -> [t| Int64 |]
                _  -> error "I don't know what architecture I am" ) |] )

$( runQ [d| type HTYPE_WORD = $(
              case finiteBitSize (undefined::Word) of
                32 -> [t| Word32 |]
                64 -> [t| Word64 |]
                _  -> error "I don't know what architecture I am" ) |] )

$( runQ [d| type HTYPE_LONG = $(
              case finiteBitSize (undefined::CLong) of
                32 -> [t| Int32 |]
                64 -> [t| Int64 |]
                _  -> error "I don't know what architecture I am" ) |] )

$( runQ [d| type HTYPE_UNSIGNED_LONG = $(
              case finiteBitSize (undefined::CULong) of
                32 -> [t| Word32 |]
                64 -> [t| Word64 |]
                _  -> error "I don't know what architecture I am" ) |] )

$( runQ [d| type HTYPE_CCHAR = $(
              case isSigned (undefined::CChar) of
                True  -> [t| Int8  |]
                False -> [t| Word8 |] ) |] )

-- Unique arrays
-- -------------

-- |A Uniquely identifiable array.
--
-- For the purposes of memory management, we use arrays as keys in a table. For
-- this reason we need a way to uniquely identify each array we create. We do
-- this by attaching an Int to each array, the value of which we get from a
-- global counter that we increment for every array construction.
--
data UniqueArray i e = UniqueArray !Int !(StorableArray i e)

-- |Create a unique array from a storable array
--
{-# INLINE uniqueFromStorable #-}
uniqueFromStorable :: StorableArray i a -> IO (UniqueArray i a)
uniqueFromStorable sa = do
  i <- modifyMVar counter (\n -> return (n+1,n))
  return $ UniqueArray i sa

-- |Get the storable array backing the unique array
--
{-# INLINE storableFromUnique #-}
storableFromUnique :: UniqueArray i a -> StorableArray i a
storableFromUnique (UniqueArray _ sa) = sa

-- |Get the unique identifier associated with the unique array
--
{-# INLINE getUniqueId #-}
getUniqueId :: UniqueArray i a -> IO Int
getUniqueId (UniqueArray n _) = return n

instance Storable e => MArray UniqueArray e IO where
  getBounds (UniqueArray _ sa) = getBounds sa

  newArray lu i = uniqueFromStorable =<< newArray lu i

  unsafeNewArray_ lu = uniqueFromStorable =<< unsafeNewArray_ lu

  newArray_ = unsafeNewArray_

  unsafeRead (UniqueArray _ sa) = MArray.unsafeRead sa

  unsafeWrite (UniqueArray _ sa) = MArray.unsafeWrite sa

-- Array representation
-- --------------------

-- |Immutable array representation
--
type ArrayData e = MutableArrayData e

-- |Mutable array representation
--
type MutableArrayData e = GArrayData (UniqueArray Int) e

-- Array representation in dependence on the element type, but abstracting
-- over the basic array type (in particular, abstracting over mutability)
--
data family GArrayData :: (* -> *) -> * -> *
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
data instance GArrayData ba CShort  = AD_CShort  (ba Int16)
data instance GArrayData ba CUShort = AD_CUShort (ba Word16)
data instance GArrayData ba CInt    = AD_CInt    (ba Int32)
data instance GArrayData ba CUInt   = AD_CUInt   (ba Word32)
data instance GArrayData ba CLong   = AD_CLong   (ba HTYPE_LONG)
data instance GArrayData ba CULong  = AD_CULong  (ba HTYPE_UNSIGNED_LONG)
data instance GArrayData ba CLLong  = AD_CLLong  (ba Int64)
data instance GArrayData ba CULLong = AD_CULLong (ba Word64)
data instance GArrayData ba Float   = AD_Float   (ba Float)
data instance GArrayData ba Double  = AD_Double  (ba Double)
data instance GArrayData ba CFloat  = AD_CFloat  (ba Float)
data instance GArrayData ba CDouble = AD_CDouble (ba Double)
data instance GArrayData ba Bool    = AD_Bool    (ba Word8)
data instance GArrayData ba Char    = AD_Char    (ba Char)
data instance GArrayData ba CChar   = AD_CChar   (ba HTYPE_CCHAR)
data instance GArrayData ba CSChar  = AD_CSChar  (ba Int8)
data instance GArrayData ba CUChar  = AD_CUChar  (ba Word8)
data instance GArrayData ba (a, b)  = AD_Pair (GArrayData ba a)
                                              (GArrayData ba b)

deriving instance Typeable GArrayData


-- | GADT to reify the 'ArrayElt' class.
--
data ArrayEltR a where
  ArrayEltRunit    :: ArrayEltR ()
  ArrayEltRint     :: ArrayEltR Int
  ArrayEltRint8    :: ArrayEltR Int8
  ArrayEltRint16   :: ArrayEltR Int16
  ArrayEltRint32   :: ArrayEltR Int32
  ArrayEltRint64   :: ArrayEltR Int64
  ArrayEltRword    :: ArrayEltR Word
  ArrayEltRword8   :: ArrayEltR Word8
  ArrayEltRword16  :: ArrayEltR Word16
  ArrayEltRword32  :: ArrayEltR Word32
  ArrayEltRword64  :: ArrayEltR Word64
  ArrayEltRcshort  :: ArrayEltR CShort
  ArrayEltRcushort :: ArrayEltR CUShort
  ArrayEltRcint    :: ArrayEltR CInt
  ArrayEltRcuint   :: ArrayEltR CUInt
  ArrayEltRclong   :: ArrayEltR CLong
  ArrayEltRculong  :: ArrayEltR CULong
  ArrayEltRcllong  :: ArrayEltR CLLong
  ArrayEltRcullong :: ArrayEltR CULLong
  ArrayEltRfloat   :: ArrayEltR Float
  ArrayEltRdouble  :: ArrayEltR Double
  ArrayEltRcfloat  :: ArrayEltR CFloat
  ArrayEltRcdouble :: ArrayEltR CDouble
  ArrayEltRbool    :: ArrayEltR Bool
  ArrayEltRchar    :: ArrayEltR Char
  ArrayEltRcchar   :: ArrayEltR CChar
  ArrayEltRcschar  :: ArrayEltR CSChar
  ArrayEltRcuchar  :: ArrayEltR CUChar
  ArrayEltRpair    :: (ArrayElt a, ArrayElt b)
                   => ArrayEltR a -> ArrayEltR b -> ArrayEltR (a,b)

-- Array operations
-- ----------------
--
-- TLM: do we need to INLINE these functions to get good performance interfacing
--      to external libraries, especially Repa?

class ArrayElt e where
  type ArrayPtrs e
  --
  unsafeIndexArrayData   :: ArrayData e -> Int -> e
  ptrsOfArrayData        :: ArrayData e -> ArrayPtrs e
  touchArrayData         :: ArrayData e -> IO ()
  --
  newArrayData           :: Int -> IO (MutableArrayData e)
  unsafeReadArrayData    :: MutableArrayData e -> Int      -> IO e
  unsafeWriteArrayData   :: MutableArrayData e -> Int -> e -> IO ()
  unsafeFreezeArrayData  :: MutableArrayData e -> IO (ArrayData e)
  unsafeFreezeArrayData  = return
  ptrsOfMutableArrayData :: MutableArrayData e -> IO (ArrayPtrs e)
  ptrsOfMutableArrayData = return . ptrsOfArrayData
  --
  arrayElt               :: ArrayEltR e

instance ArrayElt () where
  type ArrayPtrs () = ()
  unsafeIndexArrayData AD_Unit i    = i `seq` ()
  ptrsOfArrayData AD_Unit           = ()
  touchArrayData AD_Unit            = return ()
  newArrayData size                 = size `seq` return AD_Unit
  unsafeReadArrayData AD_Unit i     = i `seq` return ()
  unsafeWriteArrayData AD_Unit i () = i `seq` return ()
  arrayElt                          = ArrayEltRunit

instance ArrayElt Int where
  type ArrayPtrs Int = Ptr Int
  unsafeIndexArrayData (AD_Int ba) i   = unsafeIndexArray ba i
  ptrsOfArrayData (AD_Int ba)          = uniqueArrayPtr ba
  touchArrayData (AD_Int ba)           = touchUniqueArray ba
  newArrayData size                    = liftM AD_Int $ unsafeNewArray_ (0,size-1)
  unsafeReadArrayData (AD_Int ba) i    = unsafeReadArray ba i
  unsafeWriteArrayData (AD_Int ba) i e = unsafeWriteArray ba i e
  arrayElt                             = ArrayEltRint

instance ArrayElt Int8 where
  type ArrayPtrs Int8 = Ptr Int8
  unsafeIndexArrayData (AD_Int8 ba) i   = unsafeIndexArray ba i
  ptrsOfArrayData (AD_Int8 ba)          = uniqueArrayPtr ba
  touchArrayData (AD_Int8 ba)           = touchUniqueArray ba
  newArrayData size                     = liftM AD_Int8 $ unsafeNewArray_ (0,size-1)
  unsafeReadArrayData (AD_Int8 ba) i    = unsafeReadArray ba i
  unsafeWriteArrayData (AD_Int8 ba) i e = unsafeWriteArray ba i e
  arrayElt                              = ArrayEltRint8

instance ArrayElt Int16 where
  type ArrayPtrs Int16 = Ptr Int16
  unsafeIndexArrayData (AD_Int16 ba) i   = unsafeIndexArray ba i
  ptrsOfArrayData (AD_Int16 ba)          = uniqueArrayPtr ba
  touchArrayData (AD_Int16 ba)           = touchUniqueArray ba
  newArrayData size                      = liftM AD_Int16 $ unsafeNewArray_ (0,size-1)
  unsafeReadArrayData (AD_Int16 ba) i    = unsafeReadArray ba i
  unsafeWriteArrayData (AD_Int16 ba) i e = unsafeWriteArray ba i e
  arrayElt                               = ArrayEltRint16

instance ArrayElt Int32 where
  type ArrayPtrs Int32 = Ptr Int32
  unsafeIndexArrayData (AD_Int32 ba) i   = unsafeIndexArray ba i
  ptrsOfArrayData (AD_Int32 ba)          = uniqueArrayPtr ba
  touchArrayData (AD_Int32 ba)           = touchUniqueArray ba
  newArrayData size                      = liftM AD_Int32 $ unsafeNewArray_ (0,size-1)
  unsafeReadArrayData (AD_Int32 ba) i    = unsafeReadArray ba i
  unsafeWriteArrayData (AD_Int32 ba) i e = unsafeWriteArray ba i e
  arrayElt                               = ArrayEltRint32

instance ArrayElt Int64 where
  type ArrayPtrs Int64 = Ptr Int64
  unsafeIndexArrayData (AD_Int64 ba) i   = unsafeIndexArray ba i
  ptrsOfArrayData (AD_Int64 ba)          = uniqueArrayPtr ba
  touchArrayData (AD_Int64 ba)           = touchUniqueArray ba
  newArrayData size                      = liftM AD_Int64 $ unsafeNewArray_ (0,size-1)
  unsafeReadArrayData (AD_Int64 ba) i    = unsafeReadArray ba i
  unsafeWriteArrayData (AD_Int64 ba) i e = unsafeWriteArray ba i e
  arrayElt                               = ArrayEltRint64

instance ArrayElt Word where
  type ArrayPtrs Word = Ptr Word
  unsafeIndexArrayData (AD_Word ba) i   = unsafeIndexArray ba i
  ptrsOfArrayData (AD_Word ba)          = uniqueArrayPtr ba
  touchArrayData (AD_Word ba)           = touchUniqueArray ba
  newArrayData size                     = liftM AD_Word $ unsafeNewArray_ (0,size-1)
  unsafeReadArrayData (AD_Word ba) i    = unsafeReadArray ba i
  unsafeWriteArrayData (AD_Word ba) i e = unsafeWriteArray ba i e
  arrayElt                              = ArrayEltRword

instance ArrayElt Word8 where
  type ArrayPtrs Word8 = Ptr Word8
  unsafeIndexArrayData (AD_Word8 ba) i   = unsafeIndexArray ba i
  ptrsOfArrayData (AD_Word8 ba)          = uniqueArrayPtr ba
  touchArrayData (AD_Word8 ba)           = touchUniqueArray ba
  newArrayData size                      = liftM AD_Word8 $ unsafeNewArray_ (0,size-1)
  unsafeReadArrayData (AD_Word8 ba) i    = unsafeReadArray ba i
  unsafeWriteArrayData (AD_Word8 ba) i e = unsafeWriteArray ba i e
  arrayElt                               = ArrayEltRword8

instance ArrayElt Word16 where
  type ArrayPtrs Word16 = Ptr Word16
  unsafeIndexArrayData (AD_Word16 ba) i   = unsafeIndexArray ba i
  ptrsOfArrayData (AD_Word16 ba)          = uniqueArrayPtr ba
  touchArrayData (AD_Word16 ba)           = touchUniqueArray ba
  newArrayData size                       = liftM AD_Word16 $ unsafeNewArray_ (0,size-1)
  unsafeReadArrayData (AD_Word16 ba) i    = unsafeReadArray ba i
  unsafeWriteArrayData (AD_Word16 ba) i e = unsafeWriteArray ba i e
  arrayElt                                = ArrayEltRword16

instance ArrayElt Word32 where
  type ArrayPtrs Word32 = Ptr Word32
  unsafeIndexArrayData (AD_Word32 ba) i   = unsafeIndexArray ba i
  ptrsOfArrayData (AD_Word32 ba)          = uniqueArrayPtr ba
  touchArrayData (AD_Word32 ba)           = touchUniqueArray ba
  newArrayData size                       = liftM AD_Word32 $ unsafeNewArray_ (0,size-1)
  unsafeReadArrayData (AD_Word32 ba) i    = unsafeReadArray ba i
  unsafeWriteArrayData (AD_Word32 ba) i e = unsafeWriteArray ba i e
  arrayElt                                = ArrayEltRword32

instance ArrayElt Word64 where
  type ArrayPtrs Word64 = Ptr Word64
  unsafeIndexArrayData (AD_Word64 ba) i   = unsafeIndexArray ba i
  ptrsOfArrayData (AD_Word64 ba)          = uniqueArrayPtr ba
  touchArrayData (AD_Word64 ba)           = touchUniqueArray ba
  newArrayData size                       = liftM AD_Word64 $ unsafeNewArray_ (0,size-1)
  unsafeReadArrayData (AD_Word64 ba) i    = unsafeReadArray ba i
  unsafeWriteArrayData (AD_Word64 ba) i e = unsafeWriteArray ba i e
  arrayElt                                = ArrayEltRword64

instance ArrayElt CShort where
  type ArrayPtrs CShort = Ptr Int16
  unsafeIndexArrayData (AD_CShort ba) i   = CShort $ unsafeIndexArray ba i
  ptrsOfArrayData (AD_CShort ba)          = uniqueArrayPtr ba
  touchArrayData (AD_CShort ba)           = touchUniqueArray ba
  newArrayData size                       = liftM AD_CShort $ unsafeNewArray_ (0,size-1)
  unsafeReadArrayData (AD_CShort ba) i    = CShort <$> unsafeReadArray ba i
  unsafeWriteArrayData (AD_CShort ba) i (CShort e)
    = unsafeWriteArray ba i e
  arrayElt                                = ArrayEltRcshort

instance ArrayElt CUShort where
  type ArrayPtrs CUShort = Ptr Word16
  unsafeIndexArrayData (AD_CUShort ba) i   = CUShort $ unsafeIndexArray ba i
  ptrsOfArrayData (AD_CUShort ba)          = uniqueArrayPtr ba
  touchArrayData (AD_CUShort ba)           = touchUniqueArray ba
  newArrayData size                        = liftM AD_CUShort $ unsafeNewArray_ (0,size-1)
  unsafeReadArrayData (AD_CUShort ba) i    = CUShort <$> unsafeReadArray ba i
  unsafeWriteArrayData (AD_CUShort ba) i (CUShort e)
    = unsafeWriteArray ba i e
  arrayElt                                 = ArrayEltRcushort

instance ArrayElt CInt where
  type ArrayPtrs CInt = Ptr Int32
  unsafeIndexArrayData (AD_CInt ba) i   = CInt $ unsafeIndexArray ba i
  ptrsOfArrayData (AD_CInt ba)          = uniqueArrayPtr ba
  touchArrayData (AD_CInt ba)           = touchUniqueArray ba
  newArrayData size                     = liftM AD_CInt $ unsafeNewArray_ (0,size-1)
  unsafeReadArrayData (AD_CInt ba) i    = CInt <$> unsafeReadArray ba i
  unsafeWriteArrayData (AD_CInt ba) i (CInt e)
    = unsafeWriteArray ba i e
  arrayElt                              = ArrayEltRcint

instance ArrayElt CUInt where
  type ArrayPtrs CUInt = Ptr Word32
  unsafeIndexArrayData (AD_CUInt ba) i   = CUInt $ unsafeIndexArray ba i
  ptrsOfArrayData (AD_CUInt ba)          = uniqueArrayPtr ba
  touchArrayData (AD_CUInt ba)           = touchUniqueArray ba
  newArrayData size                      = liftM AD_CUInt $ unsafeNewArray_ (0,size-1)
  unsafeReadArrayData (AD_CUInt ba) i    = CUInt <$> unsafeReadArray ba i
  unsafeWriteArrayData (AD_CUInt ba) i (CUInt e)
    = unsafeWriteArray ba i e
  arrayElt                               = ArrayEltRcuint

instance ArrayElt CLong where
  type ArrayPtrs CLong = Ptr HTYPE_LONG
  unsafeIndexArrayData (AD_CLong ba) i   = CLong $ unsafeIndexArray ba i
  ptrsOfArrayData (AD_CLong ba)          = uniqueArrayPtr ba
  touchArrayData (AD_CLong ba)           = touchUniqueArray ba
  newArrayData size                      = liftM AD_CLong $ unsafeNewArray_ (0,size-1)
  unsafeReadArrayData (AD_CLong ba) i    = CLong <$> unsafeReadArray ba i
  unsafeWriteArrayData (AD_CLong ba) i (CLong e)
    = unsafeWriteArray ba i e
  arrayElt                               = ArrayEltRclong

instance ArrayElt CULong where
  type ArrayPtrs CULong = Ptr HTYPE_UNSIGNED_LONG
  unsafeIndexArrayData (AD_CULong ba) i   = CULong $ unsafeIndexArray ba i
  ptrsOfArrayData (AD_CULong ba)          = uniqueArrayPtr ba
  touchArrayData (AD_CULong ba)           = touchUniqueArray ba
  newArrayData size                       = liftM AD_CULong $ unsafeNewArray_ (0,size-1)
  unsafeReadArrayData (AD_CULong ba) i    = CULong <$> unsafeReadArray ba i
  unsafeWriteArrayData (AD_CULong ba) i (CULong e)
    = unsafeWriteArray ba i e
  arrayElt                                = ArrayEltRculong

instance ArrayElt CLLong where
  type ArrayPtrs CLLong = Ptr Int64
  unsafeIndexArrayData (AD_CLLong ba) i   = CLLong $ unsafeIndexArray ba i
  ptrsOfArrayData (AD_CLLong ba)          = uniqueArrayPtr ba
  touchArrayData (AD_CLLong ba)           = touchUniqueArray ba
  newArrayData size                       = liftM AD_CLLong $ unsafeNewArray_ (0,size-1)
  unsafeReadArrayData (AD_CLLong ba) i    = CLLong <$> unsafeReadArray ba i
  unsafeWriteArrayData (AD_CLLong ba) i (CLLong e)
    = unsafeWriteArray ba i e
  arrayElt                                = ArrayEltRcllong

instance ArrayElt CULLong where
  type ArrayPtrs CULLong = Ptr Word64
  unsafeIndexArrayData (AD_CULLong ba) i   = CULLong $ unsafeIndexArray ba i
  ptrsOfArrayData (AD_CULLong ba)          = uniqueArrayPtr ba
  touchArrayData (AD_CULLong ba)           = touchUniqueArray ba
  newArrayData size                        = liftM AD_CULLong $ unsafeNewArray_ (0,size-1)
  unsafeReadArrayData (AD_CULLong ba) i    = CULLong <$> unsafeReadArray ba i
  unsafeWriteArrayData (AD_CULLong ba) i (CULLong e)
    = unsafeWriteArray ba i e
  arrayElt                                 = ArrayEltRcullong

instance ArrayElt Float where
  type ArrayPtrs Float = Ptr Float
  unsafeIndexArrayData (AD_Float ba) i   = unsafeIndexArray ba i
  ptrsOfArrayData (AD_Float ba)          = uniqueArrayPtr ba
  touchArrayData (AD_Float ba)           = touchUniqueArray ba
  newArrayData size                      = liftM AD_Float $ unsafeNewArray_ (0,size-1)
  unsafeReadArrayData (AD_Float ba) i    = unsafeReadArray ba i
  unsafeWriteArrayData (AD_Float ba) i e = unsafeWriteArray ba i e
  arrayElt                               = ArrayEltRfloat

instance ArrayElt Double where
  type ArrayPtrs Double = Ptr Double
  unsafeIndexArrayData (AD_Double ba) i   = unsafeIndexArray ba i
  ptrsOfArrayData (AD_Double ba)          = uniqueArrayPtr ba
  touchArrayData (AD_Double ba)           = touchUniqueArray ba
  newArrayData size                       = liftM AD_Double $ unsafeNewArray_ (0,size-1)
  unsafeReadArrayData (AD_Double ba) i    = unsafeReadArray ba i
  unsafeWriteArrayData (AD_Double ba) i e = unsafeWriteArray ba i e
  arrayElt                                = ArrayEltRdouble

instance ArrayElt CFloat where
  type ArrayPtrs CFloat = Ptr Float
  unsafeIndexArrayData (AD_CFloat ba) i   = CFloat $ unsafeIndexArray ba i
  ptrsOfArrayData (AD_CFloat ba)          = uniqueArrayPtr ba
  touchArrayData (AD_CFloat ba)           = touchUniqueArray ba
  newArrayData size                       = liftM AD_CFloat $ unsafeNewArray_ (0,size-1)
  unsafeReadArrayData (AD_CFloat ba) i    = CFloat <$> unsafeReadArray ba i
  unsafeWriteArrayData (AD_CFloat ba) i (CFloat e)
    = unsafeWriteArray ba i e
  arrayElt                                = ArrayEltRcfloat

instance ArrayElt CDouble where
  type ArrayPtrs CDouble = Ptr Double
  unsafeIndexArrayData (AD_CDouble ba) i   = CDouble $ unsafeIndexArray ba i
  ptrsOfArrayData (AD_CDouble ba)          = uniqueArrayPtr ba
  touchArrayData (AD_CDouble ba)           = touchUniqueArray ba
  newArrayData size                        = liftM AD_CDouble $ unsafeNewArray_ (0,size-1)
  unsafeReadArrayData (AD_CDouble ba) i    = CDouble <$> unsafeReadArray ba i
  unsafeWriteArrayData (AD_CDouble ba) i (CDouble e)
    = unsafeWriteArray ba i e
  arrayElt                                 = ArrayEltRcdouble

-- Bool arrays are stored as arrays of bytes. While this is memory inefficient,
-- it is better suited to parallel backends than the native Unboxed Bool
-- array representation that uses packed bit vectors, as that would require
-- atomic operations when writing data necessarily serialising threads.
--
instance ArrayElt Bool where
  type ArrayPtrs Bool = Ptr Word8
  unsafeIndexArrayData (AD_Bool ba) i   = toBool (unsafeIndexArray ba i)
  ptrsOfArrayData (AD_Bool ba)          = uniqueArrayPtr ba
  touchArrayData (AD_Bool ba)           = touchUniqueArray ba
  newArrayData size                     = liftM AD_Bool $ unsafeNewArray_ (0,size-1)
  unsafeReadArrayData (AD_Bool ba) i    = liftM toBool  $ unsafeReadArray ba i
  unsafeWriteArrayData (AD_Bool ba) i e = unsafeWriteArray ba i (fromBool e)
  arrayElt                              = ArrayEltRbool

{-# INLINE toBool #-}
toBool :: Word8 -> Bool
toBool 0 = False
toBool _ = True

{-# INLINE fromBool #-}
fromBool :: Bool -> Word8
fromBool True  = 1
fromBool False = 0


-- Unboxed Char is stored as a wide character, which is 4-bytes
--
instance ArrayElt Char where
  type ArrayPtrs Char = Ptr Char
  unsafeIndexArrayData (AD_Char ba) i   = unsafeIndexArray ba i
  ptrsOfArrayData (AD_Char ba)          = uniqueArrayPtr ba
  touchArrayData (AD_Char ba)           = touchUniqueArray ba
  newArrayData size                     = liftM AD_Char $ unsafeNewArray_ (0,size-1)
  unsafeReadArrayData (AD_Char ba) i    = unsafeReadArray ba i
  unsafeWriteArrayData (AD_Char ba) i e = unsafeWriteArray ba i e
  arrayElt                              = ArrayEltRchar

instance ArrayElt CChar where
  type ArrayPtrs CChar = Ptr HTYPE_CCHAR
  unsafeIndexArrayData (AD_CChar ba) i   = CChar $ unsafeIndexArray ba i
  ptrsOfArrayData (AD_CChar ba)          = uniqueArrayPtr ba
  touchArrayData (AD_CChar ba)           = touchUniqueArray ba
  newArrayData size                      = liftM AD_CChar $ unsafeNewArray_ (0,size-1)
  unsafeReadArrayData (AD_CChar ba) i    = CChar <$> unsafeReadArray ba i
  unsafeWriteArrayData (AD_CChar ba) i (CChar e)
    = unsafeWriteArray ba i e
  arrayElt                               = ArrayEltRcchar

instance ArrayElt CSChar where
  type ArrayPtrs CSChar = Ptr Int8
  unsafeIndexArrayData (AD_CSChar ba) i   = CSChar $ unsafeIndexArray ba i
  ptrsOfArrayData (AD_CSChar ba)          = uniqueArrayPtr ba
  touchArrayData (AD_CSChar ba)           = touchUniqueArray ba
  newArrayData size                       = liftM AD_CSChar $ unsafeNewArray_ (0,size-1)
  unsafeReadArrayData (AD_CSChar ba) i    = CSChar <$> unsafeReadArray ba i
  unsafeWriteArrayData (AD_CSChar ba) i (CSChar e)
    = unsafeWriteArray ba i e
  arrayElt                                = ArrayEltRcschar

instance ArrayElt CUChar where
  type ArrayPtrs CUChar = Ptr Word8
  unsafeIndexArrayData (AD_CUChar ba) i   = CUChar $ unsafeIndexArray ba i
  ptrsOfArrayData (AD_CUChar ba)          = uniqueArrayPtr ba
  touchArrayData (AD_CUChar ba)           = touchUniqueArray ba
  newArrayData size                       = liftM AD_CUChar $ unsafeNewArray_ (0,size-1)
  unsafeReadArrayData (AD_CUChar ba) i    = CUChar <$> unsafeReadArray ba i
  unsafeWriteArrayData (AD_CUChar ba) i (CUChar e)
    = unsafeWriteArray ba i e
  arrayElt                                = ArrayEltRcuchar

instance (ArrayElt a, ArrayElt b) => ArrayElt (a, b) where
  type ArrayPtrs (a, b)                = (ArrayPtrs a, ArrayPtrs b)
  unsafeIndexArrayData (AD_Pair a b) i = (unsafeIndexArrayData a i, unsafeIndexArrayData b i)
  ptrsOfArrayData (AD_Pair a b)        = (ptrsOfArrayData a, ptrsOfArrayData b)
  touchArrayData (AD_Pair a b)         = touchArrayData a >> touchArrayData b
  newArrayData size
    = do
        a <- newArrayData size
        b <- newArrayData size
        return $ AD_Pair a b
  unsafeReadArrayData (AD_Pair a b) i
    = do
        x <- unsafeReadArrayData a i
        y <- unsafeReadArrayData b i
        return (x, y)
  unsafeWriteArrayData (AD_Pair a b) i (x, y)
    = do
        unsafeWriteArrayData a i x
        unsafeWriteArrayData b i y
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
{-# INLINE runArrayData #-}
runArrayData :: ArrayElt e
             => IO (MutableArrayData e, e) -> (ArrayData e, e)
runArrayData st = unsafePerformIO $ do
                    (mad, r) <- st
                    return (mad, r)

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

-- Returns the element of an immutable array at the specified index.
--
-- This does no bounds checking unless you configured with -funsafe-checks. This
-- is usually OK, since the functions that convert from multidimensional to
-- linear indexing do bounds checking by default.
--
{-# INLINE unsafeIndexArray #-}
unsafeIndexArray :: MArray a e IO => a Int e -> Int -> e
unsafeIndexArray a i = unsafePerformIO $ unsafeReadArray a i

-- Read an element from a mutable array.
--
-- This does no bounds checking unless you configured with -funsafe-checks. This
-- is usually OK, since the functions that convert from multidimensional to
-- linear indexing do bounds checking by default.
--
{-# INLINE unsafeReadArray #-}
unsafeReadArray :: MArray a e m => a Int e -> Int -> m e
#ifdef ACCELERATE_UNSAFE_CHECKS
unsafeReadArray = MArray.readArray
#else
unsafeReadArray = MArray.unsafeRead
#endif

-- Write an element into a mutable array.
--
-- This does no bounds checking unless you configured with -funsafe-checks. This
-- is usually OK, since the functions that convert from multidimensional to
-- linear indexing do bounds checking by default.
--
{-# INLINE unsafeWriteArray #-}
unsafeWriteArray :: MArray a e m => a Int e -> Int -> e -> m ()
#ifdef ACCELERATE_UNSAFE_CHECKS
unsafeWriteArray = MArray.writeArray
#else
unsafeWriteArray = MArray.unsafeWrite
#endif

-- Keeps a unique array alive.
--
{-# INLINE touchUniqueArray #-}
touchUniqueArray :: UniqueArray i a -> IO ()
touchUniqueArray (UniqueArray _ sa) = touchStorableArray sa

-- Obtains a pointer to the payload of an unique array.
--
{-# INLINE uniqueArrayPtr #-}
uniqueArrayPtr :: UniqueArray i a -> Ptr a
uniqueArrayPtr (UniqueArray _ (StorableArray _ _ _ fp)) = unsafeForeignPtrToPtr fp

-- The global counter that gives new ids for unique arrays.
{-# NOINLINE counter #-}
counter :: MVar Int
counter = unsafePerformIO $ newMVar 0
