{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UnboxedTuples       #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Array.Data
-- Copyright   : [2008..2016] Manuel M T Chakravarty, Gabriele Keller
--               [2009..2016] Trevor L. McDonell
--               [2008..2009] Sean Lee
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

  -- * Allocator internals
  registerForeignPtrAllocator,

) where

-- friends
import Data.Array.Accelerate.Array.Unique
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.Debug.Flags
import Data.Array.Accelerate.Debug.Trace

-- standard libraries
import Control.Applicative
import Control.Monad
import Data.Bits
import Data.IORef
import Data.Typeable                                                ( Typeable )
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Language.Haskell.TH
import System.IO.Unsafe
import Text.Printf
import Prelude

import GHC.Base                                                     ( Int(..), IO(..), unsafeCoerce#, newAlignedPinnedByteArray#, byteArrayContents# )
import GHC.ForeignPtr                                               ( ForeignPtr(..), ForeignPtrContents(..) )


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


-- Array representation
-- --------------------

-- |Immutable array representation
--
type ArrayData e = MutableArrayData e

-- |Mutable array representation
--
type MutableArrayData e = GArrayData UniqueArray e

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
  arrayElt               :: ArrayEltR e
  --
  unsafeIndexArrayData   :: ArrayData e -> Int -> e
  ptrsOfArrayData        :: ArrayData e -> ArrayPtrs e
  touchArrayData         :: ArrayData e -> IO ()
  --
  newArrayData           :: Int -> IO (MutableArrayData e)
  unsafeCopyArrayData    :: ArrayData e -> ArrayData e -> Int -> Int -> Int -> IO ()
  unsafeReadArrayData    :: MutableArrayData e -> Int      -> IO e
  unsafeWriteArrayData   :: MutableArrayData e -> Int -> e -> IO ()
  unsafeFreezeArrayData  :: MutableArrayData e -> IO (ArrayData e)
  ptrsOfMutableArrayData :: MutableArrayData e -> IO (ArrayPtrs e)
  --
  {-# INLINE unsafeFreezeArrayData  #-}
  {-# INLINE ptrsOfMutableArrayData #-}
  unsafeFreezeArrayData  = return
  ptrsOfMutableArrayData = return . ptrsOfArrayData

instance ArrayElt () where
  type ArrayPtrs () = ()
  arrayElt          = ArrayEltRunit
  {-# INLINE unsafeIndexArrayData #-}
  unsafeIndexArrayData AD_Unit i          = i `seq` ()
  {-# INLINE ptrsOfArrayData #-}
  ptrsOfArrayData AD_Unit                 = ()
  {-# INLINE touchArrayData #-}
  touchArrayData AD_Unit                  = return ()
  {-# INLINE unsafeCopyArrayData #-}
  unsafeCopyArrayData AD_Unit _ _ _ _     = return ()
  {-# INLINE newArrayData #-}
  newArrayData size                       = size `seq` return AD_Unit
  {-# INLINE unsafeReadArrayData #-}
  unsafeReadArrayData AD_Unit i           = i `seq` return ()
  {-# INLINE unsafeWriteArrayData #-}
  unsafeWriteArrayData AD_Unit i ()       = i `seq` return ()

instance ArrayElt Int where
  type ArrayPtrs Int = Ptr Int
  arrayElt           = ArrayEltRint
  {-# INLINE unsafeIndexArrayData #-}
  unsafeIndexArrayData (AD_Int ba) i      = unsafeIndexArray ba i
  {-# INLINE ptrsOfArrayData #-}
  ptrsOfArrayData (AD_Int ba)             = unsafeUniqueArrayPtr ba
  {-# INLINE touchArrayData #-}
  touchArrayData (AD_Int ba)              = touchUniqueArray ba
  {-# INLINE newArrayData #-}
  newArrayData size                       = AD_Int <$> newArrayData' size
  {-# INLINE unsafeCopyArrayData #-}
  unsafeCopyArrayData (AD_Int ba) (AD_Int ba') s d n
    = unsafeCopyArray ba ba' s d n
  {-# INLINE unsafeReadArrayData #-}
  unsafeReadArrayData (AD_Int ba) i       = unsafeReadArray ba i
  {-# INLINE unsafeWriteArrayData #-}
  unsafeWriteArrayData (AD_Int ba) i e    = unsafeWriteArray ba i e

instance ArrayElt Int8 where
  type ArrayPtrs Int8 = Ptr Int8
  arrayElt            = ArrayEltRint8
  {-# INLINE unsafeIndexArrayData #-}
  unsafeIndexArrayData (AD_Int8 ba) i     = unsafeIndexArray ba i
  {-# INLINE ptrsOfArrayData #-}
  ptrsOfArrayData (AD_Int8 ba)            = unsafeUniqueArrayPtr ba
  {-# INLINE touchArrayData #-}
  touchArrayData (AD_Int8 ba)             = touchUniqueArray ba
  {-# INLINE newArrayData #-}
  newArrayData size                       = AD_Int8 <$> newArrayData' size
  {-# INLINE unsafeCopyArrayData #-}
  unsafeCopyArrayData (AD_Int8 ba) (AD_Int8 ba') s d n
    = unsafeCopyArray ba ba' s d n
  {-# INLINE unsafeReadArrayData #-}
  unsafeReadArrayData (AD_Int8 ba) i      = unsafeReadArray ba i
  {-# INLINE unsafeWriteArrayData #-}
  unsafeWriteArrayData (AD_Int8 ba) i e   = unsafeWriteArray ba i e

instance ArrayElt Int16 where
  type ArrayPtrs Int16 = Ptr Int16
  arrayElt             = ArrayEltRint16
  {-# INLINE unsafeIndexArrayData #-}
  unsafeIndexArrayData (AD_Int16 ba) i    = unsafeIndexArray ba i
  {-# INLINE ptrsOfArrayData #-}
  ptrsOfArrayData (AD_Int16 ba)           = unsafeUniqueArrayPtr ba
  {-# INLINE touchArrayData #-}
  touchArrayData (AD_Int16 ba)            = touchUniqueArray ba
  {-# INLINE newArrayData #-}
  newArrayData size                       = AD_Int16 <$> newArrayData' size
  {-# INLINE unsafeCopyArrayData #-}
  unsafeCopyArrayData (AD_Int16 ba) (AD_Int16 ba') s d n
    = unsafeCopyArray ba ba' s d n
  {-# INLINE unsafeReadArrayData #-}
  unsafeReadArrayData (AD_Int16 ba) i     = unsafeReadArray ba i
  {-# INLINE unsafeWriteArrayData #-}
  unsafeWriteArrayData (AD_Int16 ba) i e  = unsafeWriteArray ba i e

instance ArrayElt Int32 where
  type ArrayPtrs Int32 = Ptr Int32
  arrayElt             = ArrayEltRint32
  {-# INLINE unsafeIndexArrayData #-}
  unsafeIndexArrayData (AD_Int32 ba) i    = unsafeIndexArray ba i
  {-# INLINE ptrsOfArrayData #-}
  ptrsOfArrayData (AD_Int32 ba)           = unsafeUniqueArrayPtr ba
  {-# INLINE touchArrayData #-}
  touchArrayData (AD_Int32 ba)            = touchUniqueArray ba
  {-# INLINE newArrayData #-}
  newArrayData size                       = AD_Int32 <$> newArrayData' size
  {-# INLINE unsafeCopyArrayData #-}
  unsafeCopyArrayData (AD_Int32 ba) (AD_Int32 ba') s d n
    = unsafeCopyArray ba ba' s d n
  {-# INLINE unsafeReadArrayData #-}
  unsafeReadArrayData (AD_Int32 ba) i     = unsafeReadArray ba i
  {-# INLINE unsafeWriteArrayData #-}
  unsafeWriteArrayData (AD_Int32 ba) i e  = unsafeWriteArray ba i e

instance ArrayElt Int64 where
  type ArrayPtrs Int64 = Ptr Int64
  arrayElt             = ArrayEltRint64
  {-# INLINE unsafeIndexArrayData #-}
  unsafeIndexArrayData (AD_Int64 ba) i    = unsafeIndexArray ba i
  {-# INLINE ptrsOfArrayData #-}
  ptrsOfArrayData (AD_Int64 ba)           = unsafeUniqueArrayPtr ba
  {-# INLINE touchArrayData #-}
  touchArrayData (AD_Int64 ba)            = touchUniqueArray ba
  {-# INLINE newArrayData #-}
  newArrayData size                       = AD_Int64 <$> newArrayData' size
  {-# INLINE unsafeCopyArrayData #-}
  unsafeCopyArrayData (AD_Int64 ba) (AD_Int64 ba') s d n
    = unsafeCopyArray ba ba' s d n
  {-# INLINE unsafeReadArrayData #-}
  unsafeReadArrayData (AD_Int64 ba) i     = unsafeReadArray ba i
  {-# INLINE unsafeWriteArrayData #-}
  unsafeWriteArrayData (AD_Int64 ba) i e  = unsafeWriteArray ba i e

instance ArrayElt Word where
  type ArrayPtrs Word = Ptr Word
  arrayElt            = ArrayEltRword
  {-# INLINE unsafeIndexArrayData #-}
  unsafeIndexArrayData (AD_Word ba) i     = unsafeIndexArray ba i
  {-# INLINE ptrsOfArrayData #-}
  ptrsOfArrayData (AD_Word ba)            = unsafeUniqueArrayPtr ba
  {-# INLINE touchArrayData #-}
  touchArrayData (AD_Word ba)             = touchUniqueArray ba
  {-# INLINE newArrayData #-}
  newArrayData size                       = AD_Word <$> newArrayData' size
  {-# INLINE unsafeCopyArrayData #-}
  unsafeCopyArrayData (AD_Word ba) (AD_Word ba') s d n
    = unsafeCopyArray ba ba' s d n
  {-# INLINE unsafeReadArrayData #-}
  unsafeReadArrayData (AD_Word ba) i      = unsafeReadArray ba i
  {-# INLINE unsafeWriteArrayData #-}
  unsafeWriteArrayData (AD_Word ba) i e   = unsafeWriteArray ba i e

instance ArrayElt Word8 where
  type ArrayPtrs Word8 = Ptr Word8
  arrayElt             = ArrayEltRword8
  {-# INLINE unsafeIndexArrayData #-}
  unsafeIndexArrayData (AD_Word8 ba) i    = unsafeIndexArray ba i
  {-# INLINE ptrsOfArrayData #-}
  ptrsOfArrayData (AD_Word8 ba)           = unsafeUniqueArrayPtr ba
  {-# INLINE touchArrayData #-}
  touchArrayData (AD_Word8 ba)            = touchUniqueArray ba
  {-# INLINE newArrayData #-}
  newArrayData size                       = AD_Word8 <$> newArrayData' size
  {-# INLINE unsafeCopyArrayData #-}
  unsafeCopyArrayData (AD_Word8 ba) (AD_Word8 ba') s d n
    = unsafeCopyArray ba ba' s d n
  {-# INLINE unsafeReadArrayData #-}
  unsafeReadArrayData (AD_Word8 ba) i     = unsafeReadArray ba i
  {-# INLINE unsafeWriteArrayData #-}
  unsafeWriteArrayData (AD_Word8 ba) i e  = unsafeWriteArray ba i e

instance ArrayElt Word16 where
  type ArrayPtrs Word16 = Ptr Word16
  arrayElt              = ArrayEltRword16
  {-# INLINE unsafeIndexArrayData #-}
  unsafeIndexArrayData (AD_Word16 ba) i   = unsafeIndexArray ba i
  {-# INLINE ptrsOfArrayData #-}
  ptrsOfArrayData (AD_Word16 ba)          = unsafeUniqueArrayPtr ba
  {-# INLINE touchArrayData #-}
  touchArrayData (AD_Word16 ba)           = touchUniqueArray ba
  {-# INLINE newArrayData #-}
  newArrayData size                       = AD_Word16 <$> newArrayData' size
  {-# INLINE unsafeCopyArrayData #-}
  unsafeCopyArrayData (AD_Word16 ba) (AD_Word16 ba') s d n
    = unsafeCopyArray ba ba' s d n
  {-# INLINE unsafeReadArrayData #-}
  unsafeReadArrayData (AD_Word16 ba) i    = unsafeReadArray ba i
  {-# INLINE unsafeWriteArrayData #-}
  unsafeWriteArrayData (AD_Word16 ba) i e = unsafeWriteArray ba i e

instance ArrayElt Word32 where
  type ArrayPtrs Word32 = Ptr Word32
  arrayElt              = ArrayEltRword32
  {-# INLINE unsafeIndexArrayData #-}
  unsafeIndexArrayData (AD_Word32 ba) i   = unsafeIndexArray ba i
  {-# INLINE ptrsOfArrayData #-}
  ptrsOfArrayData (AD_Word32 ba)          = unsafeUniqueArrayPtr ba
  {-# INLINE touchArrayData #-}
  touchArrayData (AD_Word32 ba)           = touchUniqueArray ba
  {-# INLINE newArrayData #-}
  newArrayData size                       = AD_Word32 <$> newArrayData' size
  {-# INLINE unsafeCopyArrayData #-}
  unsafeCopyArrayData (AD_Word32 ba) (AD_Word32 ba') s d n
    = unsafeCopyArray ba ba' s d n
  {-# INLINE unsafeReadArrayData #-}
  unsafeReadArrayData (AD_Word32 ba) i    = unsafeReadArray ba i
  {-# INLINE unsafeWriteArrayData #-}
  unsafeWriteArrayData (AD_Word32 ba) i e = unsafeWriteArray ba i e

instance ArrayElt Word64 where
  type ArrayPtrs Word64 = Ptr Word64
  arrayElt              = ArrayEltRword64
  {-# INLINE unsafeIndexArrayData #-}
  unsafeIndexArrayData (AD_Word64 ba) i   = unsafeIndexArray ba i
  {-# INLINE ptrsOfArrayData #-}
  ptrsOfArrayData (AD_Word64 ba)          = unsafeUniqueArrayPtr ba
  {-# INLINE touchArrayData #-}
  touchArrayData (AD_Word64 ba)           = touchUniqueArray ba
  {-# INLINE newArrayData #-}
  newArrayData size                       = AD_Word64 <$> newArrayData' size
  {-# INLINE unsafeCopyArrayData #-}
  unsafeCopyArrayData (AD_Word64 ba) (AD_Word64 ba') s d n
    = unsafeCopyArray ba ba' s d n
  {-# INLINE unsafeReadArrayData #-}
  unsafeReadArrayData (AD_Word64 ba) i    = unsafeReadArray ba i
  {-# INLINE unsafeWriteArrayData #-}
  unsafeWriteArrayData (AD_Word64 ba) i e = unsafeWriteArray ba i e

instance ArrayElt CShort where
  type ArrayPtrs CShort = Ptr Int16
  arrayElt              = ArrayEltRcshort
  {-# INLINE unsafeIndexArrayData #-}
  unsafeIndexArrayData (AD_CShort ba) i   = CShort $ unsafeIndexArray ba i
  {-# INLINE ptrsOfArrayData #-}
  ptrsOfArrayData (AD_CShort ba)          = unsafeUniqueArrayPtr ba
  {-# INLINE touchArrayData #-}
  touchArrayData (AD_CShort ba)           = touchUniqueArray ba
  {-# INLINE newArrayData #-}
  newArrayData size                       = AD_CShort <$> newArrayData' size
  {-# INLINE unsafeCopyArrayData #-}
  unsafeCopyArrayData (AD_CShort ba) (AD_CShort ba') s d n
    = unsafeCopyArray ba ba' s d n
  {-# INLINE unsafeReadArrayData #-}
  unsafeReadArrayData (AD_CShort ba) i    = CShort <$> unsafeReadArray ba i
  {-# INLINE unsafeWriteArrayData #-}
  unsafeWriteArrayData (AD_CShort ba) i (CShort e)
    = unsafeWriteArray ba i e

instance ArrayElt CUShort where
  type ArrayPtrs CUShort = Ptr Word16
  arrayElt               = ArrayEltRcushort
  {-# INLINE unsafeIndexArrayData #-}
  unsafeIndexArrayData (AD_CUShort ba) i  = CUShort $ unsafeIndexArray ba i
  {-# INLINE ptrsOfArrayData #-}
  ptrsOfArrayData (AD_CUShort ba)         = unsafeUniqueArrayPtr ba
  {-# INLINE touchArrayData #-}
  touchArrayData (AD_CUShort ba)          = touchUniqueArray ba
  {-# INLINE newArrayData #-}
  newArrayData size                       = AD_CUShort <$> newArrayData' size
  {-# INLINE unsafeCopyArrayData #-}
  unsafeCopyArrayData (AD_CUShort ba) (AD_CUShort ba') s d n
    = unsafeCopyArray ba ba' s d n
  {-# INLINE unsafeReadArrayData #-}
  unsafeReadArrayData (AD_CUShort ba) i   = CUShort <$> unsafeReadArray ba i
  {-# INLINE unsafeWriteArrayData #-}
  unsafeWriteArrayData (AD_CUShort ba) i (CUShort e)
    = unsafeWriteArray ba i e

instance ArrayElt CInt where
  type ArrayPtrs CInt = Ptr Int32
  arrayElt            = ArrayEltRcint
  {-# INLINE unsafeIndexArrayData #-}
  unsafeIndexArrayData (AD_CInt ba) i     = CInt $ unsafeIndexArray ba i
  {-# INLINE ptrsOfArrayData #-}
  ptrsOfArrayData (AD_CInt ba)            = unsafeUniqueArrayPtr ba
  {-# INLINE touchArrayData #-}
  touchArrayData (AD_CInt ba)             = touchUniqueArray ba
  {-# INLINE newArrayData #-}
  newArrayData size                       = AD_CInt <$> newArrayData' size
  {-# INLINE unsafeCopyArrayData #-}
  unsafeCopyArrayData (AD_CInt ba) (AD_CInt ba') s d n
    = unsafeCopyArray ba ba' s d n
  {-# INLINE unsafeReadArrayData #-}
  unsafeReadArrayData (AD_CInt ba) i      = CInt <$> unsafeReadArray ba i
  {-# INLINE unsafeWriteArrayData #-}
  unsafeWriteArrayData (AD_CInt ba) i (CInt e)
    = unsafeWriteArray ba i e

instance ArrayElt CUInt where
  type ArrayPtrs CUInt = Ptr Word32
  arrayElt             = ArrayEltRcuint
  {-# INLINE unsafeIndexArrayData #-}
  unsafeIndexArrayData (AD_CUInt ba) i    = CUInt $ unsafeIndexArray ba i
  {-# INLINE ptrsOfArrayData #-}
  ptrsOfArrayData (AD_CUInt ba)           = unsafeUniqueArrayPtr ba
  {-# INLINE touchArrayData #-}
  touchArrayData (AD_CUInt ba)            = touchUniqueArray ba
  {-# INLINE newArrayData #-}
  newArrayData size                       = AD_CUInt <$> newArrayData' size
  {-# INLINE unsafeCopyArrayData #-}
  unsafeCopyArrayData (AD_CUInt ba) (AD_CUInt ba') s d n
    = unsafeCopyArray ba ba' s d n
  {-# INLINE unsafeReadArrayData #-}
  unsafeReadArrayData (AD_CUInt ba) i     = CUInt <$> unsafeReadArray ba i
  {-# INLINE unsafeWriteArrayData #-}
  unsafeWriteArrayData (AD_CUInt ba) i (CUInt e)
    = unsafeWriteArray ba i e

instance ArrayElt CLong where
  type ArrayPtrs CLong = Ptr HTYPE_LONG
  arrayElt             = ArrayEltRclong
  {-# INLINE unsafeIndexArrayData #-}
  unsafeIndexArrayData (AD_CLong ba) i    = CLong $ unsafeIndexArray ba i
  {-# INLINE ptrsOfArrayData #-}
  ptrsOfArrayData (AD_CLong ba)           = unsafeUniqueArrayPtr ba
  {-# INLINE touchArrayData #-}
  touchArrayData (AD_CLong ba)            = touchUniqueArray ba
  {-# INLINE newArrayData #-}
  newArrayData size                       = AD_CLong <$> newArrayData' size
  {-# INLINE unsafeCopyArrayData #-}
  unsafeCopyArrayData (AD_CLong ba) (AD_CLong ba') s d n
    = unsafeCopyArray ba ba' s d n
  {-# INLINE unsafeReadArrayData #-}
  unsafeReadArrayData (AD_CLong ba) i     = CLong <$> unsafeReadArray ba i
  {-# INLINE unsafeWriteArrayData #-}
  unsafeWriteArrayData (AD_CLong ba) i (CLong e)
    = unsafeWriteArray ba i e

instance ArrayElt CULong where
  type ArrayPtrs CULong = Ptr HTYPE_UNSIGNED_LONG
  arrayElt              = ArrayEltRculong
  {-# INLINE unsafeIndexArrayData #-}
  unsafeIndexArrayData (AD_CULong ba) i   = CULong $ unsafeIndexArray ba i
  {-# INLINE ptrsOfArrayData #-}
  ptrsOfArrayData (AD_CULong ba)          = unsafeUniqueArrayPtr ba
  {-# INLINE touchArrayData #-}
  touchArrayData (AD_CULong ba)           = touchUniqueArray ba
  {-# INLINE newArrayData #-}
  newArrayData size                       = AD_CULong <$> newArrayData' size
  {-# INLINE unsafeCopyArrayData #-}
  unsafeCopyArrayData (AD_CULong ba) (AD_CULong ba') s d n
    = unsafeCopyArray ba ba' s d n
  {-# INLINE unsafeReadArrayData #-}
  unsafeReadArrayData (AD_CULong ba) i    = CULong <$> unsafeReadArray ba i
  {-# INLINE unsafeWriteArrayData #-}
  unsafeWriteArrayData (AD_CULong ba) i (CULong e)
    = unsafeWriteArray ba i e

instance ArrayElt CLLong where
  type ArrayPtrs CLLong = Ptr Int64
  arrayElt              = ArrayEltRcllong
  {-# INLINE unsafeIndexArrayData #-}
  unsafeIndexArrayData (AD_CLLong ba) i   = CLLong $ unsafeIndexArray ba i
  {-# INLINE ptrsOfArrayData #-}
  ptrsOfArrayData (AD_CLLong ba)          = unsafeUniqueArrayPtr ba
  {-# INLINE touchArrayData #-}
  touchArrayData (AD_CLLong ba)           = touchUniqueArray ba
  {-# INLINE newArrayData #-}
  newArrayData size                       = AD_CLLong <$> newArrayData' size
  {-# INLINE unsafeCopyArrayData #-}
  unsafeCopyArrayData (AD_CLLong ba) (AD_CLLong ba') s d n
    = unsafeCopyArray ba ba' s d n
  {-# INLINE unsafeReadArrayData #-}
  unsafeReadArrayData (AD_CLLong ba) i    = CLLong <$> unsafeReadArray ba i
  {-# INLINE unsafeWriteArrayData #-}
  unsafeWriteArrayData (AD_CLLong ba) i (CLLong e)
    = unsafeWriteArray ba i e

instance ArrayElt CULLong where
  type ArrayPtrs CULLong = Ptr Word64
  arrayElt               = ArrayEltRcullong
  {-# INLINE unsafeIndexArrayData #-}
  unsafeIndexArrayData (AD_CULLong ba) i  = CULLong $ unsafeIndexArray ba i
  {-# INLINE ptrsOfArrayData #-}
  ptrsOfArrayData (AD_CULLong ba)         = unsafeUniqueArrayPtr ba
  {-# INLINE touchArrayData #-}
  touchArrayData (AD_CULLong ba)          = touchUniqueArray ba
  {-# INLINE newArrayData #-}
  newArrayData size                       = AD_CULLong <$> newArrayData' size
  {-# INLINE unsafeCopyArrayData #-}
  unsafeCopyArrayData (AD_CULLong ba) (AD_CULLong ba') s d n
    = unsafeCopyArray ba ba' s d n
  {-# INLINE unsafeReadArrayData #-}
  unsafeReadArrayData (AD_CULLong ba) i   = CULLong <$> unsafeReadArray ba i
  {-# INLINE unsafeWriteArrayData #-}
  unsafeWriteArrayData (AD_CULLong ba) i (CULLong e)
    = unsafeWriteArray ba i e

instance ArrayElt Float where
  type ArrayPtrs Float = Ptr Float
  arrayElt             = ArrayEltRfloat
  {-# INLINE unsafeIndexArrayData #-}
  unsafeIndexArrayData (AD_Float ba) i    = unsafeIndexArray ba i
  {-# INLINE ptrsOfArrayData #-}
  ptrsOfArrayData (AD_Float ba)           = unsafeUniqueArrayPtr ba
  {-# INLINE touchArrayData #-}
  touchArrayData (AD_Float ba)            = touchUniqueArray ba
  {-# INLINE newArrayData #-}
  newArrayData size                       = AD_Float <$> newArrayData' size
  {-# INLINE unsafeCopyArrayData #-}
  unsafeCopyArrayData (AD_Float ba) (AD_Float ba') s d n
    = unsafeCopyArray ba ba' s d n
  {-# INLINE unsafeReadArrayData #-}
  unsafeReadArrayData (AD_Float ba) i     = unsafeReadArray ba i
  {-# INLINE unsafeWriteArrayData #-}
  unsafeWriteArrayData (AD_Float ba) i e  = unsafeWriteArray ba i e

instance ArrayElt Double where
  type ArrayPtrs Double = Ptr Double
  arrayElt              = ArrayEltRdouble
  {-# INLINE unsafeIndexArrayData #-}
  unsafeIndexArrayData (AD_Double ba) i   = unsafeIndexArray ba i
  {-# INLINE ptrsOfArrayData #-}
  ptrsOfArrayData (AD_Double ba)          = unsafeUniqueArrayPtr ba
  {-# INLINE touchArrayData #-}
  touchArrayData (AD_Double ba)           = touchUniqueArray ba
  {-# INLINE newArrayData #-}
  newArrayData size                       = AD_Double <$> newArrayData' size
  {-# INLINE unsafeCopyArrayData #-}
  unsafeCopyArrayData (AD_Double ba) (AD_Double ba') s d n
    = unsafeCopyArray ba ba' s d n
  {-# INLINE unsafeReadArrayData #-}
  unsafeReadArrayData (AD_Double ba) i    = unsafeReadArray ba i
  {-# INLINE unsafeWriteArrayData #-}
  unsafeWriteArrayData (AD_Double ba) i e = unsafeWriteArray ba i e

instance ArrayElt CFloat where
  type ArrayPtrs CFloat = Ptr Float
  arrayElt              = ArrayEltRcfloat
  {-# INLINE unsafeIndexArrayData #-}
  unsafeIndexArrayData (AD_CFloat ba) i   = CFloat $ unsafeIndexArray ba i
  {-# INLINE ptrsOfArrayData #-}
  ptrsOfArrayData (AD_CFloat ba)          = unsafeUniqueArrayPtr ba
  {-# INLINE touchArrayData #-}
  touchArrayData (AD_CFloat ba)           = touchUniqueArray ba
  {-# INLINE newArrayData #-}
  newArrayData size                       = AD_CFloat <$> newArrayData' size
  {-# INLINE unsafeCopyArrayData #-}
  unsafeCopyArrayData (AD_CFloat ba) (AD_CFloat ba') s d n
    = unsafeCopyArray ba ba' s d n
  {-# INLINE unsafeReadArrayData #-}
  unsafeReadArrayData (AD_CFloat ba) i    = CFloat <$> unsafeReadArray ba i
  {-# INLINE unsafeWriteArrayData #-}
  unsafeWriteArrayData (AD_CFloat ba) i (CFloat e)
    = unsafeWriteArray ba i e

instance ArrayElt CDouble where
  type ArrayPtrs CDouble = Ptr Double
  arrayElt               = ArrayEltRcdouble
  {-# INLINE unsafeIndexArrayData #-}
  unsafeIndexArrayData (AD_CDouble ba) i  = CDouble $ unsafeIndexArray ba i
  {-# INLINE ptrsOfArrayData #-}
  ptrsOfArrayData (AD_CDouble ba)         = unsafeUniqueArrayPtr ba
  {-# INLINE touchArrayData #-}
  touchArrayData (AD_CDouble ba)          = touchUniqueArray ba
  {-# INLINE newArrayData #-}
  newArrayData size                       = AD_CDouble <$> newArrayData' size
  {-# INLINE unsafeCopyArrayData #-}
  unsafeCopyArrayData (AD_CDouble ba) (AD_CDouble ba') s d n
    = unsafeCopyArray ba ba' s d n
  {-# INLINE unsafeReadArrayData #-}
  unsafeReadArrayData (AD_CDouble ba) i   = CDouble <$> unsafeReadArray ba i
  {-# INLINE unsafeWriteArrayData #-}
  unsafeWriteArrayData (AD_CDouble ba) i (CDouble e)
    = unsafeWriteArray ba i e

-- Bool arrays are stored as arrays of bytes. While this is memory inefficient,
-- it is better suited to parallel backends than a packed bit-vector
-- representation.
--
instance ArrayElt Bool where
  type ArrayPtrs Bool = Ptr Word8
  arrayElt            = ArrayEltRbool
  {-# INLINE unsafeIndexArrayData #-}
  unsafeIndexArrayData (AD_Bool ba) i     = toBool (unsafeIndexArray ba i)
  {-# INLINE ptrsOfArrayData #-}
  ptrsOfArrayData (AD_Bool ba)            = unsafeUniqueArrayPtr ba
  {-# INLINE touchArrayData #-}
  touchArrayData (AD_Bool ba)             = touchUniqueArray ba
  {-# INLINE unsafeCopyArrayData #-}
  unsafeCopyArrayData (AD_Bool ba) (AD_Bool ba') s d n
    = unsafeCopyArray ba ba' s d n
  {-# INLINE newArrayData #-}
  newArrayData size                       = AD_Bool <$> newArrayData' size
  {-# INLINE unsafeReadArrayData #-}
  unsafeReadArrayData (AD_Bool ba) i      = toBool  <$> unsafeReadArray ba i
  {-# INLINE unsafeWriteArrayData #-}
  unsafeWriteArrayData (AD_Bool ba) i e   = unsafeWriteArray ba i (fromBool e)

-- Unboxed Char is stored as a wide character, which is 4-bytes
--
instance ArrayElt Char where
  type ArrayPtrs Char = Ptr Char
  arrayElt            = ArrayEltRchar
  {-# INLINE unsafeIndexArrayData #-}
  unsafeIndexArrayData (AD_Char ba) i     = unsafeIndexArray ba i
  {-# INLINE ptrsOfArrayData #-}
  ptrsOfArrayData (AD_Char ba)            = unsafeUniqueArrayPtr ba
  {-# INLINE touchArrayData #-}
  touchArrayData (AD_Char ba)             = touchUniqueArray ba
  {-# INLINE newArrayData #-}
  newArrayData size                       = AD_Char <$> newArrayData' size
  {-# INLINE unsafeCopyArrayData #-}
  unsafeCopyArrayData (AD_Char ba) (AD_Char ba') s d n
    = unsafeCopyArray ba ba' s d n
  {-# INLINE unsafeReadArrayData #-}
  unsafeReadArrayData (AD_Char ba) i      = unsafeReadArray ba i
  {-# INLINE unsafeWriteArrayData #-}
  unsafeWriteArrayData (AD_Char ba) i e   = unsafeWriteArray ba i e

instance ArrayElt CChar where
  type ArrayPtrs CChar = Ptr HTYPE_CCHAR
  arrayElt             = ArrayEltRcchar
  {-# INLINE unsafeIndexArrayData #-}
  unsafeIndexArrayData (AD_CChar ba) i    = CChar $ unsafeIndexArray ba i
  {-# INLINE ptrsOfArrayData #-}
  ptrsOfArrayData (AD_CChar ba)           = unsafeUniqueArrayPtr ba
  {-# INLINE touchArrayData #-}
  touchArrayData (AD_CChar ba)            = touchUniqueArray ba
  {-# INLINE newArrayData #-}
  newArrayData size                       = AD_CChar <$> newArrayData' size
  {-# INLINE unsafeCopyArrayData #-}
  unsafeCopyArrayData (AD_CChar ba) (AD_CChar ba') s d n
    = unsafeCopyArray ba ba' s d n
  {-# INLINE unsafeReadArrayData #-}
  unsafeReadArrayData (AD_CChar ba) i     = CChar <$> unsafeReadArray ba i
  {-# INLINE unsafeWriteArrayData #-}
  unsafeWriteArrayData (AD_CChar ba) i (CChar e)
    = unsafeWriteArray ba i e

instance ArrayElt CSChar where
  type ArrayPtrs CSChar = Ptr Int8
  arrayElt              = ArrayEltRcschar
  {-# INLINE unsafeIndexArrayData #-}
  unsafeIndexArrayData (AD_CSChar ba) i   = CSChar $ unsafeIndexArray ba i
  {-# INLINE ptrsOfArrayData #-}
  ptrsOfArrayData (AD_CSChar ba)          = unsafeUniqueArrayPtr ba
  {-# INLINE touchArrayData #-}
  touchArrayData (AD_CSChar ba)           = touchUniqueArray ba
  {-# INLINE newArrayData #-}
  newArrayData size                       = AD_CSChar <$> newArrayData' size
  {-# INLINE unsafeCopyArrayData #-}
  unsafeCopyArrayData (AD_CSChar ba) (AD_CSChar ba') s d n
    = unsafeCopyArray ba ba' s d n
  {-# INLINE unsafeReadArrayData #-}
  unsafeReadArrayData (AD_CSChar ba) i    = CSChar <$> unsafeReadArray ba i
  {-# INLINE unsafeWriteArrayData #-}
  unsafeWriteArrayData (AD_CSChar ba) i (CSChar e)
    = unsafeWriteArray ba i e

instance ArrayElt CUChar where
  type ArrayPtrs CUChar = Ptr Word8
  arrayElt              = ArrayEltRcuchar
  {-# INLINE unsafeIndexArrayData #-}
  unsafeIndexArrayData (AD_CUChar ba) i   = CUChar $ unsafeIndexArray ba i
  {-# INLINE ptrsOfArrayData #-}
  ptrsOfArrayData (AD_CUChar ba)          = unsafeUniqueArrayPtr ba
  {-# INLINE touchArrayData #-}
  touchArrayData (AD_CUChar ba)           = touchUniqueArray ba
  {-# INLINE newArrayData #-}
  newArrayData size                       = AD_CUChar <$> newArrayData' size
  {-# INLINE unsafeCopyArrayData #-}
  unsafeCopyArrayData (AD_CUChar ba) (AD_CUChar ba') s d n
    = unsafeCopyArray ba ba' s d n
  {-# INLINE unsafeReadArrayData #-}
  unsafeReadArrayData (AD_CUChar ba) i    = CUChar <$> unsafeReadArray ba i
  {-# INLINE unsafeWriteArrayData #-}
  unsafeWriteArrayData (AD_CUChar ba) i (CUChar e)
    = unsafeWriteArray ba i e

instance (ArrayElt a, ArrayElt b) => ArrayElt (a, b) where
  type ArrayPtrs (a, b) = (ArrayPtrs a, ArrayPtrs b)
  arrayElt              = ArrayEltRpair arrayElt arrayElt
  {-# INLINE unsafeIndexArrayData #-}
  unsafeIndexArrayData (AD_Pair a b) i        = (unsafeIndexArrayData a i, unsafeIndexArrayData b i)
  {-# INLINE ptrsOfArrayData #-}
  ptrsOfArrayData (AD_Pair a b)               = (ptrsOfArrayData a, ptrsOfArrayData b)
  {-# INLINE touchArrayData #-}
  touchArrayData (AD_Pair a b)                = touchArrayData a >> touchArrayData b
  {-# INLINE unsafeWriteArrayData #-}
  unsafeWriteArrayData (AD_Pair a b) i (x, y) = unsafeWriteArrayData a i x >> unsafeWriteArrayData b i y
  {-# INLINE unsafeCopyArrayData #-}
  unsafeCopyArrayData (AD_Pair a b) (AD_Pair a' b') s d n
    = unsafeCopyArrayData a a' s d n >> unsafeCopyArrayData b b' s d n
  {-# INLINE newArrayData #-}
  newArrayData size                           = AD_Pair <$> newArrayData size <*> newArrayData size
  {-# INLINE unsafeFreezeArrayData #-}
  unsafeFreezeArrayData (AD_Pair a b)         = AD_Pair <$> unsafeFreezeArrayData a <*> unsafeFreezeArrayData b
  {-# INLINE unsafeReadArrayData #-}
  unsafeReadArrayData  (AD_Pair a b) i        = (,) <$> unsafeReadArrayData a i <*> unsafeReadArrayData b i
  {-# INLINE ptrsOfMutableArrayData #-}
  ptrsOfMutableArrayData (AD_Pair a b)        = (,) <$> ptrsOfMutableArrayData a <*> ptrsOfMutableArrayData b


-- Array tuple operations
-- ----------------------

{-# INLINE fstArrayData #-}
fstArrayData :: ArrayData (a, b) -> ArrayData a
fstArrayData (AD_Pair x _) = x

{-# INLINE sndArrayData #-}
sndArrayData :: ArrayData (a, b) -> ArrayData b
sndArrayData (AD_Pair _ y) = y

{-# INLINE pairArrayData #-}
pairArrayData :: ArrayData a -> ArrayData b -> ArrayData (a, b)
pairArrayData = AD_Pair


-- Auxiliary functions
-- -------------------

{-# INLINE toBool #-}
toBool :: Word8 -> Bool
toBool 0 = False
toBool _ = True

{-# INLINE fromBool #-}
fromBool :: Bool -> Word8
fromBool True  = 1
fromBool False = 0

-- | Safe combination of creating and fast freezing of array data.
--
{-# INLINE runArrayData #-}
runArrayData
    :: IO (MutableArrayData e, e)
    -> (ArrayData e, e)
runArrayData st = unsafePerformIO $ do
  (mad, r) <- st
  return (mad, r)

-- Returns the element of an immutable array at the specified index. This does
-- no bounds checking.
--
{-# INLINE unsafeIndexArray #-}
unsafeIndexArray :: Storable e => UniqueArray e -> Int -> e
unsafeIndexArray ua i = unsafePerformIO $! unsafeReadArray ua i

-- Read an element from a mutable array at the given index. This does no bounds
-- checking.
--
{-# INLINE unsafeReadArray #-}
unsafeReadArray :: Storable e => UniqueArray e -> Int -> IO e
unsafeReadArray ua i =
  withUniqueArrayPtr ua $ \ptr -> peekElemOff ptr i

-- Write an element into a mutable array at the given index. This does no bounds
-- checking.
--
{-# INLINE unsafeWriteArray #-}
unsafeWriteArray :: Storable e => UniqueArray e -> Int -> e -> IO ()
unsafeWriteArray ua i e =
  withUniqueArrayPtr ua $ \ptr -> pokeElemOff ptr i e

-- Copy a section of an array into a new mutable array. This does no bounds
-- checking.
unsafeCopyArray :: Storable e => UniqueArray e -> UniqueArray e -> Int -> Int -> Int -> IO ()
unsafeCopyArray src dst start_src start_dst n = do
  withUniqueArrayPtr dst $ \dst' ->
    withUniqueArrayPtr src $ \src' -> copyArray (advancePtr dst' start_dst) (advancePtr src' start_src) n
  return ()

-- Allocate a new array with enough storage to hold the given number of
-- elements.
--
-- The array is uninitialised and, in particular, allocated lazily. The latter
-- is important because it means that for backends that have discrete memory
-- spaces (e.g. GPUs), we will not increase host memory pressure simply to track
-- intermediate arrays that contain meaningful data only on the device.
--
{-# INLINE newArrayData' #-}
newArrayData' :: forall e. Storable e => Int -> IO (UniqueArray e)
newArrayData' size
  = $internalCheck "newArrayData" "size must be >= 0" (size >= 0)
  $ newUniqueArray <=< unsafeInterleaveIO $ do
      let bytes = size * sizeOf (undefined :: e)
      new <- readIORef __mallocForeignPtrBytes
      ptr <- new bytes
      traceIO dump_gc $ printf "gc: allocated new host array (size=%d, ptr=%s)" bytes (show ptr)
      return (castForeignPtr ptr)

-- | Register the given function as the callback to use to allocate new array
-- data on the host containing the specified number of bytes. The returned array
-- must be pinned (with respect to Haskell's GC), so that it can be passed to
-- foreign code.
--
registerForeignPtrAllocator
    :: (Int -> IO (ForeignPtr Word8))
    -> IO ()
registerForeignPtrAllocator new = do
  traceIO dump_gc "registering new array allocator"
  atomicWriteIORef __mallocForeignPtrBytes new

{-# NOINLINE __mallocForeignPtrBytes #-}
__mallocForeignPtrBytes :: IORef (Int -> IO (ForeignPtr Word8))
__mallocForeignPtrBytes = unsafePerformIO $! newIORef mallocPlainForeignPtrBytesAligned

-- | Allocate the given number of bytes with 16-byte alignment. This is
-- essential for SIMD instructions.
--
-- Additionally, we return a plain ForeignPtr, which unlike a regular ForeignPtr
-- created with 'mallocForeignPtr' carries no finalisers. It is an error to try
-- to add a finaliser to the plain ForeignPtr. For our purposes this is fine,
-- since in Accelerate finalisers are handled using Lifetime
--
mallocPlainForeignPtrBytesAligned :: Int -> IO (ForeignPtr a)
mallocPlainForeignPtrBytesAligned (I# size) = IO $ \s ->
  case newAlignedPinnedByteArray# size 16# s of
    (# s', mbarr# #) -> (# s', ForeignPtr (byteArrayContents# (unsafeCoerce# mbarr#)) (PlainPtr mbarr#) #)

