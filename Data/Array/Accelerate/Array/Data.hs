{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UnboxedTuples       #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Array.Data
-- Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
--               [2009..2012] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
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
import Foreign.C.Types
import GHC.Base           (Int(..))
import GHC.Prim           (newPinnedByteArray#, byteArrayContents#,
                           unsafeFreezeByteArray#, Int#, (*#))
import GHC.Ptr            (Ptr(Ptr))
import GHC.ST             (ST(ST))
import Data.Typeable
import Data.Functor       ((<$>))
import Control.Monad
import Control.Monad.ST
import qualified Data.Array.IArray  as IArray
#ifdef ACCELERATE_UNSAFE_CHECKS
import qualified Data.Array.Base    as MArray (readArray, writeArray)
#else
import qualified Data.Array.Base    as MArray (unsafeRead, unsafeWrite)
import qualified Data.Array.Base    as IArray (unsafeAt)
#endif
import qualified Data.Array.Unsafe  as Unsafe
import Data.Array.ST      (STUArray)
import Data.Array.Unboxed (UArray)
import Data.Array.MArray  (MArray)
import Data.Array.Base    (UArray(UArray), STUArray(STUArray),
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
data instance GArrayData ba CChar   = AD_CChar   (ba Int8)
data instance GArrayData ba CSChar  = AD_CSChar  (ba Int8)
data instance GArrayData ba CUChar  = AD_CUChar  (ba Word8)
data instance GArrayData ba (a, b)  = AD_Pair (GArrayData ba a)
                                              (GArrayData ba b)

instance (Typeable1 ba, Typeable e) => Typeable (GArrayData ba e) where
  typeOf _ = myMkTyCon "Data.Array.Accelerate.Array.Data.GArrayData"
            `mkTyConApp` [typeOf (undefined::ba e), typeOf (undefined::e)]


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
  --
  newArrayData           :: Int -> ST s (MutableArrayData s e)
  unsafeReadArrayData    :: MutableArrayData s e -> Int      -> ST s e
  unsafeWriteArrayData   :: MutableArrayData s e -> Int -> e -> ST s ()
  unsafeFreezeArrayData  :: MutableArrayData s e -> ST s (ArrayData e)
  ptrsOfMutableArrayData :: MutableArrayData s e -> ST s (ArrayPtrs e)
  --
  arrayElt               :: ArrayEltR e

instance ArrayElt () where
  type ArrayPtrs () = ()
  unsafeIndexArrayData AD_Unit i    = i `seq` ()
  ptrsOfArrayData AD_Unit           = ()
  newArrayData size                 = size `seq` return AD_Unit
  unsafeReadArrayData AD_Unit i     = i `seq` return ()
  unsafeWriteArrayData AD_Unit i () = i `seq` return ()
  unsafeFreezeArrayData AD_Unit     = return AD_Unit
  ptrsOfMutableArrayData AD_Unit    = return ()
  arrayElt                          = ArrayEltRunit

instance ArrayElt Int where
  type ArrayPtrs Int = Ptr Int
  unsafeIndexArrayData (AD_Int ba) i   = unsafeIndexArray ba i
  ptrsOfArrayData (AD_Int ba)          = uArrayPtr ba
  newArrayData size                    = liftM AD_Int $ unsafeNewArray_ size wORD_SCALE
  unsafeReadArrayData (AD_Int ba) i    = unsafeReadArray ba i
  unsafeWriteArrayData (AD_Int ba) i e = unsafeWriteArray ba i e
  unsafeFreezeArrayData (AD_Int ba)    = liftM AD_Int $ Unsafe.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_Int ba)   = sTUArrayPtr ba
  arrayElt                             = ArrayEltRint

instance ArrayElt Int8 where
  type ArrayPtrs Int8 = Ptr Int8
  unsafeIndexArrayData (AD_Int8 ba) i   = unsafeIndexArray ba i
  ptrsOfArrayData (AD_Int8 ba)          = uArrayPtr ba
  newArrayData size                     = liftM AD_Int8 $ unsafeNewArray_ size (\x -> x)
  unsafeReadArrayData (AD_Int8 ba) i    = unsafeReadArray ba i
  unsafeWriteArrayData (AD_Int8 ba) i e = unsafeWriteArray ba i e
  unsafeFreezeArrayData (AD_Int8 ba)    = liftM AD_Int8 $ Unsafe.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_Int8 ba)   = sTUArrayPtr ba
  arrayElt                              = ArrayEltRint8

instance ArrayElt Int16 where
  type ArrayPtrs Int16 = Ptr Int16
  unsafeIndexArrayData (AD_Int16 ba) i   = unsafeIndexArray ba i
  ptrsOfArrayData (AD_Int16 ba)          = uArrayPtr ba
  newArrayData size                      = liftM AD_Int16 $ unsafeNewArray_ size (*# 2#)
  unsafeReadArrayData (AD_Int16 ba) i    = unsafeReadArray ba i
  unsafeWriteArrayData (AD_Int16 ba) i e = unsafeWriteArray ba i e
  unsafeFreezeArrayData (AD_Int16 ba)    = liftM AD_Int16 $ Unsafe.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_Int16 ba)   = sTUArrayPtr ba
  arrayElt                               = ArrayEltRint16

instance ArrayElt Int32 where
  type ArrayPtrs Int32 = Ptr Int32
  unsafeIndexArrayData (AD_Int32 ba) i   = unsafeIndexArray ba i
  ptrsOfArrayData (AD_Int32 ba)          = uArrayPtr ba
  newArrayData size                      = liftM AD_Int32 $ unsafeNewArray_ size (*# 4#)
  unsafeReadArrayData (AD_Int32 ba) i    = unsafeReadArray ba i
  unsafeWriteArrayData (AD_Int32 ba) i e = unsafeWriteArray ba i e
  unsafeFreezeArrayData (AD_Int32 ba)    = liftM AD_Int32 $ Unsafe.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_Int32 ba)   = sTUArrayPtr ba
  arrayElt                               = ArrayEltRint32

instance ArrayElt Int64 where
  type ArrayPtrs Int64 = Ptr Int64
  unsafeIndexArrayData (AD_Int64 ba) i   = unsafeIndexArray ba i
  ptrsOfArrayData (AD_Int64 ba)          = uArrayPtr ba
  newArrayData size                      = liftM AD_Int64 $ unsafeNewArray_ size (*# 8#)
  unsafeReadArrayData (AD_Int64 ba) i    = unsafeReadArray ba i
  unsafeWriteArrayData (AD_Int64 ba) i e = unsafeWriteArray ba i e
  unsafeFreezeArrayData (AD_Int64 ba)    = liftM AD_Int64 $ Unsafe.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_Int64 ba)   = sTUArrayPtr ba
  arrayElt                               = ArrayEltRint64

instance ArrayElt Word where
  type ArrayPtrs Word = Ptr Word
  unsafeIndexArrayData (AD_Word ba) i   = unsafeIndexArray ba i
  ptrsOfArrayData (AD_Word ba)          = uArrayPtr ba
  newArrayData size                     = liftM AD_Word $ unsafeNewArray_ size wORD_SCALE
  unsafeReadArrayData (AD_Word ba) i    = unsafeReadArray ba i
  unsafeWriteArrayData (AD_Word ba) i e = unsafeWriteArray ba i e
  unsafeFreezeArrayData (AD_Word ba)    = liftM AD_Word $ Unsafe.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_Word ba)   = sTUArrayPtr ba
  arrayElt                              = ArrayEltRword

instance ArrayElt Word8 where
  type ArrayPtrs Word8 = Ptr Word8
  unsafeIndexArrayData (AD_Word8 ba) i   = unsafeIndexArray ba i
  ptrsOfArrayData (AD_Word8 ba)          = uArrayPtr ba
  newArrayData size                      = liftM AD_Word8 $ unsafeNewArray_ size (\x -> x)
  unsafeReadArrayData (AD_Word8 ba) i    = unsafeReadArray ba i
  unsafeWriteArrayData (AD_Word8 ba) i e = unsafeWriteArray ba i e
  unsafeFreezeArrayData (AD_Word8 ba)    = liftM AD_Word8 $ Unsafe.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_Word8 ba)   = sTUArrayPtr ba
  arrayElt                               = ArrayEltRword8

instance ArrayElt Word16 where
  type ArrayPtrs Word16 = Ptr Word16
  unsafeIndexArrayData (AD_Word16 ba) i   = unsafeIndexArray ba i
  ptrsOfArrayData (AD_Word16 ba)          = uArrayPtr ba
  newArrayData size                       = liftM AD_Word16 $ unsafeNewArray_ size (*# 2#)
  unsafeReadArrayData (AD_Word16 ba) i    = unsafeReadArray ba i
  unsafeWriteArrayData (AD_Word16 ba) i e = unsafeWriteArray ba i e
  unsafeFreezeArrayData (AD_Word16 ba)    = liftM AD_Word16 $ Unsafe.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_Word16 ba)   = sTUArrayPtr ba
  arrayElt                                = ArrayEltRword16

instance ArrayElt Word32 where
  type ArrayPtrs Word32 = Ptr Word32
  unsafeIndexArrayData (AD_Word32 ba) i   = unsafeIndexArray ba i
  ptrsOfArrayData (AD_Word32 ba)          = uArrayPtr ba
  newArrayData size                       = liftM AD_Word32 $ unsafeNewArray_ size (*# 4#)
  unsafeReadArrayData (AD_Word32 ba) i    = unsafeReadArray ba i
  unsafeWriteArrayData (AD_Word32 ba) i e = unsafeWriteArray ba i e
  unsafeFreezeArrayData (AD_Word32 ba)    = liftM AD_Word32 $ Unsafe.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_Word32 ba)   = sTUArrayPtr ba
  arrayElt                                = ArrayEltRword32

instance ArrayElt Word64 where
  type ArrayPtrs Word64 = Ptr Word64
  unsafeIndexArrayData (AD_Word64 ba) i   = unsafeIndexArray ba i
  ptrsOfArrayData (AD_Word64 ba)          = uArrayPtr ba
  newArrayData size                       = liftM AD_Word64 $ unsafeNewArray_ size (*# 8#)
  unsafeReadArrayData (AD_Word64 ba) i    = unsafeReadArray ba i
  unsafeWriteArrayData (AD_Word64 ba) i e = unsafeWriteArray ba i e
  unsafeFreezeArrayData (AD_Word64 ba)    = liftM AD_Word64 $ Unsafe.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_Word64 ba)   = sTUArrayPtr ba
  arrayElt                                = ArrayEltRword64

instance ArrayElt CShort where
  type ArrayPtrs CShort = Ptr Int16
  unsafeIndexArrayData (AD_CShort ba) i   = CShort $ unsafeIndexArray ba i
  ptrsOfArrayData (AD_CShort ba)          = uArrayPtr ba
  newArrayData size                       = liftM AD_CShort $ unsafeNewArray_ size (*# 2#)
  unsafeReadArrayData (AD_CShort ba) i    = CShort <$> unsafeReadArray ba i
  unsafeWriteArrayData (AD_CShort ba) i (CShort e)
    = unsafeWriteArray ba i e
  unsafeFreezeArrayData (AD_CShort ba)    = liftM AD_CShort $ Unsafe.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_CShort ba)   = sTUArrayPtr ba
  arrayElt                                = ArrayEltRcshort

instance ArrayElt CUShort where
  type ArrayPtrs CUShort = Ptr Word16
  unsafeIndexArrayData (AD_CUShort ba) i   = CUShort $ unsafeIndexArray ba i
  ptrsOfArrayData (AD_CUShort ba)          = uArrayPtr ba
  newArrayData size                        = liftM AD_CUShort $ unsafeNewArray_ size (*# 2#)
  unsafeReadArrayData (AD_CUShort ba) i    = CUShort <$> unsafeReadArray ba i
  unsafeWriteArrayData (AD_CUShort ba) i (CUShort e)
    = unsafeWriteArray ba i e
  unsafeFreezeArrayData (AD_CUShort ba)    = liftM AD_CUShort $ Unsafe.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_CUShort ba)   = sTUArrayPtr ba
  arrayElt                                 = ArrayEltRcushort

instance ArrayElt CInt where
  type ArrayPtrs CInt = Ptr Int32
  unsafeIndexArrayData (AD_CInt ba) i   = CInt $ unsafeIndexArray ba i
  ptrsOfArrayData (AD_CInt ba)          = uArrayPtr ba
  newArrayData size                     = liftM AD_CInt $ unsafeNewArray_ size (*# 4#)
  unsafeReadArrayData (AD_CInt ba) i    = CInt <$> unsafeReadArray ba i
  unsafeWriteArrayData (AD_CInt ba) i (CInt e)
    = unsafeWriteArray ba i e
  unsafeFreezeArrayData (AD_CInt ba)    = liftM AD_CInt $ Unsafe.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_CInt ba)   = sTUArrayPtr ba
  arrayElt                              = ArrayEltRcint

instance ArrayElt CUInt where
  type ArrayPtrs CUInt = Ptr Word32
  unsafeIndexArrayData (AD_CUInt ba) i   = CUInt $ unsafeIndexArray ba i
  ptrsOfArrayData (AD_CUInt ba)          = uArrayPtr ba
  newArrayData size                      = liftM AD_CUInt $ unsafeNewArray_ size (*# 4#)
  unsafeReadArrayData (AD_CUInt ba) i    = CUInt <$> unsafeReadArray ba i
  unsafeWriteArrayData (AD_CUInt ba) i (CUInt e)
    = unsafeWriteArray ba i e
  unsafeFreezeArrayData (AD_CUInt ba)    = liftM AD_CUInt $ Unsafe.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_CUInt ba)   = sTUArrayPtr ba
  arrayElt                               = ArrayEltRcuint

instance ArrayElt CLong where
  type ArrayPtrs CLong = Ptr HTYPE_LONG
  unsafeIndexArrayData (AD_CLong ba) i   = CLong $ unsafeIndexArray ba i
  ptrsOfArrayData (AD_CLong ba)          = uArrayPtr ba
  newArrayData size                      = liftM AD_CLong $ unsafeNewArray_ size wORD_SCALE
  unsafeReadArrayData (AD_CLong ba) i    = CLong <$> unsafeReadArray ba i
  unsafeWriteArrayData (AD_CLong ba) i (CLong e)
    = unsafeWriteArray ba i e
  unsafeFreezeArrayData (AD_CLong ba)    = liftM AD_CLong $ Unsafe.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_CLong ba)   = sTUArrayPtr ba
  arrayElt                               = ArrayEltRclong

instance ArrayElt CULong where
  type ArrayPtrs CULong = Ptr HTYPE_UNSIGNED_LONG
  unsafeIndexArrayData (AD_CULong ba) i   = CULong $ unsafeIndexArray ba i
  ptrsOfArrayData (AD_CULong ba)          = uArrayPtr ba
  newArrayData size                       = liftM AD_CULong $ unsafeNewArray_ size wORD_SCALE
  unsafeReadArrayData (AD_CULong ba) i    = CULong <$> unsafeReadArray ba i
  unsafeWriteArrayData (AD_CULong ba) i (CULong e)
    = unsafeWriteArray ba i e
  unsafeFreezeArrayData (AD_CULong ba)    = liftM AD_CULong $ Unsafe.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_CULong ba)   = sTUArrayPtr ba
  arrayElt                                = ArrayEltRculong

instance ArrayElt CLLong where
  type ArrayPtrs CLLong = Ptr Int64
  unsafeIndexArrayData (AD_CLLong ba) i   = CLLong $ unsafeIndexArray ba i
  ptrsOfArrayData (AD_CLLong ba)          = uArrayPtr ba
  newArrayData size                       = liftM AD_CLLong $ unsafeNewArray_ size (*# 8#)
  unsafeReadArrayData (AD_CLLong ba) i    = CLLong <$> unsafeReadArray ba i
  unsafeWriteArrayData (AD_CLLong ba) i (CLLong e)
    = unsafeWriteArray ba i e
  unsafeFreezeArrayData (AD_CLLong ba)    = liftM AD_CLLong $ Unsafe.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_CLLong ba)   = sTUArrayPtr ba
  arrayElt                                = ArrayEltRcllong

instance ArrayElt CULLong where
  type ArrayPtrs CULLong = Ptr Word64
  unsafeIndexArrayData (AD_CULLong ba) i   = CULLong $ unsafeIndexArray ba i
  ptrsOfArrayData (AD_CULLong ba)          = uArrayPtr ba
  newArrayData size                        = liftM AD_CULLong $ unsafeNewArray_ size (*# 8#)
  unsafeReadArrayData (AD_CULLong ba) i    = CULLong <$> unsafeReadArray ba i
  unsafeWriteArrayData (AD_CULLong ba) i (CULLong e)
    = unsafeWriteArray ba i e
  unsafeFreezeArrayData (AD_CULLong ba)    = liftM AD_CULLong $ Unsafe.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_CULLong ba)   = sTUArrayPtr ba
  arrayElt                                 = ArrayEltRcullong

instance ArrayElt Float where
  type ArrayPtrs Float = Ptr Float
  unsafeIndexArrayData (AD_Float ba) i   = unsafeIndexArray ba i
  ptrsOfArrayData (AD_Float ba)          = uArrayPtr ba
  newArrayData size                      = liftM AD_Float $ unsafeNewArray_ size fLOAT_SCALE
  unsafeReadArrayData (AD_Float ba) i    = unsafeReadArray ba i
  unsafeWriteArrayData (AD_Float ba) i e = unsafeWriteArray ba i e
  unsafeFreezeArrayData (AD_Float ba)    = liftM AD_Float $ Unsafe.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_Float ba)   = sTUArrayPtr ba
  arrayElt                               = ArrayEltRfloat

instance ArrayElt Double where
  type ArrayPtrs Double = Ptr Double
  unsafeIndexArrayData (AD_Double ba) i   = unsafeIndexArray ba i
  ptrsOfArrayData (AD_Double ba)          = uArrayPtr ba
  newArrayData size                       = liftM AD_Double $ unsafeNewArray_ size dOUBLE_SCALE
  unsafeReadArrayData (AD_Double ba) i    = unsafeReadArray ba i
  unsafeWriteArrayData (AD_Double ba) i e = unsafeWriteArray ba i e
  unsafeFreezeArrayData (AD_Double ba)    = liftM AD_Double $ Unsafe.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_Double ba)   = sTUArrayPtr ba
  arrayElt                                = ArrayEltRdouble

instance ArrayElt CFloat where
  type ArrayPtrs CFloat = Ptr Float
  unsafeIndexArrayData (AD_CFloat ba) i   = CFloat $ unsafeIndexArray ba i
  ptrsOfArrayData (AD_CFloat ba)          = uArrayPtr ba
  newArrayData size                       = liftM AD_CFloat $ unsafeNewArray_ size fLOAT_SCALE
  unsafeReadArrayData (AD_CFloat ba) i    = CFloat <$> unsafeReadArray ba i
  unsafeWriteArrayData (AD_CFloat ba) i (CFloat e)
    = unsafeWriteArray ba i e
  unsafeFreezeArrayData (AD_CFloat ba)    = liftM AD_CFloat $ Unsafe.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_CFloat ba)   = sTUArrayPtr ba
  arrayElt                                = ArrayEltRcfloat

instance ArrayElt CDouble where
  type ArrayPtrs CDouble = Ptr Double
  unsafeIndexArrayData (AD_CDouble ba) i   = CDouble $ unsafeIndexArray ba i
  ptrsOfArrayData (AD_CDouble ba)          = uArrayPtr ba
  newArrayData size                        = liftM AD_CDouble $ unsafeNewArray_ size dOUBLE_SCALE
  unsafeReadArrayData (AD_CDouble ba) i    = CDouble <$> unsafeReadArray ba i
  unsafeWriteArrayData (AD_CDouble ba) i (CDouble e)
    = unsafeWriteArray ba i e
  unsafeFreezeArrayData (AD_CDouble ba)    = liftM AD_CDouble $ Unsafe.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_CDouble ba)   = sTUArrayPtr ba
  arrayElt                                 = ArrayEltRcdouble

-- Bool arrays are stored as arrays of bytes. While this is memory inefficient,
-- it is better suited to parallel backends than the native Unboxed Bool
-- array representation that uses packed bit vectors, as that would require
-- atomic operations when writing data necessarily serialising threads.
--
instance ArrayElt Bool where
  type ArrayPtrs Bool = Ptr Word8
  unsafeIndexArrayData (AD_Bool ba) i   = toBool (unsafeIndexArray ba i)
  ptrsOfArrayData (AD_Bool ba)          = uArrayPtr ba
  newArrayData size                     = liftM AD_Bool $ unsafeNewArray_ size (\x -> x)
  unsafeReadArrayData (AD_Bool ba) i    = liftM toBool  $ unsafeReadArray ba i
  unsafeWriteArrayData (AD_Bool ba) i e = unsafeWriteArray ba i (fromBool e)
  unsafeFreezeArrayData (AD_Bool ba)    = liftM AD_Bool $ Unsafe.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_Bool ba)   = sTUArrayPtr ba
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
  ptrsOfArrayData (AD_Char ba)          = uArrayPtr ba
  newArrayData size                     = liftM AD_Char $ unsafeNewArray_ size (*# 4#)
  unsafeReadArrayData (AD_Char ba) i    = unsafeReadArray ba i
  unsafeWriteArrayData (AD_Char ba) i e = unsafeWriteArray ba i e
  unsafeFreezeArrayData (AD_Char ba)    = liftM AD_Char $ Unsafe.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_Char ba)   = sTUArrayPtr ba
  arrayElt                              = ArrayEltRchar

instance ArrayElt CChar where
  type ArrayPtrs CChar = Ptr Int8
  unsafeIndexArrayData (AD_CChar ba) i   = CChar $ unsafeIndexArray ba i
  ptrsOfArrayData (AD_CChar ba)          = uArrayPtr ba
  newArrayData size                      = liftM AD_CChar $ unsafeNewArray_ size (\x -> x)
  unsafeReadArrayData (AD_CChar ba) i    = CChar <$> unsafeReadArray ba i
  unsafeWriteArrayData (AD_CChar ba) i (CChar e)
    = unsafeWriteArray ba i e
  unsafeFreezeArrayData (AD_CChar ba)    = liftM AD_CChar $ Unsafe.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_CChar ba)   = sTUArrayPtr ba
  arrayElt                               = ArrayEltRcchar

instance ArrayElt CSChar where
  type ArrayPtrs CSChar = Ptr Int8
  unsafeIndexArrayData (AD_CSChar ba) i   = CSChar $ unsafeIndexArray ba i
  ptrsOfArrayData (AD_CSChar ba)          = uArrayPtr ba
  newArrayData size                       = liftM AD_CSChar $ unsafeNewArray_ size (\x -> x)
  unsafeReadArrayData (AD_CSChar ba) i    = CSChar <$> unsafeReadArray ba i
  unsafeWriteArrayData (AD_CSChar ba) i (CSChar e)
    = unsafeWriteArray ba i e
  unsafeFreezeArrayData (AD_CSChar ba)    = liftM AD_CSChar $ Unsafe.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_CSChar ba)   = sTUArrayPtr ba
  arrayElt                                = ArrayEltRcschar

instance ArrayElt CUChar where
  type ArrayPtrs CUChar = Ptr Word8
  unsafeIndexArrayData (AD_CUChar ba) i   = CUChar $ unsafeIndexArray ba i
  ptrsOfArrayData (AD_CUChar ba)          = uArrayPtr ba
  newArrayData size                       = liftM AD_CUChar $ unsafeNewArray_ size (\x -> x)
  unsafeReadArrayData (AD_CUChar ba) i    = CUChar <$> unsafeReadArray ba i
  unsafeWriteArrayData (AD_CUChar ba) i (CUChar e)
    = unsafeWriteArray ba i e
  unsafeFreezeArrayData (AD_CUChar ba)    = liftM AD_CUChar $ Unsafe.unsafeFreeze ba
  ptrsOfMutableArrayData (AD_CUChar ba)   = sTUArrayPtr ba
  arrayElt                                = ArrayEltRcuchar

instance (ArrayElt a, ArrayElt b) => ArrayElt (a, b) where
  type ArrayPtrs (a, b)                = (ArrayPtrs a, ArrayPtrs b)
  unsafeIndexArrayData (AD_Pair a b) i = (unsafeIndexArrayData a i, unsafeIndexArrayData b i)
  ptrsOfArrayData (AD_Pair a b)        = (ptrsOfArrayData a, ptrsOfArrayData b)
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
             => (forall s. ST s (MutableArrayData s e, e)) -> (ArrayData e, e)
runArrayData st = runST $ do
                    (mad, r) <- st
                    ad       <- unsafeFreezeArrayData mad
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

-- Returns the element of an immutable array at the specified index.
--
-- This does no bounds checking unless you configured with -funsafe-checks. This
-- is usually OK, since the functions that convert from multidimensional to
-- linear indexing do bounds checking by default.
--
{-# INLINE unsafeIndexArray #-}
unsafeIndexArray :: IArray.IArray UArray e => UArray Int e -> Int -> e
#ifdef ACCELERATE_UNSAFE_CHECKS
unsafeIndexArray = IArray.!
#else
unsafeIndexArray = IArray.unsafeAt
#endif


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


-- Our own version of the 'STUArray' allocation that uses /pinned/ memory,
-- which is aligned to 16 bytes.
--
{-# INLINE unsafeNewArray_ #-}
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
{-# INLINE uArrayPtr #-}
uArrayPtr :: UArray Int a -> Ptr a
uArrayPtr (UArray _ _ _ ba) = Ptr (byteArrayContents# ba)

-- Obtains a pointer to the payload of an unboxed ST array.
--
-- PRECONDITION: The unboxed ST array must be pinned.
--
{-# INLINE sTUArrayPtr #-}
sTUArrayPtr :: STUArray s Int a -> ST s (Ptr a)
sTUArrayPtr (STUArray _ _ _ mba) = ST $ \s ->
  case unsafeFreezeByteArray# mba s of
    (# s', ba #) -> (# s', Ptr (byteArrayContents# ba) #)

