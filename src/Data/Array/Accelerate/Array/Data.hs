{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UnboxedTuples        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Array.Data
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
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
  ArrayData, MutableArrayData, ScalarArrayData, GArrayDataR, ScalarArrayDataR,
  runArrayData,
  newArrayData,
  indexArrayData, readArrayData, writeArrayData,
  unsafeArrayDataPtr,
  touchArrayData,
  rnfArrayData,

  -- * Allocator internals
  registerForeignPtrAllocator,

  -- * TemplateHaskell
  liftArrayData,

) where

import Data.Array.Accelerate.Array.Unique
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Type
import Data.Primitive.Bit
import Data.Primitive.Vec
#ifdef ACCELERATE_DEBUG
import Data.Array.Accelerate.Lifetime
#endif

import Data.Array.Accelerate.Debug.Internal.Flags
import Data.Array.Accelerate.Debug.Internal.Profile
import Data.Array.Accelerate.Debug.Internal.Trace

import Control.Applicative
import Control.DeepSeq
import Control.Monad                                                ( (<=<) )
import Data.Bits                                                    ( testBit, setBit, clearBit )
import Data.IORef
import Foreign.ForeignPtr
import Formatting                                                   hiding ( bytes )
import Language.Haskell.TH.Extra                                    hiding ( Type )
import System.IO.Unsafe
import Prelude                                                      hiding ( mapM )

import GHC.Exts                                                     hiding ( build )
import GHC.ForeignPtr
import GHC.TypeLits
import GHC.Types


-- | Immutable array representation
--
type ArrayData e = MutableArrayData e

-- | Mutable array representation
--
type MutableArrayData e = GArrayDataR UniqueArray e

-- | Underlying array representation.
--
-- NOTE: We use a standard (non-strict) pair to enable lazy device-host data transfers
--
type family GArrayDataR ba a where
  GArrayDataR ba ()     = ()
  GArrayDataR ba (a, b) = (GArrayDataR ba a, GArrayDataR ba b)
  GArrayDataR ba a      = ba (ScalarArrayDataR a)

type ScalarArrayData a = UniqueArray (ScalarArrayDataR a)

-- | Mapping from scalar type to the type as represented in memory in an array
--
type family ScalarArrayDataR t where
  ScalarArrayDataR Bit              = Word8
  ScalarArrayDataR Int8             = Int8
  ScalarArrayDataR Int16            = Int16
  ScalarArrayDataR Int32            = Int32
  ScalarArrayDataR Int64            = Int64
  ScalarArrayDataR Int128           = Int128
  ScalarArrayDataR Word8            = Word8
  ScalarArrayDataR Word16           = Word16
  ScalarArrayDataR Word32           = Word32
  ScalarArrayDataR Word64           = Word64
  ScalarArrayDataR Word128          = Word128
  ScalarArrayDataR Half             = Half
  ScalarArrayDataR Float            = Float
  ScalarArrayDataR Double           = Double
  ScalarArrayDataR Float128         = Float128
  --
  ScalarArrayDataR (Vec n Bit)      = BitMask n
  ScalarArrayDataR (Vec n Int8)     = Vec n Int8
  ScalarArrayDataR (Vec n Int16)    = Vec n Int16
  ScalarArrayDataR (Vec n Int32)    = Vec n Int32
  ScalarArrayDataR (Vec n Int64)    = Vec n Int64
  ScalarArrayDataR (Vec n Int128)   = Vec n Int128
  ScalarArrayDataR (Vec n Word8)    = Vec n Word8
  ScalarArrayDataR (Vec n Word16)   = Vec n Word16
  ScalarArrayDataR (Vec n Word32)   = Vec n Word32
  ScalarArrayDataR (Vec n Word64)   = Vec n Word64
  ScalarArrayDataR (Vec n Word128)  = Vec n Word128
  ScalarArrayDataR (Vec n Half)     = Vec n Half
  ScalarArrayDataR (Vec n Float)    = Vec n Float
  ScalarArrayDataR (Vec n Double)   = Vec n Double
  ScalarArrayDataR (Vec n Float128) = Vec n Float128


-- Array operations
-- ----------------

newArrayData :: HasCallStack => TypeR e -> Int -> IO (MutableArrayData e)
newArrayData TupRunit         !_    = return ()
newArrayData (TupRpair t1 t2) !size = (,) <$> newArrayData t1 size <*> newArrayData t2 size
newArrayData (TupRsingle _t)  !size = scalar _t
  where
    scalar :: ScalarType t -> IO (MutableArrayData t)
    scalar (NumScalarType t) = num t
    scalar (BitScalarType t) = bit t

    bit :: BitType t -> IO (MutableArrayData t)
    bit TypeBit      = allocateArray ((size + 7) `quot` 8)
    bit (TypeMask n) = allocateArray (((size * fromInteger (natVal' n)) + 7) `quot` 8)

    num :: NumType t -> IO (MutableArrayData t)
    num (IntegralNumType t) = integral t
    num (FloatingNumType t) = floating t

    integral :: IntegralType t -> IO (MutableArrayData t)
    integral = \case
      SingleIntegralType t   -> single t
      VectorIntegralType n t -> vector n (fromInteger $ natVal' n) t
      where
        single :: SingleIntegralType t -> IO (MutableArrayData t)
        single = \case
          TypeInt8    -> allocateArray size
          TypeInt16   -> allocateArray (2 * size)
          TypeInt32   -> allocateArray (4 * size)
          TypeInt64   -> allocateArray (8 * size)
          TypeInt128  -> allocateArray (16 * size)
          TypeWord8   -> allocateArray size
          TypeWord16  -> allocateArray (2 * size)
          TypeWord32  -> allocateArray (4 * size)
          TypeWord64  -> allocateArray (8 * size)
          TypeWord128 -> allocateArray (16 * size)

        vector :: Proxy# n -> Int -> SingleIntegralType t -> IO (MutableArrayData (Vec n t))
        vector _ !k = \case
          TypeInt8    -> allocateArray (k * size)
          TypeInt16   -> allocateArray (2 * k * size)
          TypeInt32   -> allocateArray (4 * k * size)
          TypeInt64   -> allocateArray (8 * k * size)
          TypeInt128  -> allocateArray (16 * k * size)
          TypeWord8   -> allocateArray (k * size)
          TypeWord16  -> allocateArray (2 * k * size)
          TypeWord32  -> allocateArray (4 * k * size)
          TypeWord64  -> allocateArray (8 * k * size)
          TypeWord128 -> allocateArray (16 * k * size)

    floating :: FloatingType t -> IO (MutableArrayData t)
    floating = \case
      SingleFloatingType t   -> single t
      VectorFloatingType n t -> vector n (fromInteger $ natVal' n) t
      where
        single :: SingleFloatingType t -> IO (MutableArrayData t)
        single = \case
          TypeFloat16  -> allocateArray (2 * size)
          TypeFloat32  -> allocateArray (4 * size)
          TypeFloat64  -> allocateArray (8 * size)
          TypeFloat128 -> allocateArray (16 * size)

        vector :: Proxy# n -> Int -> SingleFloatingType t -> IO (MutableArrayData (Vec n t))
        vector _ !k = \case
          TypeFloat16  -> allocateArray (2 * k * size)
          TypeFloat32  -> allocateArray (4 * k * size)
          TypeFloat64  -> allocateArray (8 * k * size)
          TypeFloat128 -> allocateArray (16 * k * size)


indexArrayData :: TypeR e -> ArrayData e -> Int -> e
indexArrayData tR arr ix = unsafePerformIO $ readArrayData tR arr ix

readArrayData :: TypeR e -> MutableArrayData e -> Int -> IO e
readArrayData TupRunit         ()       !_  = return ()
readArrayData (TupRpair t1 t2) (a1, a2) !ix = (,) <$> readArrayData t1 a1 ix <*> readArrayData t2 a2 ix
readArrayData (TupRsingle _t)  arr      !ix = scalar _t arr ix
  where
    scalar :: ScalarType t -> MutableArrayData t -> Int -> IO t
    scalar (NumScalarType t) = num t
    scalar (BitScalarType t) = bit t

    bit :: BitType t -> MutableArrayData t -> Int -> IO t
    bit TypeMask{} ua i = unMask <$> unsafeReadArray ua i
    bit TypeBit    ua i =
      let (q,r) = quotRem i 8
      in do
        w <- unsafeReadArray ua q
        return $ Bit (testBit w r)

    num :: NumType t -> MutableArrayData t -> Int -> IO t
    num (IntegralNumType t) = integral t
    num (FloatingNumType t) = floating t

    integral :: IntegralType t -> MutableArrayData t -> Int -> IO t
    integral = \case
      SingleIntegralType t   -> single t
      VectorIntegralType n t -> vector n t
      where
        single :: SingleIntegralType t -> MutableArrayData t -> Int -> IO t
        single = \case
          TypeInt8    -> unsafeReadArray
          TypeInt16   -> unsafeReadArray
          TypeInt32   -> unsafeReadArray
          TypeInt64   -> unsafeReadArray
          TypeInt128  -> unsafeReadArray
          TypeWord8   -> unsafeReadArray
          TypeWord16  -> unsafeReadArray
          TypeWord32  -> unsafeReadArray
          TypeWord64  -> unsafeReadArray
          TypeWord128 -> unsafeReadArray

        vector :: KnownNat n => Proxy# n -> SingleIntegralType t -> MutableArrayData (Vec n t) -> Int -> IO (Vec n t)
        vector _ = \case
          TypeInt8    -> unsafeReadArray
          TypeInt16   -> unsafeReadArray
          TypeInt32   -> unsafeReadArray
          TypeInt64   -> unsafeReadArray
          TypeInt128  -> unsafeReadArray
          TypeWord8   -> unsafeReadArray
          TypeWord16  -> unsafeReadArray
          TypeWord32  -> unsafeReadArray
          TypeWord64  -> unsafeReadArray
          TypeWord128 -> unsafeReadArray

    floating :: FloatingType t -> MutableArrayData t -> Int -> IO t
    floating = \case
      SingleFloatingType t   -> single t
      VectorFloatingType n t -> vector n t
      where
        single :: SingleFloatingType t -> MutableArrayData t -> Int -> IO t
        single = \case
          TypeFloat16  -> unsafeReadArray
          TypeFloat32  -> unsafeReadArray
          TypeFloat64  -> unsafeReadArray
          TypeFloat128 -> unsafeReadArray

        vector :: KnownNat n => Proxy# n -> SingleFloatingType t -> MutableArrayData (Vec n t) -> Int -> IO (Vec n t)
        vector _ = \case
          TypeFloat16  -> unsafeReadArray
          TypeFloat32  -> unsafeReadArray
          TypeFloat64  -> unsafeReadArray
          TypeFloat128 -> unsafeReadArray


writeArrayData :: TypeR e -> MutableArrayData e -> Int -> e -> IO ()
writeArrayData TupRunit         ()       !_  !()      = return ()
writeArrayData (TupRpair t1 t2) (a1, a2) !ix (v1, v2) = writeArrayData t1 a1 ix v1 >> writeArrayData t2 a2 ix v2
writeArrayData (TupRsingle _t)  arr      !ix !val     = scalar _t arr ix val
  where
    scalar :: ScalarType t -> MutableArrayData t -> Int -> t -> IO ()
    scalar (NumScalarType t) = num t
    scalar (BitScalarType t) = bit t

    bit :: BitType t -> MutableArrayData t -> Int -> t -> IO ()
    bit TypeMask{} ua i m       = unsafeWriteArray ua i (BitMask m)
    bit TypeBit    ua i (Bit b) =
      let (q,r)    = quotRem i 8
          update x = if b then setBit x r else clearBit x r
      in do
        w <- unsafeReadArray ua q
        unsafeWriteArray ua q (update w)

    num :: NumType t -> MutableArrayData t -> Int -> t -> IO ()
    num (IntegralNumType t) = integral t
    num (FloatingNumType t) = floating t

    integral :: IntegralType t -> MutableArrayData t -> Int -> t -> IO ()
    integral = \case
      SingleIntegralType t   -> single t
      VectorIntegralType n t -> vector n t
      where
        single :: SingleIntegralType t -> MutableArrayData t -> Int -> t -> IO ()
        single = \case
          TypeInt8    -> unsafeWriteArray
          TypeInt16   -> unsafeWriteArray
          TypeInt32   -> unsafeWriteArray
          TypeInt64   -> unsafeWriteArray
          TypeInt128  -> unsafeWriteArray
          TypeWord8   -> unsafeWriteArray
          TypeWord16  -> unsafeWriteArray
          TypeWord32  -> unsafeWriteArray
          TypeWord64  -> unsafeWriteArray
          TypeWord128 -> unsafeWriteArray

        vector :: KnownNat n => Proxy# n -> SingleIntegralType t -> MutableArrayData (Vec n t) -> Int -> Vec n t -> IO ()
        vector _ = \case
          TypeInt8    -> unsafeWriteArray
          TypeInt16   -> unsafeWriteArray
          TypeInt32   -> unsafeWriteArray
          TypeInt64   -> unsafeWriteArray
          TypeInt128  -> unsafeWriteArray
          TypeWord8   -> unsafeWriteArray
          TypeWord16  -> unsafeWriteArray
          TypeWord32  -> unsafeWriteArray
          TypeWord64  -> unsafeWriteArray
          TypeWord128 -> unsafeWriteArray

    floating :: FloatingType t -> MutableArrayData t -> Int -> t -> IO ()
    floating = \case
      SingleFloatingType t   -> single t
      VectorFloatingType n t -> vector n t
      where
        single :: SingleFloatingType t -> MutableArrayData t -> Int -> t -> IO ()
        single = \case
          TypeFloat16  -> unsafeWriteArray
          TypeFloat32  -> unsafeWriteArray
          TypeFloat64  -> unsafeWriteArray
          TypeFloat128 -> unsafeWriteArray

        vector :: KnownNat n => Proxy# n -> SingleFloatingType t -> MutableArrayData (Vec n t) -> Int -> Vec n t -> IO ()
        vector _ = \case
          TypeFloat16  -> unsafeWriteArray
          TypeFloat32  -> unsafeWriteArray
          TypeFloat64  -> unsafeWriteArray
          TypeFloat128 -> unsafeWriteArray


unsafeArrayDataPtr :: ScalarType e -> ArrayData e -> Ptr (ScalarArrayDataR e)
unsafeArrayDataPtr = scalar
  where
    scalar :: ScalarType t -> ArrayData t -> Ptr (ScalarArrayDataR t)
    scalar (NumScalarType t) = num t
    scalar (BitScalarType t) = bit t

    bit :: BitType t -> ArrayData t -> Ptr (ScalarArrayDataR t)
    bit TypeBit    = unsafeUniqueArrayPtr
    bit TypeMask{} = unsafeUniqueArrayPtr

    num :: NumType t -> ArrayData t -> Ptr (ScalarArrayDataR t)
    num (IntegralNumType t) = integral t
    num (FloatingNumType t) = floating t

    integral :: IntegralType t -> ArrayData t -> Ptr (ScalarArrayDataR t)
    integral = \case
      SingleIntegralType t   -> single t
      VectorIntegralType n t -> vector n t
      where
        single :: SingleIntegralType t -> ArrayData t -> Ptr (ScalarArrayDataR t)
        single = \case
          TypeInt8    -> unsafeUniqueArrayPtr
          TypeInt16   -> unsafeUniqueArrayPtr
          TypeInt32   -> unsafeUniqueArrayPtr
          TypeInt64   -> unsafeUniqueArrayPtr
          TypeInt128  -> unsafeUniqueArrayPtr
          TypeWord8   -> unsafeUniqueArrayPtr
          TypeWord16  -> unsafeUniqueArrayPtr
          TypeWord32  -> unsafeUniqueArrayPtr
          TypeWord64  -> unsafeUniqueArrayPtr
          TypeWord128 -> unsafeUniqueArrayPtr

        vector :: KnownNat n => Proxy# n -> SingleIntegralType t -> ArrayData (Vec n t) -> Ptr (ScalarArrayDataR (Vec n t))
        vector _ = \case
          TypeInt8    -> unsafeUniqueArrayPtr
          TypeInt16   -> unsafeUniqueArrayPtr
          TypeInt32   -> unsafeUniqueArrayPtr
          TypeInt64   -> unsafeUniqueArrayPtr
          TypeInt128  -> unsafeUniqueArrayPtr
          TypeWord8   -> unsafeUniqueArrayPtr
          TypeWord16  -> unsafeUniqueArrayPtr
          TypeWord32  -> unsafeUniqueArrayPtr
          TypeWord64  -> unsafeUniqueArrayPtr
          TypeWord128 -> unsafeUniqueArrayPtr

    floating :: FloatingType t -> ArrayData t -> Ptr (ScalarArrayDataR t)
    floating = \case
      SingleFloatingType t   -> single t
      VectorFloatingType n t -> vector n t
      where
        single :: SingleFloatingType t -> ArrayData t -> Ptr (ScalarArrayDataR t)
        single = \case
          TypeFloat16  -> unsafeUniqueArrayPtr
          TypeFloat32  -> unsafeUniqueArrayPtr
          TypeFloat64  -> unsafeUniqueArrayPtr
          TypeFloat128 -> unsafeUniqueArrayPtr

        vector :: KnownNat n => Proxy# n -> SingleFloatingType t -> ArrayData (Vec n t) -> Ptr (ScalarArrayDataR (Vec n t))
        vector _ = \case
          TypeFloat16  -> unsafeUniqueArrayPtr
          TypeFloat32  -> unsafeUniqueArrayPtr
          TypeFloat64  -> unsafeUniqueArrayPtr
          TypeFloat128 -> unsafeUniqueArrayPtr

touchArrayData :: TupR ScalarType e -> ArrayData e -> IO ()
touchArrayData TupRunit         ()       = return ()
touchArrayData (TupRpair t1 t2) (a1, a2) = touchArrayData t1 a1 >> touchArrayData t2 a2
touchArrayData (TupRsingle ta)  arr      = scalar ta arr
  where
    scalar :: ScalarType t -> ArrayData t -> IO ()
    scalar (NumScalarType t) = num t
    scalar (BitScalarType t) = bit t

    bit :: BitType t -> ArrayData t -> IO ()
    bit TypeBit    = touchUniqueArray
    bit TypeMask{} = touchUniqueArray

    num :: NumType t -> ArrayData t -> IO ()
    num (IntegralNumType t) = integral t
    num (FloatingNumType t) = floating t

    integral :: IntegralType t -> ArrayData t -> IO ()
    integral = \case
      SingleIntegralType t   -> single t
      VectorIntegralType n t -> vector n t
      where
        single :: SingleIntegralType t -> ArrayData t -> IO ()
        single = \case
          TypeInt8    -> touchUniqueArray
          TypeInt16   -> touchUniqueArray
          TypeInt32   -> touchUniqueArray
          TypeInt64   -> touchUniqueArray
          TypeInt128  -> touchUniqueArray
          TypeWord8   -> touchUniqueArray
          TypeWord16  -> touchUniqueArray
          TypeWord32  -> touchUniqueArray
          TypeWord64  -> touchUniqueArray
          TypeWord128 -> touchUniqueArray

        vector :: KnownNat n => Proxy# n -> SingleIntegralType t -> ArrayData (Vec n t) -> IO ()
        vector _ = \case
          TypeInt8    -> touchUniqueArray
          TypeInt16   -> touchUniqueArray
          TypeInt32   -> touchUniqueArray
          TypeInt64   -> touchUniqueArray
          TypeInt128  -> touchUniqueArray
          TypeWord8   -> touchUniqueArray
          TypeWord16  -> touchUniqueArray
          TypeWord32  -> touchUniqueArray
          TypeWord64  -> touchUniqueArray
          TypeWord128 -> touchUniqueArray

    floating :: FloatingType t -> ArrayData t -> IO ()
    floating = \case
      SingleFloatingType t   -> single t
      VectorFloatingType n t -> vector n t
      where
        single :: SingleFloatingType t -> ArrayData t -> IO ()
        single = \case
          TypeFloat16  -> touchUniqueArray
          TypeFloat32  -> touchUniqueArray
          TypeFloat64  -> touchUniqueArray
          TypeFloat128 -> touchUniqueArray

        vector :: KnownNat n => Proxy# n -> SingleFloatingType t -> ArrayData (Vec n t) -> IO ()
        vector _ = \case
          TypeFloat16  -> touchUniqueArray
          TypeFloat32  -> touchUniqueArray
          TypeFloat64  -> touchUniqueArray
          TypeFloat128 -> touchUniqueArray

rnfArrayData :: TupR ScalarType e -> ArrayData e -> ()
rnfArrayData TupRunit         ()       = ()
rnfArrayData (TupRpair t1 t2) (a1, a2) = rnfArrayData t1 a1 `seq` rnfArrayData t2 a2 `seq` ()
rnfArrayData (TupRsingle t)   arr      = rnf (unsafeArrayDataPtr t arr)


-- | Safe combination of creating and fast freezing of array data.
--
runArrayData
    :: IO (MutableArrayData e, e)
    -> (ArrayData e, e)
runArrayData st = unsafePerformIO $ do
  (mad, r) <- st
  return (mad, r)

-- Allocate a new array of the given number of bytes.
--
-- The array is uninitialised and, in particular, allocated lazily. The latter
-- is important because it means that for backends that have discrete memory
-- spaces (e.g. GPUs), we will not increase host memory pressure simply to track
-- intermediate arrays that contain meaningful data only on the device.
--
allocateArray :: forall e. HasCallStack => Int -> IO (UniqueArray e)
allocateArray !size = internalCheck "size must be >= 0" (size >= 0) $ do
  arr <- newUniqueArray <=< unsafeInterleaveIO $ do
           new <- readIORef __mallocForeignPtrBytes
           ptr <- new size
           traceM dump_gc ("gc: allocated new host array (size=" % int % ", ptr=" % build % ")") size (unsafeForeignPtrToPtr ptr)
           local_memory_alloc (unsafeForeignPtrToPtr ptr) size
           return (castForeignPtr ptr)
#ifdef ACCELERATE_DEBUG
  addFinalizer (uniqueArrayData arr) (local_memory_free (unsafeUniqueArrayPtr arr))
#endif
  return arr

-- | Register the given function as the callback to use to allocate new array
-- data on the host containing the specified number of bytes. The returned array
-- must be pinned (with respect to Haskell's GC), so that it can be passed to
-- foreign code.
--
registerForeignPtrAllocator
    :: (Int -> IO (ForeignPtr Word8))
    -> IO ()
registerForeignPtrAllocator new = do
  traceM dump_gc "registering new array allocator"
  atomicWriteIORef __mallocForeignPtrBytes new

{-# NOINLINE __mallocForeignPtrBytes #-}
__mallocForeignPtrBytes :: IORef (Int -> IO (ForeignPtr Word8))
__mallocForeignPtrBytes = unsafePerformIO $! newIORef mallocPlainForeignPtrBytesAligned

-- | Allocate the given number of bytes with 64-byte (cache line)
-- alignment. This is essential for SIMD instructions.
--
-- Additionally, we return a plain ForeignPtr, which unlike a regular ForeignPtr
-- created with 'mallocForeignPtr' carries no finalisers. It is an error to try
-- to add a finaliser to the plain ForeignPtr. For our purposes this is fine,
-- since in Accelerate finalisers are handled using Lifetime
--
mallocPlainForeignPtrBytesAligned :: Int -> IO (ForeignPtr a)
mallocPlainForeignPtrBytesAligned (I# size#) = IO $ \s0 ->
  case newAlignedPinnedByteArray# size# 64# s0 of
    (# s1, mbarr# #) -> (# s1, ForeignPtr (byteArrayContents# (unsafeCoerce# mbarr#)) (PlainPtr mbarr#) #)


liftArrayData :: Int -> TypeR e -> ArrayData e -> CodeQ (ArrayData e)
liftArrayData !size = tuple
  where
    tuple :: TypeR e -> ArrayData e -> CodeQ (ArrayData e)
    tuple TupRunit         ()       = [|| () ||]
    tuple (TupRpair t1 t2) (a1, a2) = [|| ($$(tuple t1 a1), $$(tuple t2 a2)) ||]
    tuple (TupRsingle s) adata      = scalar s adata

    scalar :: ScalarType e -> ArrayData e -> CodeQ (ArrayData e)
    scalar (NumScalarType t) = num t
    scalar (BitScalarType t) = bit t

    bit :: BitType e -> ArrayData e -> CodeQ (ArrayData e)
    bit TypeBit      ua = liftUniqueArray ((size + 7) `quot` 8) ua
    bit (TypeMask n) ua = liftUniqueArray (((size * fromInteger (natVal' n)) + 7) `quot` 8) ua

    num :: NumType e -> ArrayData e -> CodeQ (ArrayData e)
    num (IntegralNumType t) = integral t
    num (FloatingNumType t) = floating t

    integral :: IntegralType e -> ArrayData e -> CodeQ (ArrayData e)
    integral = \case
      SingleIntegralType t   -> single t size
      VectorIntegralType n t -> vector n t (size * fromInteger (natVal' n))
      where
        single :: SingleIntegralType e -> Int -> ArrayData e -> CodeQ (ArrayData e)
        single TypeInt8    = liftUniqueArray
        single TypeInt16   = liftUniqueArray
        single TypeInt32   = liftUniqueArray
        single TypeInt64   = liftUniqueArray
        single TypeInt128  = liftUniqueArray
        single TypeWord8   = liftUniqueArray
        single TypeWord16  = liftUniqueArray
        single TypeWord32  = liftUniqueArray
        single TypeWord64  = liftUniqueArray
        single TypeWord128 = liftUniqueArray

        vector :: KnownNat n => Proxy# n -> SingleIntegralType e -> Int -> ArrayData (Vec n e) -> CodeQ (ArrayData (Vec n e))
        vector _ TypeInt8    = liftUniqueArray
        vector _ TypeInt16   = liftUniqueArray
        vector _ TypeInt32   = liftUniqueArray
        vector _ TypeInt64   = liftUniqueArray
        vector _ TypeInt128  = liftUniqueArray
        vector _ TypeWord8   = liftUniqueArray
        vector _ TypeWord16  = liftUniqueArray
        vector _ TypeWord32  = liftUniqueArray
        vector _ TypeWord64  = liftUniqueArray
        vector _ TypeWord128 = liftUniqueArray

    floating :: FloatingType e -> ArrayData e -> CodeQ (ArrayData e)
    floating = \case
      SingleFloatingType t   -> single t size
      VectorFloatingType n t -> vector n t (size * fromInteger (natVal' n))
      where
        single :: SingleFloatingType e -> Int -> ArrayData e -> CodeQ (ArrayData e)
        single TypeFloat16  = liftUniqueArray
        single TypeFloat32  = liftUniqueArray
        single TypeFloat64  = liftUniqueArray
        single TypeFloat128 = liftUniqueArray

        vector :: KnownNat n => Proxy# n -> SingleFloatingType e -> Int -> ArrayData (Vec n e) -> CodeQ (ArrayData (Vec n e))
        vector _ TypeFloat16  = liftUniqueArray
        vector _ TypeFloat32  = liftUniqueArray
        vector _ TypeFloat64  = liftUniqueArray
        vector _ TypeFloat128 = liftUniqueArray

