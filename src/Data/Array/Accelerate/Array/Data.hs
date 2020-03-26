{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UnboxedTuples          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Array.Data
-- Copyright   : [2008..2019] The Accelerate Team
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
  ArrayData, MutableArrayData, runArrayData, GArrayData, rnfArrayData, ScalarData, ScalarDataRepr,
  unsafeIndexArrayData, ptrOfArrayData, touchArrayData, newArrayData, unsafeReadArrayData, unsafeWriteArrayData,

  -- * Type macros
  HTYPE_INT, HTYPE_WORD, HTYPE_CLONG, HTYPE_CULONG, HTYPE_CCHAR,

  -- * Allocator internals
  registerForeignPtrAllocator,

  -- * Utilities for type classes
  ScalarDict(..), scalarDict, singleDict

) where

-- friends
import Data.Array.Accelerate.Array.Unique
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Orphans                                ()  -- Prim Half
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.Debug.Flags
import Data.Array.Accelerate.Debug.Monitoring
import Data.Array.Accelerate.Debug.Trace

-- standard libraries
import Control.Applicative
import Control.Monad                                                ( (<=<) )
import Control.DeepSeq
import Data.Bits
import Data.IORef
import Data.Primitive                                               ( sizeOf# )
import Foreign.ForeignPtr
import Foreign.Storable
import Language.Haskell.TH                                          hiding ( Type )
import System.IO.Unsafe
import Text.Printf
import Prelude                                                      hiding ( mapM )

import GHC.Base
import GHC.ForeignPtr
import GHC.Ptr
import Data.Primitive.Types                                         ( Prim )


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

$( runQ [d| type HTYPE_CLONG = $(
              case finiteBitSize (undefined::CLong) of
                32 -> [t| Int32 |]
                64 -> [t| Int64 |]
                _  -> error "I don't know what architecture I am" ) |] )

$( runQ [d| type HTYPE_CULONG = $(
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

-- | Immutable array representation
--
type ArrayData e = MutableArrayData e

-- | Mutable array representation
--
type MutableArrayData e = GArrayData e

-- Underlying array representation.
--
-- In previous versions this was abstracted over by the mutable/immutable array
-- representation, but this is now fixed to our UniqueArray type.
--
type family GArrayData a where
  GArrayData ()        = ()
  GArrayData (a, b)    = (GArrayData a, GArrayData b) -- XXX: fields of tuple are non-strict, which enables lazy device-host copying
  GArrayData a         = ScalarData a

type ScalarData a = UniqueArray (ScalarDataRepr a)

-- Mapping from scalar type to the type as represented in memory in an array.
-- Booleans are stored as Word8, other types are represented as itself.
type family ScalarDataRepr tp where
  ScalarDataRepr Int    = Int
  ScalarDataRepr Int8   = Int8
  ScalarDataRepr Int16  = Int16
  ScalarDataRepr Int32  = Int32
  ScalarDataRepr Int64  = Int64
  ScalarDataRepr Word   = Word
  ScalarDataRepr Word8  = Word8
  ScalarDataRepr Word16 = Word16
  ScalarDataRepr Word32 = Word32
  ScalarDataRepr Word64 = Word64
  ScalarDataRepr Half   = Half
  ScalarDataRepr Float  = Float
  ScalarDataRepr Double = Double
  ScalarDataRepr Bool   = Word8
  ScalarDataRepr Char   = Char
  ScalarDataRepr (Vec n tp) = ScalarDataRepr tp

-- Utilities for working with the type families & type class instances
data ScalarDict e where
  ScalarDict :: (Storable (ScalarDataRepr e), Prim (ScalarDataRepr e), ArrayData e ~ ScalarData e) => ScalarDict e

{-# INLINE scalarDict #-}
scalarDict :: ScalarType e -> (Int, ScalarDict e)
scalarDict (SingleScalarType tp)
  | (dict, _, _) <- singleDict tp = (1, dict)
scalarDict (VectorScalarType (VectorType n tp))
  | (ScalarDict, _, _) <- singleDict tp = (n, ScalarDict)

{-# INLINE singleDict #-}
singleDict :: SingleType e -> (ScalarDict e, e -> ScalarDataRepr e, ScalarDataRepr e -> e)
singleDict (NonNumSingleType TypeBool) = (ScalarDict, fromBool, toBool)
singleDict (NonNumSingleType TypeChar) = (ScalarDict, id, id)
singleDict (NumSingleType (IntegralNumType tp)) = case tp of
  TypeInt    -> (ScalarDict, id, id)
  TypeInt8   -> (ScalarDict, id, id)
  TypeInt16  -> (ScalarDict, id, id)
  TypeInt32  -> (ScalarDict, id, id)
  TypeInt64  -> (ScalarDict, id, id)
  TypeWord   -> (ScalarDict, id, id)
  TypeWord8  -> (ScalarDict, id, id)
  TypeWord16 -> (ScalarDict, id, id)
  TypeWord32 -> (ScalarDict, id, id)
  TypeWord64 -> (ScalarDict, id, id)
singleDict (NumSingleType (FloatingNumType tp)) = case tp of
  TypeHalf   -> (ScalarDict, id, id)
  TypeFloat  -> (ScalarDict, id, id)
  TypeDouble -> (ScalarDict, id, id)

-- Array operations
-- ----------------

-- Reads an element from an array
unsafeIndexArrayData :: TupleType e -> ArrayData e -> Int -> e
unsafeIndexArrayData TupRunit () !_ = ()
unsafeIndexArrayData (TupRpair t1 t2) (a1, a2) !ix = (unsafeIndexArrayData t1 a1 ix, unsafeIndexArrayData t2 a2 ix)
unsafeIndexArrayData (TupRsingle (SingleScalarType tp)) arr ix
  | (ScalarDict, _, to) <- singleDict tp = to $! unsafeIndexArray arr ix
-- VectorScalarType is handled in unsafeReadArrayData
unsafeIndexArrayData !tp !arr !ix = unsafePerformIO $! unsafeReadArrayData tp arr ix

ptrOfArrayData :: ScalarType e -> ArrayData e -> Ptr (ScalarDataRepr e)
ptrOfArrayData tp arr 
  | (_, ScalarDict) <- scalarDict tp = unsafeUniqueArrayPtr arr

touchArrayData :: TupleType e -> ArrayData e -> IO ()
touchArrayData TupRunit () = return ()
touchArrayData (TupRpair t1 t2) (a1, a2) = touchArrayData t1 a1 >> touchArrayData t2 a2
touchArrayData (TupRsingle tp) arr
  | (_, ScalarDict) <- scalarDict tp = touchUniqueArray arr

newArrayData :: TupleType e -> Int -> IO (MutableArrayData e)
newArrayData TupRunit         !_     = return ()
newArrayData (TupRpair t1 t2) !size  = (,) <$> newArrayData t1 size <*> newArrayData t2 size
newArrayData (TupRsingle tp)  !size
  | (n, ScalarDict) <- scalarDict tp = newArrayData' (n * size)

unsafeReadArrayData :: forall e. TupleType e -> MutableArrayData e -> Int -> IO e
unsafeReadArrayData TupRunit () !_ = return ()
unsafeReadArrayData (TupRpair t1 t2) (a1, a2) !ix = (,) <$> unsafeReadArrayData t1 a1 ix <*> unsafeReadArrayData t2 a2 ix
unsafeReadArrayData (TupRsingle (SingleScalarType tp)) arr !ix
  | (ScalarDict, _, to) <- singleDict tp = to <$> unsafeReadArray arr ix
unsafeReadArrayData (TupRsingle (VectorScalarType (VectorType (I# w#) tp))) arr (I# ix#)
  | (ScalarDict, _, _) <- singleDict tp =
    let
      !bytes# = w# *# sizeOf# (undefined :: ScalarDataRepr e)
      !addr#  = unPtr# (unsafeUniqueArrayPtr arr) `plusAddr#` (ix# *# bytes#)
    in
      IO $ \s ->
        case newByteArray# bytes# s                       of { (# s1, mba# #) ->
        case copyAddrToByteArray# addr# mba# 0# bytes# s1 of { s2             ->
        case unsafeFreezeByteArray# mba# s2               of { (# s3, ba# #)  ->
          (# s3, Vec ba# #)
        }}}

unsafeWriteArrayData :: forall e. TupleType e -> MutableArrayData e -> Int -> e -> IO ()
unsafeWriteArrayData TupRunit () !_ () = return ()
unsafeWriteArrayData (TupRpair t1 t2) (a1, a2) !ix (v1, v2)
  =  unsafeWriteArrayData t1 a1 ix v1
  >> unsafeWriteArrayData t2 a2 ix v2
unsafeWriteArrayData (TupRsingle (SingleScalarType tp)) arr !ix !val
  | (ScalarDict, from, _) <- singleDict tp = unsafeWriteArray arr ix (from val)
unsafeWriteArrayData (TupRsingle (VectorScalarType (VectorType (I# w#) tp))) arr !(I# ix#) (Vec ba# :: Vec n t)
  | (ScalarDict, _, _) <- singleDict tp =
    let
      !bytes# = w# *# sizeOf# (undefined :: ScalarDataRepr e)
      !addr#  = unPtr# (unsafeUniqueArrayPtr arr) `plusAddr#` (ix# *# bytes#)
    in
      IO $ \s -> case copyByteArrayToAddr# ba# 0# addr# bytes# s of
        s1 -> (# s1, () #)

rnfArrayData :: TupleType e -> ArrayData e -> ()
rnfArrayData TupRunit         ()       = ()
rnfArrayData (TupRpair t1 t2) (a1, a2) = rnfArrayData t1 a1 `seq` rnfArrayData t2 a2
rnfArrayData (TupRsingle tp)  arr      = rnf $ ptrOfArrayData tp arr

-- Auxiliary functions
-- -------------------

{-# INLINE unPtr# #-}
unPtr# :: Ptr a -> Addr#
unPtr# (Ptr addr#) = addr#

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
unsafeIndexArray !ua !i =
  unsafePerformIO $! unsafeReadArray ua i

-- Read an element from a mutable array at the given index. This does no bounds
-- checking.
--
{-# INLINE unsafeReadArray #-}
unsafeReadArray :: Storable e => UniqueArray e -> Int -> IO e
unsafeReadArray !ua !i =
  withUniqueArrayPtr ua $ \ptr -> peekElemOff ptr i

-- Write an element into a mutable array at the given index. This does no bounds
-- checking.
--
{-# INLINE unsafeWriteArray #-}
unsafeWriteArray :: Storable e => UniqueArray e -> Int -> e -> IO ()
unsafeWriteArray !ua !i !e =
  withUniqueArrayPtr ua $ \ptr -> pokeElemOff ptr i e

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
newArrayData' !size
  = $internalCheck "newArrayData" "size must be >= 0" (size >= 0)
  $ newUniqueArray <=< unsafeInterleaveIO $ do
      let bytes = size * sizeOf (undefined :: e)
      new <- readIORef __mallocForeignPtrBytes
      ptr <- new bytes
      traceIO dump_gc $ printf "gc: allocated new host array (size=%d, ptr=%s)" bytes (show ptr)
      didAllocateBytesLocal (fromIntegral bytes)
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
{-# INLINE mallocPlainForeignPtrBytesAligned #-}
mallocPlainForeignPtrBytesAligned :: Int -> IO (ForeignPtr a)
mallocPlainForeignPtrBytesAligned (I# size) = IO $ \s ->
  case newAlignedPinnedByteArray# size 64# s of
    (# s', mbarr# #) -> (# s', ForeignPtr (byteArrayContents# (unsafeCoerce# mbarr#)) (PlainPtr mbarr#) #)

