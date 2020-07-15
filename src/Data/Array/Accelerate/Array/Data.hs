{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
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
  ArrayData, MutableArrayData, GArrayData, ScalarArrayData, ScalarArrayDataR,
  runArrayData,
  newArrayData,
  indexArrayData, readArrayData, writeArrayData,
  unsafeArrayDataPtr,
  touchArrayData,
  rnfArrayData,

  -- * Type macros
  HTYPE_INT, HTYPE_WORD, HTYPE_CLONG, HTYPE_CULONG, HTYPE_CCHAR,

  -- * Allocator internals
  registerForeignPtrAllocator,

  -- * Utilities for type classes
  ScalarArrayDict(..), scalarArrayDict,
  SingleArrayDict(..), singleArrayDict,

  -- * TemplateHaskell
  liftArrayData,

) where

-- friends
import Data.Array.Accelerate.Array.Unique
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Type
import Data.Primitive.Vec

import Data.Array.Accelerate.Debug.Flags
import Data.Array.Accelerate.Debug.Monitoring
import Data.Array.Accelerate.Debug.Trace


-- standard libraries
import Control.Applicative
import Control.DeepSeq
import Control.Monad                                                ( (<=<) )
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


-- | Immutable array representation
--
type ArrayData e = MutableArrayData e

-- | Mutable array representation
--
type MutableArrayData e = GArrayData e

-- | Underlying array representation.
--
-- In previous versions this was abstracted over by the mutable/immutable array
-- representation, but this is now fixed to our UniqueArray type.
--
-- NOTE: We use a standard (non-strict) pair to enable lazy device-host data transfers
--
type family GArrayData a where
  GArrayData ()     = ()
  GArrayData (a, b) = (GArrayData a, GArrayData b)
  GArrayData a      = ScalarArrayData a

type ScalarArrayData a = UniqueArray (ScalarArrayDataR a)

-- | Mapping from scalar type to the type as represented in memory in an
-- array.
--
type family ScalarArrayDataR t where
  ScalarArrayDataR Int       = Int
  ScalarArrayDataR Int8      = Int8
  ScalarArrayDataR Int16     = Int16
  ScalarArrayDataR Int32     = Int32
  ScalarArrayDataR Int64     = Int64
  ScalarArrayDataR Word      = Word
  ScalarArrayDataR Word8     = Word8
  ScalarArrayDataR Word16    = Word16
  ScalarArrayDataR Word32    = Word32
  ScalarArrayDataR Word64    = Word64
  ScalarArrayDataR Half      = Half
  ScalarArrayDataR Float     = Float
  ScalarArrayDataR Double    = Double
  ScalarArrayDataR (Vec n t) = ScalarArrayDataR t


data ScalarArrayDict a where
  ScalarArrayDict :: ( GArrayData a ~ ScalarArrayData a, ScalarArrayDataR a ~ ScalarArrayDataR b )
                  => {-# UNPACK #-} !Int    -- vector width
                  -> SingleType b           -- base type
                  -> ScalarArrayDict a

data SingleArrayDict a where
  SingleArrayDict :: ( GArrayData a ~ ScalarArrayData a, ScalarArrayDataR a ~ a )
                  => SingleArrayDict a

scalarArrayDict :: ScalarType a -> ScalarArrayDict a
scalarArrayDict = scalar
  where
    scalar :: ScalarType a -> ScalarArrayDict a
    scalar (VectorScalarType t) = vector t
    scalar (SingleScalarType t)
      | SingleArrayDict <- singleArrayDict t
      = ScalarArrayDict 1 t

    vector :: VectorType a -> ScalarArrayDict a
    vector (VectorType w s)
      | SingleArrayDict <- singleArrayDict s
      = ScalarArrayDict w s

singleArrayDict :: SingleType a -> SingleArrayDict a
singleArrayDict = single
  where
    single :: SingleType a -> SingleArrayDict a
    single (NumSingleType t) = num t

    num :: NumType a -> SingleArrayDict a
    num (IntegralNumType t) = integral t
    num (FloatingNumType t) = floating t

    integral :: IntegralType a -> SingleArrayDict a
    integral TypeInt    = SingleArrayDict
    integral TypeInt8   = SingleArrayDict
    integral TypeInt16  = SingleArrayDict
    integral TypeInt32  = SingleArrayDict
    integral TypeInt64  = SingleArrayDict
    integral TypeWord   = SingleArrayDict
    integral TypeWord8  = SingleArrayDict
    integral TypeWord16 = SingleArrayDict
    integral TypeWord32 = SingleArrayDict
    integral TypeWord64 = SingleArrayDict

    floating :: FloatingType a -> SingleArrayDict a
    floating TypeHalf   = SingleArrayDict
    floating TypeFloat  = SingleArrayDict
    floating TypeDouble = SingleArrayDict


-- Array operations
-- ----------------

newArrayData :: HasCallStack => TupR ScalarType e -> Int -> IO (MutableArrayData e)
newArrayData TupRunit         !_    = return ()
newArrayData (TupRpair t1 t2) !size = (,) <$> newArrayData t1 size <*> newArrayData t2 size
newArrayData (TupRsingle t)   !size
  | SingleScalarType s <- t
  , SingleDict         <- singleDict s
  , SingleArrayDict    <- singleArrayDict s
  = allocateArray size
  --
  | VectorScalarType v <- t
  , VectorType w s     <- v
  , SingleDict         <- singleDict s
  , SingleArrayDict    <- singleArrayDict s
  = allocateArray (w * size)

indexArrayData :: TupR ScalarType e -> ArrayData e -> Int -> e
indexArrayData tR arr ix = unsafePerformIO $ readArrayData tR arr ix

readArrayData :: forall e. TupR ScalarType e -> MutableArrayData e -> Int -> IO e
readArrayData TupRunit         ()       !_  = return ()
readArrayData (TupRpair t1 t2) (a1, a2) !ix = (,) <$> readArrayData t1 a1 ix <*> readArrayData t2 a2 ix
readArrayData (TupRsingle t)   arr      !ix
  | SingleScalarType s <- t
  , SingleDict         <- singleDict s
  , SingleArrayDict    <- singleArrayDict s
  = unsafeReadArray arr ix
  --
  | VectorScalarType v <- t
  , VectorType w s     <- v
  , I# w#              <- w
  , I# ix#             <- ix
  , SingleDict         <- singleDict s
  , SingleArrayDict    <- singleArrayDict s
  = let
        !bytes# = w# *# sizeOf# (undefined :: ScalarArrayDataR e)
        !addr#  = unPtr# (unsafeUniqueArrayPtr arr) `plusAddr#` (ix# *# bytes#)
     in
     IO $ \s0 ->
       case newByteArray# bytes# s0                      of { (# s1, mba# #) ->
       case copyAddrToByteArray# addr# mba# 0# bytes# s1 of { s2             ->
       case unsafeFreezeByteArray# mba# s2               of { (# s3, ba# #)  ->
         (# s3, Vec ba# #)
       }}}

writeArrayData :: forall e. TupR ScalarType e -> MutableArrayData e -> Int -> e -> IO ()
writeArrayData TupRunit         ()       !_  ()       = return ()
writeArrayData (TupRpair t1 t2) (a1, a2) !ix (v1, v2) = writeArrayData t1 a1 ix v1 >> writeArrayData t2 a2 ix v2
writeArrayData (TupRsingle t)   arr      !ix !val
  | SingleScalarType s <- t
  , SingleDict         <- singleDict s
  , SingleArrayDict    <- singleArrayDict s
  = unsafeWriteArray arr ix val
  --
  | VectorScalarType v <- t
  , VectorType w s     <- v
  , Vec ba#            <- val
  , I# w#              <- w
  , I# ix#             <- ix
  , SingleDict         <- singleDict s
  , SingleArrayDict    <- singleArrayDict s
  = let
       !bytes# = w# *# sizeOf# (undefined :: ScalarArrayDataR e)
       !addr#  = unPtr# (unsafeUniqueArrayPtr arr) `plusAddr#` (ix# *# bytes#)
     in
     IO $ \s0 -> case copyByteArrayToAddr# ba# 0# addr# bytes# s0 of
                   s1 -> (# s1, () #)


unsafeArrayDataPtr :: ScalarType e -> ArrayData e -> Ptr (ScalarArrayDataR e)
unsafeArrayDataPtr t arr
  | ScalarArrayDict{} <- scalarArrayDict t
  = unsafeUniqueArrayPtr arr

touchArrayData :: TupR ScalarType e -> ArrayData e -> IO ()
touchArrayData TupRunit         ()       = return ()
touchArrayData (TupRpair t1 t2) (a1, a2) = touchArrayData t1 a1 >> touchArrayData t2 a2
touchArrayData (TupRsingle t)   arr
  | ScalarArrayDict{} <- scalarArrayDict t
  = touchUniqueArray arr

rnfArrayData :: TupR ScalarType e -> ArrayData e -> ()
rnfArrayData TupRunit         ()       = ()
rnfArrayData (TupRpair t1 t2) (a1, a2) = rnfArrayData t1 a1 `seq` rnfArrayData t2 a2 `seq` ()
rnfArrayData (TupRsingle t)   arr      = rnf (unsafeArrayDataPtr t arr)

unPtr# :: Ptr a -> Addr#
unPtr# (Ptr addr#) = addr#

-- | Safe combination of creating and fast freezing of array data.
--
runArrayData
    :: IO (MutableArrayData e, e)
    -> (ArrayData e, e)
runArrayData st = unsafePerformIO $ do
  (mad, r) <- st
  return (mad, r)

-- Allocate a new array with enough storage to hold the given number of
-- elements.
--
-- The array is uninitialised and, in particular, allocated lazily. The latter
-- is important because it means that for backends that have discrete memory
-- spaces (e.g. GPUs), we will not increase host memory pressure simply to track
-- intermediate arrays that contain meaningful data only on the device.
--
allocateArray :: forall e. (HasCallStack, Storable e) => Int -> IO (UniqueArray e)
allocateArray !size
  = internalCheck "size must be >= 0" (size >= 0)
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
mallocPlainForeignPtrBytesAligned :: Int -> IO (ForeignPtr a)
mallocPlainForeignPtrBytesAligned (I# size) = IO $ \s ->
  case newAlignedPinnedByteArray# size 64# s of
    (# s', mbarr# #) -> (# s', ForeignPtr (byteArrayContents# (unsafeCoerce# mbarr#)) (PlainPtr mbarr#) #)


liftArrayData :: Int -> TypeR e -> ArrayData e -> Q (TExp (ArrayData e))
liftArrayData n = tuple
  where
    tuple :: TypeR e -> ArrayData e -> Q (TExp (ArrayData e))
    tuple TupRunit         ()       = [|| () ||]
    tuple (TupRpair t1 t2) (a1, a2) = [|| ($$(tuple t1 a1), $$(tuple t2 a2)) ||]
    tuple (TupRsingle s) adata      = scalar s adata

    scalar :: ScalarType e -> ArrayData e -> Q (TExp (ArrayData e))
    scalar (SingleScalarType t) = single t
    scalar (VectorScalarType t) = vector t

    vector :: forall n e. VectorType (Vec n e) -> ArrayData (Vec n e) -> Q (TExp (ArrayData (Vec n e)))
    vector (VectorType w t)
      | SingleArrayDict <- singleArrayDict t
      = liftArrayData (w * n) (TupRsingle (SingleScalarType t))

    single :: SingleType e -> ArrayData e -> Q (TExp (ArrayData e))
    single (NumSingleType t) = num t

    num :: NumType e -> ArrayData e -> Q (TExp (ArrayData e))
    num (IntegralNumType t) = integral t
    num (FloatingNumType t) = floating t

    integral :: IntegralType e -> ArrayData e -> Q (TExp (ArrayData e))
    integral TypeInt    = liftUniqueArray n
    integral TypeInt8   = liftUniqueArray n
    integral TypeInt16  = liftUniqueArray n
    integral TypeInt32  = liftUniqueArray n
    integral TypeInt64  = liftUniqueArray n
    integral TypeWord   = liftUniqueArray n
    integral TypeWord8  = liftUniqueArray n
    integral TypeWord16 = liftUniqueArray n
    integral TypeWord32 = liftUniqueArray n
    integral TypeWord64 = liftUniqueArray n

    floating :: FloatingType e -> ArrayData e -> Q (TExp (ArrayData e))
    floating TypeHalf   = liftUniqueArray n
    floating TypeFloat  = liftUniqueArray n
    floating TypeDouble = liftUniqueArray n

-- Determine the underlying type of a Haskell CLong or CULong.
--
runQ [d| type HTYPE_INT = $(
              case finiteBitSize (undefined::Int) of
                32 -> [t| Int32 |]
                64 -> [t| Int64 |]
                _  -> error "I don't know what architecture I am" ) |]

runQ [d| type HTYPE_WORD = $(
              case finiteBitSize (undefined::Word) of
                32 -> [t| Word32 |]
                64 -> [t| Word64 |]
                _  -> error "I don't know what architecture I am" ) |]

runQ [d| type HTYPE_CLONG = $(
              case finiteBitSize (undefined::CLong) of
                32 -> [t| Int32 |]
                64 -> [t| Int64 |]
                _  -> error "I don't know what architecture I am" ) |]

runQ [d| type HTYPE_CULONG = $(
              case finiteBitSize (undefined::CULong) of
                32 -> [t| Word32 |]
                64 -> [t| Word64 |]
                _  -> error "I don't know what architecture I am" ) |]

runQ [d| type HTYPE_CCHAR = $(
              if isSigned (undefined::CChar)
                then [t| Int8  |]
                else [t| Word8 |] ) |]

