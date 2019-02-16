{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UnboxedTuples        #-}
{-# LANGUAGE UndecidableInstances #-}
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
  ArrayElt(..), ArrayData, MutableArrayData, runArrayData,
  ArrayEltR(..), GArrayData(..),

  -- * Array tuple operations
  fstArrayData, sndArrayData, pairArrayData,

  -- * Type macros
  HTYPE_INT, HTYPE_WORD, HTYPE_CLONG, HTYPE_CULONG, HTYPE_CCHAR,

  -- * Allocator internals
  registerForeignPtrAllocator,

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
import Data.Bits
import Data.Char
import Data.IORef
import Data.Primitive                                               ( sizeOf# )
import Data.Typeable                                                ( Typeable )
import Foreign.ForeignPtr
import Foreign.Storable
import Language.Haskell.TH
import System.IO.Unsafe
import Text.Printf
import Prelude                                                      hiding ( mapM )

import GHC.Base
import GHC.ForeignPtr
import GHC.Ptr
import GHC.TypeLits


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
data family GArrayData a :: *
data instance GArrayData ()        = AD_Unit
data instance GArrayData Int       = AD_Int    {-# UNPACK #-} !(UniqueArray Int)
data instance GArrayData Int8      = AD_Int8   {-# UNPACK #-} !(UniqueArray Int8)
data instance GArrayData Int16     = AD_Int16  {-# UNPACK #-} !(UniqueArray Int16)
data instance GArrayData Int32     = AD_Int32  {-# UNPACK #-} !(UniqueArray Int32)
data instance GArrayData Int64     = AD_Int64  {-# UNPACK #-} !(UniqueArray Int64)
data instance GArrayData Word      = AD_Word   {-# UNPACK #-} !(UniqueArray Word)
data instance GArrayData Word8     = AD_Word8  {-# UNPACK #-} !(UniqueArray Word8)
data instance GArrayData Word16    = AD_Word16 {-# UNPACK #-} !(UniqueArray Word16)
data instance GArrayData Word32    = AD_Word32 {-# UNPACK #-} !(UniqueArray Word32)
data instance GArrayData Word64    = AD_Word64 {-# UNPACK #-} !(UniqueArray Word64)
data instance GArrayData Half      = AD_Half   {-# UNPACK #-} !(UniqueArray Half)
data instance GArrayData Float     = AD_Float  {-# UNPACK #-} !(UniqueArray Float)
data instance GArrayData Double    = AD_Double {-# UNPACK #-} !(UniqueArray Double)
data instance GArrayData Bool      = AD_Bool   {-# UNPACK #-} !(UniqueArray Word8)
data instance GArrayData Char      = AD_Char   {-# UNPACK #-} !(UniqueArray Char)
data instance GArrayData (Vec n a) = AD_Vec !Int# !(GArrayData a)           -- sad this does not get unpacked ):
data instance GArrayData (a, b)    = AD_Pair (GArrayData a) (GArrayData b)  -- XXX: non-strict to support lazy device-host copying

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
  ArrayEltRhalf    :: ArrayEltR Half
  ArrayEltRfloat   :: ArrayEltR Float
  ArrayEltRdouble  :: ArrayEltR Double
  ArrayEltRbool    :: ArrayEltR Bool
  ArrayEltRchar    :: ArrayEltR Char
  ArrayEltRpair    :: ArrayEltR a -> ArrayEltR b -> ArrayEltR (a,b)
  ArrayEltRvec     :: (KnownNat n, ArrayPtrs (Vec n a) ~ ArrayPtrs a, ArrayPtrs a ~ Ptr a) => ArrayEltR a -> ArrayEltR (Vec n a)
    -- XXX: Do we really require these embedded class constraints?

-- Array operations
-- ----------------

class ArrayElt e where
  type ArrayPtrs e
  arrayElt               :: ArrayEltR e
  --
  unsafeIndexArrayData   :: ArrayData e -> Int -> e
  ptrsOfArrayData        :: ArrayData e -> ArrayPtrs e
  touchArrayData         :: ArrayData e -> IO ()
  --
  newArrayData           :: Int -> IO (MutableArrayData e)
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
  {-# INLINE arrayElt             #-}
  {-# INLINE newArrayData         #-}
  {-# INLINE ptrsOfArrayData      #-}
  {-# INLINE touchArrayData       #-}
  {-# INLINE unsafeIndexArrayData #-}
  {-# INLINE unsafeReadArrayData  #-}
  {-# INLINE unsafeWriteArrayData #-}
  newArrayData !_                    = return AD_Unit
  ptrsOfArrayData      AD_Unit       = ()
  touchArrayData       AD_Unit       = return ()
  unsafeIndexArrayData AD_Unit !_    = ()
  unsafeReadArrayData  AD_Unit !_    = return ()
  unsafeWriteArrayData AD_Unit !_ () = return ()

-- Bool arrays are stored as arrays of bytes. While this is memory inefficient,
-- it is better suited to parallel backends than a packed bit-vector
-- representation.
--
-- XXX: Currently there are _no_ (Vec n Bool) instances. We could use efficient
--      bit-packed representations for these cases...
--
instance ArrayElt Bool where
  type ArrayPtrs Bool = Ptr Word8
  arrayElt            = ArrayEltRbool
  {-# INLINE arrayElt             #-}
  {-# INLINE newArrayData         #-}
  {-# INLINE ptrsOfArrayData      #-}
  {-# INLINE touchArrayData       #-}
  {-# INLINE unsafeIndexArrayData #-}
  {-# INLINE unsafeReadArrayData  #-}
  {-# INLINE unsafeWriteArrayData #-}
  newArrayData size                     = AD_Bool <$> newArrayData' size
  ptrsOfArrayData      (AD_Bool ba)     = unsafeUniqueArrayPtr ba
  touchArrayData       (AD_Bool ba)     = touchUniqueArray ba
  unsafeIndexArrayData (AD_Bool ba) i   = toBool  $! unsafeIndexArray ba i
  unsafeReadArrayData  (AD_Bool ba) i   = toBool <$> unsafeReadArray ba i
  unsafeWriteArrayData (AD_Bool ba) i e = unsafeWriteArray ba i (fromBool e)

instance (ArrayElt a, ArrayElt b) => ArrayElt (a, b) where
  type ArrayPtrs (a, b) = (ArrayPtrs a, ArrayPtrs b)
  arrayElt              = ArrayEltRpair arrayElt arrayElt
  {-# INLINEABLE arrayElt               #-}
  {-# INLINEABLE newArrayData           #-}
  {-# INLINEABLE ptrsOfArrayData        #-}
  {-# INLINEABLE ptrsOfMutableArrayData #-}
  {-# INLINEABLE touchArrayData         #-}
  {-# INLINEABLE unsafeFreezeArrayData  #-}
  {-# INLINEABLE unsafeIndexArrayData   #-}
  {-# INLINEABLE unsafeReadArrayData    #-}
  {-# INLINEABLE unsafeWriteArrayData   #-}
  newArrayData size                             = AD_Pair <$> newArrayData size <*> newArrayData size
  touchArrayData         (AD_Pair a b)          = touchArrayData a >> touchArrayData b
  ptrsOfArrayData        (AD_Pair a b)          = (ptrsOfArrayData a, ptrsOfArrayData b)
  ptrsOfMutableArrayData (AD_Pair a b)          = (,) <$> ptrsOfMutableArrayData a <*> ptrsOfMutableArrayData b
  unsafeReadArrayData    (AD_Pair a b) i        = (,) <$> unsafeReadArrayData a i <*> unsafeReadArrayData b i
  unsafeIndexArrayData   (AD_Pair a b) i        = (unsafeIndexArrayData a i, unsafeIndexArrayData b i)
  unsafeWriteArrayData   (AD_Pair a b) i (x, y) = unsafeWriteArrayData a i x >> unsafeWriteArrayData b i y
  unsafeFreezeArrayData  (AD_Pair a b)          = AD_Pair <$> unsafeFreezeArrayData a <*> unsafeFreezeArrayData b


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
  case newAlignedPinnedByteArray# size 16# s of
    (# s', mbarr# #) -> (# s', ForeignPtr (byteArrayContents# (unsafeCoerce# mbarr#)) (PlainPtr mbarr#) #)


-- Instances
-- ---------
--
$(runQ $ do
    let
        integralTypes :: [Name]
        integralTypes =
          [ ''Int
          , ''Int8
          , ''Int16
          , ''Int32
          , ''Int64
          , ''Word
          , ''Word8
          , ''Word16
          , ''Word32
          , ''Word64
          ]

        floatingTypes :: [Name]
        floatingTypes =
          [ ''Half
          , ''Float
          , ''Double
          ]

        nonNumTypes :: [Name]
        nonNumTypes =
          [ ''Char      -- wide characters are 4-bytes
          --''Bool      -- handled explicitly; stored as Word8
          ]

        allTypes :: [Name]
        allTypes = integralTypes ++ floatingTypes ++ nonNumTypes

        mkSingleElt :: Name -> Q [Dec]
        mkSingleElt name =
          let
              n       = nameBase name
              t       = conT name
              con     = conE (mkName ("AD_" ++ n))
              pat     = conP (mkName ("AD_" ++ n)) [varP (mkName "ba")]
          in
          [d| instance ArrayElt $t where
                type ArrayPtrs $t = Ptr $t
                arrayElt = $(conE (mkName ("ArrayEltR" ++ map toLower n)))
                {-# INLINE arrayElt             #-}
                {-# INLINE newArrayData         #-}
                {-# INLINE ptrsOfArrayData      #-}
                {-# INLINE touchArrayData       #-}
                {-# INLINE unsafeIndexArrayData #-}
                {-# INLINE unsafeReadArrayData  #-}
                {-# INLINE unsafeWriteArrayData #-}
                newArrayData size             = $con <$> newArrayData' size
                ptrsOfArrayData      $pat     = unsafeUniqueArrayPtr ba
                touchArrayData       $pat     = touchUniqueArray ba
                unsafeIndexArrayData $pat i   = unsafeIndexArray ba i
                unsafeReadArrayData  $pat i   = unsafeReadArray  ba i
                unsafeWriteArrayData $pat i e = unsafeWriteArray ba i e
            |]

        mkVectorElt :: Name -> Q [Dec]
        mkVectorElt name =
          let t = conT name
          in
          [d| instance KnownNat n => ArrayElt (Vec n $t) where
                type ArrayPtrs (Vec n $t) = ArrayPtrs $t
                arrayElt                  = ArrayEltRvec arrayElt
                {-# INLINE arrayElt             #-}
                {-# INLINE newArrayData         #-}
                {-# INLINE ptrsOfArrayData      #-}
                {-# INLINE touchArrayData       #-}
                {-# INLINE unsafeIndexArrayData #-}
                {-# INLINE unsafeReadArrayData  #-}
                {-# INLINE unsafeWriteArrayData #-}
                newArrayData size =
                  let !w@(I# w#) = fromIntegral (natVal' (proxy# :: Proxy# n))
                  in  AD_Vec w# <$> newArrayData (w * size)

                ptrsOfArrayData (AD_Vec _ ba) = ptrsOfArrayData ba
                touchArrayData  (AD_Vec _ ba) = touchArrayData ba
                unsafeIndexArrayData vec ix   = unsafePerformIO $! unsafeReadArrayData vec ix
                unsafeReadArrayData (AD_Vec w# ad) (I# ix#) =
                  let !bytes# = w# *# sizeOf# (undefined :: $t)
                      !addr#  = unPtr# (ptrsOfArrayData ad) `plusAddr#` (ix# *# bytes#)
                  in
                  IO $ \s ->
                    case newByteArray# bytes# s                       of { (# s1, mba# #) ->
                    case copyAddrToByteArray# addr# mba# 0# bytes# s1 of { s2             ->
                    case unsafeFreezeByteArray# mba# s2               of { (# s3, ba# #)  ->
                      (# s3, Vec ba# #)
                    }}}
                unsafeWriteArrayData (AD_Vec w# ad) (I# ix#) (Vec ba#) =
                  let !bytes# = w# *# sizeOf# (undefined :: $t)
                      !addr#  = unPtr# (ptrsOfArrayData ad) `plusAddr#` (ix# *# bytes#)
                  in
                  IO $ \s ->
                    case copyByteArrayToAddr# ba# 0# addr# bytes# s of
                      s1 -> (# s1, () #)
            |]
    --
    ss <- mapM mkSingleElt allTypes
    vv <- mapM mkVectorElt allTypes
    return (concat ss ++ concat vv)
 )

