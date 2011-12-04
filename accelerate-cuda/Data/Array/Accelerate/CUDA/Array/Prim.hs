{-# LANGUAGE BangPatterns, CPP, GADTs, TypeFamilies, ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.CUDA.Array.Prim
-- Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.CUDA.Array.Prim (

  DevicePtrs, HostPtrs,

  mallocArray, useArray, useArrayAsync, indexArray, copyArray, peekArray, peekArrayAsync,
  pokeArray, pokeArrayAsync, marshalArrayData, marshalTextureData, devicePtrsOfArrayData

) where

-- libraries
import Prelude                                          hiding (catch, lookup)
import Data.Int
import Data.Word
import Data.Maybe
import Data.Functor
import Data.Typeable
import Control.Monad
import Control.Exception
import System.Mem.StableName
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.CUDA.Driver.Error
import qualified Foreign.CUDA.Driver                    as CUDA
import qualified Foreign.CUDA.Driver.Stream             as CUDA
import qualified Foreign.CUDA.Driver.Texture            as CUDA

-- friends
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.CUDA.Array.Table

#include "accelerate.h"


-- Device array representation
-- ---------------------------

type family DevicePtrs e :: *
type family HostPtrs   e :: *

type instance DevicePtrs () = ()
type instance HostPtrs   () = ()

#define primArrayElt(ty)                                                      \
type instance DevicePtrs ty = CUDA.DevicePtr ty ;                             \
type instance HostPtrs   ty = CUDA.HostPtr   ty ;                             \

primArrayElt(Int)
primArrayElt(Int8)
primArrayElt(Int16)
primArrayElt(Int32)
primArrayElt(Int64)

primArrayElt(Word)
primArrayElt(Word8)
primArrayElt(Word16)
primArrayElt(Word32)
primArrayElt(Word64)

-- FIXME:
-- CShort
-- CUShort
-- CInt
-- CUInt
-- CLong
-- CULong
-- CLLong
-- CULLong

primArrayElt(Float)
primArrayElt(Double)

-- FIXME:
-- CFloat
-- CDouble

-- FIXME:
-- No concrete implementation in Data.Array.Accelerate.Array.Data
--
type instance HostPtrs   Bool = ()
type instance DevicePtrs Bool = ()

type instance HostPtrs   Char = ()
type instance DevicePtrs Char = ()

-- FIXME:
-- CChar
-- CSChar
-- CUChar

type instance DevicePtrs (a,b) = (DevicePtrs a, DevicePtrs b)
type instance HostPtrs   (a,b) = (HostPtrs   a, HostPtrs   b)



-- Texture References
-- ------------------

-- This representation must match the code generator's understanding of how to
-- utilise the texture cache.
--
class TextureData a where
  format :: a -> (CUDA.Format, Int)

instance TextureData Int8   where format _ = (CUDA.Int8,   1)
instance TextureData Int16  where format _ = (CUDA.Int16,  1)
instance TextureData Int32  where format _ = (CUDA.Int32,  1)
instance TextureData Int64  where format _ = (CUDA.Int32,  2)
instance TextureData Word8  where format _ = (CUDA.Word8,  1)
instance TextureData Word16 where format _ = (CUDA.Word16, 1)
instance TextureData Word32 where format _ = (CUDA.Word32, 1)
instance TextureData Word64 where format _ = (CUDA.Word32, 2)
instance TextureData Float  where format _ = (CUDA.Float,  1)
instance TextureData Double where format _ = (CUDA.Int32,  2)

instance TextureData Int where
  format _ = case sizeOf (undefined :: Int) of
                  4 -> (CUDA.Int32, 1)
                  8 -> (CUDA.Int32, 2)
                  _ -> error "we can never get here"

instance TextureData Word where
  format _ = case sizeOf (undefined :: Word) of
                  4 -> (CUDA.Word32, 1)
                  8 -> (CUDA.Word32, 2)
                  _ -> error "we can never get here"



-- Primitive array operations
-- --------------------------

-- Allocate a device-side array associated with the given host array. If the
-- allocation fails due to a lack of memory, run the garbage collector to
-- release any inaccessible arrays and try again.
--
mallocArray
    :: forall e b. (ArrayElt e, DevicePtrs e ~ CUDA.DevicePtr b, Typeable e, Typeable b, Storable b)
    => MemoryTable
    -> ArrayData e
    -> Int
    -> IO ()
mallocArray !mt !ad !n = do
  exists <- isJust <$> (lookup mt ad :: IO (Maybe (CUDA.DevicePtr b)))
  unless exists $ do
    ptr <- CUDA.mallocArray n `catch` \(e :: CUDAException) ->
      case e of
        ExitCode OutOfMemory -> reclaim mt >> CUDA.mallocArray n
        _                    -> throwIO e
    insert mt ad (ptr :: CUDA.DevicePtr b)


-- A combination of 'mallocArray' and 'pokeArray' to allocate space on the
-- device and upload an existing array. This is specialised because if the host
-- array is shared on the heap, we do not need to do anything.
--
useArray
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, DevicePtrs e ~ CUDA.DevicePtr a, Typeable e, Typeable a, Storable a)
    => MemoryTable
    -> ArrayData e
    -> Int
    -> IO ()
useArray !mt !ad !n =
  let src = ptrsOfArrayData ad
  in do
    exists <- isJust <$> (lookup mt ad :: IO (Maybe (CUDA.DevicePtr a)))
    unless exists $ do
      dst <- CUDA.mallocArray n `catch` \(e :: CUDAException) ->
        case e of
          ExitCode OutOfMemory -> reclaim mt >> CUDA.mallocArray n
          _                    -> throwIO e
      CUDA.pokeArray n src dst
      insert mt ad dst


useArrayAsync
    :: forall e a. (ArrayElt e, ArrayPtrs e ~ Ptr a, DevicePtrs e ~ CUDA.DevicePtr a, Typeable e, Typeable a, Storable a)
    => MemoryTable
    -> ArrayData e
    -> Int
    -> Maybe CUDA.Stream
    -> IO ()
useArrayAsync !mt !ad !n !ms =
  let src = CUDA.HostPtr (ptrsOfArrayData ad)
  in do
    exists <- isJust <$> (lookup mt ad :: IO (Maybe (CUDA.DevicePtr a)))
    unless exists $ do
      dst <- CUDA.mallocArray n `catch` \(e :: CUDAException) ->
        case e of
          ExitCode OutOfMemory -> reclaim mt >> CUDA.mallocArray n
          _                    -> throwIO e
      CUDA.pokeArrayAsync n src dst ms
      insert mt ad dst


-- Read a single element from an array at the given row-major index
--
indexArray
    :: (ArrayElt e, DevicePtrs e ~ CUDA.DevicePtr b, Typeable e, Typeable b, Storable b)
    => MemoryTable
    -> ArrayData e
    -> Int
    -> IO b
indexArray !mt !ad !i =
  alloca                        $ \dst ->
  devicePtrsOfArrayData mt ad >>= \src -> do
    CUDA.peekArray 1 (src `CUDA.advanceDevPtr` i) dst
    peek dst


-- Copy data between two device arrays. The operation is asynchronous with
-- respect to the host, but will never overlap kernel execution.
--
copyArray
    :: (ArrayElt e, ArrayPtrs e ~ Ptr a, DevicePtrs e ~ CUDA.DevicePtr b, Typeable a, Typeable b, Typeable e, Storable b)
    => MemoryTable
    -> ArrayData e              -- source array
    -> ArrayData e              -- destination array
    -> Int                      -- number of array elements
    -> IO ()
copyArray !mt !from !to !n = do
  src <- devicePtrsOfArrayData mt from
  dst <- devicePtrsOfArrayData mt to
  CUDA.copyArrayAsync n src dst


-- Copy data from the device into the associated Accelerate host-side array
--
peekArray
    :: (ArrayElt e, ArrayPtrs e ~ Ptr a, DevicePtrs e ~ CUDA.DevicePtr a, Typeable a, Typeable e, Storable a)
    => MemoryTable
    -> ArrayData e
    -> Int
    -> IO ()
peekArray !mt !ad !n =
  devicePtrsOfArrayData mt ad >>= \src ->
    CUDA.peekArray n src (ptrsOfArrayData ad)

peekArrayAsync
    :: (ArrayElt e, ArrayPtrs e ~ Ptr a, DevicePtrs e ~ CUDA.DevicePtr a, Typeable a, Typeable e, Storable a)
    => MemoryTable
    -> ArrayData e
    -> Int
    -> Maybe CUDA.Stream
    -> IO ()
peekArrayAsync !mt !ad !n !st =
  devicePtrsOfArrayData mt ad >>= \src ->
    CUDA.peekArrayAsync n src (CUDA.HostPtr $ ptrsOfArrayData ad) st


-- Copy data from an Accelerate array into the associated device array
--
pokeArray
    :: (ArrayElt e, ArrayPtrs e ~ Ptr a, DevicePtrs e ~ CUDA.DevicePtr a, Typeable a, Typeable e, Storable a)
    => MemoryTable
    -> ArrayData e
    -> Int
    -> IO ()
pokeArray !mt !ad !n =
  devicePtrsOfArrayData mt ad >>= \dst ->
    CUDA.pokeArray n (ptrsOfArrayData ad) dst

pokeArrayAsync
    :: (ArrayElt e, ArrayPtrs e ~ Ptr a, DevicePtrs e ~ CUDA.DevicePtr a, Typeable a, Typeable e, Storable a)
    => MemoryTable
    -> ArrayData e
    -> Int
    -> Maybe CUDA.Stream
    -> IO ()
pokeArrayAsync !mt !ad !n !st =
  devicePtrsOfArrayData mt ad >>= \dst ->
    CUDA.pokeArrayAsync n (CUDA.HostPtr $ ptrsOfArrayData ad) dst st


-- Wrap a device pointer corresponding corresponding to a host-side array into
-- arguments that can be passed to a kernel upon invocation
--
marshalArrayData
    :: (ArrayElt e, DevicePtrs e ~ CUDA.DevicePtr b, Typeable b, Typeable e)
    => MemoryTable
    -> ArrayData e
    -> IO CUDA.FunParam
marshalArrayData !mt !ad = CUDA.VArg <$> devicePtrsOfArrayData mt ad


-- Bind device memory to the given texture reference, setting appropriate type
--
marshalTextureData
    :: forall a e. (ArrayElt e, DevicePtrs e ~ CUDA.DevicePtr a, Typeable a, Typeable e, Storable a, TextureData a)
    => MemoryTable
    -> ArrayData e              -- host array
    -> Int                      -- number of elements
    -> CUDA.Texture             -- texture reference to bind array to
    -> IO ()
marshalTextureData !mt !ad !n !tex =
  let (fmt, c) = format (undefined :: a)
  in  devicePtrsOfArrayData mt ad >>= \ptr -> do
        CUDA.setFormat tex fmt c
        CUDA.bind tex ptr (fromIntegral $ n * sizeOf (undefined :: a))


-- Lookup the device memory associated with our host array
--
devicePtrsOfArrayData
    :: (ArrayElt e, DevicePtrs e ~ CUDA.DevicePtr b, Typeable e, Typeable b)
    => MemoryTable
    -> ArrayData e
    -> IO (DevicePtrs e)
devicePtrsOfArrayData !mt !ad = do
  mv <- lookup mt ad
  case mv of
    Just v  -> return v
    Nothing -> do
      sn <- makeStableName ad
      INTERNAL_ERROR(error) "devicePtrsOfArrayData" $ "lost device memory #" ++ show (hashStableName sn)

