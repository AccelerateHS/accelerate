{-# LANGUAGE GADTs, TypeFamilies, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE RankNTypes, MagicHash, UnboxedTuples #-}

module Data.Array.Accelerate.CUDA.Data (
 
  ArrayElem(..)
) where

-- standard libraries

import Foreign (Ptr, nullPtr)
import Foreign.Storable (Storable)

import GHC.Int
import GHC.Ptr (Ptr(Ptr))

import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Representation
import qualified Data.Array.Accelerate.Array.Data as Acc
import qualified Foreign.CUDA.Driver              as CUDA
import qualified Foreign.CUDA.Driver.Stream       as CUDA

class Acc.ArrayElem e => ArrayElem e where
  type DevicePtrs e
  type HostPtrs   e
  mallocArray     :: Acc.ArrayData e -> Int -> IO (DevicePtrs e)
  mallocHostArray :: Acc.ArrayData e -> [CUDA.AllocFlag] -> Int -> IO (HostPtrs e)
  pokeArray       :: Acc.ArrayData e -> Int -> Acc.ArrayPtrs e -> DevicePtrs e -> IO ()
  pokeArrayAsync  :: Acc.ArrayData e -> Int -> HostPtrs e -> DevicePtrs e -> Maybe CUDA.Stream -> IO ()
  peekArray       :: Acc.ArrayData e -> Int -> DevicePtrs e -> Acc.ArrayPtrs e -> IO ()
  peekArrayAsync  :: Acc.ArrayData e -> Int -> DevicePtrs e -> HostPtrs e -> Maybe CUDA.Stream -> IO ()
  free            :: Acc.ArrayData e -> DevicePtrs e -> IO ()
  freeHost        :: Acc.ArrayData e -> HostPtrs e -> IO ()

instance ArrayElem () where
  type DevicePtrs () = CUDA.DevicePtr ()
  type HostPtrs   () = CUDA.HostPtr   ()
  mallocArray     _ _       = return CUDA.nullDevPtr
  mallocHostArray _ _ _     = return CUDA.nullHostPtr
  pokeArray       _ _ _ _   = return ()
  pokeArrayAsync  _ _ _ _ _ = return ()
  peekArray       _ _ _ _   = return ()
  peekArrayAsync  _ _ _ _ _ = return ()
  free            _ _       = return ()
  freeHost        _ _       = return ()

instance ArrayElem Int where
  type DevicePtrs Int = CUDA.DevicePtr Int
  type HostPtrs   Int = CUDA.HostPtr   Int
  mallocArray     _  = CUDA.mallocArray
  mallocHostArray ad = CUDA.mallocHostArray
  pokeArray       ad = CUDA.pokeArray
  pokeArrayAsync  ad = CUDA.pokeArrayAsync
  peekArray       ad = CUDA.peekArray
  peekArrayAsync  ad = CUDA.peekArrayAsync
  free            ad = CUDA.free
  freeHost        ad = CUDA.freeHost

instance ArrayElem Int8 where
  type DevicePtrs Int8 = CUDA.DevicePtr Int8
  type HostPtrs   Int8 = CUDA.HostPtr   Int8
  mallocArray     _  = CUDA.mallocArray
  mallocHostArray ad = CUDA.mallocHostArray
  pokeArray       ad = CUDA.pokeArray
  pokeArrayAsync  ad = CUDA.pokeArrayAsync
  peekArray       ad = CUDA.peekArray
  peekArrayAsync  ad = CUDA.peekArrayAsync
  free            ad = CUDA.free
  freeHost        ad = CUDA.freeHost

instance ArrayElem Int16 where
  type DevicePtrs Int16 = CUDA.DevicePtr Int16
  type HostPtrs   Int16 = CUDA.HostPtr   Int16
  mallocArray     _  = CUDA.mallocArray
  mallocHostArray ad = CUDA.mallocHostArray
  pokeArray       ad = CUDA.pokeArray
  pokeArrayAsync  ad = CUDA.pokeArrayAsync
  peekArray       ad = CUDA.peekArray
  peekArrayAsync  ad = CUDA.peekArrayAsync
  free            ad = CUDA.free
  freeHost        ad = CUDA.freeHost

instance ArrayElem Int32 where
  type DevicePtrs Int32 = CUDA.DevicePtr Int32
  type HostPtrs   Int32 = CUDA.HostPtr   Int32
  mallocArray     _  = CUDA.mallocArray
  mallocHostArray ad = CUDA.mallocHostArray
  pokeArray       ad = CUDA.pokeArray
  pokeArrayAsync  ad = CUDA.pokeArrayAsync
  peekArray       ad = CUDA.peekArray
  peekArrayAsync  ad = CUDA.peekArrayAsync
  free            ad = CUDA.free
  freeHost        ad = CUDA.freeHost

instance ArrayElem Int64 where
  type DevicePtrs Int64 = CUDA.DevicePtr Int64
  type HostPtrs   Int64 = CUDA.HostPtr   Int64
  mallocArray     _  = CUDA.mallocArray
  mallocHostArray ad = CUDA.mallocHostArray
  pokeArray       ad = CUDA.pokeArray
  pokeArrayAsync  ad = CUDA.pokeArrayAsync
  peekArray       ad = CUDA.peekArray
  peekArrayAsync  ad = CUDA.peekArrayAsync
  free            ad = CUDA.free
  freeHost        ad = CUDA.freeHost

instance ArrayElem Word where
  type DevicePtrs Word = CUDA.DevicePtr Word
  type HostPtrs   Word = CUDA.HostPtr   Word
  mallocArray     _  = CUDA.mallocArray
  mallocHostArray ad = CUDA.mallocHostArray
  pokeArray       ad = CUDA.pokeArray
  pokeArrayAsync  ad = CUDA.pokeArrayAsync
  peekArray       ad = CUDA.peekArray
  peekArrayAsync  ad = CUDA.peekArrayAsync
  free            ad = CUDA.free
  freeHost        ad = CUDA.freeHost

instance ArrayElem Word8 where
  type DevicePtrs Word8 = CUDA.DevicePtr Word8
  type HostPtrs   Word8 = CUDA.HostPtr   Word8
  mallocArray     _  = CUDA.mallocArray
  mallocHostArray ad = CUDA.mallocHostArray
  pokeArray       ad = CUDA.pokeArray
  pokeArrayAsync  ad = CUDA.pokeArrayAsync
  peekArray       ad = CUDA.peekArray
  peekArrayAsync  ad = CUDA.peekArrayAsync
  free            ad = CUDA.free
  freeHost        ad = CUDA.freeHost

instance ArrayElem Word16 where
  type DevicePtrs Word16 = CUDA.DevicePtr Word16
  type HostPtrs   Word16 = CUDA.HostPtr   Word16
  mallocArray     _  = CUDA.mallocArray
  mallocHostArray ad = CUDA.mallocHostArray
  pokeArray       ad = CUDA.pokeArray
  pokeArrayAsync  ad = CUDA.pokeArrayAsync
  peekArray       ad = CUDA.peekArray
  peekArrayAsync  ad = CUDA.peekArrayAsync
  free            ad = CUDA.free
  freeHost        ad = CUDA.freeHost

instance ArrayElem Word32 where
  type DevicePtrs Word32 = CUDA.DevicePtr Word32
  type HostPtrs   Word32 = CUDA.HostPtr   Word32
  mallocArray     _  = CUDA.mallocArray
  mallocHostArray ad = CUDA.mallocHostArray
  pokeArray       ad = CUDA.pokeArray
  pokeArrayAsync  ad = CUDA.pokeArrayAsync
  peekArray       ad = CUDA.peekArray
  peekArrayAsync  ad = CUDA.peekArrayAsync
  free            ad = CUDA.free
  freeHost        ad = CUDA.freeHost

instance ArrayElem Word64 where
  type DevicePtrs Word64 = CUDA.DevicePtr Word64
  type HostPtrs   Word64 = CUDA.HostPtr   Word64
  mallocArray     _  = CUDA.mallocArray
  mallocHostArray ad = CUDA.mallocHostArray
  pokeArray       ad = CUDA.pokeArray
  pokeArrayAsync  ad = CUDA.pokeArrayAsync
  peekArray       ad = CUDA.peekArray
  peekArrayAsync  ad = CUDA.peekArrayAsync
  free            ad = CUDA.free
  freeHost        ad = CUDA.freeHost

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
  type DevicePtrs Float = CUDA.DevicePtr Float
  type HostPtrs   Float = CUDA.HostPtr   Float
  mallocArray     _  = CUDA.mallocArray
  mallocHostArray ad = CUDA.mallocHostArray
  pokeArray       ad = CUDA.pokeArray
  pokeArrayAsync  ad = CUDA.pokeArrayAsync
  peekArray       ad = CUDA.peekArray
  peekArrayAsync  ad = CUDA.peekArrayAsync
  free            ad = CUDA.free
  freeHost        ad = CUDA.freeHost

instance ArrayElem Double where
  type DevicePtrs Double = CUDA.DevicePtr Double
  type HostPtrs   Double = CUDA.HostPtr   Double
  mallocArray     _  = CUDA.mallocArray
  mallocHostArray ad = CUDA.mallocHostArray
  pokeArray       ad = CUDA.pokeArray
  pokeArrayAsync  ad = CUDA.pokeArrayAsync
  peekArray       ad = CUDA.peekArray
  peekArrayAsync  ad = CUDA.peekArrayAsync
  free            ad = CUDA.free
  freeHost        ad = CUDA.freeHost

instance ArrayElem Bool where
  type DevicePtrs Bool = CUDA.DevicePtr Word8
  type HostPtrs   Bool = CUDA.HostPtr   Word8

instance ArrayElem Char where
  type DevicePtrs Char = CUDA.DevicePtr Char
  type HostPtrs   Char = CUDA.HostPtr   Char
  -- ???unicode???

-- FIXME:
-- CChar
-- CSChar
-- CUChar

instance (ArrayElem a, ArrayElem b) => ArrayElem (a, b) where
  type DevicePtrs (a, b) = (DevicePtrs a, DevicePtrs b)
  type HostPtrs   (a, b) = (HostPtrs   a, HostPtrs   b)
  mallocArray ad n = do
    dptrA <- mallocArray (Acc.fstArrayData ad) n
    dptrB <- mallocArray (Acc.sndArrayData ad) n
    return (dptrA, dptrB)
  mallocHostArray ad flags n = do
    hptrA <- mallocHostArray (Acc.fstArrayData ad) flags n
    hptrB <- mallocHostArray (Acc.sndArrayData ad) flags n
    return (hptrA, hptrB)
  pokeArray ad n (aptrA, aptrB) (dptrA, dptrB) = do
    pokeArray (Acc.fstArrayData ad) n aptrA dptrA
    pokeArray (Acc.sndArrayData ad) n aptrB dptrB
  pokeArrayAsync ad n (hptrA, hptrB) (dptrA, dptrB) s = do
    pokeArrayAsync (Acc.fstArrayData ad) n hptrA dptrA s
    pokeArrayAsync (Acc.sndArrayData ad) n hptrB dptrB s
  peekArray ad n (dptrA, dptrB) (aptrA, aptrB) = do
    peekArray (Acc.fstArrayData ad) n dptrA aptrA
    peekArray (Acc.sndArrayData ad) n dptrB aptrB
  peekArrayAsync ad n (dptrA, dptrB) (hptrA, hptrB) s = do
    peekArrayAsync (Acc.fstArrayData ad) n dptrA hptrA s
    peekArrayAsync (Acc.sndArrayData ad) n dptrB hptrB s
  free ad (dptrA, dptrB) = do
    free (Acc.fstArrayData ad) dptrA
    free (Acc.sndArrayData ad) dptrB
  freeHost ad (hptrA, hptrB) = do
    freeHost (Acc.fstArrayData ad) hptrA
    freeHost (Acc.sndArrayData ad) hptrB
