{-# LANGUAGE GADTs, TypeFamilies, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE CPP, RankNTypes, MagicHash, TypeOperators, UnboxedTuples #-}
-- |
-- Module      : Data.Array.Accelerate.CUDA.Array.Data
-- Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.CUDA.Array.Data
  (
    ArrayElem(..)
  ) where

import Prelude hiding (id, (.), mod)
import Control.Category

import Foreign.Ptr
import Foreign.Storable                                 (Storable)

import Data.Int
import Data.Word
import Data.Maybe
import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import qualified Data.IntMap                            as IM

import Data.Array.Accelerate.CUDA.State
import qualified Data.Array.Accelerate.Array.Data       as Acc
import qualified Foreign.CUDA.Driver                    as CUDA
import qualified Foreign.CUDA.Driver.Stream             as CUDA


--
-- Instances
-- ~~~~~~~~~

class Acc.ArrayElem e => ArrayElem e where
  type DevicePtrs e
  type HostPtrs   e
  mallocArray    :: Acc.ArrayData e -> Int -> CIO ()
  peekArray      :: Acc.ArrayData e -> Int -> CIO ()
  pokeArray      :: Acc.ArrayData e -> Int -> CIO ()
  peekArrayAsync :: Acc.ArrayData e -> Int -> Maybe CUDA.Stream -> CIO ()
  pokeArrayAsync :: Acc.ArrayData e -> Int -> Maybe CUDA.Stream -> CIO ()
  devicePtrs     :: Acc.ArrayData e -> CIO [CUDA.FunParam]
  touch          :: Acc.ArrayData e -> CIO ()
  free           :: Acc.ArrayData e -> CIO ()


instance ArrayElem () where
  type DevicePtrs () = CUDA.DevicePtr ()
  type HostPtrs   () = CUDA.HostPtr   ()
  mallocArray    _ _   = return ()
  peekArray      _ _   = return ()
  pokeArray      _ _   = return ()
  peekArrayAsync _ _ _ = return ()
  pokeArrayAsync _ _ _ = return ()
  devicePtrs     _     = return []
  touch          _     = return ()
  free           _     = return ()


#define primArrayElem_(ty,con)                                                 \
instance ArrayElem ty where {                                                  \
  type DevicePtrs ty = CUDA.DevicePtr con                                      \
; type HostPtrs   ty = CUDA.HostPtr   con                                      \
; mallocArray ad n = insertArray ad =<< liftIO (CUDA.mallocArray n)            \
; peekArray        = peekArray'                                                \
; pokeArray        = pokeArray'                                                \
; peekArrayAsync   = peekArrayAsync'                                           \
; pokeArrayAsync   = pokeArrayAsync'                                           \
; devicePtrs       = devicePtrs'                                               \
; touch            = touch'                                                    \
; free             = free' }

#define primArrayElem(ty) primArrayElem_(ty,ty)

primArrayElem(Int)
primArrayElem(Int8)
primArrayElem(Int16)
primArrayElem(Int32)
primArrayElem(Int64)

primArrayElem(Word)
primArrayElem(Word8)
primArrayElem(Word16)
primArrayElem(Word32)
primArrayElem(Word64)

-- FIXME:
-- CShort
-- CUShort
-- CInt
-- CUInt
-- CLong
-- CULong
-- CLLong
-- CULLong

primArrayElem(Float)
primArrayElem(Double)

-- FIXME:
-- CFloat
-- CDouble

primArrayElem_(Bool,Word8)
primArrayElem_(Char,Word32)

-- FIXME:
-- CChar
-- CSChar
-- CUChar

instance (ArrayElem a, ArrayElem b) => ArrayElem (a,b) where
  type DevicePtrs (a,b) = (DevicePtrs a, DevicePtrs b)
  type HostPtrs   (a,b) = (HostPtrs   a, HostPtrs   b)

  mallocArray ad n      = mallocArray (fst' ad) n *> mallocArray (snd' ad) n
  peekArray ad n        = peekArray (fst' ad) n   *> peekArray (snd' ad) n
  pokeArray ad n        = pokeArray (fst' ad) n   *> pokeArray (snd' ad) n
  peekArrayAsync ad n s = peekArrayAsync (fst' ad) n s *> peekArrayAsync (snd' ad) n s
  pokeArrayAsync ad n s = pokeArrayAsync (fst' ad) n s *> pokeArrayAsync (snd' ad) n s
  touch ad              = touch (fst' ad) *> touch (snd' ad)
  free ad               = free  (fst' ad) *> free  (snd' ad)
  devicePtrs ad         = (++) <$> devicePtrs (fst' ad) <*> devicePtrs (snd' ad)


--
-- Machinery
-- ~~~~~~~~~

-- Generate a memory map key from the given ArrayData
--
arrayToKey :: (Acc.ArrayPtrs e ~ Ptr a, Acc.ArrayElem e) => Acc.ArrayData e -> IM.Key
arrayToKey = fromIntegral . ptrToWordPtr . Acc.ptrsOfArrayData
{-# INLINE arrayToKey #-}

-- Retrieve the device array from the state structure associated with a
-- particular Accelerate array
--
getArray :: (Acc.ArrayPtrs e ~ Ptr a, Acc.ArrayElem e) => Acc.ArrayData e -> CIO MemoryEntry
getArray ad = fromMaybe (error "ArrayElem: internal error")
            . IM.lookup (arrayToKey ad) <$> getM memoryEntry
{-# INLINE getArray #-}


-- Copy data from the device into the associated Accelerate array
--
peekArray' :: (Acc.ArrayPtrs e ~ Ptr a, Storable a, Acc.ArrayElem e)
           => Acc.ArrayData e -> Int -> CIO ()
peekArray' ad n =
  let dst = Acc.ptrsOfArrayData ad
      src = CUDA.wordPtrToDevPtr . get arena
  in
  getArray ad >>= \me -> liftIO $ CUDA.peekArray n (src me) dst

peekArrayAsync' :: (Acc.ArrayPtrs e ~ Ptr a, Storable a, Acc.ArrayElem e)
                => Acc.ArrayData e -> Int -> Maybe CUDA.Stream -> CIO ()
peekArrayAsync' ad n st =
  let dst = CUDA.HostPtr . Acc.ptrsOfArrayData
      src = CUDA.wordPtrToDevPtr . get arena
  in
  getArray ad >>= \me -> liftIO $ CUDA.peekArrayAsync n (src me) (dst ad) st


-- Copy data from an Accelerate array to the associated device array
--
pokeArray' :: (Acc.ArrayPtrs e ~ Ptr a, Storable a, Acc.ArrayElem e)
           => Acc.ArrayData e -> Int -> CIO ()
pokeArray' ad n =
  let src = Acc.ptrsOfArrayData ad
      dst = CUDA.wordPtrToDevPtr . get arena
  in do
    me <- mod refcount (+1) <$> getArray ad
    when (get refcount me <= 1) . liftIO $ CUDA.pokeArray n src (dst me)
    modM memoryEntry (IM.insert (arrayToKey ad) me)

pokeArrayAsync' :: (Acc.ArrayPtrs e ~ Ptr a, Storable a, Acc.ArrayElem e)
                => Acc.ArrayData e -> Int -> Maybe CUDA.Stream -> CIO ()
pokeArrayAsync' ad n st =
  let src = CUDA.HostPtr . Acc.ptrsOfArrayData
      dst = CUDA.wordPtrToDevPtr . get arena
  in do
    me <- mod refcount (+1) <$> getArray ad
    when (get refcount me <= 1) . liftIO $ CUDA.pokeArrayAsync n (src ad) (dst me) st
    modM memoryEntry (IM.insert (arrayToKey ad) me)

-- Insert the ArrayData / DevicePtr couple into the memory map
--
insertArray :: (Acc.ArrayPtrs e ~ Ptr a, Acc.ArrayElem e)
            => Acc.ArrayData e -> CUDA.DevicePtr a -> CIO ()
insertArray ad = modM memoryEntry . IM.insert (arrayToKey ad) . MemoryEntry 0 . CUDA.devPtrToWordPtr

-- Release a device array, when its reference counter drops to zero
--
free' :: (Acc.ArrayPtrs e ~ Ptr a, Acc.ArrayElem e)
      => Acc.ArrayData e -> CIO ()
free' ad = do
  me <- mod refcount (subtract 1) <$> getArray ad
  if get refcount me > 0
     then modM memoryEntry (IM.insert (arrayToKey ad) me)
     else do liftIO . CUDA.free . CUDA.wordPtrToDevPtr $ get arena me
             modM memoryEntry (IM.delete (arrayToKey ad))

-- Increase reference count of an array
--
touch' :: (Acc.ArrayPtrs e ~ Ptr a, Acc.ArrayElem e) => Acc.ArrayData e -> CIO ()
touch' ad = modM memoryEntry =<< IM.insert (arrayToKey ad) . mod refcount (+1) <$> getArray ad

-- Return the device pointers wrapped in a list of function parameters
--
devicePtrs' :: (Acc.ArrayPtrs e ~ Ptr a, Acc.ArrayElem e)
            => Acc.ArrayData e -> CIO [CUDA.FunParam]
devicePtrs' ad = (: []) . CUDA.VArg . CUDA.wordPtrToDevPtr . get arena <$> getArray ad

-- Array tuple extraction
--
fst' :: Acc.ArrayData (a,b) -> Acc.ArrayData a
fst' = Acc.fstArrayData

snd' :: Acc.ArrayData (a,b) -> Acc.ArrayData b
snd' = Acc.sndArrayData

