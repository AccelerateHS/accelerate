{-# LANGUAGE CPP, FlexibleContexts, TypeFamilies, ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.CUDA.Array.Data
-- Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.CUDA.Array.Data (
  ArrayElem(..)
) where

import Prelude hiding (id, (.))
import Control.Category

import Foreign.Ptr
import Foreign.Storable                                 (Storable, sizeOf)
import qualified Foreign                                as F

import Data.Int
import Data.Word
import Data.Maybe
import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import qualified Data.HashTable                         as HT

import Data.Array.Accelerate.CUDA.State
import qualified Data.Array.Accelerate.Array.Data       as Acc
import qualified Foreign.CUDA.Driver                    as CUDA
import qualified Foreign.CUDA.Driver.Stream             as CUDA
import qualified Foreign.CUDA.Driver.Texture            as CUDA

#include "accelerate.h"


-- Array Operations
-- ----------------

class Acc.ArrayElem e => ArrayElem e where
  type DevicePtrs e
  type HostPtrs   e
  mallocArray        :: Acc.ArrayData e -> Int -> CIO ()
  indexArray         :: Acc.ArrayData e -> Int -> CIO e
  copyArray          :: Acc.ArrayData e -> Acc.ArrayData e -> Int -> CIO ()
  peekArray          :: Acc.ArrayData e -> Int -> CIO ()
  pokeArray          :: Acc.ArrayData e -> Int -> CIO ()
  peekArrayAsync     :: Acc.ArrayData e -> Int -> Maybe CUDA.Stream -> CIO ()
  pokeArrayAsync     :: Acc.ArrayData e -> Int -> Maybe CUDA.Stream -> CIO ()
  marshalArrayData   :: Acc.ArrayData e -> CIO [CUDA.FunParam]
  marshalTextureData :: Acc.ArrayData e -> Int -> [CUDA.Texture] -> CIO Int
  freeArray          :: Acc.ArrayData e -> CIO ()

  -- FIXME: remove once all ArrayElem instances are concrete
  mallocArray        = undefined
  indexArray         = undefined
  copyArray          = undefined
  peekArray          = undefined
  pokeArray          = undefined
  peekArrayAsync     = undefined
  pokeArrayAsync     = undefined
  marshalArrayData   = undefined
  marshalTextureData = undefined
  freeArray          = undefined


instance ArrayElem () where
  type DevicePtrs () = ()
  type HostPtrs   () = ()
  mallocArray        _ _   = return ()
  indexArray         _ _   = return ()
  copyArray          _ _ _ = return ()
  peekArray          _ _   = return ()
  pokeArray          _ _   = return ()
  peekArrayAsync     _ _ _ = return ()
  pokeArrayAsync     _ _ _ = return ()
  marshalArrayData   _     = return []
  marshalTextureData _ _ _ = return 0
  freeArray          _     = return ()


#define primArrayElem_(ty,con)                                                 \
instance ArrayElem ty where {                                                  \
  type DevicePtrs ty = CUDA.DevicePtr con                                      \
; type HostPtrs   ty = CUDA.HostPtr   con                                      \
; mallocArray             = mallocArray'                                       \
; indexArray              = indexArray'                                        \
; copyArray               = copyArray'                                         \
; peekArray               = peekArray'                                         \
; pokeArray               = pokeArray'                                         \
; peekArrayAsync          = peekArrayAsync'                                    \
; pokeArrayAsync          = pokeArrayAsync'                                    \
; marshalArrayData        = marshalArrayData'                                  \
; marshalTextureData ad n = marshalTextureData' ad n . head                    \
; freeArray               = freeArray' }

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

-- FIXME:
-- No concrete implementation in Data.Array.Accelerate.Array.Data
--
instance ArrayElem Bool
instance ArrayElem Char

-- FIXME:
-- CChar
-- CSChar
-- CUChar

instance (ArrayElem a, ArrayElem b) => ArrayElem (a,b) where
  type DevicePtrs (a,b) = (DevicePtrs a, DevicePtrs b)
  type HostPtrs   (a,b) = (HostPtrs   a, HostPtrs   b)

  mallocArray ad n          = mallocArray (fst' ad) n *> mallocArray (snd' ad) n
  peekArray ad n            = peekArray (fst' ad) n   *> peekArray (snd' ad) n
  pokeArray ad n            = pokeArray (fst' ad) n   *> pokeArray (snd' ad) n
  copyArray src dst n       = copyArray (fst' src) (fst' dst) n *> copyArray (snd' src) (snd' dst) n
  peekArrayAsync ad n s     = peekArrayAsync (fst' ad) n s *> peekArrayAsync (snd' ad) n s
  pokeArrayAsync ad n s     = pokeArrayAsync (fst' ad) n s *> pokeArrayAsync (snd' ad) n s
  freeArray ad              = freeArray  (fst' ad) *> freeArray  (snd' ad)
  indexArray ad n           = (,)  <$> indexArray (fst' ad) n <*> indexArray (snd' ad) n
  marshalArrayData ad       = (++) <$> marshalArrayData (fst' ad) <*> marshalArrayData (snd' ad)
  marshalTextureData ad n t = do
    k <- marshalTextureData (fst' ad) n t
    l <- marshalTextureData (snd' ad) n (drop k t)
    return (k+l)


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


-- Implementation
-- --------------

-- Allocate a new device array to accompany the given host-side Accelerate array
--
mallocArray' :: forall a e. (Acc.ArrayPtrs e ~ Ptr a, Storable a, Acc.ArrayElem e) => Acc.ArrayData e -> Int -> CIO ()
mallocArray' ad n = do
  table  <- getM memoryTable
  exists <- isJust <$> liftIO (HT.lookup table key)
  unless exists . liftIO $ insertArray ad table =<< CUDA.mallocArray n
  where
    insertArray :: (Acc.ArrayPtrs e ~ Ptr a, Acc.ArrayElem e) => Acc.ArrayData e -> MemTable -> CUDA.DevicePtr a -> IO ()
    insertArray _ t = HT.insert t key . MemoryEntry 0 bytes . CUDA.devPtrToWordPtr
    bytes           = fromIntegral $ n * sizeOf (undefined :: a)
    key             = arrayToKey ad


-- Array indexing
--
indexArray' :: (Acc.ArrayPtrs e ~ Ptr a, Storable a, Acc.ArrayElem e) => Acc.ArrayData e -> Int -> CIO a
indexArray' ad n = do
  dp <- getArray ad
  liftIO . F.alloca $ \p -> do
    CUDA.peekArray 1 (dp `CUDA.advanceDevPtr` n) p
    F.peek p


-- Copy data between two device arrays
--
copyArray' :: forall a e. (Acc.ArrayPtrs e ~ Ptr a, Storable a, Acc.ArrayElem e) => Acc.ArrayData e -> Acc.ArrayData e -> Int -> CIO ()
copyArray' src' dst' n =
  let bytes = n * sizeOf (undefined::a)
  in do
    src <- getArray src'
    dst <- getArray dst'
    liftIO $ CUDA.copyArrayAsync bytes src dst


-- Copy data from the device into the associated Accelerate array
--
peekArray' :: (Acc.ArrayPtrs e ~ Ptr a, Storable a, Acc.ArrayElem e) => Acc.ArrayData e -> Int -> CIO ()
peekArray' ad n =
  let dst = Acc.ptrsOfArrayData ad
      src = CUDA.wordPtrToDevPtr . getL arena
  in
  lookupArray ad >>= \me -> liftIO $ CUDA.peekArray n (src me) dst

peekArrayAsync' :: (Acc.ArrayPtrs e ~ Ptr a, Storable a, Acc.ArrayElem e) => Acc.ArrayData e -> Int -> Maybe CUDA.Stream -> CIO ()
peekArrayAsync' ad n st =
  let dst = CUDA.HostPtr . Acc.ptrsOfArrayData
      src = CUDA.wordPtrToDevPtr . getL arena
  in
  lookupArray ad >>= \me -> liftIO $ CUDA.peekArrayAsync n (src me) (dst ad) st


-- Copy data from an Accelerate array to the associated device array. The data
-- will only be copied once, with subsequent invocations incrementing the
-- reference counter.
--
pokeArray' :: (Acc.ArrayPtrs e ~ Ptr a, Storable a, Acc.ArrayElem e) => Acc.ArrayData e -> Int -> CIO ()
pokeArray' ad n =
  let src = Acc.ptrsOfArrayData ad
      dst = CUDA.wordPtrToDevPtr . getL arena
  in do
    tab <- getM memoryTable
    me  <- modL refcount (+1) <$> lookupArray ad
    when (getL refcount me <= 1) . liftIO $ CUDA.pokeArray n src (dst me)
    liftIO $ HT.insert tab (arrayToKey ad) me

pokeArrayAsync' :: (Acc.ArrayPtrs e ~ Ptr a, Storable a, Acc.ArrayElem e) => Acc.ArrayData e -> Int -> Maybe CUDA.Stream -> CIO ()
pokeArrayAsync' ad n st =
  let src = CUDA.HostPtr . Acc.ptrsOfArrayData
      dst = CUDA.wordPtrToDevPtr . getL arena
  in do
    tab <- getM memoryTable
    me  <- modL refcount (+1) <$> lookupArray ad
    when (getL refcount me <= 1) . liftIO $ CUDA.pokeArrayAsync n (src ad) (dst me) st
    liftIO $ HT.insert tab (arrayToKey ad) me


-- Release a device array, when its reference counter drops to zero
--
freeArray' :: (Acc.ArrayPtrs e ~ Ptr a, Acc.ArrayElem e) => Acc.ArrayData e -> CIO ()
freeArray' ad = do
  tab <- getM memoryTable
  me  <- modL refcount (subtract 1) <$> lookupArray ad
  liftIO $ if getL refcount me > 0
              then HT.insert tab (arrayToKey ad) me
              else do CUDA.free . CUDA.wordPtrToDevPtr $ getL arena me
                      HT.delete tab (arrayToKey ad)


-- Wrap the device pointers corresponding to a host-side array into arguments
-- that can be passed to a kernel on invocation.
--
marshalArrayData' :: (Acc.ArrayPtrs e ~ Ptr a, Acc.ArrayElem e) => Acc.ArrayData e -> CIO [CUDA.FunParam]
marshalArrayData' ad = return . CUDA.VArg <$> getArray ad


-- Bind device memory to the given texture reference, setting appropriate type
--
marshalTextureData'
  :: forall a e. (Acc.ArrayPtrs e ~ Ptr a, Acc.ArrayElem e, Storable a, TextureData a)
  => Acc.ArrayData e -> Int -> CUDA.Texture -> CIO Int
marshalTextureData' ad n tex =
  let (fmt,c) = format (undefined :: a)
  in do
  ptr <- getArray ad
  liftIO $ do
    CUDA.setFormat tex fmt c
    CUDA.bind tex ptr (fromIntegral $ n * sizeOf (undefined :: a))
    return 1


-- Utilities
-- ---------

-- Generate a memory map key from the given ArrayData
--
arrayToKey :: (Acc.ArrayPtrs e ~ Ptr a, Acc.ArrayElem e) => Acc.ArrayData e -> WordPtr
arrayToKey = ptrToWordPtr . Acc.ptrsOfArrayData
{-# INLINE arrayToKey #-}

-- Retrieve the device memory entry from the state structure associated with a
-- particular Accelerate array.
--
lookupArray :: (Acc.ArrayPtrs e ~ Ptr a, Acc.ArrayElem e) => Acc.ArrayData e -> CIO MemoryEntry
{-# INLINE lookupArray #-}
lookupArray ad = do
  t <- getM memoryTable
  x <- liftIO $ HT.lookup t (arrayToKey ad)
  case x of
       Just e -> return e
       _      -> INTERNAL_ERROR(error) "lookupArray" "lost device memory reference"

-- Return the device pointer associated with a host-side Accelerate array
--
getArray :: (Acc.ArrayPtrs e ~ Ptr a, Acc.ArrayElem e) => Acc.ArrayData e -> CIO (CUDA.DevicePtr a)
getArray ad = CUDA.wordPtrToDevPtr . getL arena <$> lookupArray ad
{-# INLINE getArray #-}

-- Array tuple extraction
--
fst' :: Acc.ArrayData (a,b) -> Acc.ArrayData a
fst' = Acc.fstArrayData
{-# INLINE fst' #-}

snd' :: Acc.ArrayData (a,b) -> Acc.ArrayData b
snd' = Acc.sndArrayData
{-# INLINE snd' #-}

