{-# LANGUAGE CPP, FlexibleContexts, TypeFamilies #-}
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
import Foreign.Storable                                 (Storable, sizeOf)
import qualified Foreign                                as F

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
import qualified Foreign.CUDA.Driver.Texture            as CUDA


-- Instances
-- ~~~~~~~~~

class Acc.ArrayElem e => ArrayElem e where
  type DevicePtrs e
  type HostPtrs   e
  mallocArray    :: Acc.ArrayData e -> Int -> CIO ()
  indexArray     :: Acc.ArrayData e -> Int -> CIO e
  copyArray      :: Acc.ArrayData e -> Acc.ArrayData e -> Int -> CIO ()
  peekArray      :: Acc.ArrayData e -> Int -> CIO ()
  pokeArray      :: Acc.ArrayData e -> Int -> CIO ()
  peekArrayAsync :: Acc.ArrayData e -> Int -> Maybe CUDA.Stream -> CIO ()
  pokeArrayAsync :: Acc.ArrayData e -> Int -> Maybe CUDA.Stream -> CIO ()
  textureRefs    :: Acc.ArrayData e -> CUDA.Module -> Int -> Int -> CIO [CUDA.FunParam]
  devicePtrs     :: Acc.ArrayData e -> CIO [CUDA.FunParam]
  touchArray     :: Acc.ArrayData e -> CIO ()
  freeArray      :: Acc.ArrayData e -> CIO ()

  -- FIXME: remove once all ArrayElem instances are concrete
  mallocArray    = undefined
  indexArray     = undefined
  copyArray      = undefined
  peekArray      = undefined
  pokeArray      = undefined
  peekArrayAsync = undefined
  pokeArrayAsync = undefined
  textureRefs    = undefined
  devicePtrs     = undefined
  touchArray     = undefined
  freeArray      = undefined


instance ArrayElem () where
  type DevicePtrs () = CUDA.DevicePtr ()
  type HostPtrs   () = CUDA.HostPtr   ()
  mallocArray    _ _     = return ()
  indexArray     _ _     = return ()
  copyArray      _ _ _   = return ()
  peekArray      _ _     = return ()
  pokeArray      _ _     = return ()
  peekArrayAsync _ _ _   = return ()
  pokeArrayAsync _ _ _   = return ()
  textureRefs    _ _ _ _ = return []
  devicePtrs     _       = return []
  touchArray     _       = return ()
  freeArray      _       = return ()


#define primArrayElem_(ty,con)                                                 \
instance ArrayElem ty where {                                                  \
  type DevicePtrs ty = CUDA.DevicePtr con                                      \
; type HostPtrs   ty = CUDA.HostPtr   con                                      \
; mallocArray      = mallocArray'                                              \
; indexArray       = indexArray'                                               \
; copyArray        = copyArray'                                                \
; peekArray        = peekArray'                                                \
; pokeArray        = pokeArray'                                                \
; peekArrayAsync   = peekArrayAsync'                                           \
; pokeArrayAsync   = pokeArrayAsync'                                           \
; textureRefs      = textureRefs'                                              \
; devicePtrs       = devicePtrs'                                               \
; touchArray       = touchArray'                                               \
; freeArray        = freeArray' }

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

  mallocArray ad n       = mallocArray (fst' ad) n *> mallocArray (snd' ad) n
  peekArray ad n         = peekArray (fst' ad) n   *> peekArray (snd' ad) n
  pokeArray ad n         = pokeArray (fst' ad) n   *> pokeArray (snd' ad) n
  copyArray src dst n    = copyArray (fst' src) (fst' dst) n *> copyArray (snd' src) (snd' dst) n
  peekArrayAsync ad n s  = peekArrayAsync (fst' ad) n s *> peekArrayAsync (snd' ad) n s
  pokeArrayAsync ad n s  = pokeArrayAsync (fst' ad) n s *> pokeArrayAsync (snd' ad) n s
  touchArray ad          = touchArray (fst' ad) *> touchArray (snd' ad)
  freeArray ad           = freeArray  (fst' ad) *> freeArray  (snd' ad)
  indexArray ad n        = (,)  <$> indexArray (fst' ad) n <*> indexArray (snd' ad) n
  devicePtrs ad          = (++) <$> devicePtrs (fst' ad)   <*> devicePtrs (snd' ad)
  textureRefs ad mdl n t = do   -- PRETTY ME
    t1 <- textureRefs (fst' ad) mdl n t
    t2 <- textureRefs (snd' ad) mdl n (t+ length t1)
    return (t1 ++ t2)


-- Implementation
-- ~~~~~~~~~~~~~~

-- Allocate a new device array to accompany the given host-side Accelerate array
--
mallocArray' :: (Acc.ArrayPtrs e ~ Ptr a, Storable a, Acc.ArrayElem e) => Acc.ArrayData e -> Int -> CIO ()
mallocArray' ad n = do
  exists <- IM.member key <$> getM memoryEntry
  unless exists $ insertArray ad =<< liftIO (CUDA.mallocArray n)
  where
    insertArray :: (Acc.ArrayPtrs e ~ Ptr a, Acc.ArrayElem e) => Acc.ArrayData e -> CUDA.DevicePtr a -> CIO ()
    insertArray _ = modM memoryEntry . IM.insert key . MemoryEntry 0 . CUDA.devPtrToWordPtr
    key           = arrayToKey ad


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
      src = CUDA.wordPtrToDevPtr . get arena
  in
  lookupArray ad >>= \me -> liftIO $ CUDA.peekArray n (src me) dst

peekArrayAsync' :: (Acc.ArrayPtrs e ~ Ptr a, Storable a, Acc.ArrayElem e) => Acc.ArrayData e -> Int -> Maybe CUDA.Stream -> CIO ()
peekArrayAsync' ad n st =
  let dst = CUDA.HostPtr . Acc.ptrsOfArrayData
      src = CUDA.wordPtrToDevPtr . get arena
  in
  lookupArray ad >>= \me -> liftIO $ CUDA.peekArrayAsync n (src me) (dst ad) st


-- Copy data from an Accelerate array to the associated device array. The data
-- will only be copied once, with subsequent invocations incrementing the
-- reference counter.
--
pokeArray' :: (Acc.ArrayPtrs e ~ Ptr a, Storable a, Acc.ArrayElem e) => Acc.ArrayData e -> Int -> CIO ()
pokeArray' ad n =
  let src = Acc.ptrsOfArrayData ad
      dst = CUDA.wordPtrToDevPtr . get arena
  in do
    me <- mod refcount (+1) <$> lookupArray ad
    when (get refcount me <= 1) . liftIO $ CUDA.pokeArray n src (dst me)
    modM memoryEntry (IM.insert (arrayToKey ad) me)

pokeArrayAsync' :: (Acc.ArrayPtrs e ~ Ptr a, Storable a, Acc.ArrayElem e) => Acc.ArrayData e -> Int -> Maybe CUDA.Stream -> CIO ()
pokeArrayAsync' ad n st =
  let src = CUDA.HostPtr . Acc.ptrsOfArrayData
      dst = CUDA.wordPtrToDevPtr . get arena
  in do
    me <- mod refcount (+1) <$> lookupArray ad
    when (get refcount me <= 1) . liftIO $ CUDA.pokeArrayAsync n (src ad) (dst me) st
    modM memoryEntry (IM.insert (arrayToKey ad) me)


-- Release a device array, when its reference counter drops to zero
--
freeArray' :: (Acc.ArrayPtrs e ~ Ptr a, Acc.ArrayElem e) => Acc.ArrayData e -> CIO ()
freeArray' ad = do
  me <- mod refcount (subtract 1) <$> lookupArray ad
  if get refcount me > 0
     then modM memoryEntry (IM.insert (arrayToKey ad) me)
     else do liftIO . CUDA.free . CUDA.wordPtrToDevPtr $ get arena me
             modM memoryEntry (IM.delete (arrayToKey ad))


-- Increase the reference count of an array. You may wish to call this right
-- before another method that will implicitly attempt to release the array.
--
touchArray' :: (Acc.ArrayPtrs e ~ Ptr a, Acc.ArrayElem e) => Acc.ArrayData e -> CIO ()
touchArray' ad = modM memoryEntry =<< IM.insert (arrayToKey ad) . mod refcount (+1) <$> lookupArray ad


-- Return the device pointers wrapped in a list of function parameters
--
devicePtrs' :: (Acc.ArrayPtrs e ~ Ptr a, Acc.ArrayElem e) => Acc.ArrayData e -> CIO [CUDA.FunParam]
devicePtrs' ad = (: []) . CUDA.VArg . CUDA.wordPtrToDevPtr . get arena <$> lookupArray ad


-- Retrieve texture references from the module (beginning with the given seed),
-- bind device pointers, and return as a list of function arguments.
--
textureRefs' :: forall a e. (Acc.ArrayPtrs e ~ Ptr a, Acc.ArrayElem e, Storable a, CUDA.Format a)
             => Acc.ArrayData e -> CUDA.Module -> Int -> Int -> CIO [CUDA.FunParam]
textureRefs' ad mdl n t = do
  ptr <- getArray ad
  tex <- liftIO $ CUDA.getTex mdl ("tex" ++ show t)
  liftIO $ CUDA.setPtr tex ptr (n * sizeOf (undefined :: a))
  return [CUDA.TArg tex]


-- Utilities
-- ~~~~~~~~~

-- Generate a memory map key from the given ArrayData
--
arrayToKey :: (Acc.ArrayPtrs e ~ Ptr a, Acc.ArrayElem e) => Acc.ArrayData e -> IM.Key
arrayToKey = fromIntegral . ptrToWordPtr . Acc.ptrsOfArrayData
{-# INLINE arrayToKey #-}

-- Retrieve the device memory entry from the state structure associated with a
-- particular Accelerate array.
--
lookupArray :: (Acc.ArrayPtrs e ~ Ptr a, Acc.ArrayElem e) => Acc.ArrayData e -> CIO MemoryEntry
lookupArray ad = fromMaybe (error "ArrayElem: internal error") . IM.lookup (arrayToKey ad) <$> getM memoryEntry
{-# INLINE lookupArray #-}

-- Return the device pointer associated with a host-side Accelerate array
--
getArray :: (Acc.ArrayPtrs e ~ Ptr a, Acc.ArrayElem e) => Acc.ArrayData e -> CIO (CUDA.DevicePtr a)
getArray ad = CUDA.wordPtrToDevPtr . get arena <$> lookupArray ad
{-# INLINE getArray #-}

-- Array tuple extraction
--
fst' :: Acc.ArrayData (a,b) -> Acc.ArrayData a
fst' = Acc.fstArrayData
{-# INLINE fst' #-}

snd' :: Acc.ArrayData (a,b) -> Acc.ArrayData b
snd' = Acc.sndArrayData
{-# INLINE snd' #-}

