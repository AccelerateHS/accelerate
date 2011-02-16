{-# LANGUAGE CPP, FlexibleContexts, ScopedTypeVariables, TypeFamilies #-}
-- |
-- Module      : Data.Array.Accelerate.CUDA.Array.Data
-- Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.CUDA.Array.Data (

  -- * Array operations and representations
  ArrayElt(..),

  -- * Additional operations
  touchArray, bindArray, unbindArray,

) where

import Prelude hiding (id, (.))
import Control.Category

import Foreign.Ptr
import Foreign.Storable                                 (Storable, sizeOf)
import qualified Foreign                                as F

import Data.Int
import Data.Word
import Data.Maybe
import Control.Applicative
import Control.Monad.IO.Class
import qualified Data.HashTable                         as Hash

import Data.Array.Accelerate.CUDA.State
import qualified Data.Array.Accelerate.Array.Data       as AD
import qualified Foreign.CUDA.Driver                    as CUDA
import qualified Foreign.CUDA.Driver.Stream             as CUDA
import qualified Foreign.CUDA.Driver.Texture            as CUDA

#include "accelerate.h"


-- Array Operations
-- ----------------

class AD.ArrayElt e => ArrayElt e where
  type DevicePtrs e
  type HostPtrs   e

  -- | Allocate a new device array to accompany the given host-side array
  mallocArray :: AD.ArrayData e -> Maybe Int -> Int -> CIO ()

  -- | Release a device array, when its reference count drops to zero
  freeArray :: AD.ArrayData e -> CIO ()

  -- | Array indexing
  indexArray :: AD.ArrayData e -> Int -> CIO e

  -- | Copy data between two device arrays
  copyArray :: AD.ArrayData e -> AD.ArrayData e -> Int -> CIO ()

  -- | Copy data from the device into its associated host-side Accelerate array
  peekArray :: AD.ArrayData e -> Int -> CIO ()

  -- | Copy data from an Accelerate array into the associated device array,
  -- which must have already been allocated.
  pokeArray :: AD.ArrayData e -> Int -> CIO ()

  -- | Asynchronous device -> host copy
  peekArrayAsync :: AD.ArrayData e -> Int -> Maybe CUDA.Stream -> CIO ()

  -- | Asynchronous host -> device copy
  pokeArrayAsync :: AD.ArrayData e -> Int -> Maybe CUDA.Stream -> CIO ()

  -- | Wrap the device pointers corresponding to a host-side array into
  -- arguments that can be passed to a kernel upon invocation
  marshalArrayData :: AD.ArrayData e -> CIO [CUDA.FunParam]

  -- | Bind the device memory arrays to the given texture reference(s), setting
  -- appropriate type. The number of components bound is returned.
  marshalTextureData :: AD.ArrayData e -> Int -> [CUDA.Texture] -> CIO Int

  -- | Modify the basic device memory reference for a given host-side array
  basicModify :: AD.ArrayData e -> (MemoryEntry -> MemoryEntry) -> CIO ()


instance ArrayElt () where
  type DevicePtrs () = ()
  type HostPtrs   () = ()
  freeArray          _     = return ()
  mallocArray        _ _ _ = return ()
  indexArray         _ _   = return ()
  copyArray          _ _ _ = return ()
  peekArray          _ _   = return ()
  pokeArray          _ _   = return ()
  peekArrayAsync     _ _ _ = return ()
  pokeArrayAsync     _ _ _ = return ()
  marshalArrayData   _     = return []
  marshalTextureData _ _ _ = return 0
  basicModify        _ _   = return ()


#define primArrayElt_(ty,con)                                                  \
instance ArrayElt ty where {                                                   \
  type DevicePtrs ty = CUDA.DevicePtr con                                      \
; type HostPtrs   ty = CUDA.HostPtr   con                                      \
; mallocArray             = mallocArray'                                       \
; freeArray               = freeArray'                                         \
; indexArray              = indexArray'                                        \
; copyArray               = copyArray'                                         \
; peekArray               = peekArray'                                         \
; pokeArray               = pokeArray'                                         \
; peekArrayAsync          = peekArrayAsync'                                    \
; pokeArrayAsync          = pokeArrayAsync'                                    \
; marshalArrayData        = marshalArrayData'                                  \
; marshalTextureData ad n = marshalTextureData' ad n . head                    \
; basicModify             = basicModify' }

#define primArrayElt(ty) primArrayElt_(ty,ty)

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
instance ArrayElt Bool where
  type HostPtrs   Bool = ()
  type DevicePtrs Bool = ()
  mallocArray        = error "TODO: ArrayElt Bool"
  freeArray          = undefined
  indexArray         = undefined
  copyArray          = undefined
  peekArray          = undefined
  pokeArray          = undefined
  peekArrayAsync     = undefined
  pokeArrayAsync     = undefined
  marshalArrayData   = undefined
  marshalTextureData = undefined
  basicModify        = undefined

instance ArrayElt Char where
  type HostPtrs   Char = ()
  type DevicePtrs Char = ()
  mallocArray        = error "TODO: ArrayElt Char"
  freeArray          = undefined
  indexArray         = undefined
  copyArray          = undefined
  peekArray          = undefined
  pokeArray          = undefined
  peekArrayAsync     = undefined
  pokeArrayAsync     = undefined
  marshalArrayData   = undefined
  marshalTextureData = undefined
  basicModify        = undefined

-- FIXME:
-- CChar
-- CSChar
-- CUChar

instance (ArrayElt a, ArrayElt b) => ArrayElt (a,b) where
  type DevicePtrs (a,b) = (DevicePtrs a, DevicePtrs b)
  type HostPtrs   (a,b) = (HostPtrs   a, HostPtrs   b)

  freeArray ad              = freeArray   (fst' ad)   *> freeArray   (snd' ad)
  peekArray ad n            = peekArray   (fst' ad) n *> peekArray   (snd' ad) n
  pokeArray ad n            = pokeArray   (fst' ad) n *> pokeArray   (snd' ad) n
  basicModify ad f          = basicModify (fst' ad) f *> basicModify (snd' ad) f
  mallocArray ad rc n       = mallocArray (fst' ad) rc n   *> mallocArray (snd' ad) rc n
  peekArrayAsync ad n s     = peekArrayAsync (fst' ad) n s *> peekArrayAsync (snd' ad) n s
  pokeArrayAsync ad n s     = pokeArrayAsync (fst' ad) n s *> pokeArrayAsync (snd' ad) n s
  copyArray src dst n       = copyArray (fst' src) (fst' dst) n *> copyArray (snd' src) (snd' dst) n
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


-- Auxiliary Functions
-- -------------------

-- Increase the reference count of an array
--
touchArray :: ArrayElt e => AD.ArrayData e -> CIO ()
touchArray ad = basicModify ad (modL refcount (fmap (+1)))


-- Set/unset an array to never be released by a call to 'freeArray'. When the
-- array is unbound, its reference count is set to zero.
--
bindArray :: ArrayElt e => AD.ArrayData e -> CIO ()
bindArray ad = basicModify ad (setL refcount Nothing)

unbindArray :: ArrayElt e => AD.ArrayData e -> CIO ()
unbindArray ad = basicModify ad (setL refcount (Just 0))


-- ArrayElt Implementation
-- ------------------------

-- Allocate a new device array to accompany the given host-side Accelerate array
--
mallocArray'
  :: forall a e. (AD.ArrayPtrs e ~ Ptr a, Storable a, AD.ArrayElt e)
  => AD.ArrayData e     -- host array data (reference)
  -> Maybe Int          -- initial reference count for this array; Nothing == bound array
  -> Int                -- number of elements
  -> CIO ()

mallocArray' ad rc n = do
  tab <- getM memoryTable
  val <- liftIO $ Hash.lookup tab (arrayToKey ad)
  case val of
       Just _m -> INTERNAL_ASSERT "mallocArray" (bytes <= getL memsize _m) $ return ()
       Nothing -> insert' ad =<< liftIO (CUDA.mallocArray n)
  where
    insert' :: (AD.ArrayPtrs e ~ Ptr a, AD.ArrayElt e, Storable a) => AD.ArrayData e -> CUDA.DevicePtr a -> CIO ()
    insert' _ = updateArray ad . MemoryEntry rc bytes . CUDA.devPtrToWordPtr
    bytes     = fromIntegral $ n * sizeOf (undefined :: a)


-- Release a device array, when its reference counter drops to zero
--
freeArray'
  :: (AD.ArrayPtrs e ~ Ptr a, AD.ArrayElt e)
  => AD.ArrayData e     -- host array
  -> CIO ()

freeArray' ad = free . modL refcount (fmap (subtract 1)) =<< lookupArray ad
  where
    free v = case getL refcount v of
      Nothing        -> return ()
      Just x | x > 0 -> updateArray ad v
      _              -> deleteArray ad


-- Array indexing
--
indexArray'
  :: (AD.ArrayPtrs e ~ Ptr a, Storable a, AD.ArrayElt e)
  => AD.ArrayData e     -- host array data
  -> Int                -- index in row-major representation
  -> CIO a

indexArray' ad n = do
  dp <- getArray ad
  liftIO . F.alloca $ \p -> do
    CUDA.peekArray 1 (dp `CUDA.advanceDevPtr` n) p
    F.peek p


-- Copy data between two device arrays
--
copyArray'
  :: forall a e. (AD.ArrayPtrs e ~ Ptr a, Storable a, AD.ArrayElt e)
  => AD.ArrayData e     -- source array
  -> AD.ArrayData e     -- destination
  -> Int                -- number of elements
  -> CIO ()

copyArray' src' dst' n =
  let bytes = n * sizeOf (undefined::a)
  in do
    src <- getArray src'
    dst <- getArray dst'
    liftIO $ CUDA.copyArrayAsync bytes src dst


-- Copy data from the device into the associated Accelerate array
--
peekArray'
  :: (AD.ArrayPtrs e ~ Ptr a, Storable a, AD.ArrayElt e)
  => AD.ArrayData e     -- host array data
  -> Int                -- number of elements
  -> CIO ()

peekArray' ad n =
  let dst = AD.ptrsOfArrayData ad
      src = CUDA.wordPtrToDevPtr . getL arena
  in
  lookupArray ad >>= \me -> liftIO $ CUDA.peekArray n (src me) dst

peekArrayAsync'
  :: (AD.ArrayPtrs e ~ Ptr a, Storable a, AD.ArrayElt e)
  => AD.ArrayData e     -- host array data
  -> Int                -- number of elements
  -> Maybe CUDA.Stream  -- asynchronous stream (optional)
  -> CIO ()

peekArrayAsync' ad n st =
  let dst = CUDA.HostPtr . AD.ptrsOfArrayData
      src = CUDA.wordPtrToDevPtr . getL arena
  in
  lookupArray ad >>= \me -> liftIO $ CUDA.peekArrayAsync n (src me) (dst ad) st


-- Copy data from an Accelerate array to the associated device array. The data
-- will be copied from the host-side array each time this function is called; no
-- changes to the reference counter will be made.
--
pokeArray'
  :: (AD.ArrayPtrs e ~ Ptr a, Storable a, AD.ArrayElt e)
  => AD.ArrayData e     -- host array data
  -> Int                -- number of elements
  -> CIO ()

pokeArray' ad n = upload =<< lookupArray ad
  where
    src      = AD.ptrsOfArrayData
    dst      = CUDA.wordPtrToDevPtr . getL arena
    upload v = liftIO $ CUDA.pokeArray n (src ad) (dst v)

pokeArrayAsync'
  :: (AD.ArrayPtrs e ~ Ptr a, Storable a, AD.ArrayElt e)
  => AD.ArrayData e     -- host array reference
  -> Int                -- number of elements
  -> Maybe CUDA.Stream  -- asynchronous stream to associate (optional)
  -> CIO ()

pokeArrayAsync' ad n st = upload =<< lookupArray ad
  where
    src      = CUDA.HostPtr . AD.ptrsOfArrayData
    dst      = CUDA.wordPtrToDevPtr . getL arena
    upload v = liftIO $ CUDA.pokeArrayAsync n (src ad) (dst v) st


-- Wrap the device pointers corresponding to a host-side array into arguments
-- that can be passed to a kernel on invocation.
--
marshalArrayData'
  :: (AD.ArrayPtrs e ~ Ptr a, AD.ArrayElt e)
  => AD.ArrayData e
  -> CIO [CUDA.FunParam]

marshalArrayData' ad = return . CUDA.VArg <$> getArray ad


-- Bind device memory to the given texture reference, setting appropriate type
--
marshalTextureData'
  :: forall a e. (AD.ArrayPtrs e ~ Ptr a, AD.ArrayElt e, Storable a, TextureData a)
  => AD.ArrayData e     -- host array data
  -> Int                -- number of elements
  -> CUDA.Texture       -- texture reference to bind to
  -> CIO Int

marshalTextureData' ad n tex = do
  let (fmt,c) = format (undefined :: a)
  ptr <- getArray ad
  liftIO $ do
    CUDA.setFormat tex fmt c
    CUDA.bind tex ptr (fromIntegral $ n * sizeOf (undefined :: a))
    return 1


-- Modify the internal memory reference for a host-side array
--
basicModify'
  :: (AD.ArrayPtrs e ~ Ptr a, AD.ArrayElt e)
  => AD.ArrayData e
  -> (MemoryEntry -> MemoryEntry)
  -> CIO ()

basicModify' ad f = updateArray ad . f =<< lookupArray ad


-- Utilities
-- ---------

-- dumpMemTable :: CIO String
-- dumpMemTable = do
--   tab <- getM memoryTable
--   unlines . map entry <$> liftIO (Hash.toList tab)
--   where
--     entry (k,MemoryEntry c m a) = " -=" ++ unwords
--       [shows k ":", "refcount=", show c, "memsize=", show m, "arena=", show a]

-- Generate a memory map key from the given ArrayData
--
arrayToKey :: (AD.ArrayPtrs e ~ Ptr a, AD.ArrayElt e) => AD.ArrayData e -> WordPtr
arrayToKey = ptrToWordPtr . AD.ptrsOfArrayData

-- Retrieve the device memory entry from the state structure associated with a
-- particular Accelerate array.
--
lookupArray :: (AD.ArrayPtrs e ~ Ptr a, AD.ArrayElt e) => AD.ArrayData e -> CIO MemoryEntry
{-# INLINE lookupArray #-}
lookupArray ad = do
  t <- getM memoryTable
  x <- liftIO $ Hash.lookup t (arrayToKey ad)
  case x of
       Just e -> return e
       _      -> INTERNAL_ERROR(error) "lookupArray" "lost device memory reference"
                 -- TLM: better if the file/line markings are of the use site

-- Update (or insert) a memory entry into the state structure
--
updateArray :: (AD.ArrayPtrs e ~ Ptr a, AD.ArrayElt e) => AD.ArrayData e -> MemoryEntry -> CIO ()
{-# INLINE updateArray #-}
updateArray ad me = do
  t <- getM memoryTable
  liftIO $ Hash.update t (arrayToKey ad) me >> return ()

-- Delete an entry from the state structure and release the corresponding device
-- memory area
--
deleteArray :: (AD.ArrayPtrs e ~ Ptr a, AD.ArrayElt e) => AD.ArrayData e -> CIO ()
{-# INLINE deleteArray #-}
deleteArray ad = do
  let key = arrayToKey ad
  tab <- getM memoryTable
  liftIO $ do
    CUDA.free . CUDA.wordPtrToDevPtr . getL arena . fromJust =<< Hash.lookup tab key
    Hash.delete tab key

-- Return the device pointer associated with a host-side Accelerate array
--
getArray :: (AD.ArrayPtrs e ~ Ptr a, AD.ArrayElt e) => AD.ArrayData e -> CIO (CUDA.DevicePtr a)
{-# INLINE getArray #-}
getArray ad = CUDA.wordPtrToDevPtr . getL arena <$> lookupArray ad

-- Array tuple extraction
--
fst' :: AD.ArrayData (a,b) -> AD.ArrayData a
fst' = AD.fstArrayData

snd' :: AD.ArrayData (a,b) -> AD.ArrayData b
snd' = AD.sndArrayData

