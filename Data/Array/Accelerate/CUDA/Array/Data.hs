{-# LANGUAGE CPP, FlexibleContexts, PatternGuards, ScopedTypeVariables, GADTs, TypeFamilies #-}
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
  DevicePtrs, HostPtrs,
  freeArray, mallocArray, indexArray, copyArray, peekArray, pokeArray,
  peekArrayAsync, pokeArrayAsync, marshalArrayData, marshalTextureData,
  existsArrayData, devicePtrs,

  -- * Additional operations
  touchArray, bindArray, unbindArray

) where

import Prelude hiding (id, (.))
import Control.Category

import Foreign.Ptr
import Foreign.Storable                                 (Storable, sizeOf)
import qualified Foreign                                as F

import Data.Int
import Data.Word
import Data.Maybe
import Data.Typeable
import Data.Record.Label
import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import qualified Data.HashTable                         as Hash

import Data.Array.Accelerate.CUDA.State
import qualified Data.Array.Accelerate.Array.Data       as AD
import           Data.Array.Accelerate.Array.Data       (ArrayEltR(..))
import qualified Foreign.CUDA.Driver                    as CUDA
import qualified Foreign.CUDA.Driver.Stream             as CUDA
import qualified Foreign.CUDA.Driver.Texture            as CUDA

#include "accelerate.h"


-- Array Operations
-- ----------------

type family DevicePtrs e :: *
type family HostPtrs   e :: *

-- CPP hackery to generate the cases where we dispatch to the worker function handling
-- elementary types.
--
#define mkPrimDispatch(dispatcher,worker)                                   \
; dispatcher ArrayEltRint    = worker                                       \
; dispatcher ArrayEltRint8   = worker                                       \
; dispatcher ArrayEltRint16  = worker                                       \
; dispatcher ArrayEltRint32  = worker                                       \
; dispatcher ArrayEltRint64  = worker                                       \
; dispatcher ArrayEltRword   = worker                                       \
; dispatcher ArrayEltRword8  = worker                                       \
; dispatcher ArrayEltRword16 = worker                                       \
; dispatcher ArrayEltRword32 = worker                                       \
; dispatcher ArrayEltRword64 = worker                                       \
; dispatcher ArrayEltRfloat  = worker                                       \
; dispatcher ArrayEltRdouble = worker                                       \
; dispatcher ArrayEltRbool   = error "mkPrimDispatcher: ArrayEltRbool"      \
; dispatcher ArrayEltRchar   = error "mkPrimDispatcher: ArrayEltRchar"      \
; dispatcher _               = error "mkPrimDispatcher: not primitive"


-- |Allocate a new device array to accompany the given host-side array.
--
mallocArray :: AD.ArrayElt e => AD.ArrayData e -> Maybe Int -> Int -> CIO ()
mallocArray adata rc n = doMalloc AD.arrayElt adata
  where
    doMalloc :: ArrayEltR e -> AD.ArrayData e -> CIO ()
    doMalloc ArrayEltRunit             _  = return ()
    doMalloc (ArrayEltRpair aeR1 aeR2) ad = doMalloc aeR1 (fst' ad) *> doMalloc aeR2 (snd' ad)
    doMalloc aer                       ad = doMallocPrim aer ad rc n
      where
        { doMallocPrim :: ArrayEltR e -> AD.ArrayData e -> Maybe Int -> Int -> CIO ()
        mkPrimDispatch(doMallocPrim,mallocArrayPrim)
        }

-- |Release a device array, when its reference count drops to zero.
--
freeArray :: AD.ArrayElt e => AD.ArrayData e -> CIO ()
freeArray adata = doFree AD.arrayElt adata
  where
    doFree :: ArrayEltR e -> AD.ArrayData e -> CIO ()
    doFree ArrayEltRunit             _  = return ()
    doFree (ArrayEltRpair aeR1 aeR2) ad = doFree aeR1 (fst' ad) *> doFree aeR2 (snd' ad)
    doFree aer                       ad = doFreePrim aer ad
      where
        { doFreePrim :: ArrayEltR e -> AD.ArrayData e -> CIO ()
        mkPrimDispatch(doFreePrim,freeArrayPrim)
        }

-- |Array indexing
--
indexArray :: AD.ArrayElt e => AD.ArrayData e -> Int -> CIO e
indexArray adata i = doIndex AD.arrayElt adata
  where
    doIndex :: ArrayEltR e -> AD.ArrayData e -> CIO e
    doIndex ArrayEltRunit             _  = return ()
    doIndex (ArrayEltRpair aeR1 aeR2) ad = (,) <$> doIndex aeR1 (fst' ad)
                                               <*> doIndex aeR2 (snd' ad)
    doIndex aer                       ad = doIndexPrim aer ad i
      where
        { doIndexPrim :: ArrayEltR e -> AD.ArrayData e -> Int -> CIO e
        mkPrimDispatch(doIndexPrim,indexArrayPrim)
        }

-- |Copy data between two device arrays.
--
copyArray :: AD.ArrayElt e => AD.ArrayData e -> AD.ArrayData e -> Int -> CIO ()
copyArray adata1 adata2 i = doCopy AD.arrayElt adata1 adata2
  where
    doCopy :: ArrayEltR e -> AD.ArrayData e -> AD.ArrayData e -> CIO ()
    doCopy ArrayEltRunit             _   _   = return ()
    doCopy (ArrayEltRpair aeR1 aeR2) ad1 ad2 = doCopy aeR1 (fst' ad1) (fst' ad2) *>
                                               doCopy aeR2 (snd' ad1) (snd' ad2)
    doCopy aer                       ad1 ad2 = doCopyPrim aer ad1 ad2 i
      where
        { doCopyPrim :: ArrayEltR e -> AD.ArrayData e -> AD.ArrayData e -> Int -> CIO ()
        mkPrimDispatch(doCopyPrim,copyArrayPrim)
        }

-- |Copy data from the device into its associated host-side Accelerate array.
--
peekArray :: AD.ArrayElt e => AD.ArrayData e -> Int -> CIO ()
peekArray adata i = doPeek AD.arrayElt adata
  where
    doPeek :: ArrayEltR e -> AD.ArrayData e -> CIO ()
    doPeek ArrayEltRunit             _  = return ()
    doPeek (ArrayEltRpair aeR1 aeR2) ad = doPeek aeR1 (fst' ad) *> doPeek aeR2 (snd' ad)
    doPeek aer                       ad = doPeekPrim aer ad i
      where
        { doPeekPrim :: ArrayEltR e -> AD.ArrayData e -> Int -> CIO ()
        mkPrimDispatch(doPeekPrim,peekArrayPrim)
        }

-- |Copy data from an Accelerate array into the associated device array,
-- which must have already been allocated.
--
pokeArray :: AD.ArrayElt e => AD.ArrayData e -> Int -> CIO ()
pokeArray adata i = doPoke AD.arrayElt adata
  where
    doPoke :: ArrayEltR e -> AD.ArrayData e -> CIO ()
    doPoke ArrayEltRunit             _  = return ()
    doPoke (ArrayEltRpair aeR1 aeR2) ad = doPoke aeR1 (fst' ad) *> doPoke aeR2 (snd' ad)
    doPoke aer                       ad = doPokePrim aer ad i
      where
        { doPokePrim :: ArrayEltR e -> AD.ArrayData e -> Int -> CIO ()
        mkPrimDispatch(doPokePrim,pokeArrayPrim)
        }

-- |Asynchronous device -> host copy
--
peekArrayAsync :: AD.ArrayElt e => AD.ArrayData e -> Int -> Maybe CUDA.Stream -> CIO ()
peekArrayAsync adata i s = doPeek AD.arrayElt adata
  where
    doPeek :: ArrayEltR e -> AD.ArrayData e -> CIO ()
    doPeek ArrayEltRunit             _  = return ()
    doPeek (ArrayEltRpair aeR1 aeR2) ad = doPeek aeR1 (fst' ad) *> doPeek aeR2 (snd' ad)
    doPeek aer                       ad = doPeekPrim aer ad i s
      where
        { doPeekPrim :: ArrayEltR e -> AD.ArrayData e -> Int -> Maybe CUDA.Stream -> CIO ()
        mkPrimDispatch(doPeekPrim,peekArrayAsyncPrim)
        }

-- |Asynchronous host -> device copy
--
pokeArrayAsync :: AD.ArrayElt e => AD.ArrayData e -> Int -> Maybe CUDA.Stream -> CIO ()
pokeArrayAsync adata i s = doPoke AD.arrayElt adata
  where
    doPoke :: ArrayEltR e -> AD.ArrayData e -> CIO ()
    doPoke ArrayEltRunit             _  = return ()
    doPoke (ArrayEltRpair aeR1 aeR2) ad = doPoke aeR1 (fst' ad) *> doPoke aeR2 (snd' ad)
    doPoke aer                       ad = doPokePrim aer ad i s
      where
        { doPokePrim :: ArrayEltR e -> AD.ArrayData e -> Int -> Maybe CUDA.Stream -> CIO ()
        mkPrimDispatch(doPokePrim,pokeArrayAsyncPrim)
        }

-- |Wrap the device pointers corresponding to a host-side array into arguments that can be passed
-- to a kernel upon invocation.
--
marshalArrayData :: AD.ArrayElt e => AD.ArrayData e -> CIO [CUDA.FunParam]
marshalArrayData adata = doMarshal AD.arrayElt adata
  where
    doMarshal :: ArrayEltR e -> AD.ArrayData e -> CIO [CUDA.FunParam]
    doMarshal ArrayEltRunit             _  = return []
    doMarshal (ArrayEltRpair aeR1 aeR2) ad = (++) <$> doMarshal aeR1 (fst' ad)
                                                  <*> doMarshal aeR2 (snd' ad)
    doMarshal aer                       ad = doMarshalPrim aer ad
      where
        { doMarshalPrim :: ArrayEltR e -> AD.ArrayData e -> CIO [CUDA.FunParam]
        mkPrimDispatch(doMarshalPrim,marshalArrayDataPrim)
        }

-- |Bind the device memory arrays to the given texture reference(s), setting
-- appropriate type. The arrays are bound, and the list of textures thereby
-- consumed, in projection index order --- i.e. right-to-left
--
marshalTextureData :: AD.ArrayElt e => AD.ArrayData e -> Int -> [CUDA.Texture] -> CIO ()
marshalTextureData adata n texs = doMarshal AD.arrayElt adata texs >> return ()
  where
    doMarshal :: ArrayEltR e -> AD.ArrayData e -> [CUDA.Texture] -> CIO Int
    doMarshal ArrayEltRunit             _  _ = return 0
    doMarshal (ArrayEltRpair aeR1 aeR2) ad t
      = do
          r <- doMarshal aeR2 (snd' ad) t
          l <- doMarshal aeR1 (fst' ad) (drop r t)
          return $ l + r
    doMarshal aer                       ad t = doMarshalPrim aer ad n (head t) >> return 1
      where
        { doMarshalPrim :: ArrayEltR e -> AD.ArrayData e -> Int -> CUDA.Texture -> CIO ()
        mkPrimDispatch(doMarshalPrim,marshalTextureDataPrim)
        }

-- |Modify the basic device memory reference for a given host-side array.
--
basicModify :: AD.ArrayElt e => AD.ArrayData e -> (MemoryEntry -> MemoryEntry) -> CIO ()
basicModify adata fmod = doModify AD.arrayElt adata
  where
    doModify :: ArrayEltR e -> AD.ArrayData e -> CIO ()
    doModify ArrayEltRunit             _  = return ()
    doModify (ArrayEltRpair aeR1 aeR2) ad = doModify aeR1 (fst' ad) *> doModify aeR2 (snd' ad)
    doModify aer                       ad = doModifyPrim aer ad fmod
      where
        { doModifyPrim :: ArrayEltR e -> AD.ArrayData e -> (MemoryEntry -> MemoryEntry) -> CIO ()
        mkPrimDispatch(doModifyPrim,basicModifyPrim)
        }

-- |Does the array already exist on the device?
--
existsArrayData :: AD.ArrayElt e => AD.ArrayData e -> CIO Bool
existsArrayData adata = isJust <$> devicePtrs adata

-- |Return the device pointers associated with a given host-side array
--
devicePtrs :: AD.ArrayElt e => AD.ArrayData e -> CIO (Maybe (DevicePtrs e))
devicePtrs adata = doPtrs AD.arrayElt adata
  where
    doPtrs :: ArrayEltR e -> AD.ArrayData e -> CIO (Maybe (DevicePtrs e))
    doPtrs ArrayEltRunit             _  = return (Just ())
    doPtrs (ArrayEltRpair aeR1 aeR2) ad = liftM2 (,) <$> doPtrs aeR1 (fst' ad)
                                                     <*> doPtrs aeR2 (snd' ad)
    doPtrs aer                       ad = doPtrsPrim aer ad
      where
        { doPtrsPrim :: ArrayEltR e -> AD.ArrayData e -> CIO (Maybe (DevicePtrs e))
        mkPrimDispatch(doPtrsPrim, devicePtrsPrim)
        }


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


-- Auxiliary Functions
-- -------------------

-- |Increase the reference count of an array
--
touchArray :: AD.ArrayElt e => AD.ArrayData e -> Int -> CIO ()
touchArray ad n = basicModify ad (modL refcount (fmap (+n)))

-- |Set an array to never be released by a call to 'freeArray'. When the
-- array is unbound, its reference count is set to zero.
--
bindArray :: AD.ArrayElt e => AD.ArrayData e -> CIO ()
bindArray ad = basicModify ad (setL refcount Nothing)

-- |Unset an array to never be released by a call to 'freeArray'.
unbindArray :: AD.ArrayElt e => AD.ArrayData e -> CIO ()
unbindArray ad = basicModify ad (setL refcount (Just 0))


-- ArrayElt Implementation
-- -----------------------

-- Allocate a new device array to accompany the given host-side Accelerate array
--
mallocArrayPrim :: forall a b e.
                   ( AD.ArrayElt e, AD.ArrayPtrs e ~ Ptr a, DevicePtrs e ~ CUDA.DevicePtr b
                   , Typeable a, Typeable b, Storable b)
                => AD.ArrayData e -- host array data (reference)
                -> Maybe Int      -- initial reference count for this array; Nothing == bound array
                -> Int            -- number of elements
                -> CIO ()
mallocArrayPrim ad rc n =
  do let key = arrayToKey ad
     tab <- getM memoryTable
     mem <- liftIO $ Hash.lookup tab key
     when (isNothing mem) $ do
       _ <- liftIO $
             Hash.update tab key . MemoryEntry rc =<< (CUDA.mallocArray n :: IO (CUDA.DevicePtr b))
       return ()


-- Release a device array, when its reference counter drops to zero
--
freeArrayPrim :: ( AD.ArrayElt e, AD.ArrayPtrs e ~ Ptr a, DevicePtrs e ~ CUDA.DevicePtr b
                 , Typeable a, Typeable b)
              => AD.ArrayData e     -- host array
              -> CIO ()
freeArrayPrim ad = free . modL refcount (fmap (subtract 1)) =<< lookupArray ad
  where
    free v = case getL refcount v of
      Nothing        -> return ()
      Just x | x > 0 -> updateArray ad v
      _              -> deleteArray ad


-- Array indexing
--
indexArrayPrim :: ( AD.ArrayElt e, AD.ArrayPtrs e ~ Ptr a, DevicePtrs e ~ CUDA.DevicePtr b
                  , Storable b, Typeable a, Typeable b)
               => AD.ArrayData e     -- host array data
               -> Int                -- index in row-major representation
               -> CIO b
indexArrayPrim ad n = do
  dp <- getArray ad
  liftIO . F.alloca $ \p -> do
    CUDA.peekArray 1 (dp `CUDA.advanceDevPtr` n) p
    F.peek p


-- Copy data between two device arrays.
--
copyArrayPrim :: ( AD.ArrayElt e, AD.ArrayPtrs e ~ Ptr a, DevicePtrs e ~ CUDA.DevicePtr b
                 , Storable b, Typeable a, Typeable b)
              => AD.ArrayData e     -- source array
              -> AD.ArrayData e     -- destination
              -> Int                -- number of elements
              -> CIO ()
copyArrayPrim src' dst' n = do
  src <- getArray src'
  dst <- getArray dst'
  liftIO $ CUDA.copyArrayAsync n src dst


-- Copy data from the device into the associated Accelerate array
--
peekArrayPrim :: ( AD.ArrayElt e, AD.ArrayPtrs e ~ Ptr a, DevicePtrs e ~ CUDA.DevicePtr a
                 , Storable a, Typeable a)
              => AD.ArrayData e     -- host array data
              -> Int                -- number of elements
              -> CIO ()
peekArrayPrim ad n =
  let dst = AD.ptrsOfArrayData ad
      src = arena ad
  in
  lookupArray ad >>= \me -> liftIO $ CUDA.peekArray n (src me) dst

peekArrayAsyncPrim :: ( AD.ArrayElt e, AD.ArrayPtrs e ~ Ptr a, DevicePtrs e ~ CUDA.DevicePtr a
                      , Storable a, Typeable a)
                   => AD.ArrayData e     -- host array data
                   -> Int                -- number of elements
                   -> Maybe CUDA.Stream  -- asynchronous stream (optional)
                   -> CIO ()
peekArrayAsyncPrim ad n st =
  let dst = CUDA.HostPtr . AD.ptrsOfArrayData
      src = arena ad
  in
  lookupArray ad >>= \me -> liftIO $ CUDA.peekArrayAsync n (src me) (dst ad) st


-- Copy data from an Accelerate array to the associated device array. The data
-- will be copied from the host-side array each time this function is called; no
-- changes to the reference counter will be made.
--
pokeArrayPrim :: ( AD.ArrayElt e, AD.ArrayPtrs e ~ Ptr a, DevicePtrs e ~ CUDA.DevicePtr a
                 , Storable a, Typeable a)
              => AD.ArrayData e     -- host array data
              -> Int                -- number of elements
              -> CIO ()
pokeArrayPrim ad n = upload =<< lookupArray ad
  where
    src      = AD.ptrsOfArrayData
    dst      = arena ad
    upload v = liftIO $ CUDA.pokeArray n (src ad) (dst v)

pokeArrayAsyncPrim :: ( AD.ArrayElt e, AD.ArrayPtrs e ~ Ptr a, DevicePtrs e ~ CUDA.DevicePtr a
                      , Storable a, Typeable a)
                   => AD.ArrayData e     -- host array reference
                   -> Int                -- number of elements
                   -> Maybe CUDA.Stream  -- asynchronous stream to associate (optional)
                   -> CIO ()
pokeArrayAsyncPrim ad n st = upload =<< lookupArray ad
  where
    src      = CUDA.HostPtr . AD.ptrsOfArrayData
    dst      = arena ad
    upload v = liftIO $ CUDA.pokeArrayAsync n (src ad) (dst v) st


-- Wrap the device pointers corresponding to a host-side array into arguments
-- that can be passed to a kernel on invocation.
--
marshalArrayDataPrim :: ( AD.ArrayElt e, AD.ArrayPtrs e ~ Ptr a, DevicePtrs e ~ CUDA.DevicePtr b
                        , Typeable a, Typeable b)
                     => AD.ArrayData e
                     -> CIO [CUDA.FunParam]
marshalArrayDataPrim ad = return . CUDA.VArg <$> getArray ad


-- Bind device memory to the given texture reference, setting appropriate type
--
marshalTextureDataPrim :: forall a e.
                          ( AD.ArrayElt e, AD.ArrayPtrs e ~ Ptr a, DevicePtrs e ~ CUDA.DevicePtr a
                          , Storable a, TextureData a, Typeable a)
                       => AD.ArrayData e     -- host array data
                       -> Int                -- number of elements
                       -> CUDA.Texture       -- texture reference to bind to
                       -> CIO ()
marshalTextureDataPrim ad n tex = do
  let (fmt,c) = format (undefined :: a)
  ptr <- getArray ad
  liftIO $ do
    CUDA.setFormat tex fmt c
    CUDA.bind tex ptr (fromIntegral $ n * sizeOf (undefined :: a))


-- Modify the internal memory reference for a host-side array.
--
basicModifyPrim :: (AD.ArrayElt e, AD.ArrayPtrs e ~ Ptr a, Typeable a)
                => AD.ArrayData e
                -> (MemoryEntry -> MemoryEntry)
                -> CIO ()
basicModifyPrim ad f = updateArray ad . f =<< lookupArray ad


-- Return the device pointers
--
devicePtrsPrim :: ( AD.ArrayPtrs e ~ Ptr a, AD.ArrayElt e, Typeable a
                  , DevicePtrs e ~ CUDA.DevicePtr b, Typeable b)
               => AD.ArrayData e
               -> CIO (Maybe (DevicePtrs e))
devicePtrsPrim ad = do
  t <- getM memoryTable
  x <- liftIO $ Hash.lookup t (arrayToKey ad)
  return (arena ad `fmap` x)


-- Utility functions
-- -----------------

-- Get a device pointer out of our existential wrapper
--
arena :: (DevicePtrs e ~ CUDA.DevicePtr b, Typeable b)
      => AD.ArrayData e
      -> MemoryEntry
      -> CUDA.DevicePtr b
arena _ (MemoryEntry _ p)
  | Just ptr <- gcast p = ptr
  | otherwise           = INTERNAL_ERROR(error) "arena" "type mismatch"

-- Generate a memory map key from the given ArrayData
--
arrayToKey :: (AD.ArrayElt e, AD.ArrayPtrs e ~ Ptr a, Typeable a)
           => AD.ArrayData e
           -> AccArrayData
arrayToKey = AccArrayData

-- Retrieve the device memory entry from the state structure associated with a
-- particular Accelerate array.
--
lookupArray :: (AD.ArrayElt e, AD.ArrayPtrs e ~ Ptr a, Typeable a)
            => AD.ArrayData e
            -> CIO MemoryEntry
lookupArray ad = do
  t <- getM memoryTable
  x <- liftIO $ Hash.lookup t (arrayToKey ad)
  case x of
       Just e -> return e
       _      -> INTERNAL_ERROR(error) "lookupArray" "lost device memory reference"
                 -- TLM: better if the file/line markings are of the use site

-- Update (or insert) a memory entry into the state structure
--
updateArray :: (AD.ArrayPtrs e ~ Ptr a, Typeable a, AD.ArrayElt e)
            => AD.ArrayData e
            -> MemoryEntry
            -> CIO ()
updateArray ad me = do
  t <- getM memoryTable
  liftIO $ Hash.update t (arrayToKey ad) me >> return ()

-- Delete an entry from the state structure and release the corresponding device
-- memory area
--
deleteArray :: ( AD.ArrayElt e, AD.ArrayPtrs e ~ Ptr a, DevicePtrs e ~ CUDA.DevicePtr b
               , Typeable a, Typeable b)
            => AD.ArrayData e
            -> CIO ()
deleteArray ad = do
  let key = arrayToKey ad
  tab <- getM memoryTable
  val <- liftIO $ Hash.lookup tab key
  case val of
       Just m -> liftIO $ CUDA.free (arena ad m) >> Hash.delete tab key
       _      -> INTERNAL_ERROR(error) "deleteArray" "lost device memory reference: double free?"

-- Return the device pointer associated with a host-side Accelerate array
--
getArray :: ( AD.ArrayElt e, AD.ArrayPtrs e ~ Ptr a, DevicePtrs e ~ CUDA.DevicePtr b
            , Typeable a, Typeable b)
         => AD.ArrayData e
         -> CIO (CUDA.DevicePtr b)
getArray ad = arena ad <$> lookupArray ad

-- Array tuple extraction
--
fst' :: AD.ArrayData (a,b) -> AD.ArrayData a
fst' = AD.fstArrayData

snd' :: AD.ArrayData (a,b) -> AD.ArrayData b
snd' = AD.sndArrayData

