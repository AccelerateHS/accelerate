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

  -- Array representation
  DevicePtrs, HostPtrs,

  -- Memory manager
  newMT, MemoryTable,

  -- Array operations
  mallocArray, useArray, indexArray, copyArray, peekArray, peekArrayAsync,
  pokeArray, pokeArrayAsync, marshalArrayData, marshalTextureData

) where

-- libraries
import Prelude                                          hiding (lookup, catch)
import Data.Int
import Data.Word
import Data.Maybe
import Data.Typeable
import Control.Monad
import Control.Exception
import System.Mem
import System.Mem.Weak
import System.Mem.StableName
import Foreign.Ptr
import Foreign.Marshal                                  (alloca)
import Foreign.Storable
import Foreign.CUDA.Driver.Error
import qualified Data.HashTable                         as Hash
import qualified Foreign.CUDA.Driver                    as CUDA
import qualified Foreign.CUDA.Driver.Stream             as CUDA
import qualified Foreign.CUDA.Driver.Texture            as CUDA

-- friends
import Data.Array.Accelerate.Array.Data

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


-- Memory manager
-- --------------

data ArrayPtr where
  ArrayPtr :: (ArrayPtrs e ~ Ptr a, Typeable a, Typeable e)
           => StableName (ArrayData e)
           -> ArrayPtr

data DevicePtr where
  DevicePtr :: Typeable b
            => CUDA.DevicePtr b
            -> DevicePtr

instance Eq ArrayPtr where
  ArrayPtr p1 == ArrayPtr p2
    = maybe False (== p2) (gcast p1)

type MMap        = Hash.HashTable ArrayPtr (Weak DevicePtr)
data MemoryTable = MemoryTable MMap (Weak MMap)


-- Create a new hash table from host to device arrays. When this structure is
-- collected it will finalise all entries in the table. This maps a single
-- memory --- as arrays of tuples are represented as tuples of arrays, such
-- arrays will generate multiple entries in the table.
--
-- The structure contains a weak pointer to itself so that individual entries
-- can remove themselves once their host-side array dies.
--
newMT :: IO MemoryTable
newMT =
  let hashPtr (ArrayPtr p) = fromIntegral . hashStableName $ p
  in do
    t <- Hash.new (==) hashPtr
    w <- mkWeak t t (Just $ mapM_ (finalize . snd) =<< Hash.toList t)
    return $ MemoryTable t w


-- Retrieve the device memory pointer associated with a given Accelerate array.
-- This will throw an exception if the array can not be found.
--
-- Note that there is an awkward race condition going on here. After we use the
-- array data to key the hash table, there might, conceivably, be no further
-- references to it. If that is so, and a garbage collection intervenes, the
-- weak pointer might be tombstoned before deRefWeak gets to it. In this unusual
-- case we call notFound again. Critically, because the array data is used as
-- part of the error message, this means it is available in the continuation and
-- deRefWeak always succeeds.
--
lookup :: (ArrayElt e, ArrayPtrs e ~ Ptr a, DevicePtrs e ~ CUDA.DevicePtr b
          ,Typeable a, Typeable b, Typeable e)
       => MemoryTable
       -> ArrayData e
       -> IO (DevicePtrs e)
lookup (MemoryTable tbl _) ad =
  let ptrs     = ptrsOfArrayData ad
      notFound = INTERNAL_ERROR(error) "lookup" ("lost device memory " ++ show ptrs)
  in do
    mw <- Hash.lookup tbl =<< arrayPtr ad
    case mw of
      Nothing -> return notFound
      Just w  -> maybe  notFound (devicePtr ad) `fmap` deRefWeak w


-- Check whether a device array exists
--
exists :: (ArrayElt e, ArrayPtrs e ~ Ptr a, Typeable a, Typeable e)
       => MemoryTable
       -> ArrayData e
       -> IO Bool
exists (MemoryTable tbl _) ad =
  liftM isJust $ Hash.lookup tbl =<< arrayPtr ad


-- Record an association between the given host and device side arrays. The
-- device array will be released at some point after the host array becomes
-- unreachable.
--
-- If the finaliser is called because the hash table itself is being collected,
-- the weak pointer to the table will be tombstoned and there is nothing further
-- to do, otherwise we also remove the entry from the hash table.
--
insert :: (ArrayElt e, ArrayPtrs e ~ Ptr a, DevicePtrs e ~ CUDA.DevicePtr b
          ,Typeable a, Typeable b, Typeable e)
       => MemoryTable
       -> ArrayData e
       -> DevicePtrs e
       -> IO ()
insert (MemoryTable tbl weakTbl) ad ptr =
  let val = DevicePtr ptr
  in do
    key  <- arrayPtr ad
    weak <- mkWeak ad val (Just $ do CUDA.free ptr
                                     maybe (return ()) (`Hash.delete` key) =<< deRefWeak weakTbl)
    Hash.insert tbl key weak


-- Create the stable key used to associate host and device arrays. Add
-- strictness to the key, since its stable name may change after it is
-- evaluated to normal form.
--
arrayPtr :: (ArrayPtrs e ~ Ptr a, ArrayElt e, Typeable e, Typeable a)
         => ArrayData e
         -> IO ArrayPtr
arrayPtr !ad = ArrayPtr `fmap` makeStableName ad


-- Get a typed device pointer out of our untyped wrapper
--
devicePtr :: (DevicePtrs e ~ CUDA.DevicePtr b, Typeable b)
          => ArrayData e
          -> DevicePtr
          -> DevicePtrs e
devicePtr _ (DevicePtr p)
  | Just ptr <- gcast p = ptr
  | otherwise           = INTERNAL_ERROR(error) "devicePtr" "type mismatch"


-- Primitive array operations
-- --------------------------

-- Allocate a device-side array associated with the given host array. If the
-- allocation fails due to a lack of memory, run the garbage collector to
-- release any inaccessible arrays and try again.
--
-- Make sure that we do not allocate multiple entries if the object is actually
-- shared on the heap.
--
mallocArray :: (ArrayElt e, ArrayPtrs e ~ Ptr a, DevicePtrs e ~ CUDA.DevicePtr b
               ,Typeable a, Typeable b, Typeable e, Storable b)
            => MemoryTable
            -> ArrayData e              -- host array data
            -> Int                      -- number of array elements
            -> IO ()
mallocArray mmap@(MemoryTable tbl _) ad n =
  let reclaim = Hash.toList tbl >>= \l     -> forM l $
                                    \(_,w) -> whenM (isNothing `fmap` deRefWeak w) $ finalize w
  in
  unlessM (exists mmap ad) $ do
    dptr <- CUDA.mallocArray n `catch` \(e :: CUDAException) ->
      case e of
        ExitCode OutOfMemory -> performGC >> reclaim >> CUDA.mallocArray n
        _                    -> throwIO e
    insert mmap ad dptr


-- A combination of 'mallocArray' and 'pokeArray' to allocate space on the
-- device and upload an existing array. This is specialised because if the host
-- array is shared on the heap, we do not need to do anything.
--
useArray :: (ArrayElt e, ArrayPtrs e ~ Ptr a, DevicePtrs e ~ CUDA.DevicePtr a
            ,Typeable a, Typeable e, Storable a)
         => MemoryTable
         -> ArrayData e                 -- host array data
         -> Int                         -- number of array elements
         -> IO ()
useArray mmap ad n =
  unlessM (exists mmap ad) $ do
    mallocArray    mmap ad n
    pokeArrayAsync mmap ad n Nothing


-- Read a single element from an array at the given row-major index. This is a
-- synchronous operation.
--
indexArray :: (ArrayElt e, ArrayPtrs e ~ Ptr a, DevicePtrs e ~ CUDA.DevicePtr b
              ,Typeable a, Typeable b, Typeable e, Storable b)
           => MemoryTable
           -> ArrayData e               -- host array
           -> Int                       -- index in row-major representation
           -> IO b
indexArray mmap ad i =
  lookup mmap ad >>= \d ->
  alloca           $ \p -> do
    CUDA.peekArray 1 (d `CUDA.advanceDevPtr` i) p
    peek p


-- Copy data between two device arrays. The operation is asynchronous with
-- respect to the host, but will never overlap kernel execution.
--
copyArray :: (ArrayElt e, ArrayPtrs e ~ Ptr a, DevicePtrs e ~ CUDA.DevicePtr b
             ,Typeable a, Typeable b, Typeable e, Storable b)
          => MemoryTable
          -> ArrayData e                -- source array
          -> ArrayData e                -- destination array
          -> Int                        -- number of array elements
          -> IO ()
copyArray mmap from to n = do
  src <- lookup mmap from
  dst <- lookup mmap to
  CUDA.copyArrayAsync n src dst


-- Copy data from the device into the associated Accelerate host-side array
--
peekArray :: (ArrayElt e, ArrayPtrs e ~ Ptr a, DevicePtrs e ~ CUDA.DevicePtr a
             ,Typeable a, Typeable e, Storable a)
          => MemoryTable
          -> ArrayData e                -- host array
          -> Int                        -- number of elements
          -> IO ()
peekArray mmap ad n =
  let dst = ptrsOfArrayData ad
  in  lookup mmap ad >>= \src -> CUDA.peekArray n src dst

peekArrayAsync :: (ArrayElt e, ArrayPtrs e ~ Ptr a, DevicePtrs e ~ CUDA.DevicePtr a
                 ,Typeable a, Typeable e, Storable a)
              => MemoryTable
              -> ArrayData e            -- host array
              -> Int                    -- number of elements
              -> Maybe CUDA.Stream      -- asynchronous stream (optional)
              -> IO ()
peekArrayAsync mmap ad n st =
  let dst = CUDA.HostPtr $ ptrsOfArrayData ad
  in  lookup mmap ad >>= \src -> CUDA.peekArrayAsync n src dst st


-- Copy data from an Accelerate array into the associated device array
--
pokeArray :: (ArrayElt e, ArrayPtrs e ~ Ptr a, DevicePtrs e ~ CUDA.DevicePtr a
             ,Typeable a, Typeable e, Storable a)
          => MemoryTable
          -> ArrayData e                -- host array
          -> Int                        -- number of elements
          -> IO ()
pokeArray mmap ad n =
  let src = ptrsOfArrayData ad
  in  lookup mmap ad >>= \dst -> CUDA.pokeArray n src dst

pokeArrayAsync :: (ArrayElt e, ArrayPtrs e ~ Ptr a, DevicePtrs e ~ CUDA.DevicePtr a
                 ,Typeable a, Typeable e, Storable a)
              => MemoryTable
              -> ArrayData e            -- host array
              -> Int                    -- number of elements
              -> Maybe CUDA.Stream      -- asynchronous stream (optional)
              -> IO ()
pokeArrayAsync mmap ad n st =
  let src = CUDA.HostPtr $ ptrsOfArrayData ad
  in  lookup mmap ad >>= \dst -> CUDA.pokeArrayAsync n src dst st


-- Wrap a device pointer corresponding corresponding to a host-side array into
-- arguments that can be passed to a kernel upon invocation
--
marshalArrayData :: (ArrayElt e, ArrayPtrs e ~ Ptr a, DevicePtrs e ~ CUDA.DevicePtr b
                    ,Typeable a, Typeable b, Typeable e)
                 => MemoryTable
                 -> ArrayData e         -- host array
                 -> IO CUDA.FunParam
marshalArrayData mmap ad = CUDA.VArg `fmap` lookup mmap ad


-- Bind device memory to the given texture reference, setting appropriate type
--
marshalTextureData :: forall a e.
                      (ArrayElt e, ArrayPtrs e ~ Ptr a, DevicePtrs e ~ CUDA.DevicePtr a
                      ,Typeable a, Typeable e, Storable a, TextureData a)
                   => MemoryTable
                   -> ArrayData e       -- host array
                   -> Int               -- number of elements
                   -> CUDA.Texture      -- texture reference to bind array to
                   -> IO ()
marshalTextureData mmap ad n tex =
  let (fmt, c) = format (undefined :: a)
  in  lookup mmap ad >>= \ptr -> do
        CUDA.setFormat tex fmt c
        CUDA.bind tex ptr (fromIntegral $ n * sizeOf (undefined :: a))


-- Auxiliary
-- ---------

whenM :: Monad m => m Bool -> m () -> m ()
whenM predicate action = do
  p <- predicate
  when p action

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM predicate action = do
  p <- predicate
  unless p action

