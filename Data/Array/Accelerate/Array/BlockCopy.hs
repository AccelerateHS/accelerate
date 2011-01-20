{-# LANGUAGE GADTs, ForeignFunctionInterface, ScopedTypeVariables #-}
-- | This module provides functions for efficient block copies of primitive arrays
-- (i.e. one dimensional, in row-major order in contiguous memory) to Accelerate Arrays.
--
-- You should only use this module if you really know what you are doing.
-- Potential pitfalls include:
--
--   * copying from memory your program doesn't have access to (e.g. it may be unallocated or not enough memory is
--     allocated)
--
--   * memory alignment errors
--
module Data.Array.Accelerate.Array.BlockCopy (
  -- * Types
  BlockCopyFun, BlockCopyFuns, BlockPtrs,
  -- * Functions
  blockCopyToArray, blockCopyFromArray, blockCopyToArrayWithFunctions, blockCopyFromArrayWithFunctions,
  byteStringsToArray, arrayToByteStrings
) where

-- standard libraries
import Foreign
import Foreign.C
import GHC.Base
import Data.Array.Base (bOOL_SCALE, wORD_SCALE, fLOAT_SCALE, dOUBLE_SCALE)
import Data.ByteString

-- friends
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Sugar

-- | Functions of this type are passed as arguments to 'blockCopyToArrayWithFunctions'.
--   A function of this type should copy a number of bytes (equal to the value of the parameter
--   of type 'Int') to the destination memory pointed to by @Ptr e@.
type BlockCopyFun e = Ptr e -> Int -> IO ()

-- | Represents a collection of "block copy functions" (see 'BlockCopyFun'). The
--   structure of the collection of 'BlockCopyFun's depends on the element type @e@.
--
-- e.g.
--
-- If @e :: Float@
-- then @BlockCopyFuns (EltRepr e) :: ((), Ptr Float -> Int -> IO ())@
--
-- If @e :: (Double, Float)@
-- then @BlockCopyFuns (EltRepr e) :: (((), Ptr Double -> Int -> IO ()), Ptr Float -> Int -> IO ())@
type family BlockCopyFuns e

type instance BlockCopyFuns ()     = ()
type instance BlockCopyFuns Int    = BlockCopyFun Int
type instance BlockCopyFuns Int8   = BlockCopyFun Int8
type instance BlockCopyFuns Int16  = BlockCopyFun Int16
type instance BlockCopyFuns Int32  = BlockCopyFun Int32
type instance BlockCopyFuns Int64  = BlockCopyFun Int64
type instance BlockCopyFuns Word   = BlockCopyFun Word
type instance BlockCopyFuns Word8  = BlockCopyFun Word8
type instance BlockCopyFuns Word16 = BlockCopyFun Word16
type instance BlockCopyFuns Word32 = BlockCopyFun Word32
type instance BlockCopyFuns Word64 = BlockCopyFun Word64
type instance BlockCopyFuns Float  = BlockCopyFun Float
type instance BlockCopyFuns Double = BlockCopyFun Double
type instance BlockCopyFuns Bool   = BlockCopyFun Word8 -- Packed a bit vector
type instance BlockCopyFuns Char   = BlockCopyFun Char
type instance BlockCopyFuns (a,b)  = (BlockCopyFuns a, BlockCopyFuns b)

-- | A family of types that represents a collection of pointers that are the source/destination
--   addresses for a block copy. The structure of the collection of pointers depends on
--   the element type @e@.
--
--  e.g.
--
--  If @e :: Int@,            then @BlockPtrs (EltRepr e) :: ((), Ptr Int)@
--
--  If @e :: (Double, Float)@ then @BlockPtrs (EltRepr e) :: (((), Ptr Double), Ptr Float)@
type family BlockPtrs e

type instance BlockPtrs ()     = ()
type instance BlockPtrs Int    = Ptr Int
type instance BlockPtrs Int8   = Ptr Int8
type instance BlockPtrs Int16  = Ptr Int16
type instance BlockPtrs Int32  = Ptr Int32
type instance BlockPtrs Int64  = Ptr Int64
type instance BlockPtrs Word   = Ptr Word
type instance BlockPtrs Word8  = Ptr Word8
type instance BlockPtrs Word16 = Ptr Word16
type instance BlockPtrs Word32 = Ptr Word32
type instance BlockPtrs Word64 = Ptr Word64
type instance BlockPtrs Float  = Ptr Float
type instance BlockPtrs Double = Ptr Double
type instance BlockPtrs Bool   = Ptr Word8 -- Packed as a bit vector
type instance BlockPtrs Char   = Ptr Char
type instance BlockPtrs (a,b)  = (BlockPtrs a, BlockPtrs b)

-- | A family of types that represents a collection of 'ByteString's. They are the source
--   data for function 'byteStringsToArray' and the result data for 'arrayToByteStrings'
type family ByteStrings e

type instance ByteStrings ()     = ()
type instance ByteStrings Int    = ByteString
type instance ByteStrings Int8   = ByteString
type instance ByteStrings Int16  = ByteString
type instance ByteStrings Int32  = ByteString
type instance ByteStrings Int64  = ByteString
type instance ByteStrings Word   = ByteString
type instance ByteStrings Word8  = ByteString
type instance ByteStrings Word16 = ByteString
type instance ByteStrings Word32 = ByteString
type instance ByteStrings Word64 = ByteString
type instance ByteStrings Float  = ByteString
type instance ByteStrings Double = ByteString
type instance ByteStrings Bool   = ByteString
type instance ByteStrings Char   = ByteString
type instance ByteStrings (a,b)  = (ByteStrings a, ByteStrings b)

-- | Block copy regions of memory into a freshly allocated Accelerate array. The type of elements
--   (@e@) in the output Accelerate array determines the structure of the collection of pointers
--   that will be required as the second argument to this function. See 'BlockPtrs'
--
--   Each one of these pointers points to a block of memory that is the source of data
--   for the Accelerate array (unlike function 'blockCopyToArrayWithFunctions' where one passes
--   in function which copies data to a destination address.).
--
blockCopyToArray :: (Shape sh, Elt e) => sh -> BlockPtrs (EltRepr e) -> IO (Array sh e)
blockCopyToArray sh blkPtrs = do
  let arr = uninitNewArray sh
      copier = let ((f,_),_,_) = blockCopyFunGenerator arr in f
  copier blkPtrs
  return arr

-- | Block copies bytes from a collection of 'ByteString's to freshly allocated Accelerate array.
--
--   The type of elements (@e@) in the output Accelerate array determines the structure of the
--   collection of 'ByteString's that will be required as the second argument to this function.
--   See 'ByteStrings'
--
byteStringsToArray :: (Shape sh, Elt e) => sh -> ByteStrings (EltRepr e) -> IO (Array sh e)
byteStringsToArray sh byteStrings = do
  let arr = uninitNewArray sh
      copier = let ((_,f),_,_) = blockCopyFunGenerator arr in f
  copier byteStrings
  return arr

-- | Block copy from Accelerate array to pre-allocated regions of memory. The type of element of
--   the input Accelerate array (@e@) determines the structure of the collection of pointers
--   that will be required as the second argument to this function. See 'BlockPtrs'
--
--   The memory associated with the pointers must have already been allocated.
--
blockCopyFromArray :: (Shape sh, Elt e) => Array sh e -> BlockPtrs (EltRepr e) -> IO ()
blockCopyFromArray arr blockPtrs = do
  let copier = let (_,(f,_),_) = blockCopyFunGenerator arr in f
  copier blockPtrs
  return ()

-- | Block copy from an Accelerate array to a collection of freshly allocated 'ByteString's.
--
--   The type of elements (@e@) in the input Accelerate array determines the structure of the
--   collection of 'ByteString's that will be output.
--   See 'ByteStrings'
--
arrayToByteStrings :: (Shape sh, Elt e) => Array sh e -> IO (ByteStrings (EltRepr e))
arrayToByteStrings arr = do
  let copier = let (_,(_,f),_) = blockCopyFunGenerator arr in f
  copier

-- | Copy values to a freshly allocated Accelerate array using a collection of
--   functions that have type 'BlockCopyFun'. The argument of type @Ptr e@ in each of
--   these functions refers to the address of the /destination/ block of memory in the
--   Accelerate Array. The /source/ address is implicit. e.g. the 'BlockCopyFun' could
--   be the result of a partial application to a @Ptr e@ pointing to the source block.
--
--   The structure of this collection of functions depends on the elemente type @e@. Each
--   function (of type 'BlockCopyFun') copies data to a destination address (pointed to by
--   the argument of type @Ptr ()@).
--
--   Unless there is a particularly pressing reason to use this function, the 'blockCopyToArray'
--   function is sufficient as it uses an efficient low-level call to libc's
--   @memcpy@ to perform the copy.
--
blockCopyToArrayWithFunctions :: (Shape sh, Elt e) => sh -> BlockCopyFuns (EltRepr e) -> IO (Array sh e)
blockCopyToArrayWithFunctions sh blockCopyFuns = do
  let arr = uninitNewArray sh
      copier = let (_,_,f) = blockCopyFunGenerator arr in f
  copier blockCopyFuns
  return arr

-- | Copy values from an Accelerate array using a collection of functions that have type
--   'BlockCopyFun'. The argument of type @Ptr e@ in each of these functions refers to the
--   address of the /source/ block of memory in the Accelerate Array. The /destination/
--   address is implicit. e.g. the 'BlockCopyFun' could be the result of partially
--   application to a @Ptr e@ pointing to the destination block.
--
--   The structure of this collection of functions depends on the elemente type @e@. Each
--   function (of type 'BlockCopyFun') copies data to a destination address (pointed to
--   by the argument of type @Ptr ()@).
--
--   Unless there is a particularly pressing reason to use this function, the 'blockCopyToArray'
--   function is sufficient as it uses an efficient low-level call to libc's @memcpy@ to
--   perform the copy.
--
blockCopyFromArrayWithFunctions :: (Shape sh, Elt e) => Array sh e -> BlockCopyFuns (EltRepr e) -> IO ()
blockCopyFromArrayWithFunctions arr blockCopyFuns = do
   let copier = let (_,_,f) = blockCopyFunGenerator arr in f
   copier blockCopyFuns
   return ()

type GenFuns e = (( BlockPtrs e -> IO ()
                  , ByteStrings e -> IO ())
                 ,( BlockPtrs e -> IO ()
                  , IO (ByteStrings e))
                 , BlockCopyFuns e -> IO ())

base :: forall a b. Ptr b -> Int -> (( Ptr a -> IO (), ByteString -> IO ())
                                    ,( Ptr a -> IO (), IO ByteString)
                                    ,(Ptr b -> Int -> IO ()) -> IO ())
base accArrayPtr byteSize =
   ((blockPtrToArray, byteStringToArray)
   ,(arrayToBlockPtr, arrayToByteString)
   , blockCopyFunToOrFromArray)
  where
    blockPtrToArray :: Ptr a -> IO ()
    blockPtrToArray blockPtr = blockCopy blockPtr accArrayPtr byteSize
    arrayToBlockPtr :: Ptr a -> IO ()
    arrayToBlockPtr blockPtr = blockCopy accArrayPtr blockPtr byteSize
    blockCopyFunToOrFromArray :: (Ptr b -> Int -> IO ()) -> IO ()
    blockCopyFunToOrFromArray blockCopyFun = blockCopyFun accArrayPtr byteSize
    byteStringToArray :: ByteString -> IO ()
    byteStringToArray bs = useAsCString bs (\cStr -> blockPtrToArray (castPtr cStr))
    arrayToByteString :: IO ByteString
    arrayToByteString = packCStringLen (castPtr accArrayPtr, byteSize)

blockCopyFunGenerator :: Array sh e -> GenFuns (EltRepr e)
blockCopyFunGenerator array@(Array _ arrayData) = aux arrayElt arrayData
  where
   sizeA = size (shape array)
   aux :: ArrayEltR e -> ArrayData e -> GenFuns e
   aux ArrayEltRunit _ = let f = \() -> return () in ((f,f),(f,return ()),f)
   aux ArrayEltRint    ad = base (ptrsOfArrayData ad) (box wORD_SCALE sizeA)
   aux ArrayEltRint8   ad = base (ptrsOfArrayData ad) sizeA
   aux ArrayEltRint16  ad = base (ptrsOfArrayData ad) (sizeA * 2)
   aux ArrayEltRint32  ad = base (ptrsOfArrayData ad) (sizeA * 4)
   aux ArrayEltRint64  ad = base (ptrsOfArrayData ad) (sizeA * 8)
   aux ArrayEltRword   ad = base (ptrsOfArrayData ad) (box wORD_SCALE sizeA)
   aux ArrayEltRword8  ad = base (ptrsOfArrayData ad) sizeA
   aux ArrayEltRword16 ad = base (ptrsOfArrayData ad) (sizeA * 2)
   aux ArrayEltRword32 ad = base (ptrsOfArrayData ad) (sizeA * 4)
   aux ArrayEltRword64 ad = base (ptrsOfArrayData ad) (sizeA * 8)
   aux ArrayEltRfloat  ad = base (ptrsOfArrayData ad) (box fLOAT_SCALE sizeA)
   aux ArrayEltRdouble ad = base (ptrsOfArrayData ad) (box dOUBLE_SCALE sizeA)
   aux ArrayEltRbool   ad = base (ptrsOfArrayData ad) (box bOOL_SCALE sizeA)
   aux ArrayEltRchar   _ = error "not defined yet" -- base (castPtr $ ptrsOfArrayData ad) (sizeA * 4)
   aux (ArrayEltRpair a b) (AD_Pair ad1 ad2) = ((bpFromC, bsFromC), (bpToC, bsToC), toH)
     where
       ((bpFromC1, bsFromC1), (bpToC1, bsToC1), toH1) = aux a ad1
       ((bpFromC2, bsFromC2), (bpToC2, bsToC2), toH2) = aux b ad2
       toH (funs1, funs2)   = toH1 funs1    >> toH2 funs2
       bpToC (ptrA, ptrB)   = bpToC1 ptrA   >> bpToC2 ptrB
       bsToC                = do { bsA <- bsToC1; bsB <- bsToC2; return (bsA, bsB) }
       bpFromC (ptrA, ptrB) = bpFromC1 ptrA >> bpFromC2 ptrB
       bsFromC (bsA, bsB)   = bsFromC1 bsA  >> bsFromC2 bsB

blockCopy :: Ptr a -> Ptr b -> Int -> IO ()
blockCopy src dst byteSize = memcpy dst src (fromIntegral byteSize)

-- | Creates a new, uninitialized Accelerate array.
uninitNewArray :: (Shape sh, Elt e) => sh -> Array sh e
{-# INLINE uninitNewArray #-}
uninitNewArray sh = adata `seq` Array (fromElt sh) adata
  where
    (adata, _) = runArrayData $ do
                   arr <- newArrayData (1024 `max` size sh)
                   return (arr, undefined)

-- Foreign imports
foreign import ccall memcpy :: Ptr a -> Ptr b -> CInt -> IO ()

-- Helpers
box :: (Int# -> Int#) -> (Int -> Int)
box f (I# x) = I# (f x)
