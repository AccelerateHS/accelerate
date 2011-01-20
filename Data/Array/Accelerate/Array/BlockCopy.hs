{-# LANGUAGE GADTs, ForeignFunctionInterface #-}
module Data.Array.Accelerate.Array.BlockCopy (
  -- * Types
  BlockCopyFun, BlockCopyFuns,
  -- * Functions
  blockCopyToArray, blockCopyToArrayWithFunctions
) where

-- standard libraries
import Foreign
import Foreign.C
import GHC.Base
import Data.Array.Base (bOOL_SCALE, wORD_SCALE, fLOAT_SCALE, dOUBLE_SCALE)

-- friends
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Sugar

-- | Functions of this type are passed as arguments to 'blockCopyToArrayWithFunctions'.
--   A function of this type should copy a number of bytes (equal to the value of the parameter
--   of type 'Int') to the destination memory pointed to by @Ptr ()@.
type BlockCopyFun = Ptr () -> Int -> IO ()
-----
type family BlockCopyFuns e

type instance BlockCopyFuns ()     = ()
type instance BlockCopyFuns Int    = BlockCopyFun
type instance BlockCopyFuns Int8   = BlockCopyFun
type instance BlockCopyFuns Int16  = BlockCopyFun
type instance BlockCopyFuns Int32  = BlockCopyFun
type instance BlockCopyFuns Int64  = BlockCopyFun
type instance BlockCopyFuns Word   = BlockCopyFun
type instance BlockCopyFuns Word8  = BlockCopyFun
type instance BlockCopyFuns Word16 = BlockCopyFun
type instance BlockCopyFuns Word32 = BlockCopyFun
type instance BlockCopyFuns Word64 = BlockCopyFun
type instance BlockCopyFuns Float  = BlockCopyFun
type instance BlockCopyFuns Double = BlockCopyFun
type instance BlockCopyFuns Bool   = BlockCopyFun
type instance BlockCopyFuns Char   = BlockCopyFun
type instance BlockCopyFuns (a,b)  = (BlockCopyFuns a, BlockCopyFuns b)
-----

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
type instance BlockPtrs Bool   = Ptr Bool
type instance BlockPtrs Char   = Ptr Char
type instance BlockPtrs (a,b)  = (BlockPtrs a, BlockPtrs b)
-----

-- | This function is used to block copy regions of memory into freshly allocated
--   Accelerate arrays. The type of elements (@e@) in the output Accelerate array determines
--   the structure of the collection of pointers that will be required as the second argument
--   to this function.
--
--   e.g. If @e :: Int@,            then @BlockPtrs (EltRepr e) :: ((), Ptr Int)@
--        If @e :: (Double, Float)@ then @BlockPtrs (EltRepr e) :: (((), Ptr Double), Ptr Float)@
--
--   Each one of these pointers points to a block of memory that is the source of data
--   for the Accelerate array (unlike function 'blockCopyToArrayWithFunctions' where one passes
--   in function which copies data to a destination address.).
--
blockCopyToArray :: (Shape sh, Elt e) => sh -> BlockPtrs (EltRepr e) -> IO (Array sh e)
blockCopyToArray sh blkPtrs = do
  let arr = uninitNewArray sh
      copier = fst $ blockCopyToArrayGen arr
  copier blkPtrs
  return arr


-- | This function is used to populate a freshly allocated Accelerate array using a collection
--   of functions that have type 'BlockCopyFun'. The structure of this collection of functions
--   depends on the elemente type @e@. Each function (of type 'BlockCopyFun') copies data to a
--   destination address (pointed to by the argument of type @Ptr ()@).
--
--   Unless there is a particularly pressing reason to use this function the 'blockCopyToArray'
--   function is probably sufficient as it uses an efficient low-level call to libc's @memcpy@ to
--   perform the copy.
--
blockCopyToArrayWithFunctions :: (Shape sh, Elt e) => sh -> BlockCopyFuns (EltRepr e) -> IO (Array sh e)
blockCopyToArrayWithFunctions sh blockCopyFuns = do
  let arr = uninitNewArray sh
      copier = snd $ blockCopyToArrayGen arr
  copier blockCopyFuns
  return arr


blockCopyToArrayGen :: Array sh e -> ( BlockPtrs (EltRepr e) -> IO ()
                                     , BlockCopyFuns (EltRepr e) -> IO ())
blockCopyToArrayGen array@(Array _ arrayData) = aux arrayElt arrayData
  where
   sizeA = size (shape array)
   base :: Ptr () -> Int -> (Ptr a -> IO (), (Ptr () -> Int -> IO ()) -> IO ())
   base dst byteSize = (\src -> blockCopy (castPtr src) dst byteSize,
                        \f -> f dst byteSize)
   aux :: ArrayEltR e -> ArrayData e -> (BlockPtrs e -> IO (), BlockCopyFuns e -> IO ())
   aux ArrayEltRunit _ = let f = \() -> return () in (f,f)
   aux ArrayEltRint    ad = base (castPtr $ ptrsOfArrayData ad) (box wORD_SCALE sizeA)
   aux ArrayEltRint8   ad = base (castPtr $ ptrsOfArrayData ad) sizeA
   aux ArrayEltRint16  ad = base (castPtr $ ptrsOfArrayData ad) (sizeA * 2)
   aux ArrayEltRint32  ad = base (castPtr $ ptrsOfArrayData ad) (sizeA * 4)
   aux ArrayEltRint64  ad = base (castPtr $ ptrsOfArrayData ad) (sizeA * 8)
   aux ArrayEltRword   ad = base (castPtr $ ptrsOfArrayData ad) (box wORD_SCALE sizeA)
   aux ArrayEltRword8  ad = base (castPtr $ ptrsOfArrayData ad) sizeA
   aux ArrayEltRword16 ad = base (castPtr $ ptrsOfArrayData ad) (sizeA * 2)
   aux ArrayEltRword32 ad = base (castPtr $ ptrsOfArrayData ad) (sizeA * 4)
   aux ArrayEltRword64 ad = base (castPtr $ ptrsOfArrayData ad) (sizeA * 8)
   aux ArrayEltRfloat  ad = base (castPtr $ ptrsOfArrayData ad) (box fLOAT_SCALE sizeA)
   aux ArrayEltRdouble ad = base (castPtr $ ptrsOfArrayData ad) (box dOUBLE_SCALE sizeA)
   aux ArrayEltRbool   ad = base (castPtr $ ptrsOfArrayData ad) (box bOOL_SCALE sizeA)
   aux ArrayEltRchar   _ = error "not defined yet"-- base (castPtr $ ptrsOfArrayData ad) (sizeA * 4)
   aux (ArrayEltRpair a b) (AD_Pair ad1 ad2) = (fromC, toH)
     where
       (fromC1, toH1) = aux a ad1
       (fromC2, toH2) = aux b ad2
       toH (funs1, funs2) = toH1 funs1 >> toH2 funs2
       fromC (ptrA, ptrB) = fromC1 ptrA >> fromC2 ptrB

blockCopy :: Ptr () -> Ptr () -> Int -> IO ()
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

foreign import ccall memcpy :: Ptr () -> Ptr () -> CInt -> IO ()

-- Helpers

box :: (Int# -> Int#) -> (Int -> Int)
box f (I# x) = I# (f x)
