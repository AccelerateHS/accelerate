{-# LANGUAGE GADTs, MagicHash, ForeignFunctionInterface, TypeFamilies, ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.IO.BlockCopy
-- Copyright   : [2010..2011] Sean Seefried
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.IO.BlockCopy (

  -- * Types
  BlockCopyFun, BlockCopyFuns, BlockPtrs, ByteStrings,

  -- * The low-level machinery
  allocateArray, blockCopyFunGenerator

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


-- | Functions of this type are passed as arguments to 'toArray'. A function of
--   this type should copy a number of bytes (equal to the value of the
--   parameter of type 'Int') to the destination memory pointed to by @Ptr e@.
--
type BlockCopyFun e = Ptr e -> Int -> IO ()

-- | Represents a collection of "block copy functions" (see 'BlockCopyFun'). The
--   structure of the collection of 'BlockCopyFun's depends on the element type
--   @e@.
--
--   e.g.
--
--   If @e :: Float@
--   then @BlockCopyFuns (EltRepr e) :: ((), Ptr Float -> Int -> IO ())@
--
--   If @e :: (Double, Float)@
--   then @BlockCopyFuns (EltRepr e) :: (((), Ptr Double -> Int -> IO ()), Ptr Float -> Int -> IO ())@
--
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

-- | A family of types that represents a collection of pointers that are the
--   source/destination addresses for a block copy. The structure of the
--   collection of pointers depends on the element type @e@.
--
--  e.g.
--
--  If @e :: Int@,            then @BlockPtrs (EltRepr e) :: ((), Ptr Int)@
--
--  If @e :: (Double, Float)@ then @BlockPtrs (EltRepr e) :: (((), Ptr Double), Ptr Float)@
--
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

-- | A family of types that represents a collection of 'ByteString's. They are
--   the source data for function 'fromByteString' and the result data for
--   'toByteString'
--
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
    byteStringToArray bs = useAsCString bs (blockPtrToArray . castPtr)
    arrayToByteString :: IO ByteString
    arrayToByteString = packCStringLen (castPtr accArrayPtr, byteSize)

blockCopyFunGenerator :: Array sh e -> GenFuns (EltRepr e)
blockCopyFunGenerator array@(Array _ arrayData) = aux arrayElt arrayData
  where
   sizeA = size (shape array)
   aux :: ArrayEltR e -> ArrayData e -> GenFuns e
   aux ArrayEltRunit _ = let f () = return () in ((f,f),(f,return ()),f)
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


-- Foreign imports
foreign import ccall memcpy :: Ptr a -> Ptr b -> CInt -> IO ()

-- Helpers
box :: (Int# -> Int#) -> Int -> Int
box f (I# x) = I# (f x)

