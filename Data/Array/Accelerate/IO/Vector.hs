-- |
-- Module      : Data.Array.Accelerate.IO.Vector
-- Copyright   : [2012] Adam C. Foltzer
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Helpers for fast conversion of 'Data.Vector.Storable' vectors into
-- Accelerate arrays.
module Data.Array.Accelerate.IO.Vector (
    -- * Vector conversions
    fromVector
  , toVector
  , fromVectorIO
  , toVectorIO
) where

import Data.Array.Accelerate ( arrayShape
                             , Array
                             , DIM1
                             , Elt
                             , Z(..)
                             , (:.)(..))
import Data.Array.Accelerate.Array.Sugar (EltRepr)
import Data.Array.Accelerate.IO.Ptr
import Data.Vector.Storable ( unsafeFromForeignPtr0
                            , unsafeToForeignPtr0
                            , Vector)

import Foreign (mallocForeignPtrArray, Ptr, Storable, withForeignPtr)

import System.IO.Unsafe

fromVectorIO :: (Storable a, Elt a, BlockPtrs (EltRepr a) ~ ((), Ptr a))
             => Vector a -> IO (Array DIM1 a)
fromVectorIO v = withForeignPtr fp $ \ptr -> fromPtr (Z :. len) ((), ptr)
  where (fp, len) = unsafeToForeignPtr0 v

toVectorIO :: (Storable a, Elt a, BlockPtrs (EltRepr a) ~ ((), Ptr a)) 
           => Array DIM1 a -> IO (Vector a)
toVectorIO arr = do
  let (Z :. len) = arrayShape arr
  fp <- mallocForeignPtrArray len
  withForeignPtr fp $ \ptr -> toPtr arr ((), ptr)
  return $ unsafeFromForeignPtr0 fp len

fromVector :: (Storable a, Elt a, BlockPtrs (EltRepr a) ~ ((), Ptr a))
           => Vector a -> Array DIM1 a
fromVector v = unsafePerformIO $ fromVectorIO v

toVector :: (Storable a, Elt a, BlockPtrs (EltRepr a) ~ ((), Ptr a)) 
         => Array DIM1 a -> Vector a
toVector arr = unsafePerformIO $ toVectorIO arr
