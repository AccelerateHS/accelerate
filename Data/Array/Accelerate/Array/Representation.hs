{-# LANGUAGE GADTs, TypeFamilies, FlexibleContexts, FlexibleInstances #-}

-- |Embedded array processing language: array representation
--
--  Copyright (c) [2008..2009] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
--
--  License: BSD3
--
--- Description ---------------------------------------------------------------
--

module Data.Array.Accelerate.Array.Representation (

  -- * Array representation
  Array(..), Scalar, Vector,

  -- * Array shapes
  DIM0, DIM1, DIM2, 

  -- * Array indexing and slicing
  Ix(..), SliceIx(..), SliceIndex(..),

  -- * Array operations
  (!), newArray

) where

-- GHC internals
import GHC.Prim

-- friends
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Data


infixl 9 !


-- |Arrays
-- -------

-- |Representation type for multi-dimensional arrays for array processing
--
-- * If device and host memory are separate, arrays will be transferred to the
--   device when necessary (if possible asynchronously and in parallel with
--   other tasks) and cached on the device if sufficient memory is available.
--
data Array dim e where
  Array :: (Ix dim, ArrayElem e) 
        => dim             -- extent of dimensions = shape
        -> ArrayData e     -- data
        -> Array dim e

-- |Shorthand for common shape representations
--
type DIM0 = ()
type DIM1 = ((), Int)
type DIM2 = (((), Int), Int)

-- Special case of singleton arrays
--
type Scalar e = Array DIM0 e

-- Special case of one-dimensional arrays
--
type Vector e = Array DIM1 e


-- |Index representation
-- -

-- |Class of index representations (which are nested pairs)
--
class Eq ix => Ix ix where
  dim       :: ix -> Int       -- number of dimensions (>= 0)
  size      :: ix -> Int       -- for a *shape* yield the total number of 
                               -- elements in that array
  intersect :: ix -> ix -> ix  -- yield the intersection of two shapes
  ignore    :: ix              -- identifies ignored elements in 'permute'
  index     :: ix -> ix -> Int -- yield the index position in a linear, 
                               -- row-major representation of the array
                               -- (first argument is the shape)

  iter  :: ix -> (ix -> a) -> (a -> a -> a) -> a -> a
                               -- iterate through the entire shape, applying
                               -- the function; third argument combines results
                               -- and fourth is returned in case of an empty
                               -- iteration space; the index space is traversed
                               -- in row-major order

  -- operations to facilitate conversion with IArray
  rangeToShape :: (ix, ix) -> ix   -- convert a minpoint-maxpoint index
                                   -- into a shape
  shapeToRange :: ix -> (ix, ix)   -- ...the converse

instance Ix () where
  dim       ()       = 0
  size      ()       = 1
  intersect () ()    = ()
  ignore             = ()
  index     () ()    = 0
  iter      () f _ _ = f ()
  
  rangeToShape ((), ()) = ()
  shapeToRange ()       = ((), ())

instance Ix ix => Ix (ix, Int) where
  dim (sh, _)                       = dim sh + 1
  size (sh, sz)                     = size sh * sz
  (sh1, sz1) `intersect` (sh2, sz2) = (sh1 `intersect` sh2, sz1 `min` sz2)
  ignore                            = (ignore, -1)
  index (sh, sz) (ix, i) 
    | i >= 0 && i < sz              = index sh ix + size sh * i
    | otherwise              
    = error "Data.Array.Accelerate.Array: index out of bounds"
  iter (sh, sz) f c r    = iter' 0
    where
      iter' i | i >= sz   = r
              | otherwise = iter sh (\ix -> f (ix, i)) c r `c` iter' (i + 1)

  rangeToShape ((sh1, sz1), (sh2, sz2)) 
    = (rangeToShape (sh1, sh2), sz2 - sz1 + 1)
  shapeToRange (sh, sz) 
    = let (low, high) = shapeToRange sh
      in 
      ((low, 0), (high, sz - 1))


-- |Slice representation
-- -

-- |Class of slice representations (which are nested pairs)
--
class SliceIx sl where
  type Slice    sl      -- the projected slice
  type CoSlice  sl      -- the complement of the slice
  type SliceDim sl      -- the combined dimension
    -- argument *value* not used; it's just a phantom value to fix the type
  sliceIndex :: sl -> SliceIndex sl 
                                     (Slice    sl) 
                                     (CoSlice  sl) 
                                     (SliceDim sl)

instance SliceIx () where
  type Slice    () = ()
  type CoSlice  () = ()
  type SliceDim () = ()
  sliceIndex _ = SliceNil

instance SliceIx sl => SliceIx (sl, ()) where
  type Slice    (sl, ()) = (Slice sl, Int)
  type CoSlice  (sl, ()) = CoSlice sl
  type SliceDim (sl, ()) = (SliceDim sl, Int)
  sliceIndex _ = SliceAll (sliceIndex (undefined::sl))

instance SliceIx sl => SliceIx (sl, Int) where
  type Slice    (sl, Int) = Slice sl
  type CoSlice  (sl, Int) = (CoSlice sl, Int)
  type SliceDim (sl, Int) = (SliceDim sl, Int)
  sliceIndex _ = SliceFixed (sliceIndex (undefined::sl))

-- |Generalised array index, which may index only in a subset of the dimensions
-- of a shape.
--
data SliceIndex ix slice coSlice sliceDim where
  SliceNil   :: SliceIndex () () () ()
  SliceAll   :: 
   SliceIndex ix slice co dim -> SliceIndex (ix, ()) (slice, Int) co (dim, Int)
  SliceFixed :: 
   SliceIndex ix slice co dim -> SliceIndex (ix, Int) slice (co, Int) (dim, Int)


-- Array operations
-- ----------------

-- |Array indexing
--
(!) :: Array dim e -> dim -> e
-- (Array sh adata) ! ix = adata `indexArrayData` index sh ix
-- FIXME: using this due to a bug in 6.10.x
(!) (Array sh adata) ix = adata `indexArrayData` index sh ix

-- |Create an array from its representation function
--
newArray :: (Ix dim, ArrayElem e) => dim -> (dim -> e) -> Array dim e
newArray sh f 
  = adata `seq` Array sh adata
  where 
    (adata, _) = runArrayData $ do
                   arr <- newArrayData (size sh)
                   let write ix = writeArrayData arr (index sh ix) (f ix)      
                   iter sh write (>>) (return ())
                   return (arr, undefined)
