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
  IxRepr(..), SliceIxRepr(..), SliceIndex(..)

) where

-- GHC internals
import GHC.Prim

-- standard libraries
import Data.Typeable

-- friends
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Data


-- |Arrays
-- -------

-- |Representation type for multi-dimensional arrays for array processing
--
-- * If device and host memory are separate, arrays will be transferred to the
--   device when necessary (if possible asynchronously and in parallel with
--   other tasks) and cached on the device if sufficient memory is available.
--
data Array dim e where
  Array :: (IxRepr dim, ArrayElem e) =>
           { arrayShape    :: dim             -- ^extent of dimensions = shape
           , arrayElemType :: TupleType e     -- ^constrains valid element types
           , arrayId       :: String          -- ^for pretty printing
           , arrayData     :: ArrayData e     -- ^data
           }               -> Array dim e

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
class IxRepr ix where
  dimRepr   :: ix -> Int           -- ^number of dimensions (>= 0)
  sizeRepr  :: ix -> Int           -- ^for a *shape* yield the total number of 
                                   -- elements in that array
  indexRepr :: ix -> ix -> Int     -- ^yield the index position in a linear, 
                                   -- row-major representation of the array
                                   -- (first argument is the shape)
  -- FIXME: we might want an unsafeIndex, too

instance IxRepr () where
  dimRepr   _   = 0
  sizeRepr  _   = 1
  indexRepr _ _ = 0

instance IxRepr ix => IxRepr (ix, Int) where
  dimRepr   (sh, _)          = dimRepr sh + 1
  sizeRepr  (sh, sz)         = sizeRepr sh * sz
  indexRepr (sh, sz) (ix, i) 
    | i >= 0 && i < sz       = indexRepr sh ix + sizeRepr sh * i
    | otherwise              
    = error "Data.Array.Accelerate.Array: index out of bounds"


-- |Slice representation
-- -

-- |Class of slice representations (which are nested pairs)
--
class SliceIxRepr sl where
  type SliceRepr    sl      -- the projected slice
  type CoSliceRepr  sl      -- the complement of the slice
  type SliceDimRepr sl      -- the combined dimension
    -- argument *value* not used; it's just a phantom value to fix the type
  sliceIndexRepr :: sl -> SliceIndex sl 
                                     (SliceRepr    sl) 
                                     (CoSliceRepr  sl) 
                                     (SliceDimRepr sl)

instance SliceIxRepr () where
  type SliceRepr    () = ()
  type CoSliceRepr  () = ()
  type SliceDimRepr () = ()
  sliceIndexRepr _ = SliceNil

instance SliceIxRepr sl => SliceIxRepr (sl, ()) where
  type SliceRepr    (sl, ()) = (SliceRepr sl, Int)
  type CoSliceRepr  (sl, ()) = CoSliceRepr sl
  type SliceDimRepr (sl, ()) = (SliceDimRepr sl, Int)
  sliceIndexRepr _ = SliceAll (sliceIndexRepr (undefined::sl))

instance SliceIxRepr sl => SliceIxRepr (sl, Int) where
  type SliceRepr    (sl, Int) = SliceRepr sl
  type CoSliceRepr  (sl, Int) = (CoSliceRepr sl, Int)
  type SliceDimRepr (sl, Int) = (SliceDimRepr sl, Int)
  sliceIndexRepr _ = SliceFixed (sliceIndexRepr (undefined::sl))

-- |Generalised array index, which may index only in a subset of the dimensions
-- of a shape.
--
data SliceIndex ix slice coSlice sliceDim where
  SliceNil   :: SliceIndex () () () ()
  SliceAll   :: 
   SliceIndex ix slice co dim -> SliceIndex (ix, ()) (slice, Int) co (dim, Int)
  SliceFixed :: 
   SliceIndex ix slice co dim -> SliceIndex (ix, Int) slice (co, Int) (dim, Int)
