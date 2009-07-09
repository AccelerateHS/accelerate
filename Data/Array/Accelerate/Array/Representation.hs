{-# LANGUAGE GADTs, TypeFamilies, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}  -- for instance Slice sl

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
  Array(..),

  -- * Array indexing and slicing
  DIM0Repr, DIM1Repr, DIM2Repr, All(..), IxRepr(..), SliceRepr(..)

) where

--standard library
import Foreign.ForeignPtr (
  ForeignPtr)

-- friends
import Data.Array.Accelerate.Type


-- |Arrays
-- -------

-- |Multi-dimensional arrays for array processing
--
-- * If device and host memory are separate, arrays will be transferred to the
--   device when necessary (if possible asynchronously and in parallel with
--   other tasks) and cached on the device if sufficient memory is available.
--
data Array dim e where
-- New record syntax for 6.12
--  Array :: { arrayShape    :: dim             -- ^extend of dimensions = shape
  Array    { arrayShape    :: dim             -- ^extend of dimensions = shape
           , arrayElemType :: TupleType e     -- ^constrains valid element types
           , arrayId       :: String          -- ^for pretty printing
           , arraySize     :: Int             -- ^data size in bytes
           , arrayPtr      :: ForeignPtr e    -- ^data
--           }               -> Array dim e
           }               :: Array dim e


-- |Shorthand for common shape representations
--
type DIM0Repr = ()
type DIM1Repr = ((), Int)
type DIM2Repr = (((), Int), Int)


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

-- |Identifier for entire dimensions in slice descriptors
--
data All = All

-- |Class of slice representations (which are nested pairs)
--
class SliceRepr sl where
  type CoSliceRepr sl

instance SliceRepr () where
  type CoSliceRepr () = ()

instance SliceRepr sl => SliceRepr (sl, All) where
  type CoSliceRepr (sl, All) = (CoSliceRepr sl, Int)

instance SliceRepr sl => SliceRepr (sl, Int) where
  type CoSliceRepr (sl, Int) = CoSliceRepr sl

{-
-- |Generalised array index, which may index only in a subset of the dimensions
-- of a shape.
--
data Index initialDim projectedDim where
  IndexNil   :: Index () ()
  IndexAll   :: Index init proj -> Index (init, Int) (proj, Int)
  IndexFixed :: Exp Int -> Index init proj -> Index (init, Int)  proj
-}