{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators, GADTs, TypeFamilies, FlexibleContexts, FlexibleInstances #-}
-- |
-- Module      : Data.Array.Accelerate.Array.Representation
-- Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Data.Array.Accelerate.Array.Representation (

  -- * Array shapes, indices, and slices
  Shape(..), Slice(..), SliceIndex(..),

) where

-- friends 
import Data.Array.Accelerate.Type

#include "accelerate.h"


-- |Index representation
--

-- |Class of index representations (which are nested pairs)
--
class Eq sh => Shape sh where
  -- user-facing methods
  dim       :: sh -> Int             -- ^number of dimensions (>= 0); rank of the array
  size      :: sh -> Int             -- ^total number of elements in an array of this /shape/

  -- internal methods
  intersect :: sh -> sh -> sh  -- yield the intersection of two shapes
  ignore    :: sh              -- identifies ignored elements in 'permute'
  index     :: sh -> sh -> Int -- yield the index position in a linear, row-major representation of
                               -- the array (first argument is the shape)
  bound     :: sh -> sh -> Boundary e -> Either e sh
                               -- apply a boundary condition to an index

  iter      :: sh -> (sh -> a) -> (a -> a -> a) -> a -> a
                               -- iterate through the entire shape, applying the function in the
                               -- second argument; third argument combines results and fourth is an
                               -- initial value that is combined with the results; the index space
                               -- is traversed in row-major order

  iter1     :: sh -> (sh -> a) -> (a -> a -> a) -> a
                               -- variant of 'iter' without an initial value

  -- operations to facilitate conversion with IArray
  rangeToShape :: (sh, sh) -> sh   -- convert a minpoint-maxpoint index
                                   -- into a shape
  shapeToRange :: sh -> (sh, sh)   -- ...the converse
  

  -- other conversions
  shapeToList :: sh -> [Int]    -- convert a shape into its list of dimensions
  listToShape :: [Int] -> sh    -- convert a list of dimensions into a shape

instance Shape () where
  dim ()            = 0
  size ()           = 1
  
  () `intersect` () = ()
  ignore            = ()
  index () ()       = 0
  bound () () _     = Right ()
  iter  () f c e    = e `c` f ()
  iter1 () f _      = f ()
  
  rangeToShape ((), ()) = ()
  shapeToRange ()       = ((), ())

  shapeToList () = []
  listToShape [] = ()
  listToShape _  = INTERNAL_ERROR(error) "listToShape" "non-empty list when converting to unit"

instance Shape sh => Shape (sh, Int) where
  dim (sh, _)                       = dim sh + 1
  size (sh, sz)                     = size sh * sz
  
  (sh1, sz1) `intersect` (sh2, sz2) = (sh1 `intersect` sh2, sz1 `min` sz2)
  ignore                            = (ignore, -1)
  index (sh, sz) (ix, i)            = BOUNDS_CHECK(checkIndex) "index" i sz
                                    $ index sh ix + size sh * i
  bound (sh, sz) (ix, i) bndy
    | i < 0                         = case bndy of
                                        Clamp      -> bound sh ix bndy `addDim` 0
                                        Mirror     -> bound sh ix bndy `addDim` (-(i+1))
                                        Wrap       -> bound sh ix bndy `addDim` (sz+i)
                                        Constant e -> Left e
    | i >= sz                       = case bndy of
                                        Clamp      -> bound sh ix bndy `addDim` (sz-1)
                                        Mirror     -> bound sh ix bndy `addDim` (sz-(i-sz+1))
                                        Wrap       -> bound sh ix bndy `addDim` (i-sz)
                                        Constant e -> Left e
    | otherwise                     = bound sh ix bndy `addDim` i
    where
      Right ix `addDim` i = Right (ix, i)
      Left e   `addDim` _ = Left e

  iter (sh, sz) f c r = iter' 0 r
    where
      iter' i v | i >= sz   = v
                | otherwise = iter' (i + 1) $ iter sh (\ix -> f (ix, i)) c v

  iter1 (_sh, 0)  _f _c = BOUNDS_ERROR(error) "iter1" "empty iteration space"
  iter1 (sh , sz) f  c  = iter1' (sz - 1)
    where
      iter1' i | i == 0    = iter1 sh (\ix -> f (ix, 0)) c
               | otherwise = iter1' (i - 1) `c` iter1 sh (\ix -> f (ix, i)) c

  rangeToShape ((sh1, sz1), (sh2, sz2)) 
    = (rangeToShape (sh1, sh2), sz2 - sz1 + 1)
  shapeToRange (sh, sz) 
    = let (low, high) = shapeToRange sh
      in 
      ((low, 0), (high, sz - 1))

  shapeToList (sh,sz) = sz : shapeToList sh
  listToShape []      = INTERNAL_ERROR(error) "listToShape" "empty list when converting to Ix"
  listToShape (x:xs)  = (listToShape xs,x)


-- |Slice representation
--

-- |Class of slice representations (which are nested pairs)
--
class Slice sl where
  type SliceShape    sl      -- the projected slice
  type CoSliceShape  sl      -- the complement of the slice
  type FullShape     sl      -- the combined dimension
    -- argument *value* not used; it's just a phantom value to fix the type
  sliceIndex :: sl -> SliceIndex sl (SliceShape sl) (CoSliceShape sl) (FullShape sl)

instance Slice () where
  type SliceShape    () = ()
  type CoSliceShape  () = ()
  type FullShape () = ()
  sliceIndex _ = SliceNil

instance Slice sl => Slice (sl, ()) where
  type SliceShape   (sl, ()) = (SliceShape sl, Int)
  type CoSliceShape (sl, ()) = CoSliceShape sl
  type FullShape    (sl, ()) = (FullShape sl, Int)
  sliceIndex _ = SliceAll (sliceIndex (undefined::sl))

instance Slice sl => Slice (sl, Int) where
  type SliceShape   (sl, Int) = SliceShape sl
  type CoSliceShape (sl, Int) = (CoSliceShape sl, Int)
  type FullShape    (sl, Int) = (FullShape sl, Int)
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
