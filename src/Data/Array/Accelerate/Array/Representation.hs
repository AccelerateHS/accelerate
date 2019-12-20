{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Array.Representation
-- Copyright   : [2008..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Array.Representation (
  -- * Array data type in terms of representation types
  Array(..), ArrayR(..), arraysRarray, arraysRtuple2,
  ArraysR, TupleType, Scalar, Vector, Matrix, fromList, toList,

  -- * Array shapes, indices, and slices
  ShapeR(..), Slice(..), SliceIndex(..),
  DIM0, DIM1, DIM2,

  -- * Shape functions
  rank, size, empty, ignore, intersect, union, toIndex, fromIndex, iter, iter1,
  rangeToShape, shapeToRange, shapeToList, listToShape, listToShape', shapeType,

  -- * Slice shape functions
  sliceShape, sliceShapeR, enumSlices,

) where

-- friends
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Data

-- standard library
import GHC.Base                                         ( quotInt, remInt )

-- |Array data type, where the type arguments regard the representation types of the shape and elements.
data Array sh e where
  Array :: sh                         -- extent of dimensions = shape
        -> ArrayData e                -- array payload
        -> Array sh e

{-# INLINEABLE fromList #-}
fromList :: ArrayR (Array sh e) -> sh -> [e] -> Array sh e
fromList (ArrayR shr tp) sh xs = adata `seq` Array sh adata
  where
    -- Assume the array is in dense row-major order. This is safe because
    -- otherwise backends would not be able to directly memcpy.
    --
    !n    = size shr sh
    (adata, _) = runArrayData $ do
                  arr <- newArrayData tp n
                  let go !i _ | i >= n = return ()
                      go !i (v:vs)     = unsafeWriteArrayData tp arr i v >> go (i+1) vs
                      go _  []         = error "Data.Array.Accelerate.fromList: not enough input data"
                  --
                  go 0 xs
                  return (arr, undefined)


-- | Convert an accelerated 'Array' to a list in row-major order.
--
{-# INLINEABLE toList #-}
toList :: ArrayR (Array sh e) -> Array sh e -> [e]
toList (ArrayR shr tp) (Array sh adata) = go 0
  where
    -- Assume underling array is in row-major order. This is safe because
    -- otherwise backends would not be able to directly memcpy.
    --
    !n                  = size shr sh
    go !i | i >= n      = []
          | otherwise   = (unsafeIndexArrayData tp adata i) : go (i+1)

type ArraysR = TupR ArrayR
data ArrayR a where
  ArrayR :: ShapeR sh -> TupleType e -> ArrayR (Array sh e)

arraysRarray :: ShapeR sh -> TupleType e -> ArraysR (Array sh e)
arraysRarray shr tp = TupRsingle $ ArrayR shr tp

arraysRtuple2 :: ArrayR a -> ArrayR b -> ArraysR (((), a), b)
arraysRtuple2 a b = TupRpair TupRunit (TupRsingle a) `TupRpair` TupRsingle b

type Scalar = Array DIM0
type Vector = Array DIM1
type Matrix = Array DIM2

-- |Index representation
--
type DIM0 = ()
type DIM1 = ((), Int)
type DIM2 = (((), Int), Int)

-- |Index representations (which are nested pairs)
--
    
data ShapeR sh where
  ShapeRz :: ShapeR ()
  ShapeRcons :: ShapeR sh -> ShapeR (sh, Int)

rank :: ShapeR sh -> Int
rank ShapeRz = 0
rank (ShapeRcons shr) = rank shr + 1

size :: ShapeR sh -> sh -> Int
size ShapeRz () = 1
size (ShapeRcons shr) (sh, sz)
  | sz <= 0   = 0
  | otherwise = size shr sh * sz

empty :: ShapeR sh -> sh
empty ShapeRz = ()
empty (ShapeRcons shr) = (empty shr, 0)

ignore :: ShapeR sh -> sh
ignore ShapeRz = ()
ignore (ShapeRcons shr) = (ignore shr, -1)

shapeZip :: (Int -> Int -> Int) -> ShapeR sh -> sh -> sh -> sh
shapeZip _ ShapeRz () () = ()
shapeZip f (ShapeRcons shr) (as, a) (bs, b) = (shapeZip f shr as bs, f a b)

intersect, union :: ShapeR sh -> sh -> sh -> sh
intersect = shapeZip min
union = shapeZip max

toIndex :: ShapeR sh -> sh -> sh -> Int
toIndex ShapeRz () () = 0
toIndex (ShapeRcons shr) (sh, sz) (ix, i)
  = $indexCheck "toIndex" i sz
  $ toIndex shr sh ix * sz + i

fromIndex :: ShapeR sh -> sh -> Int -> sh
fromIndex ShapeRz () _ = ()
fromIndex (ShapeRcons shr) (sh, sz) i
  = (fromIndex shr sh (i `quotInt` sz), r)
  -- If we assume that the index is in range, there is no point in computing
  -- the remainder for the highest dimension since i < sz must hold.
  --
  where
    r = case shr of -- Check if rank of shr is 0
      ShapeRz -> $indexCheck "fromIndex" i sz i
      _       -> i `remInt` sz

-- iterate through the entire shape, applying the function in the
-- second argument; third argument combines results and fourth is an
-- initial value that is combined with the results; the index space
-- is traversed in row-major order
iter :: ShapeR sh -> sh -> (sh -> a) -> (a -> a -> a) -> a -> a
iter ShapeRz () f _ _    = f ()
iter (ShapeRcons shr) (sh, sz) f c r = iter shr sh (\ix -> iter' (ix,0)) c r
  where
    iter' (ix,i) | i >= sz   = r
                 | otherwise = f (ix,i) `c` iter' (ix,i+1)

-- variant of 'iter' without an initial value
iter1 :: ShapeR sh -> sh -> (sh -> a) -> (a -> a -> a) -> a
iter1 ShapeRz () f _      = f ()
iter1 (ShapeRcons _  ) (_,  0)  _ _ = $boundsError "iter1" "empty iteration space"
iter1 (ShapeRcons shr) (sh, sz) f c = iter1 shr sh (\ix -> iter1' (ix,0)) c
  where
    iter1' (ix,i) | i == sz-1 = f (ix,i)
                  | otherwise = f (ix,i) `c` iter1' (ix,i+1)

-- Operations to facilitate conversion with IArray

-- convert a minpoint-maxpoint index into a shape
rangeToShape :: ShapeR sh -> (sh, sh) -> sh
rangeToShape ShapeRz ((), ()) = ()
rangeToShape (ShapeRcons shr) ((sh1, sz1), (sh2, sz2)) = (rangeToShape shr (sh1, sh2), sz2 - sz1 + 1)

-- the converse
shapeToRange :: ShapeR sh -> sh -> (sh, sh)
shapeToRange ShapeRz () = ((), ())
shapeToRange (ShapeRcons shr) (sh, sz) = let (low, high) = shapeToRange shr sh in ((low, 0), (high, sz - 1))

-- Other conversions

-- Convert a shape into its list of dimensions
shapeToList :: ShapeR sh -> sh -> [Int]
shapeToList ShapeRz () = []
shapeToList (ShapeRcons shr) (sh,sz) = sz : shapeToList shr sh

-- Convert a list of dimensions into a shape
listToShape :: ShapeR sh -> [Int] -> sh
listToShape shr ds = case listToShape' shr ds of
  Just sh -> sh
  Nothing -> $internalError "listToShape" "unable to convert list to a shape at the specified type"

-- Attempt to convert a list of dimensions into a shape
listToShape' :: ShapeR sh -> [Int] -> Maybe sh
listToShape' ShapeRz [] = Just ()
listToShape' (ShapeRcons shr) (x:xs) = (, x) <$> listToShape' shr xs
listToShape' _ _ = Nothing

shapeType :: ShapeR sh -> TupleType sh
shapeType ShapeRz = TupRunit
shapeType (ShapeRcons shr) = shapeType shr `TupRpair` (TupRsingle $ SingleScalarType $ NumSingleType $ IntegralNumType TypeInt)

-- |Slice representation
--

-- |Class of slice representations (which are nested pairs)
--
class Slice sl where
  type SliceShape    sl      -- the projected slice
  type CoSliceShape  sl      -- the complement of the slice
  type FullShape     sl      -- the combined dimension
    -- argument *value* not used; it's just a phantom value to fix the type
  sliceIndex :: SliceIndex sl (SliceShape sl) (CoSliceShape sl) (FullShape sl)

instance Slice () where
  type SliceShape    () = ()
  type CoSliceShape  () = ()
  type FullShape     () = ()
  sliceIndex = SliceNil

instance Slice sl => Slice (sl, ()) where
  type SliceShape   (sl, ()) = (SliceShape  sl, Int)
  type CoSliceShape (sl, ()) = CoSliceShape sl
  type FullShape    (sl, ()) = (FullShape   sl, Int)
  sliceIndex = SliceAll (sliceIndex @sl)

instance Slice sl => Slice (sl, Int) where
  type SliceShape   (sl, Int) = SliceShape sl
  type CoSliceShape (sl, Int) = (CoSliceShape sl, Int)
  type FullShape    (sl, Int) = (FullShape    sl, Int)
  sliceIndex = SliceFixed (sliceIndex @sl)

-- |Generalised array index, which may index only in a subset of the dimensions
-- of a shape.
--
data SliceIndex ix slice coSlice sliceDim where
  SliceNil   :: SliceIndex () () () ()
  SliceAll   ::
   SliceIndex ix slice co dim -> SliceIndex (ix, ()) (slice, Int) co (dim, Int)
  SliceFixed ::
   SliceIndex ix slice co dim -> SliceIndex (ix, Int) slice (co, Int) (dim, Int)

instance Show (SliceIndex ix slice coSlice sliceDim) where
  show SliceNil          = "SliceNil"
  show (SliceAll rest)   = "SliceAll (" ++ show rest ++ ")"
  show (SliceFixed rest) = "SliceFixed (" ++ show rest ++ ")"

-- | Project the shape of a slice from the full shape.
--
sliceShape :: forall slix co sl dim.
              SliceIndex slix sl co dim
           -> dim
           -> sl
sliceShape SliceNil        ()      = ()
sliceShape (SliceAll   sl) (sh, n) = (sliceShape sl sh, n)
sliceShape (SliceFixed sl) (sh, _) = sliceShape sl sh

sliceShapeR :: SliceIndex slix sl co dim -> ShapeR sl
sliceShapeR SliceNil        = ShapeRz
sliceShapeR (SliceAll sl)   = ShapeRcons $ sliceShapeR sl
sliceShapeR (SliceFixed sl) = sliceShapeR sl

-- | Enumerate all slices within a given bound. The innermost dimension changes
-- most rapidly.
--
-- See 'Data.Array.Accelerate.Array.Sugar.enumSlices' for an example.
--
enumSlices :: forall slix co sl dim.
              SliceIndex slix sl co dim
           -> dim
           -> [slix]
enumSlices SliceNil        ()       = [()]
enumSlices (SliceAll   sl) (sh, _)  = [ (sh', ()) | sh' <- enumSlices sl sh]
enumSlices (SliceFixed sl) (sh, n)  = [ (sh', i)  | sh' <- enumSlices sl sh, i <- [0..n-1]]

