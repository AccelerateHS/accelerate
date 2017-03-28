{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Array.Representation
-- Copyright   : [2008..2014] Manuel M T Chakravarty, Gabriele Keller
--               [2008..2009] Sean Lee
--               [2009..2014] Trevor L. McDonell
--               [2014..2014] Frederik M. Madsen
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Array.Representation (

  -- * Array shapes, indices, and slices
  Shape(..), Slice(..), SliceIndex(..),

  -- * Slice shape functions
  sliceShape, enumSlices,

) where

-- friends
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Type

-- standard library
import GHC.Base                                         ( quotInt, remInt )


-- |Index representation
--

-- |Class of index representations (which are nested pairs)
--
class (Eq sh, Slice sh) => Shape sh where
  -- user-facing methods
  rank      :: sh -> Int             -- ^number of dimensions (>= 0); rank of the array
  size      :: sh -> Int             -- ^total number of elements in an array of this /shape/
  empty     :: sh                    -- ^empty shape.

  -- internal methods
  intersect :: sh -> sh -> sh  -- yield the intersection of two shapes
  union     :: sh -> sh -> sh  -- yield the union of two shapes
  offset    :: sh -> sh -> sh  -- yield the offset of two shapes
  transpose :: sh -> sh        -- transpose a shape
  ignore    :: sh              -- identifies ignored elements in 'permute'
  toIndex   :: sh -> sh -> Int -- yield the index position in a linear, row-major representation of
                               -- the array (first argument is the shape)
  fromIndex :: sh -> Int -> sh -- inverse of `toIndex`
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

  transpose = listToShape . reverse . shapeToList

instance Shape () where
  rank _            = 0
  empty             = ()

  () `intersect` () = ()
  () `union` ()     = ()
  () `offset` ()    = ()
  ignore            = ()
  size ()           = 1
  toIndex () ()     = 0
  fromIndex () _    = ()
  bound () () _     = Right ()
  iter  () f _ _    = f ()
  iter1 () f _      = f ()

  rangeToShape ((), ()) = ()
  shapeToRange ()       = ((), ())

  shapeToList () = []
  listToShape [] = ()
  listToShape _  = $internalError "listToShape" "non-empty list when converting to unit"

instance Shape sh => Shape (sh, Int) where
  rank _                            = rank (undefined :: sh) + 1
  empty                             = (empty, 0)

  (sh1, sz1) `intersect` (sh2, sz2) = (sh1 `intersect` sh2, sz1 `min` sz2)
  (sh1, sz1) `union` (sh2, sz2)     = (sh1 `union` sh2, sz1 `max` sz2)
  (sh1, sz1) `offset` (sh2, sz2)    = (sh1 `offset` sh2, sz1 + sz2)
  ignore                            = (ignore, -1)

  size (sh, sz)                     = $boundsCheck "size" "negative shape dimension" (sz >= 0)
                                    $ size sh * sz
  toIndex (sh, sz) (ix, i)          = $indexCheck "toIndex" i sz
                                    $ toIndex sh ix * sz + i

  fromIndex (sh, sz) i              = (fromIndex sh (i `quotInt` sz), r)
    -- If we assume that the index is in range, there is no point in computing
    -- the remainder for the highest dimension since i < sz must hold.
    --
    where
      r | rank sh == 0  = $indexCheck "fromIndex" i sz i
        | otherwise     = i `remInt` sz

  bound (sh, sz) (ix, i) bndy
    | i < 0                         = case bndy of
                                        Clamp      -> next `addDim` 0
                                        Mirror     -> next `addDim` (-i)
                                        Wrap       -> next `addDim` (sz+i)
                                        Constant e -> Left e
    | i >= sz                       = case bndy of
                                        Clamp      -> next `addDim` (sz-1)
                                        Mirror     -> next `addDim` (sz-(i-sz+2))
                                        Wrap       -> next `addDim` (i-sz)
                                        Constant e -> Left e
    | otherwise                     = next `addDim` i
    where
      -- This function is quite difficult to optimize due to the deep recursion
      -- that is can generate with high-dimensional arrays. If we let 'next' be
      -- inlined into each alternative of the cases above the size of this
      -- function on an n-dimensional array will grow as 7^n. This quickly causes
      -- GHC's head to explode. See GHC Trac #10491 for more details.
      next = bound sh ix bndy
      {-# NOINLINE next #-}

      Right ds `addDim` d = Right (ds, d)
      Left e   `addDim` _ = Left e

  iter (sh, sz) f c r = iter sh (\ix -> iter' (ix,0)) c r
    where
      iter' (ix,i) | i >= sz   = r
                   | otherwise = f (ix,i) `c` iter' (ix,i+1)

  iter1 (_,  0)  _ _ = $boundsError "iter1" "empty iteration space"
  iter1 (sh, sz) f c = iter1 sh (\ix -> iter1' (ix,0)) c
    where
      iter1' (ix,i) | i == sz-1 = f (ix,i)
                    | otherwise = f (ix,i) `c` iter1' (ix,i+1)

  rangeToShape ((sh1, sz1), (sh2, sz2))
    = (rangeToShape (sh1, sh2), sz2 - sz1 + 1)
  shapeToRange (sh, sz)
    = let (low, high) = shapeToRange sh
      in
      ((low, 0), (high, sz - 1))

  shapeToList (sh,sz) = sz : shapeToList sh
  listToShape []      = $internalError "listToShape" "empty list when converting to Ix"
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
  sliceIndex :: {-dummy-} sl -> SliceIndex sl (SliceShape sl) (CoSliceShape sl) (FullShape sl)

instance Slice () where
  type SliceShape    () = ()
  type CoSliceShape  () = ()
  type FullShape     () = ()
  sliceIndex _ = SliceNil

instance Slice sl => Slice (sl, ()) where
  type SliceShape   (sl, ()) = (SliceShape  sl, Int)
  type CoSliceShape (sl, ()) = CoSliceShape sl
  type FullShape    (sl, ()) = (FullShape   sl, Int)
  sliceIndex _ = SliceAll (sliceIndex (undefined::sl))

instance Slice sl => Slice (sl, Int) where
  type SliceShape   (sl, Int) = SliceShape sl
  type CoSliceShape (sl, Int) = (CoSliceShape sl, Int)
  type FullShape    (sl, Int) = (FullShape    sl, Int)
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
