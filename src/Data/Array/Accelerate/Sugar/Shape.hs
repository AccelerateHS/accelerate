{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Sugar.Shape
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Array indices are snoc lists at both the type and value level. That is,
-- they're backwards, where the end-of-list token, 'Z', occurs first. For
-- example, the type of a rank-2 array index is @Z :. Int :. Int@, and
-- shape of a rank-2 array with 5 rows and 10 columns is @Z :. 5 :. 10@.
--
-- In Accelerate the rightmost dimension is the /fastest varying/ or
-- innermost; these values are adjacent in memory.
--

module Data.Array.Accelerate.Sugar.Shape
  where

import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Representation.Tag
import Data.Array.Accelerate.Representation.Type
import qualified Data.Array.Accelerate.Representation.Shape         as R
import qualified Data.Array.Accelerate.Representation.Slice         as R

import Data.Kind
import GHC.Generics


-- Shorthand for common shape types
--
type DIM0 = Z
type DIM1 = DIM0 :. Int
type DIM2 = DIM1 :. Int
type DIM3 = DIM2 :. Int
type DIM4 = DIM3 :. Int
type DIM5 = DIM4 :. Int
type DIM6 = DIM5 :. Int
type DIM7 = DIM6 :. Int
type DIM8 = DIM7 :. Int
type DIM9 = DIM8 :. Int

-- | Rank-0 index
--
data Z = Z
  deriving (Show, Eq, Generic, Elt)

-- | Increase an index rank by one dimension. The ':.' operator is used to
-- construct both values and types.
--
infixl 3 :.
data tail :. head = !tail :. !head
  deriving (Eq, Generic)  -- Not deriving Elt or Show

-- We don't we use a derived Show instance for (:.) because this will insert
-- parenthesis to demonstrate which order the operator is applied, i.e.:
--
--   (((Z :. z) :. y) :. x)
--
-- This is fine, but I find it a little unsightly. Instead, we drop all
-- parenthesis and just display the shape thus:
--
--   Z :. z :. y :. x
--
-- and then require the down-stream user to wrap the whole thing in parentheses.
-- This works fine for the most important case, which is to show Acc and Exp
-- expressions via the pretty printer, although Show-ing a Shape directly
-- results in no parenthesis being displayed.
--
-- One way around this might be to have specialised instances for DIM1, DIM2,
-- etc.
--
instance (Show sh, Show sz) => Show (sh :. sz) where
  showsPrec p (sh :. sz) =
    showsPrec p sh . showString " :. " . showsPrec p sz

-- | Marker for entire dimensions in 'Data.Array.Accelerate.Language.slice' and
-- 'Data.Array.Accelerate.Language.replicate' descriptors.
--
-- Occurrences of 'All' indicate the dimensions into which the array's existing
-- extent will be placed unchanged.
--
-- See 'Data.Array.Accelerate.Language.slice' and
-- 'Data.Array.Accelerate.Language.replicate' for examples.
--
data All = All
  deriving (Show, Eq, Generic, Elt)

-- | Marker for arbitrary dimensions in 'Data.Array.Accelerate.Language.slice'
-- and 'Data.Array.Accelerate.Language.replicate' descriptors.
--
-- 'Any' can be used in the leftmost position of a slice instead of 'Z',
-- indicating that any dimensionality is admissible in that position.
--
-- See 'Data.Array.Accelerate.Language.slice' and
-- 'Data.Array.Accelerate.Language.replicate' for examples.
--
data Any sh = Any
  deriving (Show, Eq, Generic)

-- | Marker for splitting along an entire dimension in division descriptors.
--
-- For example, when used in a division descriptor passed to
-- 'Data.Array.Accelerate.toSeq', a `Split` indicates that the array should be
-- divided along this dimension forming the elements of the output sequence.
--
data Split = Split
  deriving (Show, Eq)

-- | Marker for arbitrary shapes in slices descriptors, where it is desired to
-- split along an unknown number of dimensions.
--
-- For example, in the following definition, 'Divide' matches against any shape
-- and flattens everything but the innermost dimension.
--
-- > vectors :: (Shape sh, Elt e) => Acc (Array (sh:.Int) e) -> Seq [Vector e]
-- > vectors = toSeq (Divide :. All)
--
data Divide sh = Divide
  deriving (Show, Eq)


-- | Number of dimensions of a /shape/ or /index/ (>= 0)
--
rank :: forall sh. Shape sh => Int
rank = R.rank (shapeR @sh)

-- | Total number of elements in an array of the given /shape/
--
size :: forall sh. Shape sh => sh -> Int
size = R.size (shapeR @sh) . fromElt

-- | The empty /shape/
--
empty :: forall sh. Shape sh => sh
empty = toElt $ R.empty (shapeR @sh)

-- | Yield the intersection of two shapes
intersect :: forall sh. Shape sh => sh -> sh -> sh
intersect x y = toElt $ R.intersect (shapeR @sh) (fromElt x) (fromElt y)

-- | Yield the union of two shapes
--
union :: forall sh. Shape sh => sh -> sh -> sh
union x y = toElt $ R.union (shapeR @sh) (fromElt x) (fromElt y)

-- | Map a multi-dimensional index into one in a linear, row-major
-- representation of the array (first argument is the /shape/, second
-- argument is the index).
--
toIndex :: forall sh. Shape sh
        => sh       -- ^ Total shape (extent) of the array
        -> sh       -- ^ The argument index
        -> Int      -- ^ Corresponding linear index
toIndex sh ix = R.toIndex (shapeR @sh) (fromElt sh) (fromElt ix)

-- | Inverse of 'toIndex'.
--
fromIndex :: forall sh. Shape sh
          => sh     -- ^ Total shape (extent) of the array
          -> Int    -- ^ The argument index
          -> sh     -- ^ Corresponding multi-dimensional index
fromIndex sh = toElt . R.fromIndex (shapeR @sh) (fromElt sh)

-- | Iterate through all of the indices of a shape, applying the given
-- function at each index. The index space is traversed in row-major order.
--
iter :: forall sh e. Shape sh
     => sh              -- ^ The total shape (extent) of the index space
     -> (sh -> e)       -- ^ Function to apply at each index
     -> (e -> e -> e)   -- ^ Function to combine results
     -> e               -- ^ Value to return in case of an empty iteration space
     -> e
iter sh f = R.iter (shapeR @sh) (fromElt sh) (f . toElt)

-- | Variant of 'iter' without an initial value
--
iter1 :: forall sh e. Shape sh
      => sh
      -> (sh -> e)
      -> (e -> e -> e)
      -> e
iter1 sh f = R.iter1 (shapeR @sh) (fromElt sh) (f . toElt)

-- | Convert a minpoint-maxpoint index into a zero-indexed shape
--
rangeToShape :: forall sh. Shape sh => (sh, sh) -> sh
rangeToShape (u, v) = toElt $ R.rangeToShape (shapeR @sh) (fromElt u, fromElt v)

-- | Convert a shape into a minpoint-maxpoint index
--
shapeToRange :: forall sh. Shape sh => sh -> (sh, sh)
shapeToRange ix =
  let (u, v) = R.shapeToRange (shapeR @sh) (fromElt ix)
   in (toElt u, toElt v)

-- | Convert a shape to a list of dimensions
--
shapeToList :: forall sh. Shape sh => sh -> [Int]
shapeToList = R.shapeToList (shapeR @sh) . fromElt

-- | Convert a list of dimensions into a shape. If the list does not
-- contain exactly the number of elements as specified by the type of the
-- shape: error.
--
listToShape :: forall sh. Shape sh => [Int] -> sh
listToShape = toElt . R.listToShape (shapeR @sh)

-- | Attempt to convert a list of dimensions into a shape
--
listToShape' :: forall sh. Shape sh => [Int] -> Maybe sh
listToShape' = fmap toElt . R.listToShape' (shapeR @sh)

-- | Nicely format a shape as a string
--
showShape :: Shape sh => sh -> String
showShape = foldr (\sh str -> str ++ " :. " ++ show sh) "Z" . shapeToList

-- | Project the shape of a slice from the full shape
--
sliceShape
    :: (Shape sl, Shape dim)
    => R.SliceIndex slix (EltR sl) co (EltR dim)
    -> dim
    -> sl
sliceShape slx = toElt . R.sliceShape slx . fromElt

-- | Project the full shape from a slice
--
sliceDomain
    :: (Elt slix, Shape sl, Shape dim)
    => R.SliceIndex (EltR slix) (EltR sl) co (EltR dim)
    -> slix
    -> sl
    -> dim
sliceDomain slx slix sl = toElt $ R.sliceDomain slx (fromElt slix) (fromElt sl)

-- | Enumerate all slices within a given bound. The innermost dimension
-- changes most rapidly.
--
-- Example:
--
-- > let slix = sliceIndex @(Z :. Int :. Int :. All)
-- >     sh   = Z :. 2 :. 3 :. 1 :: DIM3
-- > in
-- > enumSlices slix sh :: [ Z :. Int :. Int :. All ]
--
enumSlices :: forall slix co sl dim. (Elt slix, Elt dim)
           => R.SliceIndex (EltR slix) sl co (EltR dim)
           -> dim    -- Bounds
           -> [slix] -- All slices within bounds.
enumSlices slix = map toElt . R.enumSlices slix . fromElt

-- | Shapes and indices of multi-dimensional arrays
--
class (Elt sh, Elt (Any sh), FullShape sh ~ sh, CoSliceShape sh ~ sh, SliceShape sh ~ Z)
       => Shape sh where

  -- | Reified type witness for shapes
  shapeR :: R.ShapeR (EltR sh)

  -- | The slice index for slice specifier 'Any sh'
  sliceAnyIndex  :: R.SliceIndex (EltR (Any sh)) (EltR sh) () (EltR sh)

  -- | The slice index for specifying a slice with only the Z component projected
  sliceNoneIndex :: R.SliceIndex (EltR sh) () (EltR sh) (EltR sh)


-- | Slices, aka generalised indices, as /n/-tuples and mappings of slice
-- indices to slices, co-slices, and slice dimensions
--
class (Elt sl, Shape (SliceShape sl), Shape (CoSliceShape sl), Shape (FullShape sl))
       => Slice sl where
  type SliceShape   sl :: Type    -- the projected slice
  type CoSliceShape sl :: Type    -- the complement of the slice
  type FullShape    sl :: Type    -- the combined dimension
  sliceIndex :: R.SliceIndex (EltR sl)
                             (EltR (SliceShape   sl))
                             (EltR (CoSliceShape sl))
                             (EltR (FullShape    sl))

-- | Generalised array division, like above but use for splitting an array
-- into many subarrays, as opposed to extracting a single subarray.
--
class (Slice (DivisionSlice sl)) => Division sl where
  type DivisionSlice sl :: Type   -- the slice
  slicesIndex :: slix ~ DivisionSlice sl
              => R.SliceIndex (EltR slix)
                              (EltR (SliceShape   slix))
                              (EltR (CoSliceShape slix))
                              (EltR (FullShape    slix))

instance (Elt t, Elt h) => Elt (t :. h) where
  type EltR (t :. h) = (EltR t, EltR h)
  eltR           = TupRpair (eltR @t) (eltR @h)
  tagsR          = [TagRpair t h | t <- tagsR @t, h <- tagsR @h]
  fromElt (t:.h) = (fromElt t, fromElt h)
  toElt (t, h)   = toElt t :. toElt h

instance Elt (Any Z)
instance Shape sh => Elt (Any (sh :. Int)) where
  type EltR (Any (sh :. Int)) = (EltR (Any sh), ())
  eltR      = TupRpair (eltR @(Any sh)) TupRunit
  tagsR     = [TagRpair t TagRunit | t <- tagsR @(Any sh)]
  fromElt _ = (fromElt (Any :: Any sh), ())
  toElt _   = Any

instance Shape Z where
  shapeR         = R.ShapeRz
  sliceAnyIndex  = R.SliceNil
  sliceNoneIndex = R.SliceNil

-- Note that the constraint 'i ~ Int' allows the compiler to infer that
-- the right argument of ':.' should be an Int.
-- The compiler can now infer that this instance is used, before knowing
-- the type of 'i'.
--
instance (Shape sh, i ~ Int) => Shape (sh:.i) where
  shapeR         = R.ShapeRsnoc (shapeR @sh)
  sliceAnyIndex  = R.SliceAll   (sliceAnyIndex  @sh)
  sliceNoneIndex = R.SliceFixed (sliceNoneIndex @sh)

instance Slice Z where
  type SliceShape   Z = Z
  type CoSliceShape Z = Z
  type FullShape    Z = Z
  sliceIndex = R.SliceNil

instance Slice sl => Slice (sl:.All) where
  type SliceShape   (sl:.All) = SliceShape   sl :. Int
  type CoSliceShape (sl:.All) = CoSliceShape sl
  type FullShape    (sl:.All) = FullShape    sl :. Int
  sliceIndex = R.SliceAll (sliceIndex @sl)

instance Slice sl => Slice (sl:.Int) where
  type SliceShape   (sl:.Int) = SliceShape   sl
  type CoSliceShape (sl:.Int) = CoSliceShape sl :. Int
  type FullShape    (sl:.Int) = FullShape    sl :. Int
  sliceIndex = R.SliceFixed (sliceIndex @sl)

instance Shape sh => Slice (Any sh) where
  type SliceShape   (Any sh) = sh
  type CoSliceShape (Any sh) = Z
  type FullShape    (Any sh) = sh
  sliceIndex = sliceAnyIndex @sh

instance Division Z where
  type DivisionSlice Z = Z
  slicesIndex = R.SliceNil

instance Division sl => Division (sl:.All) where
  type DivisionSlice (sl:.All) = DivisionSlice sl :. All
  slicesIndex = R.SliceAll (slicesIndex @sl)

instance Division sl => Division (sl:.Split) where
  type DivisionSlice (sl:.Split) = DivisionSlice sl :. Int
  slicesIndex = R.SliceFixed (slicesIndex @sl)

instance Shape sh => Division (Any sh) where
  type DivisionSlice (Any sh) = Any sh
  slicesIndex = sliceAnyIndex @sh

instance (Shape sh, Slice sh) => Division (Divide sh) where
  type DivisionSlice (Divide sh) = sh
  slicesIndex = sliceNoneIndex @sh

