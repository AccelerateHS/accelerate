{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
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
  Array(..), ArrayR(..), arraysRarray, arraysRtuple2, arrayRshape, arrayRtype, rnfArray, rnfShape,
  ArraysR, TupleType, Scalar, Vector, Matrix, fromList, toList, Segments, shape, reshape, concatVectors,
  showArrayR, showArraysR, fromFunction, fromFunctionM, reduceRank, allocateArray,

  -- * Array shapes, indices, and slices
  ShapeR(..), Slice(..), SliceIndex(..),
  DIM0, DIM1, DIM2, dim0, dim1, dim2, (!), (!!),

  -- * Shape functions
  rank, size, empty, ignore, intersect, union, toIndex, fromIndex, iter, iter1,
  rangeToShape, shapeToRange, shapeToList, listToShape, listToShape', shapeType, shapeEq,

  -- * Slice shape functions
  sliceShape, sliceShapeR, sliceDomainR, enumSlices,

  -- * Vec representation & utilities
  VecR(..), vecRvector, vecRtuple, vecPack, vecUnpack,

  -- * Stencils
  StencilR(..), stencilElt, stencilShape, stencilType, stencilArrayR, stencilHalo,

  -- * Show
  showShape, showElement, showArray, showArray',

) where

-- friends
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Data

-- standard library
import GHC.Base                                         ( quotInt, remInt, Int(..), Int#, (-#) )
import GHC.TypeNats
import Data.Primitive.ByteArray
import Data.Primitive.Types
import Prelude                                          hiding ((!!))
import Data.List                                        ( intercalate )
import Text.Show                                        ( showListWith )
import System.IO.Unsafe                                 ( unsafePerformIO )
import qualified Data.Vector.Unboxed                    as U
import Control.Monad.ST

-- |Array data type, where the type arguments regard the representation types of the shape and elements.
data Array sh e where
  Array :: sh                         -- extent of dimensions = shape
        -> ArrayData e                -- array payload
        -> Array sh e

{-# INLINE shape #-}
shape :: Array sh e -> sh
shape (Array sh _) = sh

{-# INLINE reshape #-}
reshape :: ShapeR sh -> sh -> ShapeR sh' -> Array sh' e -> Array sh e
reshape shr sh shr' (Array sh' adata)
  = $boundsCheck "reshape" "shape mismatch" (size shr sh == size shr' sh')
  $ Array sh adata

{-# INLINE [1] (!) #-}
(!) :: (ArrayR (Array sh e), Array sh e) -> sh -> e
(!) (ArrayR shr tp, Array sh adata) ix = unsafeIndexArrayData tp adata $ toIndex shr sh ix

{-# INLINE [1] (!!) #-}
(!!) :: (TupleType e, Array sh e) -> Int -> e
(tp, Array _ adata) !! i = unsafeIndexArrayData tp adata i

-- | Create an array from its representation function, applied at each index of
-- the array.
--
{-# INLINEABLE fromFunction #-}
fromFunction :: ArrayR (Array sh e) -> sh -> (sh -> e) -> Array sh e
fromFunction repr sh f = unsafePerformIO $! fromFunctionM repr sh (return . f)

-- | Create an array using a monadic function applied at each index.
--
-- @since 1.2.0.0
--
{-# INLINEABLE fromFunctionM #-}
fromFunctionM :: ArrayR (Array sh e) -> sh -> (sh -> IO e) -> IO (Array sh e)
fromFunctionM (ArrayR shr tp) sh f = do
  let !n = size shr sh
  arr <- newArrayData tp n
  --
  let write !i
        | i >= n    = return ()
        | otherwise = do
            v <- f (fromIndex shr sh i)
            unsafeWriteArrayData tp arr i v
            write (i+1)
  --
  write 0
  return $! arr `seq` Array sh arr


{-# INLINEABLE concatVectors #-}
concatVectors :: forall e. TupleType e -> [Vector e] -> Vector e
concatVectors tp vs = adata `seq` Array ((), len) adata
  where
    offsets     = scanl (+) 0 (map (size dim1 . shape) vs)
    len         = last offsets
    (adata, _)  = runArrayData @e $ do
              arr <- newArrayData tp len
              sequence_ [ unsafeWriteArrayData tp arr (i + k) (unsafeIndexArrayData tp ad i)
                        | (Array ((), n) ad, k) <- vs `zip` offsets
                        , i <- [0 .. n - 1] ]
              return (arr, undefined)

-- | Creates a new, uninitialized Accelerate array.
--
{-# INLINEABLE allocateArray #-}
allocateArray :: ArrayR (Array sh e) -> sh -> IO (Array sh e)
allocateArray (ArrayR shr tp) sh = do
  adata  <- newArrayData tp (size shr sh)
  return $! Array sh adata

{-# INLINEABLE fromList #-}
fromList :: forall sh e. ArrayR (Array sh e) -> sh -> [e] -> Array sh e
fromList (ArrayR shr tp) sh xs = adata `seq` Array sh adata
  where
    -- Assume the array is in dense row-major order. This is safe because
    -- otherwise backends would not be able to directly memcpy.
    --
    !n    = size shr sh
    (adata, _) = runArrayData @e $ do
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

arrayRshape :: ArrayR (Array sh e) -> ShapeR sh
arrayRshape (ArrayR sh _) = sh

arrayRtype :: ArrayR (Array sh e) -> TupleType e
arrayRtype (ArrayR _ tp) = tp

arraysRarray :: ShapeR sh -> TupleType e -> ArraysR (Array sh e)
arraysRarray shr tp = TupRsingle $ ArrayR shr tp

arraysRtuple2 :: ArrayR a -> ArrayR b -> ArraysR (((), a), b)
arraysRtuple2 a b = TupRpair TupRunit (TupRsingle a) `TupRpair` TupRsingle b

showArrayR :: ArrayR a -> ShowS
showArrayR (ArrayR shr tp) = showString "Array DIM" . shows (rank shr) . showString " " . showType tp

showArraysR :: ArraysR tp -> ShowS
showArraysR TupRunit = showString "()"
showArraysR (TupRsingle repr) = showArrayR repr
showArraysR (TupRpair t1 t2) = showString "(" . showArraysR t1 . showString ", " . showArraysR t2 . showString ")"

type Scalar = Array DIM0
type Vector = Array DIM1
type Matrix = Array DIM2

-- | Segment descriptor (vector of segment lengths).
--
-- To represent nested one-dimensional arrays, we use a flat array of data
-- values in conjunction with a /segment descriptor/, which stores the lengths
-- of the subarrays.
--
type Segments = Vector

-- |Index representation
--
type DIM0 = ()
type DIM1 = ((), Int)
type DIM2 = (((), Int), Int)

dim0 :: ShapeR DIM0
dim0 = ShapeRz

dim1 :: ShapeR DIM1
dim1 = ShapeRsnoc dim0

dim2 :: ShapeR DIM2
dim2 = ShapeRsnoc dim1

-- |Index representations (which are nested pairs)
--

data ShapeR sh where
  ShapeRz    :: ShapeR ()
  ShapeRsnoc :: ShapeR sh -> ShapeR (sh, Int)

rank :: ShapeR sh -> Int
rank ShapeRz          = 0
rank (ShapeRsnoc shr) = rank shr + 1

size :: ShapeR sh -> sh -> Int
size ShapeRz () = 1
size (ShapeRsnoc shr) (sh, sz)
  | sz <= 0   = 0
  | otherwise = size shr sh * sz

empty :: ShapeR sh -> sh
empty ShapeRz          = ()
empty (ShapeRsnoc shr) = (empty shr, 0)

ignore :: ShapeR sh -> sh
ignore ShapeRz          = ()
ignore (ShapeRsnoc shr) = (ignore shr, -1)

shapeZip :: (Int -> Int -> Int) -> ShapeR sh -> sh -> sh -> sh
shapeZip _ ShapeRz          ()      ()      = ()
shapeZip f (ShapeRsnoc shr) (as, a) (bs, b) = (shapeZip f shr as bs, f a b)

intersect, union :: ShapeR sh -> sh -> sh -> sh
intersect = shapeZip min
union     = shapeZip max

toIndex :: ShapeR sh -> sh -> sh -> Int
toIndex ShapeRz () () = 0
toIndex (ShapeRsnoc shr) (sh, sz) (ix, i)
  = $indexCheck "toIndex" i sz
  $ toIndex shr sh ix * sz + i

fromIndex :: ShapeR sh -> sh -> Int -> sh
fromIndex ShapeRz () _ = ()
fromIndex (ShapeRsnoc shr) (sh, sz) i
  = (fromIndex shr sh (i `quotInt` sz), r)
  -- If we assume that the index is in range, there is no point in computing
  -- the remainder for the highest dimension since i < sz must hold.
  --
  where
    r = case shr of -- Check if rank of shr is 0
      ShapeRz -> $indexCheck "fromIndex" i sz i
      _       -> i `remInt` sz

shapeEq :: ShapeR sh -> sh -> sh -> Bool
shapeEq ShapeRz          ()      ()        = True
shapeEq (ShapeRsnoc shr) (sh, i) (sh', i') = i == i' && shapeEq shr sh sh'

-- iterate through the entire shape, applying the function in the
-- second argument; third argument combines results and fourth is an
-- initial value that is combined with the results; the index space
-- is traversed in row-major order
iter :: ShapeR sh -> sh -> (sh -> a) -> (a -> a -> a) -> a -> a
iter ShapeRz          ()       f _ _ = f ()
iter (ShapeRsnoc shr) (sh, sz) f c r = iter shr sh (\ix -> iter' (ix,0)) c r
  where
    iter' (ix,i) | i >= sz   = r
                 | otherwise = f (ix,i) `c` iter' (ix,i+1)

-- variant of 'iter' without an initial value
iter1 :: ShapeR sh -> sh -> (sh -> a) -> (a -> a -> a) -> a
iter1 ShapeRz          ()       f _ = f ()
iter1 (ShapeRsnoc _  ) (_,  0)  _ _ = $boundsError "iter1" "empty iteration space"
iter1 (ShapeRsnoc shr) (sh, sz) f c = iter1 shr sh (\ix -> iter1' (ix,0)) c
  where
    iter1' (ix,i) | i == sz-1 = f (ix,i)
                  | otherwise = f (ix,i) `c` iter1' (ix,i+1)

-- Operations to facilitate conversion with IArray

-- convert a minpoint-maxpoint index into a shape
rangeToShape :: ShapeR sh -> (sh, sh) -> sh
rangeToShape ShapeRz          ((), ())                 = ()
rangeToShape (ShapeRsnoc shr) ((sh1, sz1), (sh2, sz2)) = (rangeToShape shr (sh1, sh2), sz2 - sz1 + 1)

-- the converse
shapeToRange :: ShapeR sh -> sh -> (sh, sh)
shapeToRange ShapeRz          ()       = ((), ())
shapeToRange (ShapeRsnoc shr) (sh, sz) = let (low, high) = shapeToRange shr sh in ((low, 0), (high, sz - 1))

-- Other conversions

-- Convert a shape into its list of dimensions
shapeToList :: ShapeR sh -> sh -> [Int]
shapeToList ShapeRz          ()      = []
shapeToList (ShapeRsnoc shr) (sh,sz) = sz : shapeToList shr sh

-- Convert a list of dimensions into a shape
listToShape :: ShapeR sh -> [Int] -> sh
listToShape shr ds = case listToShape' shr ds of
  Just sh -> sh
  Nothing -> $internalError "listToShape" "unable to convert list to a shape at the specified type"

-- Attempt to convert a list of dimensions into a shape
listToShape' :: ShapeR sh -> [Int] -> Maybe sh
listToShape' ShapeRz          []     = Just ()
listToShape' (ShapeRsnoc shr) (x:xs) = (, x) <$> listToShape' shr xs
listToShape' _ _ = Nothing

shapeType :: ShapeR sh -> TupleType sh
shapeType ShapeRz          = TupRunit
shapeType (ShapeRsnoc shr) = shapeType shr `TupRpair` (TupRsingle $ SingleScalarType $ NumSingleType $ IntegralNumType TypeInt)

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
  SliceAll   :: SliceIndex ix slice co dim -> SliceIndex (ix, ()) (slice, Int) co       (dim, Int)
  SliceFixed :: SliceIndex ix slice co dim -> SliceIndex (ix, Int) slice      (co, Int) (dim, Int)

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
sliceShapeR (SliceAll sl)   = ShapeRsnoc $ sliceShapeR sl
sliceShapeR (SliceFixed sl) = sliceShapeR sl

sliceDomainR :: SliceIndex slix sl co dim -> ShapeR dim
sliceDomainR SliceNil        = ShapeRz
sliceDomainR (SliceAll sl)   = ShapeRsnoc $ sliceDomainR sl
sliceDomainR (SliceFixed sl) = ShapeRsnoc $ sliceDomainR sl

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


-- | GADT reifying the 'Stencil' class
--
data StencilR sh e pat where
  StencilRunit3 :: TupleType e -> StencilR DIM1 e (Tup3 e e e)
  StencilRunit5 :: TupleType e -> StencilR DIM1 e (Tup5 e e e e e)
  StencilRunit7 :: TupleType e -> StencilR DIM1 e (Tup7 e e e e e e e)
  StencilRunit9 :: TupleType e -> StencilR DIM1 e (Tup9 e e e e e e e e e)

  StencilRtup3  :: StencilR sh e pat1
                -> StencilR sh e pat2
                -> StencilR sh e pat3
                -> StencilR (sh, Int) e (Tup3 pat1 pat2 pat3)

  StencilRtup5  :: StencilR sh e pat1
                -> StencilR sh e pat2
                -> StencilR sh e pat3
                -> StencilR sh e pat4
                -> StencilR sh e pat5
                -> StencilR (sh, Int) e (Tup5 pat1 pat2 pat3 pat4 pat5)

  StencilRtup7  :: StencilR sh e pat1
                -> StencilR sh e pat2
                -> StencilR sh e pat3
                -> StencilR sh e pat4
                -> StencilR sh e pat5
                -> StencilR sh e pat6
                -> StencilR sh e pat7
                -> StencilR (sh, Int) e (Tup7 pat1 pat2 pat3 pat4 pat5 pat6 pat7)

  StencilRtup9  :: StencilR sh e pat1
                -> StencilR sh e pat2
                -> StencilR sh e pat3
                -> StencilR sh e pat4
                -> StencilR sh e pat5
                -> StencilR sh e pat6
                -> StencilR sh e pat7
                -> StencilR sh e pat8
                -> StencilR sh e pat9
                -> StencilR (sh, Int) e (Tup9 pat1 pat2 pat3 pat4 pat5 pat6 pat7 pat8 pat9)

stencilElt :: StencilR sh e pat -> TupleType e
stencilElt (StencilRunit3 tp) = tp
stencilElt (StencilRunit5 tp) = tp
stencilElt (StencilRunit7 tp) = tp
stencilElt (StencilRunit9 tp) = tp
stencilElt (StencilRtup3 sr _ _) = stencilElt sr
stencilElt (StencilRtup5 sr _ _ _ _) = stencilElt sr
stencilElt (StencilRtup7 sr _ _ _ _ _ _) = stencilElt sr
stencilElt (StencilRtup9 sr _ _ _ _ _ _ _ _) = stencilElt sr

stencilShape :: StencilR sh e pat -> ShapeR sh
stencilShape (StencilRunit3 _) = ShapeRsnoc ShapeRz
stencilShape (StencilRunit5 _) = ShapeRsnoc ShapeRz
stencilShape (StencilRunit7 _) = ShapeRsnoc ShapeRz
stencilShape (StencilRunit9 _) = ShapeRsnoc ShapeRz
stencilShape (StencilRtup3 sr _ _) = ShapeRsnoc $ stencilShape sr
stencilShape (StencilRtup5 sr _ _ _ _) = ShapeRsnoc $ stencilShape sr
stencilShape (StencilRtup7 sr _ _ _ _ _ _) = ShapeRsnoc $ stencilShape sr
stencilShape (StencilRtup9 sr _ _ _ _ _ _ _ _) = ShapeRsnoc $ stencilShape sr

stencilType :: StencilR sh e pat -> TupleType pat
stencilType (StencilRunit3 tp)                        = tupR3 tp tp tp
stencilType (StencilRunit5 tp)                        = tupR5 tp tp tp tp tp
stencilType (StencilRunit7 tp)                        = tupR7 tp tp tp tp tp tp tp
stencilType (StencilRunit9 tp)                        = tupR9 tp tp tp tp tp tp tp tp tp
stencilType (StencilRtup3 s1 s2 s3)                   = tupR3 (stencilType s1) (stencilType s2) (stencilType s3)
stencilType (StencilRtup5 s1 s2 s3 s4 s5)             = tupR5 (stencilType s1) (stencilType s2) (stencilType s3)
                                                              (stencilType s4) (stencilType s5)
stencilType (StencilRtup7 s1 s2 s3 s4 s5 s6 s7)       = tupR7 (stencilType s1) (stencilType s2) (stencilType s3)
                                                              (stencilType s4) (stencilType s5) (stencilType s6)
                                                              (stencilType s7)
stencilType (StencilRtup9 s1 s2 s3 s4 s5 s6 s7 s8 s9) = tupR9 (stencilType s1) (stencilType s2) (stencilType s3)
                                                              (stencilType s4) (stencilType s5) (stencilType s6)
                                                              (stencilType s7) (stencilType s8) (stencilType s9)

stencilArrayR :: StencilR sh e pat -> ArrayR (Array sh e)
stencilArrayR stencil = ArrayR (stencilShape stencil) (stencilElt stencil)

stencilHalo :: StencilR sh e stencil -> (ShapeR sh, sh)
stencilHalo = go'
  where
    go' :: StencilR sh e stencil -> (ShapeR sh, sh)
    go' StencilRunit3{} = (dim1, ((), 1))
    go' StencilRunit5{} = (dim1, ((), 2))
    go' StencilRunit7{} = (dim1, ((), 3))
    go' StencilRunit9{} = (dim1, ((), 4))
    --
    go' (StencilRtup3 a b c            ) = (ShapeRsnoc shr, cons shr 1 $ foldl1 (union shr) [a', go b, go c])
      where (shr, a') = go' a
    go' (StencilRtup5 a b c d e        ) = (ShapeRsnoc shr, cons shr 2 $ foldl1 (union shr) [a', go b, go c, go d, go e])
      where (shr, a') = go' a
    go' (StencilRtup7 a b c d e f g    ) = (ShapeRsnoc shr, cons shr 3 $ foldl1 (union shr) [a', go b, go c, go d, go e, go f, go g])
      where (shr, a') = go' a
    go' (StencilRtup9 a b c d e f g h i) = (ShapeRsnoc shr, cons shr 4 $ foldl1 (union shr) [a', go b, go c, go d, go e, go f, go g, go h, go i])
      where (shr, a') = go' a

    go :: StencilR sh e stencil -> sh
    go = snd . go'

    cons :: ShapeR sh -> Int -> sh -> (sh, Int)
    cons ShapeRz          ix ()       = ((), ix)
    cons (ShapeRsnoc shr) ix (sh, sz) = (cons shr ix sh, sz)

rnfArray :: ArrayR a -> a -> ()
rnfArray (ArrayR shr tp) (Array sh ad) = rnfShape shr sh `seq` rnfArrayData tp ad

rnfShape :: ShapeR sh -> sh -> ()
rnfShape ShapeRz          ()      = ()
rnfShape (ShapeRsnoc shr) (sh, s) = s `seq` rnfShape shr sh

-- | SIMD Vectors (Vec n t)
--

-- Declares the size of a SIMD vector and the type of its elements.
-- This data type is used to denote the relation between a vector
-- type (Vec n single) with its tuple representation (tuple).
-- Conversions between those types are exposed through vecPack and
-- vecUnpack.
--
data VecR (n :: Nat) single tuple where
  VecRnil  :: SingleType s -> VecR 0       s ()
  VecRsucc :: VecR n s t   -> VecR (n + 1) s (t, s)

vecRvector :: KnownNat n => VecR n s tuple -> VectorType (Vec n s)
vecRvector = uncurry VectorType . go
  where
    go :: VecR n s tuple -> (Int, SingleType s)
    go (VecRnil tp)   = (0,     tp)
    go (VecRsucc vec) = (n + 1, tp)
      where (n, tp) = go vec

vecRtuple :: VecR n s tuple -> TupleType tuple
vecRtuple = snd . go
  where
    go :: VecR n s tuple -> (SingleType s, TupleType tuple)
    go (VecRnil tp)           = (tp, TupRunit)
    go (VecRsucc vec)
      | (tp, tuple) <- go vec = (tp, TupRpair tuple $ TupRsingle $ SingleScalarType tp)

vecPack :: forall n single tuple. KnownNat n => VecR n single tuple -> tuple -> Vec n single
vecPack vecR tuple
  | VectorType n single <- vecRvector vecR
  , PrimDict <- getPrim single = runST $ do
    mba <- newByteArray (n * sizeOf (undefined :: single))
    go (n - 1) vecR tuple mba
    ByteArray ba# <- unsafeFreezeByteArray mba
    return $! Vec ba#
  where
    go :: Prim single => Int -> VecR n' single tuple' -> tuple' -> MutableByteArray s -> ST s ()
    go _ (VecRnil _)  ()      _   = return ()
    go i (VecRsucc r) (xs, x) mba = do
      writeByteArray mba i x
      go (i - 1) r xs mba

vecUnpack :: forall n single tuple. KnownNat n => VecR n single tuple -> Vec n single -> tuple
vecUnpack vecR (Vec ba#)
  | VectorType n single <- vecRvector vecR
  , (I# n#)             <- n
  , PrimDict            <- getPrim single
  = go (n# -# 1#) vecR
  where
    go :: Prim single => Int# -> VecR n' single tuple' -> tuple'
    go _  (VecRnil _)  = ()
    go i# (VecRsucc r) = x `seq` xs `seq` (xs, x)
      where
        xs = go (i# -# 1#) r
        x  = indexByteArray# ba# i#

-- | Nicely format a shape as a string
--
showShape :: ShapeR sh -> sh -> String
showShape shr = foldr (\sh str -> str ++ " :. " ++ show sh) "Z" . shapeToList shr

showElement :: TupleType e -> e -> String
showElement tuple value = showElement' tuple value ""
  where
    showElement' :: TupleType e -> e -> ShowS
    showElement' TupRunit () = showString "()"
    showElement' (TupRpair t1 t2) (e1, e2) = showString "(" . showElement' t1 e1 . showString ", " . showElement' t2 e2 . showString ")"
    showElement' (TupRsingle tp) val = showScalar tp val

    showScalar :: ScalarType e -> e -> ShowS
    showScalar (SingleScalarType t) e = showString $ showSingle t e
    showScalar (VectorScalarType t) e = showString $ showVector t e

    showSingle :: SingleType e -> e -> String
    showSingle (NumSingleType t) e = showNum t e
    showSingle (NonNumSingleType t) e = showNonNum t e

    showNum :: NumType e -> e -> String
    showNum (IntegralNumType t) e = showIntegral t e
    showNum (FloatingNumType t) e = showFloating t e

    showIntegral :: IntegralType e -> e -> String
    showIntegral TypeInt{}    e = show e
    showIntegral TypeInt8{}   e = show e
    showIntegral TypeInt16{}  e = show e
    showIntegral TypeInt32{}  e = show e
    showIntegral TypeInt64{}  e = show e
    showIntegral TypeWord{}   e = show e
    showIntegral TypeWord8{}  e = show e
    showIntegral TypeWord16{} e = show e
    showIntegral TypeWord32{} e = show e
    showIntegral TypeWord64{} e = show e

    showFloating :: FloatingType e -> e -> String
    showFloating TypeHalf{}   e = show e
    showFloating TypeFloat{}  e = show e
    showFloating TypeDouble{} e = show e

    showNonNum :: NonNumType e -> e -> String
    showNonNum TypeChar e = show e
    showNonNum TypeBool e = show e

    showVector :: VectorType (Vec n a) -> Vec n a -> String
    showVector (VectorType _ single) vec
      | PrimDict <- getPrim single = "<" ++ intercalate ", " (showSingle single <$> vecToArray vec) ++ ">"

showArray :: ArrayR (Array sh e) -> Array sh e -> String
showArray repr@(ArrayR _ tp) = showArray' (showString . showElement tp) repr

{-# INLINE showArray' #-}
showArray' :: (e -> ShowS) -> ArrayR (Array sh e) -> Array sh e -> String
showArray' f repr@(ArrayR shr tp) arr@(Array sh _) = case shr of
  ShapeRz                         -> "Scalar Z "                       ++ list
  ShapeRsnoc ShapeRz              -> "Vector (" ++ shapeString ++ ") " ++ list
  ShapeRsnoc (ShapeRsnoc ShapeRz) -> "Matrix (" ++ shapeString ++ ") " ++ showMatrix f tp arr
  _                               -> "Array ("  ++ shapeString ++ ") " ++ list
  where
    shapeString = showShape shr sh
    list = showListWith f (toList repr arr) ""

-- TODO:
-- Make special formatting optional? It is more difficult to copy/paste the
-- result, for example. Also it does not look good if the matrix row does
-- not fit on a single line.
--
showMatrix :: (e -> ShowS) -> TupleType e -> Array DIM2 e -> String
showMatrix f tp arr@(Array sh _)
  | rows * cols == 0 = "[]"
  | otherwise        = "\n  [" ++ ppMat 0 0
    where
      (((), rows), cols) = sh
      lengths            = U.generate (rows*cols) (\i -> length (f ((tp, arr) !! i) ""))
      widths             = U.generate cols (\c -> U.maximum (U.generate rows (\r -> lengths U.! (r*cols+c))))
      --
      ppMat :: Int -> Int -> String
      ppMat !r !c | c >= cols = ppMat (r+1) 0
      ppMat !r !c             =
        let
            !i    = r*cols+c
            !l    = lengths U.! i
            !w    = widths  U.! c
            !pad  = 1
            cell  = replicate (w-l+pad) ' ' ++ f ((tp, arr) !! i) ""
            --
            before
              | r > 0 && c == 0 = "\n   "
              | otherwise       = ""
            --
            after
              | r >= rows-1 && c >= cols-1 = "]"
              | otherwise                  = ',' : ppMat r (c+1)
        in
        before ++ cell ++ after

reduceRank :: ArrayR (Array (sh, Int) e) -> ArrayR (Array sh e)
reduceRank (ArrayR (ShapeRsnoc shr) tp) = ArrayR shr tp

