{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Data.Array.Accelerate.Language
-- Copyright   : [2008..2016] Manuel M T Chakravarty, Gabriele Keller
--               [2009..2016] Trevor L. McDonell
--               [2014..2014] Frederik M. Madsen
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- We use the dictionary view of overloaded operations (such as arithmetic and
-- bit manipulation) to reify such expressions.  With non-overloaded
-- operations (such as, the logical connectives) and partially overloaded
-- operations (such as comparisons), we use the standard operator names with a
-- \'*\' attached.  We keep the standard alphanumeric names as they can be
-- easily qualified.
--

module Data.Array.Accelerate.Language (

  -- * Array and scalar expressions
  Acc, Exp,                                 -- re-exporting from 'Smart'

  -- * Scalar introduction
  constant,                                 -- re-exporting from 'Smart'

  -- * Array construction
  use, unit, replicate, generate,

  -- * Shape manipulation
  reshape,

  -- * Extraction of sub-arrays
  slice,

  -- * Map-like functions
  map, zipWith,

  -- -- * Sequence collection
  -- collect,

  -- -- * Sequence producers
  -- streamIn, toSeq,

  -- -- * Sequence transducers
  -- mapSeq, zipWithSeq, scanSeq,

  -- -- * Sequence consumers
  -- foldSeq, foldSeqFlatten,

  -- * Reductions
  fold, fold1, foldSeg, fold1Seg,

  -- * Scan functions
  scanl, scanl', scanl1, scanr, scanr', scanr1,

  -- * Permutations
  permute, backpermute,

  -- * Stencil operations
  stencil, stencil2,

  -- ** Stencil specification
  Boundary(..), Stencil,

  -- ** Common stencil types
  Stencil3, Stencil5, Stencil7, Stencil9,
  Stencil3x3, Stencil5x3, Stencil3x5, Stencil5x5,
  Stencil3x3x3, Stencil5x3x3, Stencil3x5x3, Stencil3x3x5, Stencil5x5x3, Stencil5x3x5,
  Stencil3x5x5, Stencil5x5x5,

  -- * Foreign functions
  foreignAcc,
  foreignExp,

  -- * Pipelining
  (>->),

  -- * Index construction and destruction
  indexHead, indexTail, toIndex, fromIndex,
  intersect, union,

  -- * Flow-control
  acond, awhile,
  cond,  while,

  -- * Array operations with a scalar result
  (!), (!!), shape, size, shapeSize,

  -- * Numeric functions
  subtract, even, odd, gcd, lcm, (^), (^^),

  -- * Conversions
  ord, chr, boolToInt, bitcast,

  -- * Constants
  ignore

) where

-- friends
import Data.Array.Accelerate.Array.Sugar                hiding ((!), ignore, shape, size, toIndex, fromIndex, intersect, union)
import Data.Array.Accelerate.Classes
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type
import qualified Data.Array.Accelerate.Array.Sugar      as Sugar

-- standard libraries
import Prelude                                          ( ($), (.) )


-- Array introduction
-- ------------------

-- | Array inlet: makes an array available for processing using the Accelerate
-- language.
--
-- Depending upon the backend used to execute array computations, this may
-- trigger (asynchronous) data transfer.
--
use :: Arrays arrays => arrays -> Acc arrays
use = Acc . Use

-- | Scalar inlet: injects a scalar (or a tuple of scalars) into a singleton
-- array for use in the Accelerate language.
--
unit :: Elt e => Exp e -> Acc (Scalar e)
unit = Acc . Unit

-- | Replicate an array across one or more dimensions as specified by the
-- /generalised/ array index provided as the first argument.
--
-- For example, given the following vector:
--
-- >>> let vec = fromList (Z:.10) [0..]
-- Vector (Z :. 10) [0,1,2,3,4,5,6,7,8,9]
--
-- We can replicate these elements to form a two-dimensional array either by
-- replicating those elements as new rows:
--
-- >>> replicate (lift (Z :. 4 :. All)) (use vec)
-- Matrix (Z :. 4 :. 10)
--   [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
--     0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
--     0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
--     0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
--
-- ... or as columns:
--
-- >>> replicate (lift (Z :. All :. 4)) (use vec)
-- Matrix (Z :. 10 :. 4)
--   [ 0, 0, 0, 0,
--     1, 1, 1, 1,
--     2, 2, 2, 2,
--     3, 3, 3, 3,
--     4, 4, 4, 4,
--     5, 5, 5, 5,
--     6, 6, 6, 6,
--     7, 7, 7, 7,
--     8, 8, 8, 8,
--     9, 9, 9, 9]
--
-- Replication along more than one dimension is also possible. Here we replicate
-- twice across the first dimension and three times across the third dimension:
--
-- >>> replicate (lift (Z :. 2 :. All :. 3)) (use vec)
-- Array (Z :. 2 :. 10 :. 3) [0,0,0,1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,7,7,7,8,8,8,9,9,9,0,0,0,1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,7,7,7,8,8,8,9,9,9]
--
replicate :: (Slice slix, Elt e)
          => Exp slix
          -> Acc (Array (SliceShape slix) e)
          -> Acc (Array (FullShape  slix) e)
replicate = Acc $$ Replicate

-- | Construct a new array by applying a function to each index.
--
-- For example, the following will generate a one-dimensional array
-- (`Vector`) of three floating point numbers:
--
-- >>> generate (index1 3) (\_ -> 1.2)
-- Vector (Z :. 3) [1.2,1.2,1.2]
--
-- Or equivalently:
--
-- >>> fill (constant (Z :. 3)) 1.2
-- Vector (Z :. 3) [1.2,1.2,1.2]
--
-- The following will create a vector with the elements @[1..10]@:
--
-- >>> generate (index1 10) (\ix -> unindex1 ix + 1)
-- Vector (Z :. 10) [1,2,3,4,5,6,7,8,9,10]
--
-- [/NOTE:/]
--
-- Using 'generate', it is possible to introduce nested data parallelism, which
-- will cause the program to fail.
--
-- If the index given by the scalar function is then used to dispatch further
-- parallel work, whose result is returned into 'Exp' terms by array indexing
-- operations such as (`!`) or `the`, the program will fail with the error:
-- '.\/Data\/Array\/Accelerate\/Trafo\/Sharing.hs:447 (convertSharingExp): inconsistent valuation \@ shared \'Exp\' tree ...'.
--
generate :: (Shape ix, Elt a)
         => Exp ix
         -> (Exp ix -> Exp a)
         -> Acc (Array ix a)
generate = Acc $$ Generate

-- Shape manipulation
-- ------------------

-- | Change the shape of an array without altering its contents. The 'size' of
-- the source and result arrays must be identical.
--
-- > precondition: size ix == size ix'
--
reshape :: (Shape ix, Shape ix', Elt e)
        => Exp ix
        -> Acc (Array ix' e)
        -> Acc (Array ix e)
reshape = Acc $$ Reshape

-- Extraction of sub-arrays
-- ------------------------

-- | Index an array with a /generalised/ array index, supplied as the
-- second argument. The result is a new array (possibly a singleton)
-- containing the selected dimensions (`All`s) in their entirety.
--
-- This can be used to /cut out/ entire dimensions.  The opposite of
-- `replicate`.  For example, if 'mat' is a two dimensional array, the
-- following will select a specific row and yield a one dimensional
-- result:
--
-- >>> let mat = fromList (Z:.5:.10) [0..]
-- >>> mat
-- Matrix (Z :. 5 :. 10)
--   [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
--     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
--     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
--     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
--     40, 41, 42, 43, 44, 45, 46, 47, 48, 49]
--
-- >>> slice (use mat) (lift (Z :. 2 :. All))
-- Vector (Z :. 10) [20,21,22,23,24,25,26,27,28,29]
--
-- A fully specified index (with no `All`s) returns a single element (zero
-- dimensional array).
--
-- >>> slice (use mat) (lift (Z :. 4 :. 2))
-- Scalar Z [42]
--
slice :: (Slice slix, Elt e)
      => Acc (Array (FullShape slix) e)
      -> Exp slix
      -> Acc (Array (SliceShape slix) e)
slice = Acc $$ Slice

-- Map-like functions
-- ------------------

-- | Apply the given function element-wise to the given array.
--
map :: (Shape ix, Elt a, Elt b)
    => (Exp a -> Exp b)
    -> Acc (Array ix a)
    -> Acc (Array ix b)
map = Acc $$ Map

-- | Apply the given binary function element-wise to the two arrays.  The extent of the resulting
-- array is the intersection of the extents of the two source arrays.
--
zipWith :: (Shape ix, Elt a, Elt b, Elt c)
        => (Exp a -> Exp b -> Exp c)
        -> Acc (Array ix a)
        -> Acc (Array ix b)
        -> Acc (Array ix c)
zipWith = Acc $$$ ZipWith

-- Reductions
-- ----------

-- | Reduction of the innermost dimension of an array of arbitrary rank.  The
-- first argument needs to be an /associative/ function to enable an efficient
-- parallel implementation. The initial element does not need to be an identity
-- element of the combination function.
--
-- >>> let mat = fromList (Z:.5:.10) [0..]
-- >>> mat
-- Matrix (Z :. 5 :. 10)
--   [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
--     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
--     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
--     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
--     40, 41, 42, 43, 44, 45, 46, 47, 48, 49]
--
-- >>> fold (+) 42 (use mat)
-- Vector (Z :. 5) [87,187,287,387,487]
--
-- Reductions with non-commutative operators are supported. For example, the
-- following computes the maximum segment sum problem along each innermost
-- dimension of the array.
--
-- <https://en.wikipedia.org/wiki/Maximum_subarray_problem>
--
-- > maximumSegmentSum
-- >     :: forall sh e. (Shape sh, Num e, Ord e)
-- >     => Acc (Array (sh :. Int) e)
-- >     -> Acc (Array sh e)
-- > maximumSegmentSum
-- >   = map (\v -> let (x,_,_,_) = unlift v :: (Exp e, Exp e, Exp e, Exp e) in x)
-- >   . fold1 f
-- >   . map g
-- >   where
-- >     f :: (Num a, Ord a) => Exp (a,a,a,a) -> Exp (a,a,a,a) -> Exp (a,a,a,a)
-- >     f x y =
-- >       let (mssx, misx, mcsx, tsx) = unlift x
-- >           (mssy, misy, mcsy, tsy) = unlift y
-- >       in
-- >       lift ( mssx `max` (mssy `max` (mcsx+misy))
-- >            , misx `max` (tsx+misy)
-- >            , mcsy `max` (mcsx+tsy)
-- >            , tsx+tsy
-- >            )
-- >
-- >     g :: (Num a, Ord a) => Exp a -> Exp (a,a,a,a)
-- >     g x = let y = max x 0
-- >           in  lift (y,y,y,x)
--
-- >>> let vec = fromList (Z:.10) [-2,1,-3,4,-1,2,1,-5,4,0]
-- >>> maximumSegmentSum (use vec)
-- Scalar Z [6]
--
-- See also 'Data.Array.Accelerate.Data.Fold.Fold', which can be a useful way to
-- compute multiple results from a single reduction.
--
fold :: (Shape ix, Elt a)
     => (Exp a -> Exp a -> Exp a)
     -> Exp a
     -> Acc (Array (ix:.Int) a)
     -> Acc (Array ix a)
fold = Acc $$$ Fold

-- | Variant of 'fold' that requires the reduced array to be non-empty and
-- doesn't need an default value.  The first argument needs to be an
-- /associative/ function to enable an efficient parallel implementation. The
-- initial element does not need to be an identity element.
--
fold1 :: (Shape ix, Elt a)
      => (Exp a -> Exp a -> Exp a)
      -> Acc (Array (ix:.Int) a)
      -> Acc (Array ix a)
fold1 = Acc $$ Fold1

-- | Segmented reduction along the innermost dimension of an array. The segment
-- descriptor specifies the lengths of the logical sub-arrays, each of which is
-- reduced independently. The innermost dimension must contain at least as many
-- elements as required by the segment descriptor (sum thereof).
--
-- >>> let seg = fromList (Z:.4) [1,4,0,3]
-- >>> seg
-- Vector (Z :. 4) [1,4,0,3]
--
-- >>> let mat = fromList (Z:.5:.10) [0..]
-- >>> mat
-- Matrix (Z :. 5 :. 10)
--   [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
--     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
--     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
--     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
--     40, 41, 42, 43, 44, 45, 46, 47, 48, 49]
--
-- >>> foldSeg (+) 0 (use mat) (use seg)
-- Matrix (Z :. 5 :. 4)
--   [  0,  10, 0,  18,
--     10,  50, 0,  48,
--     20,  90, 0,  78,
--     30, 130, 0, 108,
--     40, 170, 0, 138]
--
foldSeg
    :: (Shape ix, Elt a, Elt i, IsIntegral i)
    => (Exp a -> Exp a -> Exp a)
    -> Exp a
    -> Acc (Array (ix:.Int) a)
    -> Acc (Segments i)
    -> Acc (Array (ix:.Int) a)
foldSeg = Acc $$$$ FoldSeg

-- | Variant of 'foldSeg' that requires /all/ segments of the reduced array to
-- be non-empty and doesn't need a default value. The segment descriptor
-- specifies the length of each of the logical sub-arrays.
--
fold1Seg
    :: (Shape ix, Elt a, Elt i, IsIntegral i)
    => (Exp a -> Exp a -> Exp a)
    -> Acc (Array (ix:.Int) a)
    -> Acc (Segments i)
    -> Acc (Array (ix:.Int) a)
fold1Seg = Acc $$$ Fold1Seg

-- Scan functions
-- --------------

-- | Data.List style left-to-right scan along the innermost dimension of an
-- arbitrary rank array. The first argument needs to be an /associative/
-- function to enable efficient parallel implementation. The initial value
-- (second argument) may be arbitrary.
--
-- >>> scanl (+) 10 (use $ fromList (Z :. 10) [0..])
-- Array (Z :. 11) [10,10,11,13,16,20,25,31,38,46,55]
--
-- >>> scanl (+) 0 (use $ fromList (Z :. 4 :. 10) [0..])
-- Matrix (Z :. 4 :. 11)
--   [ 0,  0,  1,  3,   6,  10,  15,  21,  28,  36,  45,
--     0, 10, 21, 33,  46,  60,  75,  91, 108, 126, 145,
--     0, 20, 41, 63,  86, 110, 135, 161, 188, 216, 245,
--     0, 30, 61, 93, 126, 160, 195, 231, 268, 306, 345]
--
scanl :: (Shape sh, Elt a)
      => (Exp a -> Exp a -> Exp a)
      -> Exp a
      -> Acc (Array (sh:.Int) a)
      -> Acc (Array (sh:.Int) a)
scanl = Acc $$$ Scanl

-- | Variant of 'scanl', where the last element (final reduction result) along
-- each dimension is returned separately. Denotationally we have:
--
-- > scanl' f e arr = (init res, unit (res!len))
-- >   where
-- >     len = shape arr
-- >     res = scanl f e arr
--
-- >>> let (res,sum) = scanl' (+) 0 (use $ fromList (Z:.10) [0..])
-- >>> res
-- Vector (Z :. 10) [0,0,1,3,6,10,15,21,28,36]
-- >>> sum
-- Scalar Z [45]
--
-- >>> let (res,sums) = scanl' (+) 0 (use $ fromList (Z:.4:.10) [0..])
-- >>> res
-- Matrix (Z :. 4 :. 10)
--   [ 0,  0,  1,  3,   6,  10,  15,  21,  28,  36,
--     0, 10, 21, 33,  46,  60,  75,  91, 108, 126,
--     0, 20, 41, 63,  86, 110, 135, 161, 188, 216,
--     0, 30, 61, 93, 126, 160, 195, 231, 268, 306]
-- >>> sums
-- Vector (Z :. 4) [45,145,245,345]
--
scanl' :: (Shape sh, Elt a)
       => (Exp a -> Exp a -> Exp a)
       -> Exp a
       -> Acc (Array (sh:.Int) a)
       -> (Acc (Array (sh:.Int) a), Acc (Array sh a))
scanl' = unatup2 . Acc $$$ Scanl'

-- | Data.List style left-to-right scan along the innermost dimension without an
-- initial value (aka inclusive scan). The array must not be empty. The first
-- argument needs to be an /associative/ function. Denotationally, we have:
--
-- > scanl1 f e arr = tail (scanl f e arr)
--
-- >>> scanl (+) (use $ fromList (Z:.4:.10) [0..])
-- Matrix (Z :. 4 :. 10)
--   [  0,  1,  3,   6,  10,  15,  21,  28,  36,  45,
--     10, 21, 33,  46,  60,  75,  91, 108, 126, 145,
--     20, 41, 63,  86, 110, 135, 161, 188, 216, 245,
--     30, 61, 93, 126, 160, 195, 231, 268, 306, 345]
--
scanl1 :: (Shape sh, Elt a)
       => (Exp a -> Exp a -> Exp a)
       -> Acc (Array (sh:.Int) a)
       -> Acc (Array (sh:.Int) a)
scanl1 = Acc $$ Scanl1

-- | Right-to-left variant of 'scanl'.
--
scanr :: (Shape sh, Elt a)
      => (Exp a -> Exp a -> Exp a)
      -> Exp a
      -> Acc (Array (sh:.Int) a)
      -> Acc (Array (sh:.Int) a)
scanr = Acc $$$ Scanr

-- | Right-to-left variant of 'scanl''.
--
scanr' :: (Shape sh, Elt a)
       => (Exp a -> Exp a -> Exp a)
       -> Exp a
       -> Acc (Array (sh:.Int) a)
       -> (Acc (Array (sh:.Int) a), Acc (Array sh a))
scanr' = unatup2 . Acc $$$ Scanr'

-- | Right-to-left variant of 'scanl1'.
--
scanr1 :: (Shape sh, Elt a)
       => (Exp a -> Exp a -> Exp a)
       -> Acc (Array (sh:.Int) a)
       -> Acc (Array (sh:.Int) a)
scanr1 = Acc $$ Scanr1

-- Permutations
-- ------------

-- | Forward permutation specified by an index mapping. The result array is
-- initialised with the given defaults and any further values that are permuted
-- into the result array are added to the current value using the given
-- combination function.
--
-- The combination function must be /associative/ and /commutative/. Elements
-- that are mapped to the magic value 'ignore' by the permutation function are
-- dropped.
--
-- For example, we can use 'permute' to compute the occurrence count (histogram)
-- for an array of values in the range @[0,10)@:
--
-- > histogram :: Acc (Vector Int) -> Acc (Vector Int)
-- > histogram xs =
-- >   let zeros = fill (constant (Z:.10)) 0
-- >       ones  = fill (shape xs)         1
-- >   in
-- >   permute (+) zeros (\ix -> index1 (xs!ix)) ones
--
-- >>> let xs = fromList (Z :. 20) [0,0,1,2,1,1,2,4,8,3,4,9,8,3,2,5,5,3,1,2]
-- >>> histogram (use xs)
-- Vector (Z :. 10) [2,4,4,3,2,2,0,0,2,1]
--
permute
    :: (Shape ix, Shape ix', Elt a)
    => (Exp a -> Exp a -> Exp a)        -- ^ combination function
    -> Acc (Array ix' a)                -- ^ array of default values
    -> (Exp ix -> Exp ix')              -- ^ permutation
    -> Acc (Array ix  a)                -- ^ array to be permuted
    -> Acc (Array ix' a)
permute = Acc $$$$ Permute

-- | Backward permutation specified by an index mapping from the destination
-- array specifying which element of the source array to read.
--
-- For example, backpermute can be used to transpose a matrix:
--
-- > swap :: Exp DIM2 -> Exp DIM2
-- > swap = lift1 $ \(Z:.x:.y) -> Z:.y:.x
--
-- >>> let mat = fromList (Z:.5:.10) [0..]
-- >>> mat
-- Matrix (Z :. 5 :. 10)
--   [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
--     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
--     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
--     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
--     40, 41, 42, 43, 44, 45, 46, 47, 48, 49]
--
-- >>> let mat' = use mat
-- >>> backpermute (swap (shape mat')) swap mat'
-- Matrix (Z :. 10 :. 5)
--   [ 0, 10, 20, 30, 40,
--     1, 11, 21, 31, 41,
--     2, 12, 22, 32, 42,
--     3, 13, 23, 33, 43,
--     4, 14, 24, 34, 44,
--     5, 15, 25, 35, 45,
--     6, 16, 26, 36, 46,
--     7, 17, 27, 37, 47,
--     8, 18, 28, 38, 48,
--     9, 19, 29, 39, 49]
--
backpermute
    :: (Shape ix, Shape ix', Elt a)
    => Exp ix'                          -- ^ shape of the result array
    -> (Exp ix' -> Exp ix)              -- ^ permutation
    -> Acc (Array ix  a)                -- ^ source array
    -> Acc (Array ix' a)
backpermute = Acc $$$ Backpermute


-- Stencil operations
-- ------------------

-- Common stencil types
--

-- DIM1 stencil type
type Stencil3 a = (Exp a, Exp a, Exp a)
type Stencil5 a = (Exp a, Exp a, Exp a, Exp a, Exp a)
type Stencil7 a = (Exp a, Exp a, Exp a, Exp a, Exp a, Exp a, Exp a)
type Stencil9 a = (Exp a, Exp a, Exp a, Exp a, Exp a, Exp a, Exp a, Exp a, Exp a)

-- DIM2 stencil type
type Stencil3x3 a = (Stencil3 a, Stencil3 a, Stencil3 a)
type Stencil5x3 a = (Stencil5 a, Stencil5 a, Stencil5 a)
type Stencil3x5 a = (Stencil3 a, Stencil3 a, Stencil3 a, Stencil3 a, Stencil3 a)
type Stencil5x5 a = (Stencil5 a, Stencil5 a, Stencil5 a, Stencil5 a, Stencil5 a)

-- DIM3 stencil type
type Stencil3x3x3 a = (Stencil3x3 a, Stencil3x3 a, Stencil3x3 a)
type Stencil5x3x3 a = (Stencil5x3 a, Stencil5x3 a, Stencil5x3 a)
type Stencil3x5x3 a = (Stencil3x5 a, Stencil3x5 a, Stencil3x5 a)
type Stencil3x3x5 a = (Stencil3x3 a, Stencil3x3 a, Stencil3x3 a, Stencil3x3 a, Stencil3x3 a)
type Stencil5x5x3 a = (Stencil5x5 a, Stencil5x5 a, Stencil5x5 a)
type Stencil5x3x5 a = (Stencil5x3 a, Stencil5x3 a, Stencil5x3 a, Stencil5x3 a, Stencil5x3 a)
type Stencil3x5x5 a = (Stencil3x5 a, Stencil3x5 a, Stencil3x5 a, Stencil3x5 a, Stencil3x5 a)
type Stencil5x5x5 a = (Stencil5x5 a, Stencil5x5 a, Stencil5x5 a, Stencil5x5 a, Stencil5x5 a)


-- |Map a stencil over an array.  In contrast to 'map', the domain of a stencil function is an
--  entire /neighbourhood/ of each array element.  Neighbourhoods are sub-arrays centred around a
--  focal point.  They are not necessarily rectangular, but they are symmetric in each dimension
--  and have an extent of at least three in each dimensions — due to the symmetry requirement, the
--  extent is necessarily odd.  The focal point is the array position that is determined by the
--  stencil.
--
--  For those array positions where the neighbourhood extends past the boundaries of the source
--  array, a boundary condition determines the contents of the out-of-bounds neighbourhood
--  positions.
--
stencil
    :: (Stencil sh a stencil, Elt b)
    => (stencil -> Exp b)                     -- ^ stencil function
    -> Boundary a                             -- ^ boundary condition
    -> Acc (Array sh a)                       -- ^ source array
    -> Acc (Array sh b)                       -- ^ destination array
stencil = Acc $$$ Stencil

-- | Map a binary stencil of an array.  The extent of the resulting array is the
-- intersection of the extents of the two source arrays.
--
stencil2
    :: (Stencil sh a stencil1, Stencil sh b stencil2, Elt c)
    => (stencil1 -> stencil2 -> Exp c)        -- ^ binary stencil function
    -> Boundary a                             -- ^ boundary condition #1
    -> Acc (Array sh a)                       -- ^ source array #1
    -> Boundary b                             -- ^ boundary condition #2
    -> Acc (Array sh b)                       -- ^ source array #2
    -> Acc (Array sh c)                       -- ^ destination array
stencil2 = Acc $$$$$ Stencil2


{--
-- Sequence operations
-- ------------------

-- Common sequence types
--

streamIn :: Arrays a
         => [a]
         -> Seq [a]
streamIn arrs = Seq (StreamIn arrs)

-- | Convert the given array to a sequence by dividing the array up into subarrays.
-- The first argument captures how to the division should be performed. The
-- presence of `All` in the division descriptor indicates that elements in the
-- corresponding dimension should be retained in the subarrays, whereas `Split`
-- indicates that the input array should divided along this dimension.
--
toSeq :: (Division slsix, Elt a)
      => slsix
      -> Acc (Array (FullShape (DivisionSlice slsix)) a)
      -> Seq [Array (SliceShape (DivisionSlice slsix)) a]
toSeq spec acc = Seq (ToSeq spec acc)

-- | Apply the given array function element-wise to the given sequence.
--
mapSeq :: (Arrays a, Arrays b)
       => (Acc a -> Acc b)
       -> Seq [a]
       -> Seq [b]
mapSeq = Seq $$ MapSeq

-- | Apply the given binary function element-wise to the two sequences.  The length of the resulting
-- sequence is the minumum of the lengths of the two source sequences.
--
zipWithSeq :: (Arrays a, Arrays b, Arrays c)
           => (Acc a -> Acc b -> Acc c)
           -> Seq [a]
           -> Seq [b]
           -> Seq [c]
zipWithSeq = Seq $$$ ZipWithSeq

-- | scanSeq (+) a0 x seq. Scan a sequence x by combining each
-- element using the given binary operation (+). (+) must be
-- associative:
--
--   Forall a b c. (a + b) + c = a + (b + c),
--
-- and a0 must be the identity element for (+):
--
--   Forall a. a0 + a = a = a + a0.
--
scanSeq :: Elt a
        => (Exp a -> Exp a -> Exp a)
        -> Exp a
        -> Seq [Scalar a]
        -> Seq [Scalar a]
scanSeq = Seq $$$ ScanSeq

-- | foldSeq (+) a0 x seq. Fold a sequence x by combining each
-- element using the given binary operation (+). (+) must be
-- associative:
--
--   Forall a b c. (a + b) + c = a + (b + c),
--
-- and a0 must be the identity element for (+):
--
--   Forall a. a0 + a = a = a + a0.
--
foldSeq :: Elt a
        => (Exp a -> Exp a -> Exp a)
        -> Exp a
        -> Seq [Scalar a]
        -> Seq (Scalar a)
foldSeq = Seq $$$ FoldSeq

-- | foldSeqFlatten f a0 x seq. A specialized version of
-- FoldSeqAct where reduction with the companion operator
-- corresponds to flattening. f must be semi-associative, with vecotor
-- append (++) as the companion operator:
--
--   Forall b sh1 a1 sh2 a2.
--     f (f b sh1 a1) sh2 a2 = f b (sh1 ++ sh2) (a1 ++ a2).
--
-- It is common to ignore the shape vectors, yielding the usual
-- semi-associativity law:
--
--   f b a _ = b + a,
--
-- for some (+) satisfying:
--
--   Forall b a1 a2. (b + a1) + a2 = b + (a1 ++ a2).
--
foldSeqFlatten :: (Arrays a, Shape jx, Elt b)
               => (Acc a -> Acc (Vector jx) -> Acc (Vector b) -> Acc a)
               -> Acc a
               -> Seq [Array jx b]
               -> Seq a
foldSeqFlatten = Seq $$$ FoldSeqFlatten

collect :: Arrays arrs => Seq arrs -> Acc arrs
collect = Acc . Collect
--}

-- Foreign function calling
-- ------------------------

-- | Call a foreign array function.
--
-- The form the first argument takes is dependent on the backend being targeted.
-- Note that the foreign function only has access to the input array(s) passed
-- in as its argument.
--
-- In case the operation is being executed on a backend which does not support
-- this foreign implementation, the fallback implementation is used instead,
-- which itself could be a foreign implementation for a (presumably) different
-- backend, or an implementation of pure Accelerate. In this way, multiple
-- foreign implementations can be supplied, and will be tested for suitability
-- against the target backend in sequence.
--
foreignAcc
    :: (Arrays as, Arrays bs, Foreign asm)
    => asm (as -> bs)
    -> (Acc as -> Acc bs)
    -> Acc as
    -> Acc bs
foreignAcc = Acc $$$ Aforeign

-- | Call a foreign scalar expression.
--
-- The form of the first argument is dependent on the backend being targeted.
-- Note that the foreign function only has access to the input element(s) passed
-- in as its first argument.
--
-- As with 'foreignAcc', the fallback implementation itself may be a (sequence
-- of) foreign implementation(s) for a different backend(s), or implemented
-- purely in Accelerate.
--
foreignExp
    :: (Elt x, Elt y, Foreign asm)
    => asm (x -> y)
    -> (Exp x -> Exp y)
    -> Exp x
    -> Exp y
foreignExp = Exp $$$ Foreign


-- Composition of array computations
-- ---------------------------------

-- | Pipelining of two array computations. The first argument will be fully
-- evaluated before being passed to the second computation. This can be used to
-- prevent the argument being fused into the function, for example.
--
-- Denotationally, we have
--
-- > (acc1 >-> acc2) arrs = let tmp = acc1 arrs
-- >                        in  tmp `seq` acc2 tmp
--
infixl 1 >->
(>->) :: (Arrays a, Arrays b, Arrays c) => (Acc a -> Acc b) -> (Acc b -> Acc c) -> (Acc a -> Acc c)
(>->) = Acc $$$ Pipe


-- Flow control constructs
-- -----------------------

-- | An array-level if-then-else construct.
--
acond :: Arrays a
      => Exp Bool               -- ^ if-condition
      -> Acc a                  -- ^ then-array
      -> Acc a                  -- ^ else-array
      -> Acc a
acond = Acc $$$ Acond

-- | An array-level 'while' construct. Continue to apply the given function,
-- starting with the initial value, until the test function evaluates to
-- 'False'.
--
awhile :: Arrays a
       => (Acc a -> Acc (Scalar Bool))    -- ^ keep evaluating while this returns 'True'
       -> (Acc a -> Acc a)                -- ^ function to apply
       -> Acc a                           -- ^ initial value
       -> Acc a
awhile = Acc $$$ Awhile


-- Shapes and indices
-- ------------------

-- | Get the outermost dimension of a shape
--
indexHead :: (Slice sh, Elt a) => Exp (sh :. a) -> Exp a
indexHead = Exp . IndexHead

-- | Get all but the outermost element of a shape
--
indexTail :: (Slice sh, Elt a) => Exp (sh :. a) -> Exp sh
indexTail = Exp . IndexTail

-- | Map a multi-dimensional index into a linear, row-major representation of an
-- array. The first argument is the array shape, the second is the index.
--
toIndex :: Shape sh => Exp sh -> Exp sh -> Exp Int
toIndex = Exp $$ ToIndex

-- | Inverse of 'toIndex'
--
fromIndex :: Shape sh => Exp sh -> Exp Int -> Exp sh
fromIndex = Exp $$ FromIndex

-- | Intersection of two shapes
--
intersect :: Shape sh => Exp sh -> Exp sh -> Exp sh
intersect = Exp $$ Intersect

-- | Union of two shapes
--
union :: Shape sh => Exp sh -> Exp sh -> Exp sh
union = Exp $$ Union


-- Flow-control
-- ------------

-- | A scalar-level if-then-else construct.
--
cond :: Elt t
     => Exp Bool                -- ^ condition
     -> Exp t                   -- ^ then-expression
     -> Exp t                   -- ^ else-expression
     -> Exp t
cond = Exp $$$ Cond

-- | While construct. Continue to apply the given function, starting with the
-- initial value, until the test function evaluates to 'False'.
--
while :: Elt e
      => (Exp e -> Exp Bool)    -- ^ keep evaluating while this returns 'True'
      -> (Exp e -> Exp e)       -- ^ function to apply
      -> Exp e                  -- ^ initial value
      -> Exp e
while = Exp $$$ While


-- Array operations with a scalar result
-- -------------------------------------

-- |Expression form that extracts a scalar from an array
--
infixl 9 !
(!) :: (Shape ix, Elt e) => Acc (Array ix e) -> Exp ix -> Exp e
(!) = Exp $$ Index

-- |Expression form that extracts a scalar from an array at a linear index
--
infixl 9 !!
(!!) :: (Shape ix, Elt e) => Acc (Array ix e) -> Exp Int -> Exp e
(!!) = Exp $$ LinearIndex

-- |Expression form that yields the shape of an array
--
shape :: (Shape ix, Elt e) => Acc (Array ix e) -> Exp ix
shape = Exp . Shape

-- |Expression form that yields the size of an array
--
size :: (Shape ix, Elt e) => Acc (Array ix e) -> Exp Int
size = shapeSize . shape

-- |The total number of elements in an array of the given 'Shape'
--
shapeSize :: Shape ix => Exp ix -> Exp Int
shapeSize = Exp . ShapeSize


-- Numeric functions
-- -----------------

-- | 'subtract' is the same as @'flip' ('-')@.
--
subtract :: Num a => Exp a -> Exp a -> Exp a
subtract x y = y - x

-- | Determine if a number is even
--
even :: Integral a => Exp a -> Exp Bool
even n = n `rem` 2 ==* 0

-- | Determine if a number is odd
--
odd :: Integral a => Exp a -> Exp Bool
odd n = n `rem` 2 /=* 0

-- | @'gcd' x y@ is the non-negative factor of both @x@ and @y@ of which every
-- common factor of both @x@ and @y@ is also a factor; for example:
--
-- >>> gcd 4 2 = 2
-- >>> gcd (-4) 6 = 2
-- >>> gcd 0 4 = 4
-- >>> gcd 0 0 = 0
--
-- That is, the common divisor that is \"greatest\" in the divisibility
-- preordering.
--
gcd :: Integral a => Exp a -> Exp a -> Exp a
gcd x y = gcd' (abs x) (abs y)
  where
    gcd' :: Integral a => Exp a -> Exp a -> Exp a
    gcd' u v =
      let (r,_) = untup2
                $ while (\(untup2 -> (_,b)) -> b /=* 0)
                        (\(untup2 -> (a,b)) -> tup2 (b, a `rem` b))
                        (tup2 (u,v))
      in r


-- | @'lcm' x y@ is the smallest positive integer that both @x@ and @y@ divide.
--
lcm :: Integral a => Exp a -> Exp a -> Exp a
lcm x y
  = cond (x ==* 0 ||* y ==* 0) 0
  $ abs ((x `quot` (gcd x y)) * y)


-- | Raise a number to a non-negative integral power
--
infixr 8 ^
(^) :: forall a b. (Num a, Integral b) => Exp a -> Exp b -> Exp a
x0 ^ y0 = cond (y0 <=* 0) 1 (f x0 y0)
  where
    f :: Exp a -> Exp b -> Exp a
    f x y =
      let (x',y') = untup2
                  $ while (\(untup2 -> (_,v)) -> even v)
                          (\(untup2 -> (u,v)) -> tup2 (u * u, v `quot` 2))
                          (tup2 (x, y))
      in
      cond (y' ==* 1) x' (g (x'*x') ((y'-1) `quot` 2) x')

    g :: Exp a -> Exp b -> Exp a -> Exp a
    g x y z =
      let (x',_,z') = untup3
                    $ while (\(untup3 -> (_,v,_)) -> v /=* 1)
                            (\(untup3 -> (u,v,w)) ->
                              cond (even v) (tup3 (u*u, v     `quot` 2, w))
                                            (tup3 (u*u, (v-1) `quot` 2, w*u)))
                            (tup3 (x,y,z))
      in
      x' * z'

-- | Raise a number to an integral power
--
infixr 8 ^^
(^^) :: (Fractional a, Integral b) => Exp a -> Exp b -> Exp a
x ^^ n
  = cond (n >=* 0)
  {- then -} (x ^ n)
  {- else -} (recip (x ^ (negate n)))


-- Conversions
-- -----------

-- |Convert a character to an 'Int'.
--
ord :: Exp Char -> Exp Int
ord = mkOrd

-- |Convert an 'Int' into a character.
--
chr :: Exp Int -> Exp Char
chr = mkChr

-- |Convert a Boolean value to an 'Int', where 'False' turns into '0' and 'True'
-- into '1'.
--
boolToInt :: Exp Bool -> Exp Int
boolToInt = mkBoolToInt

-- |Reinterpret a value as another type. The two representations must have the
-- same bit size.
--
bitcast :: (Elt a, Elt b, IsScalar a, IsScalar b, BitSizeEq a b) => Exp a -> Exp b
bitcast = mkBitcast


-- Constants
-- ---------

-- |Magic value identifying elements that are ignored in a forward permutation.
-- Note that this currently does not work for singleton arrays.
--
ignore :: Shape ix => Exp ix
ignore = constant Sugar.ignore

