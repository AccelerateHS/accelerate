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
  Acc, Seq, Exp,                            -- re-exporting from 'Smart'

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

  -- * Sequence collection
  collect,

  -- * Sequence producers
  streamIn, subarrays, produce, fromSegs,

  -- * Sequence transudcers
  mapSeq, zipWithSeq, -- mapBatch,

  -- * Sequence consumers
  foldBatch, elements, tabulate,

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
  VectorisedForeign(..), LiftedType(..),

  -- * Pipelining
  (>->),

  -- * Index construction and destruction
  indexHead, indexTail, toIndex, fromIndex, indexSlice, indexFull, indexTrans,
  intersect, union, toSlice,

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
import Data.Array.Accelerate.Array.Lifted               ( VectorisedForeign(..), LiftedType(..) )
import Data.Array.Accelerate.Array.Sugar                hiding ((!), ignore, shape, size, toIndex, fromIndex, intersect, union, toSlice)
import Data.Array.Accelerate.Classes
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type
import qualified Data.Array.Accelerate.Array.Sugar      as Sugar

-- standard libraries
import Prelude                                          ( ($), (.) )


-- Array introduction
-- ------------------

-- | Make an array from vanilla Haskell available for processing within embedded
-- Accelerate computations.
--
-- Depending upon which backend is used to eventually execute array
-- computations, 'use' may entail data transfer (e.g. to a GPU).
--
-- 'use' is overloaded so that it can accept tuples of 'Arrays':
--
-- >>> let vec = fromList (Z:.10) [0..]  :: Array DIM1 Int
-- Vector (Z :. 10) [0,1,2,3,4,5,6,7,8,9]
--
-- >>> let mat = fromList (Z:.5:.10) [0..]  :: Array DIM2 Int
-- >>> mat
-- Matrix (Z :. 5 :. 10)
--   [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
--     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
--     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
--     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
--     40, 41, 42, 43, 44, 45, 46, 47, 48, 49]
--
-- >>> let vec' = use vec         :: Acc (Array DIM1 Int)
-- >>> let mat' = use mat         :: Acc (Array DIM2 Int)
-- >>> let tup  = use (vec, mat)  :: Acc (Array DIM1 Int, Array DIM2 Int)
--
use :: Arrays arrays => arrays -> Acc arrays
use = Acc . Use

-- | Construct a singleton (one element) array from a scalar value (or tuple of
-- scalar values).
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
-- ...we can replicate these elements to form a two-dimensional array either by
-- replicating those elements as new rows:
--
-- >>> replicate (lift (Z :. 4 :. All)) (use vec)
-- Matrix (Z :. 4 :. 10)
--   [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
--     0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
--     0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
--     0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
--
-- ...or as columns:
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
-- The marker 'Any' can be used in the slice specification to match against some
-- arbitrary dimension. For example, here 'Any' matches against whatever shape
-- type variable @sh@ takes.
--
-- > rep0 :: (Shape sh, Elt e) => Exp Int -> Acc (Array sh e) -> Acc (Array (sh :. Int) e)
-- > rep0 n a = replicate (lift (Any :. n)) a
--
-- >>> let x = unit 42  :: Acc (Scalar Int)
-- >>> rep0 10 x
-- Vector (Z :. 10) [42,42,42,42,42,42,42,42,42,42]
--
-- >>> rep0 5 (use vec)
-- Matrix (Z :. 10 :. 5)
--   [ 0, 0, 0, 0, 0,
--     1, 1, 1, 1, 1,
--     2, 2, 2, 2, 2,
--     3, 3, 3, 3, 3,
--     4, 4, 4, 4, 4,
--     5, 5, 5, 5, 5,
--     6, 6, 6, 6, 6,
--     7, 7, 7, 7, 7,
--     8, 8, 8, 8, 8,
--     9, 9, 9, 9, 9]
--
-- Of course, 'Any' and 'All' can be used together.
--
-- > rep1 :: (Shape sh, Elt e) => Exp Int -> Acc (Array (sh :. Int) e) -> Acc (Array (sh :. Int :. Int) e)
-- > rep1 n a = A.replicate (lift (Any :. n :. All)) a
--
-- >>> rep1 5 (use vec)
-- Matrix (Z :. 5 :. 10)
--   [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
--     0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
--     0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
--     0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
--     0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
--
replicate
    :: (Slice slix, Elt e)
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
-- operations such as ('!') or 'Data.Array.Accelerate.Prelude.the', the program
-- will fail with the error:
-- '.\/Data\/Array\/Accelerate\/Trafo\/Sharing.hs:447 (convertSharingExp): inconsistent valuation \@ shared \'Exp\' tree ...'.
--
generate
    :: (Shape sh, Elt a)
    => Exp sh
    -> (Exp sh -> Exp a)
    -> Acc (Array sh a)
generate = Acc $$ Generate

-- Shape manipulation
-- ------------------

-- | Change the shape of an array without altering its contents. The 'size' of
-- the source and result arrays must be identical.
--
-- > precondition: shapeSize sh == shapeSize sh'
--
-- If the argument array is manifest in memory, 'reshape' is a NOP. If the
-- argument is to be fused into a subsequent operation, 'reshape' corresponds to
-- an index transformation in the fused code.
--
reshape
    :: (Shape sh, Shape sh', Elt e)
    => Exp sh
    -> Acc (Array sh' e)
    -> Acc (Array sh e)
reshape = Acc $$ Reshape

-- Extraction of sub-arrays
-- ------------------------

-- | Index an array with a /generalised/ array index, supplied as the
-- second argument. The result is a new array (possibly a singleton)
-- containing the selected dimensions (`All`s) in their entirety.
--
-- 'slice' is the opposite of 'replicate', and can be used to /cut out/ entire
-- dimensions. For example, for the two dimensional array 'mat':
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
-- ...will can select a specific row to yield a one dimensional result by fixing
-- the row index (2) while allowing the column index to vary (via 'All'):
--
-- >>> slice (use mat) (lift (Z :. 2 :. All))
-- Vector (Z :. 10) [20,21,22,23,24,25,26,27,28,29]
--
-- A fully specified index (with no 'All's) returns a single element (zero
-- dimensional array).
--
-- >>> slice (use mat) (lift (Z :. 4 :. 2))
-- Scalar Z [42]
--
-- The marker 'Any' can be used in the slice specification to match against some
-- arbitrary (lower) dimension. Here 'Any' matches whatever shape type variable
-- @sh@ takes:
--
-- > sl0 :: (Shape sh, Elt e) => Acc (Array (sh:.Int) e) -> Exp Int -> Acc (Array sh e)
-- > sl0 a n = A.slice a (lift (Any :. n))
--
-- >>> let vec = fromList (Z:.10) [0..]
-- >>> sl0 (use vec) 4
-- Scalar Z [4]
--
-- >>> sl0 (use mat) 4
-- Vector (Z :. 5) [4,14,24,34,44]
--
-- Of course, 'Any' and 'All' can be used together.
--
-- > sl1 :: (Shape sh, Elt e) => Acc (Array (sh:.Int:.Int) e) -> Exp Int -> Acc (Array (sh:.Int) e)
-- > sl1 a n = A.slice a (lift (Any :. n :. All))
--
-- >>> sl1 (use mat) 4
-- Vector (Z :. 10) [40,41,42,43,44,45,46,47,48,49]
--
-- >>> let cube = fromList (Z:.3:.4:.5) [0..]
-- >>> cube
-- Array (Z :. 3 :. 4 :. 5) [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59]
--
-- >>> sl1 (use cube) 2
-- Matrix (Z :. 3 :. 5)
--   [ 10, 11, 12, 13, 14,
--     30, 31, 32, 33, 34,
--     50, 51, 52, 53, 54]
--
slice :: (Slice slix, Elt e)
      => Acc (Array (FullShape slix) e)
      -> Exp slix
      -> Acc (Array (SliceShape slix) e)
slice = Acc $$ Slice

-- Map-like functions
-- ------------------

-- | Apply the given function element-wise to an array.
--
-- > map f [x1, x2, ... xn] = [f x1, f x2, ... f xn]
--
map :: (Shape sh, Elt a, Elt b)
    => (Exp a -> Exp b)
    -> Acc (Array sh a)
    -> Acc (Array sh b)
map = Acc $$ Map

-- | Apply the given binary function element-wise to the two arrays. The extent
-- of the resulting array is the intersection of the extents of the two source
-- arrays.
--
zipWith :: (Shape sh, Elt a, Elt b, Elt c)
        => (Exp a -> Exp b -> Exp c)
        -> Acc (Array sh a)
        -> Acc (Array sh b)
        -> Acc (Array sh c)
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
fold :: (Shape sh, Elt a)
     => (Exp a -> Exp a -> Exp a)
     -> Exp a
     -> Acc (Array (sh:.Int) a)
     -> Acc (Array sh a)
fold = Acc $$$ Fold

-- | Variant of 'fold' that requires the reduced array to be non-empty and
-- doesn't need an default value.  The first argument needs to be an
-- /associative/ function to enable an efficient parallel implementation. The
-- initial element does not need to be an identity element.
--
fold1 :: (Shape sh, Elt a)
      => (Exp a -> Exp a -> Exp a)
      -> Acc (Array (sh:.Int) a)
      -> Acc (Array sh a)
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
    :: (Shape sh, Elt a, Elt i, IsIntegral i)
    => (Exp a -> Exp a -> Exp a)
    -> Exp a
    -> Acc (Array (sh:.Int) a)
    -> Acc (Segments i)
    -> Acc (Array (sh:.Int) a)
foldSeg = Acc $$$$ FoldSeg

-- | Variant of 'foldSeg' that requires /all/ segments of the reduced array to
-- be non-empty and doesn't need a default value. The segment descriptor
-- specifies the length of each of the logical sub-arrays.
--
fold1Seg
    :: (Shape sh, Elt a, Elt i, IsIntegral i)
    => (Exp a -> Exp a -> Exp a)
    -> Acc (Array (sh:.Int) a)
    -> Acc (Segments i)
    -> Acc (Array (sh:.Int) a)
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

-- | Generalised forward permutation operation (array scatter).
--
-- Forward permutation specified by a function mapping indices from the source
-- array to indices in the result array. The result array is initialised with
-- the given defaults and any further values that are permuted into the result
-- array are added to the current value using the given combination function.
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
-- [/Note:/]
--
-- Regarding array fusion:
--
--   1. The 'permute' operation will always be evaluated; it can not be fused
--      into a later step.
--
--   2. Since the index permutation function might not cover all positions in
--      the output array (the function is not surjective), the array of default
--      values must be evaluated. However, other operations may fuse into this.
--
--   3. The array of source values can fuse into the permutation operation.
--
permute
    :: (Shape sh, Shape sh', Elt a)
    => (Exp a -> Exp a -> Exp a)        -- ^ combination function
    -> Acc (Array sh' a)                -- ^ array of default values
    -> (Exp sh -> Exp sh')              -- ^ index permutation function
    -> Acc (Array sh  a)                -- ^ array of source values to be permuted
    -> Acc (Array sh' a)
permute = Acc $$$$ Permute

-- | Generalised backward permutation operation (array gather).
--
-- Backward permutation specified by a function mapping indices in the
-- destination array to indices in the source array. Elements of the output
-- array are thus generated by reading from the corresponding index in the
-- source array.
--
-- For example, backpermute can be used to
-- 'Data.Array.Accelerate.Prelude.transpose' a matrix; at every index @Z:.y:.x@
-- in the result array, we get the value at that index by reading from the
-- source array at index @Z:.x:.y@:
--
-- > swap :: Exp DIM2 -> Exp DIM2
-- > swap = lift1 $ \(Z:.y:.x) -> Z:.x:.y
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
    :: (Shape sh, Shape sh', Elt a)
    => Exp sh'                          -- ^ shape of the result array
    -> (Exp sh' -> Exp sh)              -- ^ index permutation function
    -> Acc (Array sh  a)                -- ^ source array
    -> Acc (Array sh' a)
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


-- | Map a stencil over an array. In contrast to 'map', the domain of a stencil
-- function is an entire /neighbourhood/ of each array element. Neighbourhoods
-- are sub-arrays centred around a focal point. They are not necessarily
-- rectangular, but they are symmetric and have an extent of at least three
-- along each axis. Due to the symmetry requirement the extent is necessarily
-- odd. The focal point is the array position that is determined by the stencil.
--
-- For those array positions where the neighbourhood extends past the boundaries
-- of the source array, a boundary condition determines the contents of the
-- out-of-bounds neighbourhood positions.
--
-- Stencil neighbourhoods are specified via nested tuples, where the nesting
-- depth is equal to the dimensionality of the array. For example, a 3x1 stencil
-- for a one-dimensional array:
--
-- > s31 :: Stencil3 a -> Exp a
-- > s31 (l,c,r) = ...
--
-- ...where @c@ is the focal point of the stencil, and @l@ and @r@ represent the
-- elements to the left and right of the focal point, respectively. Similarly,
-- a 3x3 stencil for a two-dimensional array:
--
-- > s33 :: Stencil3x3 a -> Exp a
-- > s33 ((_,t,_)
--       ,(l,c,r)
--       ,(_,b,_)) = ...
--
-- ...where @c@ is again the focal point and @t@, @b@, @l@ and @r@ are the
-- elements to the top, bottom, left, and right of the focal point, respectively
-- (the diagonal elements have been elided).
--
-- For example, the following computes a 5x5
-- <https://en.wikipedia.org/wiki/Gaussian_blur Gaussian blur> as a separable
-- 2-pass operation.
--
-- > type Stencil5x1 a = (Stencil3 a, Stencil5 a, Stencil3 a)
-- > type Stencil1x5 a = (Stencil3 a, Stencil3 a, Stencil3 a, Stencil3 a, Stencil3 a)
-- >
-- > convolve5x1 :: Num a => [Exp a] -> Stencil5x1 a -> Exp a
-- > convolve5x1 kernel (_, (a,b,c,d,e), _)
-- >   = Prelude.sum $ Prelude.zipWith (*) kernel [a,b,c,d,e]
-- >
-- > convolve1x5 :: Num a => [Exp a] -> Stencil1x5 a -> Exp a
-- > convolve1x5 kernel ((_,a,_), (_,b,_), (_,c,_), (_,d,_), (_,e,_))
-- >   = Prelude.sum $ Prelude.zipWith (*) kernel [a,b,c,d,e]
-- >
-- > gaussian = [0.06136,0.24477,0.38774,0.24477,0.06136]
-- >
-- > blur :: Num a => Acc (Array DIM2 a) -> Acc (Array DIM2 a)
-- > blur = stencil (convolve5x1 gaussian) Clamp
-- >      . stencil (convolve1x5 gaussian) Clamp
--
stencil
    :: (Stencil sh a stencil, Elt b)
    => (stencil -> Exp b)                     -- ^ stencil function
    -> Boundary a                             -- ^ boundary condition
    -> Acc (Array sh a)                       -- ^ source array
    -> Acc (Array sh b)                       -- ^ destination array
stencil = Acc $$$ Stencil

-- | Map a binary stencil of an array. The extent of the resulting array is the
-- intersection of the extents of the two source arrays. This is the stencil
-- equivalent of 'zipWith'.
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


-- Sequence operations
-- ------------------

-- Common sequence types
--

streamIn :: (Shape sh, Elt e)
         => [Array sh e]
         -> Seq [Array sh e]
streamIn = Seq . StreamIn

-- |Split an array up into subarrays of given shape along the outermost dimension
-- moving inward - e.g. if the array is a matrix, the returned sequence is the
-- submatrices in column major order.
--
subarrays :: (Shape sh, Elt e, sh :<= DIM2)
          => Exp sh
          -> Array sh e
          -> Seq [Array sh e]
subarrays = Seq $$ Subarrays

-- |Generate a sequence from a some segment descriptors (in (offset,shape)
-- format) and a vector of values.
--
fromSegs :: (Shape sh, Elt e)
         => Acc (Segments (Int,sh)) -- ^Segments
         -> Exp Int                 -- ^Take this many segments
         -> Acc (Vector e)          -- ^The flattened values vector
         -> Seq [Array sh e]
fromSegs = Seq $$$ FromSegs

-- |Generate a sequence by applying a function at every index.
--
produce :: Arrays a
        => Exp Int
        -> (Acc (Scalar Int) -> Acc a)
        -> Seq [a]
produce = Seq $$ Produce

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

-- |A batched map.
--
-- mapBatch :: (Arrays a, Arrays b, Arrays c, Arrays s)
--          => (Acc s -> Acc a -> Acc b)
--          -> (Acc s -> Acc (Regular b) -> Acc (s, Regular c))
--          -> (Acc s -> Acc (Irregular b) -> Acc (s, Irregular c))
--          -> Acc s
--          -> Seq [a]
--          -> Seq [(s,c)]
-- mapBatch = Seq $$$$$ MapBatch

foldBatch :: (Arrays a, Arrays b, Arrays s)
          => (Acc s -> Seq [a] -> Seq b)
          -> (Acc b -> Acc s)
          -> Acc s
          -> Seq [a]
          -> Seq s
foldBatch = Seq $$$$ FoldBatch

-- |Flatten and concatenate all elements in the sequence.
--
elements :: (Shape sh, Elt e)
         => Seq [Array sh e]
         -> Seq (Vector e)
elements = Seq . Elements

-- |Construct an array from all elements in the sequence by concatenating on the
-- outer dimension. If sequence is irregular, all elements are trimmed to the
-- shape of the smallest element in the sequence.
--
tabulate :: (Shape sh, Elt e)
         => Seq [Array sh e]
         -> Seq (Array (sh:.Int) e)
tabulate = Seq . Tabulate

collect :: Arrays arrs => Seq arrs -> Acc arrs
collect = Acc . Collect

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
-- For an example see the <https://hackage.haskell.org/package/accelerate-fft accelerate-fft> package.
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

-- | Get the innermost dimension of a shape
--
indexHead :: (Slice sh, Elt a) => Exp (sh :. a) -> Exp a
indexHead = Exp . IndexHead

-- | Get all but the innermost element of a shape
--
indexTail :: (Slice sh, Elt a) => Exp (sh :. a) -> Exp sh
indexTail = Exp . IndexTail

-- | Transpose a shape.
--
indexTrans :: Shape sh => Exp sh -> Exp sh
indexTrans = Exp . IndexTrans

-- | Project the shape of a slice from the shape of an array.
--
indexSlice :: Slice slix
           => Exp slix
           -> Exp (FullShape slix)
           -> Exp (SliceShape slix)
indexSlice = Exp $$ IndexSlice

-- | Construct a shape from the slice descriptor and a slice of the shape.
--
indexFull :: Slice slix
          => Exp slix
          -> Exp (SliceShape slix)
          -> Exp (FullShape slix)
indexFull = Exp $$ IndexFull

-- | Map a multi-dimensional index into a linear, row-major representation of an
-- array.
--
toIndex
    :: Shape sh
    => Exp sh                     -- ^ extent of the array
    -> Exp sh                     -- ^ index to remap
    -> Exp Int
toIndex = Exp $$ ToIndex

-- | Inverse of 'toIndex'
--
fromIndex :: Shape sh => Exp sh -> Exp Int -> Exp sh
fromIndex = Exp $$ FromIndex

-- | Get the index of the Nth slice in an array starting from the given index.
--
toSlice :: Slice slix
        => Exp slix
        -> Exp (FullShape slix)
        -> Exp Int
        -> Exp slix
toSlice = Exp $$$ ToSlice

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

-- | Multidimensional array indexing. Extract the value from an array at the
-- specified zero-based index.
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
-- >>> mat ! Z:.1:.2
-- 12
--
infixl 9 !
(!) :: (Shape sh, Elt e) => Acc (Array sh e) -> Exp sh -> Exp e
(!) = Exp $$ Index

-- | Extract the value from an array at the specified linear index.
-- Multidimensional arrays in Accelerate are stored in row-major order with
-- zero-based indexing.
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
-- >>> mat !! 12
-- 12
--
infixl 9 !!
(!!) :: (Shape sh, Elt e) => Acc (Array sh e) -> Exp Int -> Exp e
(!!) = Exp $$ LinearIndex

-- | Extract the shape (extent) of an array.
--
shape :: (Shape sh, Elt e) => Acc (Array sh e) -> Exp sh
shape = Exp . Shape

-- | The number of elements in the array
--
size :: (Shape sh, Elt e) => Acc (Array sh e) -> Exp Int
size = shapeSize . shape

-- | The number of elements that would be held by an array of the given shape.
--
shapeSize :: Shape sh => Exp sh -> Exp Int
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
even n = n `rem` 2 == 0

-- | Determine if a number is odd
--
odd :: Integral a => Exp a -> Exp Bool
odd n = n `rem` 2 /= 0

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
                $ while (\(untup2 -> (_,b)) -> b /= 0)
                        (\(untup2 -> (a,b)) -> tup2 (b, a `rem` b))
                        (tup2 (u,v))
      in r


-- | @'lcm' x y@ is the smallest positive integer that both @x@ and @y@ divide.
--
lcm :: Integral a => Exp a -> Exp a -> Exp a
lcm x y
  = cond (x == 0 || y == 0) 0
  $ abs ((x `quot` (gcd x y)) * y)


-- | Raise a number to a non-negative integral power
--
infixr 8 ^
(^) :: forall a b. (Num a, Integral b) => Exp a -> Exp b -> Exp a
x0 ^ y0 = cond (y0 <= 0) 1 (f x0 y0)
  where
    f :: Exp a -> Exp b -> Exp a
    f x y =
      let (x',y') = untup2
                  $ while (\(untup2 -> (_,v)) -> even v)
                          (\(untup2 -> (u,v)) -> tup2 (u * u, v `quot` 2))
                          (tup2 (x, y))
      in
      cond (y' == 1) x' (g (x'*x') ((y'-1) `quot` 2) x')

    g :: Exp a -> Exp b -> Exp a -> Exp a
    g x y z =
      let (x',_,z') = untup3
                    $ while (\(untup3 -> (_,v,_)) -> v /= 1)
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
  = cond (n >= 0)
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
--
ignore :: Shape sh => Exp sh
ignore = constant Sugar.ignore
