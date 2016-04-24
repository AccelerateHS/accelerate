{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
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
  streamIn, toSeq,

  -- * Sequence transducers
  mapSeq, zipWithSeq, scanSeq,

  -- * Sequence consumers
  foldSeq, foldSeqFlatten,

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
  foreignAcc, foreignAcc2, foreignAcc3,
  foreignExp, foreignExp2, foreignExp3,

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

  -- * Standard functions that we need to redefine as their signatures change
  (&&*), (||*), not,

  -- * Conversions
  ord, chr, boolToInt, bitcast,

  -- * Constants
  ignore

) where

-- friends
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Sugar                hiding ((!), ignore, shape, size, toIndex, fromIndex, intersect, union)
import qualified Data.Array.Accelerate.Array.Sugar      as Sugar

-- standard libraries
import Prelude                                          ( (.) )


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
-- For example, assuming 'arr' is a vector (one-dimensional array),
--
-- > replicate (lift (Z :. (2::Int) :. All :. (3::Int))) arr
--
-- yields a three dimensional array, where 'arr' is replicated twice across the
-- first and three times across the third dimension.
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
-- > generate (index1 3) (\_ -> 1.2)
--
-- Or, equivalently:
--
-- > generate (constant (Z :. (3::Int))) (\_ -> 1.2)
--
-- Finally, the following will create an array equivalent to '[1..10]':
--
-- > generate (index1 10) $ \ ix ->
-- >          let (Z :. i) = unlift ix
-- >          in fromIntegral i
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
-- > slice mat (lift (Z :. (2::Int) :. All))
--
-- A fully specified index (with no `All`s) would return a single element (zero
-- dimensional array).
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
-- parallel implementation.
--
fold :: (Shape ix, Elt a)
     => (Exp a -> Exp a -> Exp a)
     -> Exp a
     -> Acc (Array (ix:.Int) a)
     -> Acc (Array ix a)
fold = Acc $$$ Fold

-- | Variant of 'fold' that requires the reduced array to be non-empty and
-- doesn't need an default value.  The first argument needs to be an
-- /associative/ function to enable an efficient parallel implementation.
--
fold1 :: (Shape ix, Elt a)
      => (Exp a -> Exp a -> Exp a)
      -> Acc (Array (ix:.Int) a)
      -> Acc (Array ix a)
fold1 = Acc $$ Fold1

-- | Segmented reduction along the innermost dimension.  Performs one individual
-- reduction per segment of the source array.  These reductions proceed in
-- parallel.
--
-- The source array must have at least rank 1.  The 'Segments' array determines
-- the lengths of the logical sub-arrays, each of which is folded separately.
--
foldSeg :: (Shape ix, Elt a, Elt i, IsIntegral i)
        => (Exp a -> Exp a -> Exp a)
        -> Exp a
        -> Acc (Array (ix:.Int) a)
        -> Acc (Segments i)
        -> Acc (Array (ix:.Int) a)
foldSeg = Acc $$$$ FoldSeg

-- | Variant of 'foldSeg' that requires /all/ segments of the reduced array to
-- be non-empty and doesn't need a default value.
--
-- The source array must have at least rank 1. The 'Segments' array determines
-- the lengths of the logical sub-arrays, each of which is folded separately.
--
fold1Seg :: (Shape ix, Elt a, Elt i, IsIntegral i)
         => (Exp a -> Exp a -> Exp a)
         -> Acc (Array (ix:.Int) a)
         -> Acc (Segments i)
         -> Acc (Array (ix:.Int) a)
fold1Seg = Acc $$$ Fold1Seg

-- Scan functions
-- --------------

-- | Data.List style left-to-right scan, but with the additional restriction
-- that the first argument needs to be an /associative/ function to enable an
-- efficient parallel implementation. The initial value (second argument) may be
-- arbitrary.
--
scanl :: Elt a
      => (Exp a -> Exp a -> Exp a)
      -> Exp a
      -> Acc (Vector a)
      -> Acc (Vector a)
scanl = Acc $$$ Scanl

-- | Variant of 'scanl', where the final result of the reduction is returned
-- separately. Denotationally, we have
--
-- > scanl' f e arr = (init res, unit (res!len))
-- >   where
-- >     len = shape arr
-- >     res = scanl f e arr
--
scanl' :: Elt a
       => (Exp a -> Exp a -> Exp a)
       -> Exp a
       -> Acc (Vector a)
       -> (Acc (Vector a), Acc (Scalar a))
scanl' = unatup2 . Acc $$$ Scanl'

-- | Data.List style left-to-right scan without an initial value (aka inclusive
-- scan).  Again, the first argument needs to be an /associative/ function.
-- Denotationally, we have
--
-- > scanl1 f e arr = tail (scanl f e arr)
--
scanl1 :: Elt a
       => (Exp a -> Exp a -> Exp a)
       -> Acc (Vector a)
       -> Acc (Vector a)
scanl1 = Acc $$ Scanl1

-- | Right-to-left variant of 'scanl'.
--
scanr :: Elt a
      => (Exp a -> Exp a -> Exp a)
      -> Exp a
      -> Acc (Vector a)
      -> Acc (Vector a)
scanr = Acc $$$ Scanr

-- | Right-to-left variant of 'scanl''.
--
scanr' :: Elt a
       => (Exp a -> Exp a -> Exp a)
       -> Exp a
       -> Acc (Vector a)
       -> (Acc (Vector a), Acc (Scalar a))
scanr' = unatup2 . Acc $$$ Scanr'

-- | Right-to-left variant of 'scanl1'.
--
scanr1 :: Elt a
       => (Exp a -> Exp a -> Exp a)
       -> Acc (Vector a)
       -> Acc (Vector a)
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
permute :: (Shape ix, Shape ix', Elt a)
        => (Exp a -> Exp a -> Exp a)    -- ^combination function
        -> Acc (Array ix' a)            -- ^array of default values
        -> (Exp ix -> Exp ix')          -- ^permutation
        -> Acc (Array ix  a)            -- ^array to be permuted
        -> Acc (Array ix' a)
permute = Acc $$$$ Permute

-- | Backward permutation specified by an index mapping from the destination
-- array specifying which element of the source array to read.
--
backpermute :: (Shape ix, Shape ix', Elt a)
            => Exp ix'                  -- ^shape of the result array
            -> (Exp ix' -> Exp ix)      -- ^permutation
            -> Acc (Array ix  a)        -- ^source array
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
stencil :: (Shape ix, Elt a, Elt b, Stencil ix a stencil)
        => (stencil -> Exp b)                 -- ^stencil function
        -> Boundary a                         -- ^boundary condition
        -> Acc (Array ix a)                   -- ^source array
        -> Acc (Array ix b)                   -- ^destination array
stencil = Acc $$$ Stencil

-- | Map a binary stencil of an array.  The extent of the resulting array is the
-- intersection of the extents of the two source arrays.
--
stencil2 :: (Shape ix, Elt a, Elt b, Elt c,
             Stencil ix a stencil1,
             Stencil ix b stencil2)
        => (stencil1 -> stencil2 -> Exp c)    -- ^binary stencil function
        -> Boundary a                         -- ^boundary condition #1
        -> Acc (Array ix a)                   -- ^source array #1
        -> Boundary b                         -- ^boundary condition #2
        -> Acc (Array ix b)                   -- ^source array #2
        -> Acc (Array ix c)                   -- ^destination array
stencil2 = Acc $$$$$ Stencil2


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

-- Foreign function calling
-- ------------------------

-- | Call a foreign function. The form the function takes is dependent on the backend being used.
-- The arguments are passed as either a single array or as a tuple of arrays. In addition a pure
-- Accelerate version of the function needs to be provided to support backends other than the one
-- being targeted.
foreignAcc :: (Arrays acc, Arrays res, Foreign ff)
           => ff acc res
           -> (Acc acc -> Acc res)
           -> Acc acc
           -> Acc res
foreignAcc = Acc $$$ Aforeign

-- | Call a foreign function with foreign implementations for two different backends.
foreignAcc2 :: (Arrays acc, Arrays res, Foreign ff1, Foreign ff2)
            => ff1 acc res
            -> ff2 acc res
            -> (Acc acc -> Acc res)
            -> Acc acc
            -> Acc res
foreignAcc2 ff1 = Acc $$$ Aforeign ff1 $$ Acc $$$ Aforeign

-- | Call a foreign function with foreign implementations for three different backends.
foreignAcc3 :: (Arrays acc, Arrays res, Foreign ff1, Foreign ff2, Foreign ff3)
            => ff1 acc res
            -> ff2 acc res
            -> ff3 acc res
            -> (Acc acc -> Acc res)
            -> Acc acc
            -> Acc res
foreignAcc3 ff1 ff2 = Acc $$$ Aforeign ff1 $$ Acc $$$ Aforeign ff2 $$ Acc $$$ Aforeign

-- | Call a foreign expression function. The form the function takes is dependent on the
-- backend being used. The arguments are passed as either a single scalar element or as a
-- tuple of elements. In addition a pure Accelerate version of the function needs to be
-- provided to support backends other than the one being targeted.
foreignExp :: (Elt e, Elt res, Foreign ff)
           => ff e res
           -> (Exp e -> Exp res)
           -> Exp e
           -> Exp res
foreignExp = Exp $$$ Foreign

-- | Call a foreign function with foreign implementations for two different backends.
foreignExp2 :: (Elt e, Elt res, Foreign ff1, Foreign ff2)
            => ff1 e res
            -> ff2 e res
            -> (Exp e -> Exp res)
            -> Exp e
            -> Exp res
foreignExp2 ff1 = Exp $$$ Foreign ff1 $$ Exp $$$ Foreign

-- | Call a foreign function with foreign implementations for three different backends.
foreignExp3 :: (Elt e, Elt res, Foreign ff1, Foreign ff2, Foreign ff3)
            => ff1 e res
            -> ff2 e res
            -> ff3 e res
            -> (Exp e -> Exp res)
            -> Exp e
            -> Exp res
foreignExp3 ff1 ff2 = Exp $$$ Foreign ff1 $$ Exp $$$ Foreign ff2 $$ Exp $$$ Foreign


-- Composition of array computations
-- ---------------------------------

-- | Pipelining of two array computations.
--
-- Denotationally, we have
--
-- > (acc1 >-> acc2) arrs = let tmp = acc1 arrs in acc2 tmp
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

-- | An array-level while construct
--
awhile :: Arrays a
       => (Acc a -> Acc (Scalar Bool))
       -> (Acc a -> Acc a)
       -> Acc a
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
-- initial value, until the test function evaluates to true.
--
while :: Elt e
      => (Exp e -> Exp Bool)
      -> (Exp e -> Exp e)
      -> Exp e
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


-- Non-overloaded standard functions, where we need other signatures
-- -----------------------------------------------------------------

-- |Conjunction
--
infixr 3 &&*
(&&*) :: Exp Bool -> Exp Bool -> Exp Bool
(&&*) = mkLAnd

-- |Disjunction
--
infixr 2 ||*
(||*) :: Exp Bool -> Exp Bool -> Exp Bool
(||*) = mkLOr

-- |Negation
--
not :: Exp Bool -> Exp Bool
not = mkLNot


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

