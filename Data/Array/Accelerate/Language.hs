{-# LANGUAGE OverlappingInstances #-}   -- TLM: required by client code
{-# LANGUAGE TypeOperators        #-}
{-# OPTIONS -fno-warn-missing-methods #-}
{-# OPTIONS -fno-warn-orphans         #-}
-- |
-- Module      : Data.Array.Accelerate.Language
-- Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
--               [2009..2012] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
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

  -- * Extraction of subarrays
  slice,

  -- * Map-like functions
  map, zipWith,

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

  -- * Array-level flow-control
  acond, awhile,

  -- * Index construction and destruction
  indexHead, indexTail, toIndex, fromIndex,
  intersect,

  -- * Flow-control
  cond, while,

  -- * Array operations with a scalar result
  (!), (!!), shape, size, shapeSize,

  -- * Methods of H98 classes that we need to redefine as their signatures change
  (==*), (/=*), (<*), (<=*), (>*), (>=*),
  bit, setBit, clearBit, complementBit, testBit,
  shift,  shiftL,  shiftR,
  rotate, rotateL, rotateR,
  truncate, round, floor, ceiling,
  even, odd,

  -- * Standard functions that we need to redefine as their signatures change
  (&&*), (||*), not,

  -- * Conversions
  ord, chr, boolToInt, fromIntegral,

  -- * Constants
  ignore

  -- Instances of Bounded, Enum, Eq, Ord, Bits, Num, Real, Floating,
  -- Fractional, RealFrac, RealFloat

) where

-- standard libraries
import Prelude ( Bounded, Enum, Num, Real, Integral, Floating, Fractional,
  RealFloat, RealFrac, Eq, Ord, Bool, Char, Float, Double, (.), ($), id, error )
import Data.Bits ( Bits((.&.), (.|.), xor, complement) )
import qualified Prelude                                as P

-- friends
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Array.Sugar                hiding ((!), ignore, shape, size, toIndex, fromIndex, intersect)
import qualified Data.Array.Accelerate.Array.Sugar      as Sugar


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
-- > replicate (Z :.2 :.All :.3) arr
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
-- > slice mat (constant (Z :. (2::Int) :. All))
--
-- A fully specified index (with no `All`s) would return a single
-- element (zero dimensional array).
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
indexHead :: Slice sh => Exp (sh :. Int) -> Exp Int
indexHead = Exp . IndexHead

-- | Get all but the outermost element of a shape
--
indexTail :: Slice sh => Exp (sh :. Int) -> Exp sh
indexTail = Exp . IndexTail

-- | Map a multi-dimensional index into a linear, row-major representation of an
-- array. The first argument is the array shape, the second is the index.
--
toIndex :: Shape sh => Exp sh -> Exp sh -> Exp Int
toIndex = Exp $$ ToIndex

-- | Inverse of 'fromIndex'
--
fromIndex :: Shape sh => Exp sh -> Exp Int -> Exp sh
fromIndex = Exp $$ FromIndex

-- | Intersection of two shapes
--
intersect :: Shape sh => Exp sh -> Exp sh -> Exp sh
intersect = Exp $$ Intersect


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


-- Instances of all relevant H98 classes
-- -------------------------------------

instance (Elt t, IsBounded t) => Bounded (Exp t) where
  minBound = mkMinBound
  maxBound = mkMaxBound

instance (Elt t, IsScalar t) => Enum (Exp t)
--  succ = mkSucc
--  pred = mkPred
  -- FIXME: ops

instance (Elt t, IsScalar t) => Prelude.Eq (Exp t) where
  -- FIXME: instance makes no sense with standard signatures
  (==)        = error "Prelude.Eq.== applied to EDSL types"

instance (Elt t, IsScalar t) => Prelude.Ord (Exp t) where
  -- FIXME: instance makes no sense with standard signatures
  compare       = error "Prelude.Ord.compare applied to EDSL types"
  min           = mkMin
  max           = mkMax

instance (Elt t, IsNum t, IsIntegral t) => Bits (Exp t) where
  (.&.)      = mkBAnd
  (.|.)      = mkBOr
  xor        = mkBXor
  complement = mkBNot
  -- FIXME: argh, the rest have fixed types in their signatures


-- | @'shift' x i@ shifts @x@ left by @i@ bits if @i@ is positive, or right by
-- @-i@ bits otherwise. Right shifts perform sign extension on signed number
-- types; i.e. they fill the top bits with 1 if the @x@ is negative and with 0
-- otherwise.
--
shift :: (Elt t, IsIntegral t) => Exp t -> Exp Int -> Exp t
shift  x i
  = cond (i ==* 0) x
  $ cond (i <*  0) (x `shiftR` (-i))
                   (x `shiftL` i)

-- | Shift the argument left by the specified number of bits
-- (which must be non-negative).
--
shiftL :: (Elt t, IsIntegral t) => Exp t -> Exp Int -> Exp t
shiftL = mkBShiftL

-- | Shift the first argument right by the specified number of bits. The result
-- is undefined for negative shift amounts and shift amounts greater or equal to
-- the 'bitSize'.
--
-- Right shifts perform sign extension on signed number types; i.e. they fill
-- the top bits with 1 if the @x@ is negative and with 0 otherwise.
--
shiftR :: (Elt t, IsIntegral t) => Exp t -> Exp Int -> Exp t
shiftR = mkBShiftR

-- | @'rotate' x i@ rotates @x@ left by @i@ bits if @i@ is positive, or right by
-- @-i@ bits otherwise.
--
rotate :: (Elt t, IsIntegral t) => Exp t -> Exp Int -> Exp t
rotate x i
  = cond (i ==* 0) x
  $ cond (i <*  0) (x `rotateR` (-i))
                   (x `rotateL` i)

-- | Rotate the argument left by the specified number of bits
-- (which must be non-negative).
--
rotateL :: (Elt t, IsIntegral t) => Exp t -> Exp Int -> Exp t
rotateL = mkBRotateL

-- | Rotate the argument right by the specified number of bits
-- (which must be non-negative).
--
rotateR :: (Elt t, IsIntegral t) => Exp t -> Exp Int -> Exp t
rotateR = mkBRotateR

-- | @bit i@ is a value with the @i@th bit set and all other bits clear
--
bit :: (Elt t, IsIntegral t) => Exp Int -> Exp t
bit x = 1 `shiftL` x

-- | @x \`setBit\` i@ is the same as @x .|. bit i@
--
setBit :: (Elt t, IsIntegral t) => Exp t -> Exp Int -> Exp t
x `setBit` i = x .|. bit i

-- | @x \`clearBit\` i@ is the same as @x .&. complement (bit i)@
--
clearBit :: (Elt t, IsIntegral t) => Exp t -> Exp Int -> Exp t
x `clearBit` i = x .&. complement (bit i)

-- | @x \`complementBit\` i@ is the same as @x \`xor\` bit i@
--
complementBit :: (Elt t, IsIntegral t) => Exp t -> Exp Int -> Exp t
x `complementBit` i = x `xor` bit i

-- | Return 'True' if the @n@th bit of the argument is 1
--
testBit :: (Elt t, IsIntegral t) => Exp t -> Exp Int -> Exp Bool
x `testBit` i       = (x .&. bit i) /=* 0


instance (Elt t, IsNum t) => Num (Exp t) where
  (+)         = mkAdd
  (-)         = mkSub
  (*)         = mkMul
  negate      = mkNeg
  abs         = mkAbs
  signum      = mkSig
  fromInteger = constant . P.fromInteger

instance (Elt t, IsNum t) => Real (Exp t)
  -- FIXME: Why did we include this class?  We won't need `toRational' until
  --   we support rational numbers in AP computations.

instance (Elt t, IsIntegral t) => Integral (Exp t) where
  quot = mkQuot
  rem  = mkRem
  div  = mkIDiv
  mod  = mkMod
--  quotRem =
--  divMod  =
--  toInteger =  -- makes no sense

instance (Elt t, IsFloating t) => Floating (Exp t) where
  pi      = mkPi
  sin     = mkSin
  cos     = mkCos
  tan     = mkTan
  asin    = mkAsin
  acos    = mkAcos
  atan    = mkAtan
  asinh   = mkAsinh
  acosh   = mkAcosh
  atanh   = mkAtanh
  exp     = mkExpFloating
  sqrt    = mkSqrt
  log     = mkLog
  (**)    = mkFPow
  logBase = mkLogBase

instance (Elt t, IsFloating t) => Fractional (Exp t) where
  (/)          = mkFDiv
  recip        = mkRecip
  fromRational = constant . P.fromRational

instance (Elt t, IsFloating t) => RealFrac (Exp t)
  -- FIXME: add other ops

instance (Elt t, IsFloating t) => RealFloat (Exp t) where
  atan2 = mkAtan2
  -- FIXME: add other ops


-- Methods from H98 classes, where we need other signatures
-- --------------------------------------------------------

infix 4 ==*, /=*, <*, <=*, >*, >=*

-- |Equality lifted into Accelerate expressions.
--
(==*) :: (Elt t, IsScalar t) => Exp t -> Exp t -> Exp Bool
(==*) = mkEq

-- |Inequality lifted into Accelerate expressions.
--
(/=*) :: (Elt t, IsScalar t) => Exp t -> Exp t -> Exp Bool
(/=*) = mkNEq

-- compare :: a -> a -> Ordering  -- we have no enumerations at the moment
-- compare = ...

-- |Smaller-than lifted into Accelerate expressions.
--
(<*) :: (Elt t, IsScalar t) => Exp t -> Exp t -> Exp Bool
(<*)  = mkLt

-- |Greater-or-equal lifted into Accelerate expressions.
--
(>=*) :: (Elt t, IsScalar t) => Exp t -> Exp t -> Exp Bool
(>=*) = mkGtEq

-- |Greater-than lifted into Accelerate expressions.
--
(>*) :: (Elt t, IsScalar t) => Exp t -> Exp t -> Exp Bool
(>*)  = mkGt

-- |Smaller-or-equal lifted into Accelerate expressions.
--
(<=*) :: (Elt t, IsScalar t) => Exp t -> Exp t -> Exp Bool
(<=*) = mkLtEq

-- Conversions from the RealFrac class
--

-- | @truncate x@ returns the integer nearest @x@ between zero and @x@.
--
truncate :: (Elt a, Elt b, IsFloating a, IsIntegral b) => Exp a -> Exp b
truncate = mkTruncate

-- | @round x@ returns the nearest integer to @x@, or the even integer if @x@ is
-- equidistant between two integers.
--
round :: (Elt a, Elt b, IsFloating a, IsIntegral b) => Exp a -> Exp b
round = mkRound

-- | @floor x@ returns the greatest integer not greater than @x@.
--
floor :: (Elt a, Elt b, IsFloating a, IsIntegral b) => Exp a -> Exp b
floor = mkFloor

-- | @ceiling x@ returns the least integer not less than @x@.
--
ceiling :: (Elt a, Elt b, IsFloating a, IsIntegral b) => Exp a -> Exp b
ceiling = mkCeiling

-- | return if the integer is even
--
even :: (Elt a, IsIntegral a) => Exp a -> Exp Bool
even x = x .&. 1 ==* 0

-- | return if the integer is odd
--
odd :: (Elt a, IsIntegral a) => Exp a -> Exp Bool
odd x = x .&. 1 ==* 1


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

-- |General coercion from integral types
--
fromIntegral :: (Elt a, Elt b, IsIntegral a, IsNum b) => Exp a -> Exp b
fromIntegral = mkFromIntegral


-- Constants
-- ---------

-- |Magic value identifying elements that are ignored in a forward permutation.
-- Note that this currently does not work for singleton arrays.
--
ignore :: Shape ix => Exp ix
ignore = constant Sugar.ignore

