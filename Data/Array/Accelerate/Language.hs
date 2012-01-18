{-# LANGUAGE TypeOperators, FlexibleContexts, TypeFamilies, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Language
-- Copyright   : [2009..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
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

  -- ** Array and scalar expressions
  Acc, Exp,                                 -- re-exporting from 'Smart'
  
  -- ** Stencil specification
  Boundary(..), Stencil,                    -- re-exporting from 'Smart'

  -- ** Common stencil types
  Stencil3, Stencil5, Stencil7, Stencil9,
  Stencil3x3, Stencil5x3, Stencil3x5, Stencil5x5,
  Stencil3x3x3, Stencil5x3x3, Stencil3x5x3, Stencil3x3x5, Stencil5x5x3, Stencil5x3x5,
  Stencil3x5x5, Stencil5x5x5,

  -- ** Scalar introduction
  constant,                                 -- re-exporting from 'Smart'

  -- ** Array construction
  use, unit, replicate, generate,
  fstA, sndA, pairA,

  -- ** Shape manipulation
  reshape,

  -- ** Extraction of subarrays
  slice, 
  
  -- ** Map-like functions
  map, zipWith,
  
  -- ** Reductions
  fold, fold1, foldSeg, fold1Seg,
  
  -- ** Scan functions
  scanl, scanl', scanl1, scanr, scanr', scanr1,
  
  -- ** Permutations
  permute, backpermute, 
  
  -- ** Stencil operations
  stencil, stencil2,
  
  -- ** Pipelining
  (>->),
  
  -- ** Array-level flow-control
  cond, (?|),

  -- ** Lifting and unlifting
  Lift(..), Unlift(..), lift1, lift2, ilift1, ilift2,
  
  -- ** Tuple construction and destruction
  fst, snd, curry, uncurry,
  
  -- ** Index construction and destruction
  index0, index1, unindex1, index2, unindex2,
  
  -- ** Conditional expressions
  (?),
  
  -- ** Array operations with a scalar result
  (!), the, shape, size, shapeSize,
  
  -- ** Methods of H98 classes that we need to redefine as their signatures change
  (==*), (/=*), (<*), (<=*), (>*), (>=*), max, min,
  bit, setBit, clearBit, complementBit, testBit,
  shift,  shiftL,  shiftR,
  rotate, rotateL, rotateR,
  truncate, round, floor, ceiling,

  -- ** Standard functions that we need to redefine as their signatures change
  (&&*), (||*), not,
  
  -- ** Conversions
  boolToInt, fromIntegral,

  -- ** Constants
  ignore

  -- Instances of Bounded, Enum, Eq, Ord, Bits, Num, Real, Floating,
  -- Fractional, RealFrac, RealFloat

) where

-- avoid clashes with Prelude functions
import Prelude   hiding (replicate, zip, unzip, map, scanl, scanl1, scanr, scanr1, zipWith,
                         filter, max, min, not, fst, snd, curry, uncurry,
                         truncate, round, floor, ceiling, fromIntegral)

-- standard libraries
import Data.Bits (Bits((.&.), (.|.), xor, complement))

-- friends
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Array.Sugar hiding ((!), ignore, shape, size, index)
import qualified Data.Array.Accelerate.Array.Sugar as Sugar
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.AST (Arrays)


-- Array introduction
-- ------------------

-- |Array inlet: makes an array available for processing using the Accelerate
-- language; triggers asynchronous host->device transfer if necessary.
--
use :: (Shape ix, Elt e) => Array ix e -> Acc (Array ix e)
use = Acc . Use

-- |Scalar inlet: injects a scalar (or a tuple of scalars) into a singleton
-- array for use in the Accelerate language.
--
unit :: Elt e => Exp e -> Acc (Scalar e)
unit = Acc . Unit

-- |Replicate an array across one or more dimensions as specified by the
-- *generalised* array index provided as the first argument.
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

-- |Construct a new array by applying a function to each index.
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
generate :: (Shape ix, Elt a)
         => Exp ix
         -> (Exp ix -> Exp a)
         -> Acc (Array ix a)
generate = Acc $$ Generate

-- Shape manipulation
-- ------------------

-- |Change the shape of an array without altering its contents, where
--
-- > precondition: size ix == size ix'
--
reshape :: (Shape ix, Shape ix', Elt e) 
        => Exp ix 
        -> Acc (Array ix' e) 
        -> Acc (Array ix e)
reshape = Acc $$ Reshape

-- Extraction of subarrays
-- -----------------------

-- |Index an array with a *generalised* array index (supplied as the second
-- argument).  The result is a new array (possibly a singleton) containing
-- all dimensions in their entirety.
--
slice :: (Slice slix, Elt e) 
      => Acc (Array (FullShape slix) e) 
      -> Exp slix 
      -> Acc (Array (SliceShape slix) e)
slice = Acc $$ Index

-- Map-like functions
-- ------------------

-- |Apply the given function elementwise to the given array.
-- 
map :: (Shape ix, Elt a, Elt b) 
    => (Exp a -> Exp b) 
    -> Acc (Array ix a)
    -> Acc (Array ix b)
map = Acc $$ Map

-- |Apply the given binary function elementwise to the two arrays.  The extent of the resulting
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

-- |Reduction of the innermost dimension of an array of arbitrary rank.  The first argument needs to
-- be an /associative/ function to enable an efficient parallel implementation.
-- 
fold :: (Shape ix, Elt a)
     => (Exp a -> Exp a -> Exp a) 
     -> Exp a 
     -> Acc (Array (ix:.Int) a)
     -> Acc (Array ix a)
fold = Acc $$$ Fold

-- |Variant of 'fold' that requires the reduced array to be non-empty and doesn't need an default
-- value.
-- 
fold1 :: (Shape ix, Elt a)
      => (Exp a -> Exp a -> Exp a) 
      -> Acc (Array (ix:.Int) a)
      -> Acc (Array ix a)
fold1 = Acc $$ Fold1

-- |Segmented reduction along the innermost dimension.  Performs one individual reduction per
-- segment of the source array.  These reductions proceed in parallel.
--
-- The source array must have at least rank 1.  The 'Segments' array determines the lengths of the
-- logical subarrays, each of which is folded seperately.
--
foldSeg :: (Shape ix, Elt a, Elt i, IsIntegral i)
        => (Exp a -> Exp a -> Exp a)
        -> Exp a
        -> Acc (Array (ix:.Int) a)
        -> Acc (Segments i)
        -> Acc (Array (ix:.Int) a)
foldSeg = Acc $$$$ FoldSeg

-- |Variant of 'foldSeg' that requires /all/ segments of the reduced array to be non-empty and
-- doesn't need a default value.
--
-- The source array must have at least rank 1. The 'Segments' array determines the lengths of the
-- logical subarrays, each of which is folded seperately.
--
fold1Seg :: (Shape ix, Elt a, Elt i, IsIntegral i)
         => (Exp a -> Exp a -> Exp a)
         -> Acc (Array (ix:.Int) a)
         -> Acc (Segments i)
         -> Acc (Array (ix:.Int) a)
fold1Seg = Acc $$$ Fold1Seg

-- Scan functions
-- --------------

-- |'Data.List'-style left-to-right scan, but with the additional restriction that the first
-- argument needs to be an /associative/ function to enable an efficient parallel implementation.
-- The initial value (second argument) may be arbitrary.
--
scanl :: Elt a
      => (Exp a -> Exp a -> Exp a)
      -> Exp a
      -> Acc (Vector a)
      -> Acc (Vector a)
scanl = Acc $$$ Scanl

-- |Variant of 'scanl', where the final result of the reduction is returned separately. 
-- Denotationally, we have
--
-- > scanl' f e arr = (crop 0 (len - 1) res, unit (res!len))
-- >   where
-- >     len = shape arr
-- >     res = scanl f e arr
--
scanl' :: Elt a
       => (Exp a -> Exp a -> Exp a)
       -> Exp a
       -> Acc (Vector a)
       -> (Acc (Vector a), Acc (Scalar a))
scanl' = unpair . Acc $$$ Scanl'

-- |'Data.List' style left-to-right scan without an initial value (aka inclusive scan).  Again, the
-- first argument needs to be an /associative/ function.  Denotationally, we have
--
-- > scanl1 f e arr = crop 1 len res
-- >   where
-- >     len = shape arr
-- >     res = scanl f e arr
--
scanl1 :: Elt a
       => (Exp a -> Exp a -> Exp a)
       -> Acc (Vector a)
       -> Acc (Vector a)
scanl1 = Acc $$ Scanl1

-- |Right-to-left variant of 'scanl'.
--
scanr :: Elt a
      => (Exp a -> Exp a -> Exp a)
      -> Exp a
      -> Acc (Vector a)
      -> Acc (Vector a)
scanr = Acc $$$ Scanr

-- |Right-to-left variant of 'scanl''.
--
scanr' :: Elt a
       => (Exp a -> Exp a -> Exp a)
       -> Exp a
       -> Acc (Vector a)
       -> (Acc (Vector a), Acc (Scalar a))
scanr' = unpair . Acc $$$ Scanr'

-- |Right-to-left variant of 'scanl1'.
--
scanr1 :: Elt a
       => (Exp a -> Exp a -> Exp a)
       -> Acc (Vector a)
       -> Acc (Vector a)
scanr1 = Acc $$ Scanr1

-- Permutations
-- ------------

-- |Forward permutation specified by an index mapping.  The result array is
-- initialised with the given defaults and any further values that are permuted
-- into the result array are added to the current value using the given
-- combination function.
--
-- The combination function must be /associative/.  Elements that are mapped to
-- the magic value 'ignore' by the permutation function are being dropped.
--
permute :: (Shape ix, Shape ix', Elt a)
        => (Exp a -> Exp a -> Exp a)    -- ^combination function
        -> Acc (Array ix' a)            -- ^array of default values
        -> (Exp ix -> Exp ix')          -- ^permutation
        -> Acc (Array ix  a)            -- ^array to be permuted
        -> Acc (Array ix' a)
permute = Acc $$$$ Permute

-- |Backward permutation 
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

-- |Map a binary stencil of an array.  The extent of the resulting array is the intersection of
-- the extents of the two source arrays.
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


-- Composition of array computations
-- ---------------------------------

-- |Pipelining of two array computations.
--
-- Denotationally, we have
--
-- > (acc1 >-> acc2) arrs = let tmp = acc1 arrs in acc2 tmp
--
-- Operationally, the array computations 'acc1' and 'acc2' will not share any sub-computations,
-- neither between each other nor with the environment.  This makes them truly independent stages
-- that only communicate by way of the result of 'acc1' which is being fed as an argument to 'acc2'.
--
infixl 1 >->
(>->) :: (Arrays a, Arrays b, Arrays c) => (Acc a -> Acc b) -> (Acc b -> Acc c) -> (Acc a -> Acc c)
(>->) = Acc $$$ Pipe


-- Flow control constructs
-- -----------------------

-- |An array-level if-then-else construct.
--
cond :: (Arrays a)
     => Exp Bool          -- ^if-condition
     -> Acc a             -- ^then-array
     -> Acc a             -- ^else-array
     -> Acc a
cond = Acc $$$ Acond

-- |Infix version of 'cond'.
--
infix 0 ?|
(?|) :: (Arrays a) => Exp Bool -> (Acc a, Acc a) -> Acc a
c ?| (t, e) = cond c t e


-- Construction and destruction of array pairs
-- -------------------------------------------

-- |Extract the first component of an array pair.
--
fstA :: (Shape sh1, Shape sh2, Elt e1, Elt e2)
     => Acc (Array sh1 e1, Array sh2 e2)
     -> Acc (Array sh1 e1)
fstA = Acc . FstArray


-- |Extract the second component of an array pair.
--
sndA :: (Shape sh1, Shape sh2, Elt e1, Elt e2)
     => Acc (Array sh1 e1, Array sh2 e2)
     -> Acc (Array sh2 e2)
sndA = Acc . SndArray

-- |Create an array pair from two separate arrays.
--
pairA :: (Shape sh1, Shape sh2, Elt e1, Elt e2)
      => Acc (Array sh1 e1)
      -> Acc (Array sh2 e2)
      -> Acc (Array sh1 e1, Array sh2 e2)
pairA = Acc $$ PairArrays



-- Lifting scalar expressions
-- --------------------------

class Lift e where
  type Plain e

  -- |Lift the given value into 'Exp'.  The value may already contain subexpressions in 'Exp'.
  --
  lift :: e -> Exp (Plain e)

class Lift e => Unlift e where

  -- |Unlift the outermost constructor through 'Exp'.  This is only possible if the constructor is
  -- fully determined by its type - i.e., it is a singleton.
  --
  unlift :: Exp (Plain e) -> e

-- instances for indices

instance Lift () where
  type Plain () = ()
  lift _ = Exp $ Tuple NilTup

instance Unlift () where
  unlift _ = ()

instance Lift Z where
  type Plain Z = Z
  lift _ = Exp $ IndexNil

instance Unlift Z where
  unlift _ = Z

instance (Slice (Plain ix), Lift ix) => Lift (ix :. Int) where
  type Plain (ix :. Int) = Plain ix :. Int
  lift (ix:.i) = Exp $ IndexCons (lift ix) (Exp $ Const i)

instance (Slice (Plain ix), Lift ix) => Lift (ix :. All) where
  type Plain (ix :. All) = Plain ix :. All
  lift (ix:.i) = Exp $ IndexCons (lift ix) (Exp $ Const i)

instance (Elt e, Slice (Plain ix), Lift ix) => Lift (ix :. Exp e) where
  type Plain (ix :. Exp e) = Plain ix :. e
  lift (ix:.i) = Exp $ IndexCons (lift ix) i

instance (Elt e, Slice (Plain ix), Unlift ix) => Unlift (ix :. Exp e) where
  unlift e = unlift (Exp $ IndexTail e) :. Exp (IndexHead e)

instance Shape sh => Lift (Any sh) where
 type Plain (Any sh) = Any sh
 lift Any = Exp $ IndexAny

-- instances for numeric types

instance Lift Int where
  type Plain Int = Int
  lift = Exp . Const
  
instance Lift Int8 where
  type Plain Int8 = Int8
  lift = Exp . Const
  
instance Lift Int16 where
  type Plain Int16 = Int16
  lift = Exp . Const
  
instance Lift Int32 where
  type Plain Int32 = Int32
  lift = Exp . Const
  
instance Lift Int64 where
  type Plain Int64 = Int64
  lift = Exp . Const
  
instance Lift Word where
  type Plain Word = Word
  lift = Exp . Const
  
instance Lift Word8 where
  type Plain Word8 = Word8
  lift = Exp . Const
  
instance Lift Word16 where
  type Plain Word16 = Word16
  lift = Exp . Const
  
instance Lift Word32 where
  type Plain Word32 = Word32
  lift = Exp . Const
  
instance Lift Word64 where
  type Plain Word64 = Word64
  lift = Exp . Const

{-  
instance Lift CShort where
  type Plain CShort = CShort
  lift = Exp . Const
  
instance Lift CUShort where
  type Plain CUShort = CUShort
  lift = Exp . Const
  
instance Lift CInt where
  type Plain CInt = CInt
  lift = Exp . Const
  
instance Lift CUInt where
  type Plain CUInt = CUInt
  lift = Exp . Const
  
instance Lift CLong where
  type Plain CLong = CLong
  lift = Exp . Const
  
instance Lift CULong where
  type Plain CULong = CULong
  lift = Exp . Const
  
instance Lift CLLong where
  type Plain CLLong = CLLong
  lift = Exp . Const
  
instance Lift CULLong where
  type Plain CULLong = CULLong
  lift = Exp . Const
 -}
 
instance Lift Float where
  type Plain Float = Float
  lift = Exp . Const

instance Lift Double where
  type Plain Double = Double
  lift = Exp . Const

{-
instance Lift CFloat where
  type Plain CFloat = CFloat
  lift = Exp . Const

instance Lift CDouble where
  type Plain CDouble = CDouble
  lift = Exp . Const
 -}

instance Lift Bool where
  type Plain Bool = Bool
  lift = Exp . Const

instance Lift Char where
  type Plain Char = Char
  lift = Exp . Const

{-
instance Lift CChar where
  type Plain CChar = CChar
  lift = Exp . Const

instance Lift CSChar where
  type Plain CSChar = CSChar
  lift = Exp . Const

instance Lift CUChar where
  
type Plain CUChar = CUChar
  lift = Exp . Const
 -}

-- Instances for tuples

instance (Lift a, Lift b, Elt (Plain a), Elt (Plain b)) => Lift (a, b) where
  type Plain (a, b) = (Plain a, Plain b)
  lift (x, y) = tup2 (lift x, lift y)

instance (Elt a, Elt b) => Unlift (Exp a, Exp b) where
  unlift = untup2

instance (Lift a, Lift b, Lift c, Elt (Plain a), Elt (Plain b), Elt (Plain c)) => Lift (a, b, c) where
  type Plain (a, b, c) = (Plain a, Plain b, Plain c)
  lift (x, y, z) = tup3 (lift x, lift y, lift z)

instance (Elt a, Elt b, Elt c) => Unlift (Exp a, Exp b, Exp c) where
  unlift = untup3

instance (Lift a, Lift b, Lift c, Lift d,
          Elt (Plain a), Elt (Plain b), Elt (Plain c), Elt (Plain d)) 
  => Lift (a, b, c, d) where
  type Plain (a, b, c, d) = (Plain a, Plain b, Plain c, Plain d)
  lift (x, y, z, u) = tup4 (lift x, lift y, lift z, lift u)

instance (Elt a, Elt b, Elt c, Elt d) => Unlift (Exp a, Exp b, Exp c, Exp d) where
  unlift = untup4

instance (Lift a, Lift b, Lift c, Lift d, Lift e,
          Elt (Plain a), Elt (Plain b), Elt (Plain c), Elt (Plain d), Elt (Plain e)) 
  => Lift (a, b, c, d, e) where
  type Plain (a, b, c, d, e) = (Plain a, Plain b, Plain c, Plain d, Plain e)
  lift (x, y, z, u, v) = tup5 (lift x, lift y, lift z, lift u, lift v)

instance (Elt a, Elt b, Elt c, Elt d, Elt e) => Unlift (Exp a, Exp b, Exp c, Exp d, Exp e) where
  unlift = untup5

instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f,
          Elt (Plain a), Elt (Plain b), Elt (Plain c), Elt (Plain d), Elt (Plain e), Elt (Plain f)) 
  => Lift (a, b, c, d, e, f) where
  type Plain (a, b, c, d, e, f) = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f)
  lift (x, y, z, u, v, w) = tup6 (lift x, lift y, lift z, lift u, lift v, lift w)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f) 
  => Unlift (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f) where
  unlift = untup6

instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f, Lift g,
          Elt (Plain a), Elt (Plain b), Elt (Plain c), Elt (Plain d), Elt (Plain e), Elt (Plain f),
          Elt (Plain g)) 
  => Lift (a, b, c, d, e, f, g) where
  type Plain (a, b, c, d, e, f, g) = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f, Plain g)
  lift (x, y, z, u, v, w, r) = tup7 (lift x, lift y, lift z, lift u, lift v, lift w, lift r)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g) 
  => Unlift (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g) where
  unlift = untup7

instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f, Lift g, Lift h,
          Elt (Plain a), Elt (Plain b), Elt (Plain c), Elt (Plain d), Elt (Plain e), Elt (Plain f),
          Elt (Plain g), Elt (Plain h)) 
  => Lift (a, b, c, d, e, f, g, h) where
  type Plain (a, b, c, d, e, f, g, h) 
    = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f, Plain g, Plain h)
  lift (x, y, z, u, v, w, r, s) 
    = tup8 (lift x, lift y, lift z, lift u, lift v, lift w, lift r, lift s)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h) 
  => Unlift (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h) where
  unlift = untup8

instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f, Lift g, Lift h, Lift i,
          Elt (Plain a), Elt (Plain b), Elt (Plain c), Elt (Plain d), Elt (Plain e), Elt (Plain f),
          Elt (Plain g), Elt (Plain h), Elt (Plain i)) 
  => Lift (a, b, c, d, e, f, g, h, i) where
  type Plain (a, b, c, d, e, f, g, h, i) 
    = (Plain a, Plain b, Plain c, Plain d, Plain e, Plain f, Plain g, Plain h, Plain i)
  lift (x, y, z, u, v, w, r, s, t) 
    = tup9 (lift x, lift y, lift z, lift u, lift v, lift w, lift r, lift s, lift t)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i) 
  => Unlift (Exp a, Exp b, Exp c, Exp d, Exp e, Exp f, Exp g, Exp h, Exp i) where
  unlift = untup9

-- Instance for scalar Accelerate expressions

instance Lift (Exp e) where
  type Plain (Exp e) = e
  lift = id

-- Helpers to lift functions

-- |Lift a unary function into 'Exp'.
--
lift1 :: (Unlift e1, Lift e2) 
      => (e1 -> e2) -> Exp (Plain e1) -> Exp (Plain e2)
lift1 f = lift . f . unlift

-- |Lift a binary function into 'Exp'.
--
lift2 :: (Unlift e1, Unlift e2, Lift e3) 
      => (e1 -> e2 -> e3) -> Exp (Plain e1) -> Exp (Plain e2) -> Exp (Plain e3)
lift2 f x y = lift $ f (unlift x) (unlift y)

-- |Lift a unary function to a computation over rank-1 indices.
--
ilift1 :: (Exp Int -> Exp Int) -> Exp (Z :. Int) -> Exp (Z :. Int)
ilift1 f = lift1 (\(Z:.i) -> Z :. f i)

-- |Lift a binary function to a computation over rank-1 indices.
--
ilift2 :: (Exp Int -> Exp Int -> Exp Int) -> Exp (Z :. Int) -> Exp (Z :. Int) -> Exp (Z :. Int)
ilift2 f = lift2 (\(Z:.i) (Z:.j) -> Z :. f i j)


-- Helpers to lift tuples

-- |Extract the first component of a pair.
--
fst :: forall a b. (Elt a, Elt b) => Exp (a, b) -> Exp a
fst e = let (x, _:: Exp b) = unlift e in x

-- |Extract the second component of a pair.
--
snd :: forall a b. (Elt a, Elt b) => Exp (a, b) -> Exp b
snd e = let (_ :: Exp a, y) = unlift e in y

-- |Converts an uncurried function to a curried function.
--
curry :: (Elt a, Elt b) => (Exp (a, b) -> Exp c) -> Exp a -> Exp b -> Exp c
curry f x y = f (lift (x, y))

-- |Converts a curried function to a function on pairs.
--
uncurry :: (Elt a, Elt b) => (Exp a -> Exp b -> Exp c) -> Exp (a, b) -> Exp c
uncurry f t = let (x, y) = unlift t in f x y

-- Helpers to lift shapes and indices

-- |The one index for a rank-0 array.
--
index0 :: Exp Z
index0 = lift Z

-- |Turn an 'Int' expression into a rank-1 indexing expression.
--
index1 :: Exp Int -> Exp (Z:. Int)
index1 = lift . (Z:.)

-- |Turn a rank-1 indexing expression into an 'Int' expression.
--
unindex1 :: Exp (Z:. Int) -> Exp Int
unindex1 ix = let Z:.i = unlift ix in i
  
-- | Creates a rank-2 index from two exp ints.
-- 
index2 :: Exp Int -> Exp Int -> Exp DIM2
index2 i j = lift (Z :. i :. j)

-- | Destructs a rank-2 index to an exp tuple of two ints.
-- 
unindex2 :: Exp DIM2 -> Exp (Int, Int)
unindex2 ix = let Z :. i :. j = unlift ix in lift ((i, j) :: (Exp Int, Exp Int))


-- Conditional expressions
-- -----------------------

-- |Conditional expression.
--
infix 0 ?
(?) :: Elt t => Exp Bool -> (Exp t, Exp t) -> Exp t
c ? (t, e) = Exp $ Cond c t e


-- Array operations with a scalar result
-- -------------------------------------

-- |Expression form that extracts a scalar from an array.
--
infixl 9 !
(!) :: (Shape ix, Elt e) => Acc (Array ix e) -> Exp ix -> Exp e
(!) arr ix = Exp $ IndexScalar arr ix

-- |Extraction of the element in a singleton array.
--
the :: Elt e => Acc (Scalar e) -> Exp e
the = (!index0)

-- |Expression form that yields the shape of an array.
--
shape :: (Shape ix, Elt e) => Acc (Array ix e) -> Exp ix
shape = Exp . Shape

-- |Expression form that yields the size of an array.
--
size :: (Shape ix, Elt e) => Acc (Array ix e) -> Exp Int
size = shapeSize . shape

-- |The same as `size` but not operates directly on a shape without the
--  array.
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
  compare     = error "Prelude.Ord.compare applied to EDSL types"

instance (Elt t, IsNum t, IsIntegral t) => Bits (Exp t) where
  (.&.)      = mkBAnd
  (.|.)      = mkBOr
  xor        = mkBXor
  complement = mkBNot
  -- FIXME: argh, the rest have fixed types in their signatures

shift, shiftL, shiftR :: (Elt t, IsIntegral t) => Exp t -> Exp Int -> Exp t
shift  x i = i ==* 0 ? (x, i <* 0 ? (x `shiftR` (-i), x `shiftL` i))
shiftL     = mkBShiftL
shiftR     = mkBShiftR

rotate, rotateL, rotateR :: (Elt t, IsIntegral t) => Exp t -> Exp Int -> Exp t
rotate  x i = i ==* 0 ? (x, i <* 0 ? (x `rotateR` (-i), x `rotateL` i))
rotateL     = mkBRotateL
rotateR     = mkBRotateR

bit :: (Elt t, IsIntegral t) => Exp Int -> Exp t
bit x = 1 `shiftL` x

setBit, clearBit, complementBit :: (Elt t, IsIntegral t) => Exp t -> Exp Int -> Exp t
x `setBit` i        = x .|. bit i
x `clearBit` i      = x .&. complement (bit i)
x `complementBit` i = x `xor` bit i

testBit :: (Elt t, IsIntegral t) => Exp t -> Exp Int -> Exp Bool
x `testBit` i       = (x .&. bit i) /=* 0


instance (Elt t, IsNum t) => Num (Exp t) where
  (+)         = mkAdd
  (-)         = mkSub
  (*)         = mkMul
  negate      = mkNeg
  abs         = mkAbs
  signum      = mkSig
  fromInteger = constant . fromInteger

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
  fromRational = constant . fromRational

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

-- |Determine the maximum of two scalars.
--
max :: (Elt t, IsScalar t) => Exp t -> Exp t -> Exp t
max = mkMax

-- |Determine the minimum of two scalars.
--
min :: (Elt t, IsScalar t) => Exp t -> Exp t -> Exp t
min = mkMin

-- |Conversions from the RealFrac class
--
truncate :: (Elt a, Elt b, IsFloating a, IsIntegral b) => Exp a -> Exp b
truncate = mkTruncate

round :: (Elt a, Elt b, IsFloating a, IsIntegral b) => Exp a -> Exp b
round = mkRound

floor :: (Elt a, Elt b, IsFloating a, IsIntegral b) => Exp a -> Exp b
floor = mkFloor

ceiling :: (Elt a, Elt b, IsFloating a, IsIntegral b) => Exp a -> Exp b
ceiling = mkCeiling


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

-- |Magic value identifying elements that are ignored in a forward permutation
--
ignore :: Shape ix => Exp ix
ignore = constant Sugar.ignore

