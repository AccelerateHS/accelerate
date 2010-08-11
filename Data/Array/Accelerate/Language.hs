{-# LANGUAGE FlexibleContexts, TypeFamilies, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

-- |Embedded array processing language: user-visible language
--
--  Copyright (c) 2009 Manuel M T Chakravarty, Gabriele Keller, Sean Lee
--
--  License: BSD3
--
--- Description ---------------------------------------------------------------
--
-- We use the dictionary view of overloaded operations (such as arithmetic and
-- bit manipulation) to reify such expressions.  With non-overloaded
-- operations (such as, the logical connectives) and partially overloaded
-- operations (such as comparisons), we use the standard operator names with a
-- '*' attached.  We keep the standard alphanumeric names as they can be
-- easily qualified.

module Data.Array.Accelerate.Language (

  -- ** Array and scalar expressions
  Acc, Exp,                                 -- re-exporting from 'Smart'

  -- ** Scalar introduction
  constant,                                 -- re-exporting from 'Smart'

  -- ** Array introduction
  use, unit,

  -- ** Shape manipulation
  reshape,

  -- ** Collective array operations
  slice, replicate, zip, unzip, map, zipWith, scanl, scanr, fold, foldSeg,
  permute, backpermute,
  
  -- ** Tuple construction and destruction
  Tuple(..), fst, snd, curry, uncurry,
  
  -- ** Conditional expressions
  (?),
  
  -- ** Array operations with a scalar result
  (!), shape,
  
  -- ** Methods of H98 classes that we need to redefine as their signatures change
  (==*), (/=*), (<*), (<=*), (>*), (>=*), max, min,
  bit, setBit, clearBit, complementBit, testBit,
  shift,  shiftL,  shiftR,
  rotate, rotateL, rotateR,

  -- ** Standard functions that we need to redefine as their signatures change
  (&&*), (||*), not,
  
  -- ** Conversions
  boolToInt, intToFloat, roundFloatToInt, truncateFloatToInt,

  -- ** Constants
  ignore

  -- ** Instances of Bounded, Enum, Eq, Ord, Bits, Num, Real, Floating,
  --    Fractional, RealFrac, RealFloat

) where

-- avoid clashes with Prelude functions
import Prelude   hiding (replicate, zip, unzip, map, scanl, scanr, zipWith,
                         filter, max, min, not, const, fst, snd, curry, uncurry)

-- standard libraries
import Data.Bits (Bits((.&.), (.|.), xor, complement))

-- friends
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Sugar hiding ((!), ignore, shape)
import qualified Data.Array.Accelerate.Array.Sugar as Sugar
import Data.Array.Accelerate.Smart


-- Collective operations
-- ---------------------

-- |Array inlet: makes an array available for processing using the Accelerate
-- language; triggers asynchronous host->device transfer if necessary.
--
use :: (Ix dim, Elem e) => Array dim e -> Acc (Array dim e)
use = Use

-- |Scalar inlet: injects a scalar (or a tuple of scalars) into a singleton
-- array for use in the Accelerate language.
--
unit :: Elem e => Exp e -> Acc (Scalar e)
unit = Unit

-- |Change the shape of an array without altering its contents, where
--
-- > precondition: size dim == size dim'
--
reshape :: (Ix dim, Ix dim', Elem e) 
        => Exp dim 
        -> Acc (Array dim' e) 
        -> Acc (Array dim e)
reshape = Reshape

-- |Replicate an array across one or more dimensions as specified by the
-- *generalised* array index provided as the first argument.
--
-- For example, assuming 'arr' is a vector (one-dimensional array),
--
-- > replicate (2, All, 3) arr
--
-- yields a three dimensional array, where 'arr' is replicated twice across the
-- first and three times across the third dimension.
--
replicate :: forall slix e. (SliceIx slix, Elem e) 
          => Exp slix 
          -> Acc (Array (Slice    slix) e) 
          -> Acc (Array (SliceDim slix) e)
replicate = Replicate

-- |Index an array with a *generalised* array index (supplied as the second
-- argument).  The result is a new array (possibly a singleton) containing
-- all dimensions in their entirety.
--
slice :: forall slix e. (SliceIx slix, Elem e) 
      => Acc (Array (SliceDim slix) e) 
      -> Exp slix 
      -> Acc (Array (Slice slix) e)
slice = Index

-- |Combine the elements of two arrays pairwise.  The shape of the result is 
-- the intersection of the two argument shapes.
--
zip :: (Ix dim, Elem a, Elem b) 
    => Acc (Array dim a)
    -> Acc (Array dim b)
    -> Acc (Array dim (a, b))
zip = zipWith (\x y -> tuple (x, y))

-- |The converse of 'zip', but the shape of the two results is identical to the
-- shape of the argument.
-- 
unzip :: (Ix dim, Elem a, Elem b)
      => Acc (Array dim (a, b))
      -> (Acc (Array dim a), Acc (Array dim b))
unzip arr = (map fst arr, map snd arr)

-- |Apply the given function elementwise to the given array.
-- 
map :: (Ix dim, Elem a, Elem b) 
    => (Exp a -> Exp b) 
    -> Acc (Array dim a)
    -> Acc (Array dim b)
map = Map

-- |Apply the given binary function elementwise to the two arrays.
--
zipWith :: (Ix dim, Elem a, Elem b, Elem c)
        => (Exp a -> Exp b -> Exp c) 
        -> Acc (Array dim a)
        -> Acc (Array dim b)
        -> Acc (Array dim c)
zipWith = ZipWith

-- |Prescan of a vector.  The type \'a\' together with the binary function
-- (first argument) and value (second argument) must form a monoid; i.e., the
-- function must be /associative/ and the value must be its /neutral element/.
--
-- The resulting vector of prescan values has the same size as the argument 
-- vector.  The resulting scalar is the reduction value.
--
scanl :: Elem a
      => (Exp a -> Exp a -> Exp a)
      -> Exp a
      -> Acc (Vector a)
      -> (Acc (Vector a), Acc (Scalar a))
scanl f e arr = unpair (Scanl f e arr)

-- |The right-to-left dual of 'scanl'.
--
scanr :: Elem a
      => (Exp a -> Exp a -> Exp a)
      -> Exp a
      -> Acc (Vector a)
      -> (Acc (Vector a), Acc (Scalar a))
scanr f e arr = unpair (Scanr f e arr)

-- |Reduction of an array.  The type 'a' together with the binary function
-- (first argument) and value (second argument) must form a monoid; i.e., the 
-- function must be /associative/ and the value must be its /neutral element/.
-- 
fold :: (Ix dim, Elem a)
     => (Exp a -> Exp a -> Exp a) 
     -> Exp a 
     -> Acc (Array dim a)
     -> Acc (Scalar a)
fold = Fold

-- |Segmented reduction.
--
foldSeg :: Elem a 
        => (Exp a -> Exp a -> Exp a) 
        -> Exp a 
        -> Acc (Vector a)
        -> Acc Segments
        -> Acc (Vector a)
foldSeg = FoldSeg

-- |Forward permutation specified by an index mapping.  The result array is
-- initialised with the given defaults and any further values that are permuted
-- into the result array are added to the current value using the given
-- combination function.
--
-- The combination function must be /associative/.  Elements that are mapped to
-- the magic value 'ignore' by the permutation function are being dropped.
--
permute :: (Ix dim, Ix dim', Elem a)
        => (Exp a -> Exp a -> Exp a)    -- ^combination function
        -> Acc (Array dim' a)           -- ^array of default values
        -> (Exp dim -> Exp dim')        -- ^permutation
        -> Acc (Array dim  a)           -- ^permuted array
        -> Acc (Array dim' a)
permute = Permute

-- |Backward permutation 
backpermute :: (Ix dim, Ix dim', Elem a)
            => Exp dim'                 -- ^shape of the result array
            -> (Exp dim' -> Exp dim)    -- ^permutation
            -> Acc (Array dim  a)       -- ^permuted array
            -> Acc (Array dim' a)
backpermute = Backpermute


-- Tuples
-- ------

class Tuple tup where
  type TupleT tup

  -- |Turn a tuple of scalar expressions into a scalar expressions that yields
  -- a tuple.
  -- 
  tuple   :: tup -> TupleT tup
  
  -- |Turn a scalar expression that yields a tuple into a tuple of scalar
  -- expressions.
  untuple :: TupleT tup -> tup
  
instance (Elem a, Elem b) => Tuple (Exp a, Exp b) where
  type TupleT (Exp a, Exp b) = Exp (a, b)
  tuple   = tup2
  untuple = untup2

instance (Elem a, Elem b, Elem c) => Tuple (Exp a, Exp b, Exp c) where
  type TupleT (Exp a, Exp b, Exp c) = Exp (a, b, c)
  tuple   = tup3
  untuple = untup3

instance (Elem a, Elem b, Elem c, Elem d) 
  => Tuple (Exp a, Exp b, Exp c, Exp d) where
  type TupleT (Exp a, Exp b, Exp c, Exp d) = Exp (a, b, c, d)
  tuple   = tup4
  untuple = untup4

instance (Elem a, Elem b, Elem c, Elem d, Elem e) 
  => Tuple (Exp a, Exp b, Exp c, Exp d, Exp e) where
  type TupleT (Exp a, Exp b, Exp c, Exp d, Exp e) = Exp (a, b, c, d, e)
  tuple   = tup5
  untuple = untup5


-- |Extract the first component of a pair
--
fst :: forall a b. (Elem a, Elem b) => Exp (a, b) -> Exp a
fst e = let (x, _:: Exp b) = untuple e in x

-- |Extract the second component of a pair
snd :: forall a b. (Elem a, Elem b) => Exp (a, b) -> Exp b
snd e = let (_ :: Exp a, y) = untuple e in y

-- |Converts an uncurried function to a curried function
--
curry :: (Elem a, Elem b) => (Exp (a,b) -> Exp c) -> Exp a -> Exp b -> Exp c
curry f x y = f (tuple (x,y))

-- |Converts a curried function to a function on pairs
--
uncurry :: (Elem a, Elem b) => (Exp a -> Exp b -> Exp c) -> Exp (a,b) -> Exp c
uncurry f t = let (x,y) = untuple t in f x y


-- Conditional expressions
-- -----------------------

-- |Conditional expression.
--
infix 0 ?
(?) :: Elem t => Exp Bool -> (Exp t, Exp t) -> Exp t
c ? (t, e) = Cond c t e


-- Array operations with a scalar result
-- -------------------------------------

-- |Expression form that extracts a scalar from an array.
--
infixl 9 !
(!) :: (Ix dim, Elem e) => Acc (Array dim e) -> Exp dim -> Exp e
(!) = IndexScalar

shape :: (Ix dim, Elem dim) => Acc (Array dim e) -> Exp dim
shape = Shape


-- Instances of all relevant H98 classes
-- -------------------------------------

instance (Elem t, IsBounded t) => Bounded (Exp t) where
  minBound = mkMinBound
  maxBound = mkMaxBound

instance (Elem t, IsScalar t) => Enum (Exp t)
--  succ = mkSucc
--  pred = mkPred
  -- FIXME: ops

instance (Elem t, IsScalar t) => Prelude.Eq (Exp t) where
  -- FIXME: instance makes no sense with standard signatures
  (==)        = error "Prelude.Eq.== applied to EDSL types"

instance (Elem t, IsScalar t) => Prelude.Ord (Exp t) where
  -- FIXME: instance makes no sense with standard signatures
  compare     = error "Prelude.Ord.compare applied to EDSL types"

instance (Elem t, IsNum t, IsIntegral t) => Bits (Exp t) where
  (.&.)      = mkBAnd
  (.|.)      = mkBOr
  xor        = mkBXor
  complement = mkBNot
  -- FIXME: argh, the rest have fixed types in their signatures

shift, shiftL, shiftR :: (Elem t, IsIntegral t) => Exp t -> Exp Int -> Exp t
shift  x i = i ==* 0 ? (x, i <* 0 ? (x `shiftR` (-i), x `shiftL` i))
shiftL     = mkBShiftL
shiftR     = mkBShiftR

rotate, rotateL, rotateR :: (Elem t, IsIntegral t) => Exp t -> Exp Int -> Exp t
rotate  x i = i ==* 0 ? (x, i <* 0 ? (x `rotateR` (-i), x `rotateL` i))
rotateL     = mkBRotateL
rotateR     = mkBRotateR

bit :: (Elem t, IsIntegral t) => Exp Int -> Exp t
bit x = 1 `shiftL` x

setBit, clearBit, complementBit :: (Elem t, IsIntegral t) => Exp t -> Exp Int -> Exp t
x `setBit` i        = x .|. bit i
x `clearBit` i      = x .&. complement (bit i)
x `complementBit` i = x `xor` bit i

testBit :: (Elem t, IsIntegral t) => Exp t -> Exp Int -> Exp Bool
x `testBit` i       = (x .&. bit i) /=* 0


instance (Elem t, IsNum t) => Num (Exp t) where
  (+)         = mkAdd
  (-)         = mkSub
  (*)         = mkMul
  negate      = mkNeg
  abs         = mkAbs
  signum      = mkSig
  fromInteger = constant . fromInteger

instance (Elem t, IsNum t) => Real (Exp t)
  -- FIXME: Why did we include this class?  We won't need `toRational' until
  --   we support rational numbers in AP computations.

instance (Elem t, IsIntegral t) => Integral (Exp t) where
  quot = mkQuot
  rem  = mkRem
  div  = mkIDiv
  mod  = mkMod
--  quotRem =
--  divMod  =
--  toInteger =  -- makes no sense

instance (Elem t, IsFloating t) => Floating (Exp t) where
  pi  = mkPi
  sin = mkSin
  cos = mkCos
  tan = mkTan
  asin = mkAsin
  acos = mkAcos
  atan = mkAtan
  asinh = mkAsinh
  acosh = mkAcosh
  atanh = mkAtanh
  exp = mkExpFloating
  sqrt = mkSqrt
  log = mkLog
  (**) = mkFPow
  logBase = mkLogBase
  -- FIXME: add other ops

instance (Elem t, IsFloating t) => Fractional (Exp t) where
  (/)          = mkFDiv
  recip        = mkRecip
  fromRational = constant . fromRational
  -- FIXME: add other ops

instance (Elem t, IsFloating t) => RealFrac (Exp t)
  -- FIXME: add ops

instance (Elem t, IsFloating t) => RealFloat (Exp t) where
  atan2 = mkAtan2
  -- FIXME: add ops


-- Methods from H98 classes, where we need other signatures
-- --------------------------------------------------------

infix 4 ==*, /=*, <*, <=*, >*, >=*

-- |Equality lifted into Accelerate expressions.
--
(==*) :: (Elem t, IsScalar t) => Exp t -> Exp t -> Exp Bool
(==*) = mkEq

-- |Inequality lifted into Accelerate expressions.
--
(/=*) :: (Elem t, IsScalar t) => Exp t -> Exp t -> Exp Bool
(/=*) = mkNEq

-- compare :: a -> a -> Ordering  -- we have no enumerations at the moment
-- compare = ...

-- |Smaller-than lifted into Accelerate expressions.
--
(<*) :: (Elem t, IsScalar t) => Exp t -> Exp t -> Exp Bool
(<*)  = mkLt

-- |Greater-or-equal lifted into Accelerate expressions.
--
(>=*) :: (Elem t, IsScalar t) => Exp t -> Exp t -> Exp Bool
(>=*) = mkGtEq

-- |Greater-than lifted into Accelerate expressions.
--
(>*) :: (Elem t, IsScalar t) => Exp t -> Exp t -> Exp Bool
(>*)  = mkGt

-- |Smaller-or-equal lifted into Accelerate expressions.
--
(<=*) :: (Elem t, IsScalar t) => Exp t -> Exp t -> Exp Bool
(<=*) = mkLtEq

-- |Determine the maximum of two scalars.
--
max :: (Elem t, IsScalar t) => Exp t -> Exp t -> Exp t
max = mkMax

-- |Determine the minimum of two scalars.
--
min :: (Elem t, IsScalar t) => Exp t -> Exp t -> Exp t
min = mkMin


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

-- |Convert an Int to a Float
intToFloat :: Exp Int -> Exp Float
intToFloat = mkIntFloat

-- |Round Float to Int
roundFloatToInt :: Exp Float -> Exp Int
roundFloatToInt = mkRoundFloatInt

-- |Truncate Float to Int
truncateFloatToInt :: Exp Float -> Exp Int
truncateFloatToInt = mkTruncFloatInt


-- Constants
-- ---------

-- |Magic value identifying elements that are ignored in a forward permutation
--
ignore :: Ix dim => Exp dim
ignore = constant Sugar.ignore
