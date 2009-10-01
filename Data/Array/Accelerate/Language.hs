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

  -- * Array and scalar expressions
  Acc, Exp,                                 -- re-exporting from 'Smart'

  -- * Scalar introduction
  constant,                                 -- re-exporting from 'Smart'

  -- * Array introduction
  use, unit,

  -- * Shape manipulation
  reshape,

  -- * Collective array operations
  slice, replicate, zip, unzip, map, zipWith, scan, fold, foldSeg, permute,
  backpermute,
  
  -- * Tuple construction and destruction
  Tuple(..),
  
  -- * Conditional expressions
  (?),
  
  -- * Array operations with a scalar result
  (!), shape,
  
  -- * Instances of Bounded, Enum, Eq, Ord, Bits, Num, Real, Floating,
  --   Fractional, RealFrac, RealFloat

  -- * Methods of H98 classes that we need to redefine as their signatures
  --   change 
  (==*), (/=*), (<*), (<=*), (>*), (>=*), max, min,

  -- * Standard functions that we need to redefine as their signatures change
  (&&*), (||*), not,
  
  -- * Conversions
  boolToInt,
  
  -- * Constants
  ignore

) where

-- avoid clashes with Prelude functions
import Prelude   hiding (replicate, zip, unzip, map, zipWith, filter, max, min,
                         not, const)
import qualified Prelude

-- standard libraries
import Data.Bits

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

zip :: (Ix dim, Elem a, Elem b) 
    => Acc (Array dim a)
    -> Acc (Array dim b)
    -> Acc (Array dim (a, b))
zip = zipWith (\x y -> tuple (x, y))

unzip :: forall a b dim. (Ix dim, Elem a, Elem b) 
      => Acc (Array dim (a, b))
      -> (Acc (Array dim a), Acc (Array dim b))
unzip arr = (map fst arr, map snd arr)
  where
    fst :: Exp (a, b) -> Exp a
    fst e = let (x, _:: Exp b) = untuple e in x

    snd :: Exp (a, b) -> Exp b
    snd e = let (_ :: Exp a, y) = untuple e in y

map :: (Ix dim, Elem a, Elem b) 
    => (Exp a -> Exp b) 
    -> Acc (Array dim a)
    -> Acc (Array dim b)
map = Map

zipWith :: (Ix dim, Elem a, Elem b, Elem c)
        => (Exp a -> Exp b -> Exp c) 
        -> Acc (Array dim a)
        -> Acc (Array dim b)
        -> Acc (Array dim c)
zipWith = ZipWith

scan :: Elem a 
     => (Exp a -> Exp a -> Exp a) 
     -> Exp a 
     -> Acc (Vector a)
     -> (Acc (Vector a), Acc (Scalar a))
scan f e arr = unpair (Scan f e arr)

fold :: (Ix dim, Elem a)
     => (Exp a -> Exp a -> Exp a) 
     -> Exp a 
     -> Acc (Array dim a)
     -> Acc (Scalar a)
fold = Fold

foldSeg :: Elem a 
        => (Exp a -> Exp a -> Exp a) 
        -> Exp a 
        -> Acc (Vector a)
        -> Acc Segments
        -> Acc (Vector a)
foldSeg = FoldSeg

permute :: (Ix dim, Ix dim', Elem a)
        => (Exp a -> Exp a -> Exp a) 
        -> Acc (Array dim' a) 
        -> (Exp dim -> Exp dim') 
        -> Acc (Array dim  a) 
        -> Acc (Array dim' a)
permute = Permute

backpermute :: (Ix dim, Ix dim', Elem a)
            => Exp dim' 
            -> (Exp dim' -> Exp dim) 
            -> Acc (Array dim  a) 
            -> Acc (Array dim' a)
backpermute = Backpermute


-- Tuples
-- ------

class Tuple tup where
  type TupleT tup
  tuple   :: tup -> TupleT tup
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


-- Conditional expressions
-- -----------------------

infix 0 ?
(?) :: Elem t => Exp Bool -> (Exp t, Exp t) -> Exp t
c ? (t, e) = Cond c t e


-- Array operations with a scalar result
-- -------------------------------------

infixl 9 !
(!) :: (Ix dim, Elem e) => Acc (Array dim e) -> Exp dim -> Exp e
(!) = IndexScalar

shape :: Ix dim => Acc (Array dim e) -> Exp dim
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

instance (Elem t, IsScalar t) => Prelude.Eq (Exp t)
  -- FIXME: instance makes no sense with standard signatures

instance (Elem t, IsScalar t) => Prelude.Ord (Exp t)
  -- FIXME: instance makes no sense with standard signatures

instance (Elem t, IsNum t, IsIntegral t) => Bits (Exp t) where
  (.&.)      = mkBAnd
  (.|.)      = mkBOr
  xor        = mkBXor
  complement = mkBNot
  -- FIXME: argh, the rest have fixed types in their signatures

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
  -- FIXME: add other ops

instance (Elem t, IsFloating t) => Fractional (Exp t) where
  (/)          = mkFDiv
  recip        = mkRecip
  fromRational = exp . fromRational
  -- FIXME: add other ops

instance (Elem t, IsFloating t) => RealFrac (Exp t)
  -- FIXME: add ops

instance (Elem t, IsFloating t) => RealFloat (Exp t)
  -- FIXME: add ops


-- Methods from H98 classes, where we need other signatures
-- --------------------------------------------------------

infix 4 ==*, /=*, <*, <=*, >*, >=*

(==*) :: (Elem t, IsScalar t) => Exp t -> Exp t -> Exp Bool
(==*) = mkEq

(/=*) :: (Elem t, IsScalar t) => Exp t -> Exp t -> Exp Bool
(/=*) = mkNEq

-- compare :: a -> a -> Ordering  -- we have no enumerations at the moment
-- compare = ...

(<*) :: (Elem t, IsScalar t) => Exp t -> Exp t -> Exp Bool
(<*)  = mkLt

(>=*) :: (Elem t, IsScalar t) => Exp t -> Exp t -> Exp Bool
(>=*) = mkGtEq

(>*) :: (Elem t, IsScalar t) => Exp t -> Exp t -> Exp Bool
(>*)  = mkGt

(<=*) :: (Elem t, IsScalar t) => Exp t -> Exp t -> Exp Bool
(<=*) = mkLtEq

max :: (Elem t, IsScalar t) => Exp t -> Exp t -> Exp t
max = mkMax

min :: (Elem t, IsScalar t) => Exp t -> Exp t -> Exp t
min = mkMin


-- Non-overloaded standard functions, where we need other signatures
-- -----------------------------------------------------------------

infixr 3 &&*
(&&*) :: Exp Bool -> Exp Bool -> Exp Bool
(&&*) = mkLAnd

infixr 2 ||*
(||*) :: Exp Bool -> Exp Bool -> Exp Bool
(||*) = mkLOr

not :: Exp Bool -> Exp Bool
not = mkLNot


-- Conversions
-- -----------

boolToInt :: Exp Bool -> Exp Int
boolToInt = mkBoolToInt


-- Constants
-- ---------

ignore :: Ix dim => Exp dim
ignore = constant Sugar.ignore
