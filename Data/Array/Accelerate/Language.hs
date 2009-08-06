{-# LANGUAGE FlexibleContexts, TypeFamilies, RankNTypes, ScopedTypeVariables #-}

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
  Acc, Exp,             -- re-exporting from 'Smart'

  -- * Scalar introduction
  exp,                  -- re-exporting from 'Smart'

  -- * Array introduction
  use, unit,

  -- * Shape manipulation
  reshape,

  -- * Collective array operations
  replicate, zip, map, zipWith, filter, scan, fold, permute, backpermute,

  -- * Instances of Bounded, Enum, Eq, Ord, Bits, Num, Real, Floating,
  --   Fractional, RealFrac, RealFloat

  -- * Methods of H98 classes that we need to redefine as their signatures
  --   change 
  (==*), (/=*), (<*), (<=*), (>*), (>=*), max, min,

  -- * Standard functions that we need to redefine as their signatures change
  (&&*), (||*), not

) where

-- avoid clashes with Prelude functions
import Prelude   hiding (replicate, zip, map, zipWith, filter, max, min, not, 
                         exp)
import qualified Prelude

-- standard libraries
import Control.Monad
import Data.Bits

-- friends
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Pretty


infixr 2 ||*
infixr 3 &&*
infix  4 ==*, /=*, <*, <=*, >*, >=*
infixl 9 !


-- |Collective operations
-- ----------------------

use :: (Ix dim, Elem e) => Array dim e -> Acc (Array dim e)
use = Use

unit :: Elem e => Exp e -> Acc (Scalar e)
unit = Unit

reshape :: (Ix dim, Ix dim', Elem e) 
        => Exp dim 
        -> Acc (Array dim' e) 
        -> Acc (Array dim e)
reshape = Reshape

replicate :: forall slix e. (SliceIx slix, Elem e) 
          => Exp slix 
          -> Acc (Array (Slice    slix) e) 
          -> Acc (Array (SliceDim slix) e)
replicate = Replicate (undefined::slix) (undefined::e)

(!) :: forall slix e. (SliceIx slix, Elem e) 
    => Acc (Array (SliceDim slix) e) 
    -> Exp slix 
    -> Acc (Array (Slice slix) e)
(!) = Index (undefined::slix) (undefined::e) 

zip :: (Ix dim, Elem a, Elem b) 
    => Acc (Array dim a)
    -> Acc (Array dim b)
    -> Acc (Array dim (a, b))
zip = zipWith (\x y -> x `Pair` y)

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

filter :: Elem a 
       => (Exp a -> Exp Bool) 
       -> Acc (Vector a) 
       -> Acc (Vector a)
filter = Filter

scan :: Elem a 
     => (Exp a -> Exp a -> Exp a) 
     -> Exp a 
     -> Acc (Vector a)
     -> Acc (Scalar a, Vector a)
scan = Scan

fold :: Elem a 
     => (Exp a -> Exp a -> Exp a) 
     -> Exp a 
     -> Vector a 
     -> Acc (Scalar a)
fold f e arr = error "fold f e arr = scan f e arr >>= return . fst"

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


-- |Instances of all relevant H98 classes
-- --------------------------------------

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
  fromInteger = exp . fromInteger

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


-- |Methods from H98 classes, where we need other signatures
-- ---------------------------------------------------------

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


-- |Non-overloaded standard functions, where we need other signatures
-- ------------------------------------------------------------------

(&&*) :: Exp Bool -> Exp Bool -> Exp Bool
(&&*) = mkLAnd

(||*) :: Exp Bool -> Exp Bool -> Exp Bool
(||*) = mkLOr

not :: Exp Bool -> Exp Bool
not = mkLNot

