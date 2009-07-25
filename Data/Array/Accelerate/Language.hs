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

  -- * Array processing computation monad
  AP, Arr,              -- re-exporting from 'Smart'

  -- * Expressions
  Exp, exp,             -- re-exporting from 'Smart'

  -- * Slice expressions
  All(..),              -- re-exporting from 'Sugar'

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
import Data.Array.Accelerate.AST          hiding (Exp, OpenExp(..), Arr, Scalar)
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Pretty


infixr 2 ||*
infixr 3 &&*
infix  4 ==*, /=*, <*, <=*, >*, >=*
infixl 9 !


-- |Collective operations
-- ----------------------

use :: (Ix dim, Elem e) => Array dim e -> AP (Arr dim e)
use array = wrapComp $ Use (convertArray array)

unit :: Elem e => Exp e -> AP (Scalar e)
unit e = wrapComp $ Unit (convertExp e)

reshape :: forall dim dim' e. (Ix dim, Ix dim', Elem e) 
        => Exp dim -> Arr dim' e -> AP (Arr dim e)
reshape e arr = wrapComp $ Reshape (convertExp e)
                                   (convertArr arr)

replicate :: forall slix e. (SliceIx slix, Elem e) 
          => Exp slix -> Arr (Slice slix) e -> AP (Arr (SliceDim slix) e)
replicate ix arr = wrapComp $ 
                     mkReplicate (undefined::slix) (undefined::e)
                                 (convertExp ix) (convertArr arr)

(!) :: forall slix e. (SliceIx slix, Elem e) 
    => Arr (SliceDim slix) e -> Exp slix -> AP (Arr (Slice slix) e)
arr ! ix = wrapComp $ 
             mkIndex (undefined::slix) (undefined::e) 
                     (convertArr arr) (convertExp ix)

zip :: forall dim a b. (Ix dim, Elem a, Elem b) 
    => Arr dim a -> Arr dim b -> AP (Arr dim (a, b))
zip = zipWith (\x y -> x `Pair` y)
{-
zip arr1 arr2 
  = wrapComp $ 
      mkZip (undefined::dim) (undefined::a) (undefined::b) 
            (convertArr arr1) (convertArr arr2)
 -}

map :: (Ix dim, Elem a, Elem b) 
    => (Exp a -> Exp b) -> Arr dim a -> AP (Arr dim b)
map f arr = wrapComp $ Map (convertFun1 f) (convertArr arr)

zipWith :: (Ix dim, Elem a, Elem b, Elem c)
        => (Exp a -> Exp b -> Exp c) -> Arr dim a -> Arr dim b -> AP (Arr dim c)
zipWith f arr1 arr2 
--  = zip arr1 arr2 >>= map (\xy -> f (Fst xy) (Snd xy))
  = wrapComp $ ZipWith (convertFun2 f) (convertArr arr1) (convertArr arr2)

filter :: Elem a => (Exp a -> Exp Bool) -> Arr DIM1 a -> AP (Arr DIM1 a)
filter p arr = wrapComp $ Filter (convertFun1 p) (convertArr arr)

scan :: Elem a 
     => (Exp a -> Exp a -> Exp a) -> Exp a -> Vector a -> AP (Scalar a, Vector a)
scan f e arr = wrapComp2 $ Scan (convertFun2 f) (convertExp e) (convertArr arr)

fold :: Elem a => (Exp a -> Exp a -> Exp a) -> Exp a -> Vector a -> AP (Scalar a)
fold f e arr
  = scan f e arr >>= return . fst

permute :: (Ix dim, Ix dim', Elem a)
        => (Exp a -> Exp a -> Exp a) -> Arr dim' a -> (Exp dim -> Exp dim') 
        -> Arr dim a -> AP (Arr dim' a)
permute f dftArr perm arr 
  = wrapComp $ Permute (convertFun2 f) (convertArr dftArr) (convertFun1 perm)
                       (convertArr arr)

backpermute :: (Ix dim, Ix dim', Elem a)
            => Exp dim' -> (Exp dim' -> Exp dim) -> Arr dim a -> AP (Arr dim' a)
backpermute newDim perm arr 
  = wrapComp $ 
      Backpermute (convertExp newDim) (convertFun1 perm) (convertArr arr)


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

