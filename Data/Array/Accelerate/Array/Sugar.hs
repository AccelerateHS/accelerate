{-# LANGUAGE GADTs, TypeFamilies, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}  -- for instance Slice sl

-- |Embedded array processing language: user-visible array operations
--
--  Copyright (c) 2009 Manuel M T Chakravarty, Gabriele Keller, Sean Lee
--
--  License: BSD3
--
--- Description ---------------------------------------------------------------
--

module Data.Array.Accelerate.Array.Sugar (

  -- * Array shapes
  DIM0, DIM1, DIM2, DIM3, DIM4, DIM5,

  -- * Array indexing and slicing
  ShapeElem, Shape(..), Ix(..), Slice(..)

) where

-- friends
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Representation


-- |Shorthand for common shape types
--
type DIM0 = ()
type DIM1 = (Int)
type DIM2 = (Int, Int)
type DIM3 = (Int, Int, Int)
type DIM4 = (Int, Int, Int, Int)
type DIM5 = (Int, Int, Int, Int, Int)

-- |Shape constraints and indexing
-- -

-- |Shape elements
--
class Shape she => ShapeElem she
instance ShapeElem Int
instance ShapeElem All

class Shape sh where
  type ToShapeRepr sh :: *
  toShapeRepr   :: sh -> ToShapeRepr sh
  fromShapeRepr :: ToShapeRepr sh -> sh

instance Shape () where
  type ToShapeRepr () = ()
  toShapeRepr   = id
  fromShapeRepr = id

instance Shape Int where
  type ToShapeRepr Int = ((), Int)
  toShapeRepr i = ((), i)
  fromShapeRepr ((), i) = i

instance Shape All where
  type ToShapeRepr All = ((), All)
  toShapeRepr a = ((), a)
  fromShapeRepr ((), a) = a

instance (ShapeElem a, ShapeElem b) => Shape (a, b) where
  type ToShapeRepr (a, b) = (ToShapeRepr a, b)
  toShapeRepr (a, b) = (toShapeRepr a, b)
  fromShapeRepr (repr, b) = let a = fromShapeRepr repr in (a, b)

instance (ShapeElem a, ShapeElem b, ShapeElem c) => Shape (a, b, c) where
  type ToShapeRepr (a, b, c) = (ToShapeRepr (a, b), c)
  toShapeRepr (a, b, c) = (toShapeRepr (a, b), c)
  fromShapeRepr (repr, c) = let (a, b) = fromShapeRepr repr in (a, b, c)

instance (ShapeElem a, ShapeElem b, ShapeElem c, ShapeElem d) 
  => Shape (a, b, c, d) where
  type ToShapeRepr (a, b, c, d) = (ToShapeRepr (a, b, c), d)
  toShapeRepr (a, b, c, d) = (toShapeRepr (a, b, c), d)
  fromShapeRepr (repr, d) = let (a, b, c) = fromShapeRepr repr in (a, b, c, d)

instance (ShapeElem a, ShapeElem b, ShapeElem c, ShapeElem d, ShapeElem e) 
  => Shape (a, b, c, d, e) where
  type ToShapeRepr (a, b, c, d, e) = (ToShapeRepr (a, b, c, d), e)
  toShapeRepr (a, b, c, d, e) = (toShapeRepr (a, b, c, d), e)
  fromShapeRepr (repr, e) 
    = let (a, b, c, d) = fromShapeRepr repr in (a, b, c, d, e)

type family FromShapeRepr shr :: *
type instance FromShapeRepr () = ()
type instance FromShapeRepr ((), a) = a
type instance FromShapeRepr (((), a), b) = (a, b)
type instance FromShapeRepr ((((), a), b), c) = (a, b, c)
type instance FromShapeRepr (((((), a), b), c), d) = (a, b, c, d)
type instance FromShapeRepr ((((((), a), b), c), d), e) = (a, b, c, d, e)

-- |Indices as n-tuples
--
class (Shape ix, IxRepr (ToShapeRepr ix)) => Ix ix where
  dim   :: ix -> Int           -- ^number of dimensions (>= 0)
  size  :: ix -> Int           -- ^for a *shape* yield the total number of 
                               -- elements in that array
  index :: ix -> ix -> Int     -- ^corresponding index into a linear, row-major 
                               -- representation of the array (first argument
                               -- is the shape)
  -- FIXME: we might want an unsafeIndex, too

  dim         = dimRepr . toShapeRepr
  size        = sizeRepr . toShapeRepr
  index sh ix = indexRepr (toShapeRepr sh) (toShapeRepr ix)

instance Ix ()
instance Ix (Int)
instance Ix (Int, Int)
instance Ix (Int, Int, Int)
instance Ix (Int, Int, Int, Int)
instance Ix (Int, Int, Int, Int, Int)

-- Slices -aka generalised indices- as n-tuples
--
class Slice sl where
  type CoSlice sl :: *

instance (Shape sl, SliceRepr (ToShapeRepr sl)) => Slice sl where
  type CoSlice sl = FromShapeRepr (CoSliceRepr (ToShapeRepr sl))

{-
class (Shape sl, SliceRepr (ShapeRepr sl)) => Slice sl where
  type CoSlice sl :: *

instance Slice () where
  type CoSlice () = ()

instance Slice Int where
  type CoSlice Int = ()

instance Slice All where
  type CoSlice All = ()

instance (ShapeElem a, ShapeElem b) => Slice (a, b) where
  type CoSlice (a, b) = 
 -}

