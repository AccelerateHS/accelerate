{-# LANGUAGE GADTs, TypeFamilies, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

  -- * Array representation
  Array(..),

  -- * Class of element types and of array shapes
  Elem(..),

  -- * Array shapes
  DIM0, DIM1, DIM2, DIM3, DIM4, DIM5,

  -- * Array indexing and slicing
  ShapeElem, Shape(..), Ix(..), Slice(..)

) where

-- standard library
import Data.Typeable

-- friends
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Representation hiding (Array)


-- |Surface types (tuples of scalars)
-- ----------------------------------

class (Typeable a, Typeable (ElemRepr a)) => Elem a where
  type ElemRepr a :: *
  elemType :: {-dummy-} a -> TupleType (ElemRepr a)
  fromElem :: a -> ElemRepr a
  toElem   :: ElemRepr a -> a

instance Elem () where
  type ElemRepr () = ()
  elemType _ = UnitTuple
  fromElem = id
  toElem   = id

instance Elem Int where
  type ElemRepr Int = Int
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem Int8 where
  type ElemRepr Int8 = Int8
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem Int16 where
  type ElemRepr Int16 = Int16
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem Int32 where
  type ElemRepr Int32 = Int32
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem Int64 where
  type ElemRepr Int64 = Int64
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem Word where
  type ElemRepr Word = Word
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem Word8 where
  type ElemRepr Word8 = Word8
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem Word16 where
  type ElemRepr Word16 = Word16
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem Word32 where
  type ElemRepr Word32 = Word32
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem Word64 where
  type ElemRepr Word64 = Word64
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem CShort where
  type ElemRepr CShort = CShort
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem CUShort where
  type ElemRepr CUShort = CUShort
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem CInt where
  type ElemRepr CInt = CInt
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem CUInt where
  type ElemRepr CUInt = CUInt
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem CLong where
  type ElemRepr CLong = CLong
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem CULong where
  type ElemRepr CULong = CULong
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem CLLong where
  type ElemRepr CLLong = CLLong
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem CULLong where
  type ElemRepr CULLong = CULLong
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem Float where
  type ElemRepr Float = Float
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem Double where
  type ElemRepr Double = Double
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem CFloat where
  type ElemRepr CFloat = CFloat
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem CDouble where
  type ElemRepr CDouble = CDouble
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem Bool where
  type ElemRepr Bool = Bool
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem Char where
  type ElemRepr Char = Char
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem CChar where
  type ElemRepr CChar = CChar
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem CSChar where
  type ElemRepr CSChar = CSChar
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem CUChar where
  type ElemRepr CUChar = CUChar
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance (Elem a, Elem b) => Elem (a, b) where
  type ElemRepr (a, b) = (ElemRepr a, ElemRepr b)
  elemType (_::(a, b)) 
    = PairTuple (elemType (undefined :: a)) (elemType (undefined :: b))
  fromElem (a, b) = (fromElem a, fromElem b)
  toElem   (a, b) = (toElem a, toElem b)

instance (Elem a, Elem b, Elem c) => Elem (a, b, c) where
  type ElemRepr (a, b, c) = (ElemRepr (a, b), ElemRepr c)
  elemType (_::(a, b, c)) 
    = PairTuple (elemType (undefined :: (a, b))) (elemType (undefined :: c))
  fromElem (a, b, c) = (fromElem (a, b), fromElem c)
  toElem   (ab, c) = let (a, b) = toElem ab in (a, b, toElem c)
  
instance (Elem a, Elem b, Elem c, Elem d) => Elem (a, b, c, d) where
  type ElemRepr (a, b, c, d) = (ElemRepr (a, b, c), ElemRepr d)
  elemType (_::(a, b, c, d)) 
    = PairTuple (elemType (undefined :: (a, b, c))) (elemType (undefined :: d))
  fromElem (a, b, c, d) = (fromElem (a, b, c), fromElem d)
  toElem   (abc, d) = let (a, b, c) = toElem abc in (a, b, c, toElem d)

instance (Elem a, Elem b, Elem c, Elem d, Elem e) => Elem (a, b, c, d, e) where
  type ElemRepr (a, b, c, d, e) = (ElemRepr (a, b, c, d), ElemRepr e)
  elemType (_::(a, b, c, d, e)) 
    = PairTuple (elemType (undefined :: (a, b, c, d))) 
                (elemType (undefined :: e))
  fromElem (a, b, c, d, e) = (fromElem (a, b, c, d), fromElem e)
  toElem   (abcd, e) = let (a, b, c, d) = toElem abcd in (a, b, c, d, toElem e)


-- |Surface arrays
-- ---------------

-- |Multi-dimensional arrays for array processing
--
data Array dim e where
  Array :: (Ix dim, Elem e, ArrayElem (ElemRepr e)) =>
           { arrayShape    :: dim             -- ^extend of dimensions = shape
           , arrayId       :: String          -- ^for pretty printing
           , arrayPtr      :: ArrayData (ElemRepr e)
                                              -- ^data, same layout as in
           }               -> Array dim e

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

