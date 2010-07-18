{-# LANGUAGE GADTs, TypeFamilies, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE CPP, ScopedTypeVariables, DeriveDataTypeable #-}
{-# LANGUAGE UndecidableInstances #-}  -- for instance SliceIxConv sl

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
  Array(..), Scalar, Vector, Segments,

  -- * Class of element types and of array shapes
  Elem(..), ElemRepr, ElemRepr', FromShapeRepr,
  
  -- * Derived functions
  liftToElem, liftToElem2, sinkFromElem, sinkFromElem2,

  -- * Array shapes
  DIM0, DIM1, DIM2, DIM3, DIM4, DIM5,

  -- * Array indexing and slicing
  ShapeBase, Shape, Ix(..), All(..), SliceIx(..), convertSliceIndex,
  
  -- * Array shape query, indexing, and conversions
  shape, (!), newArray, fromIArray, toIArray, fromList, toList,
  
  -- * Array analysis
  arrayType

) where

-- standard library
import Data.Array.IArray (IArray)
import qualified Data.Array.IArray as IArray
import Data.Typeable
import Unsafe.Coerce

-- friends
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Data
import qualified Data.Array.Accelerate.Array.Representation as Repr

#ifdef ACCELERATE_CUDA_BACKEND
import qualified Data.Array.Accelerate.CUDA.Array.Data      as CUDA
#endif


-- |Representation change for array element types
-- ----------------------------------------------

-- |Type representation mapping
--
-- The idea is to use '()' and '(,)' as type-level nil and snoc to construct 
-- snoc-lists of types.
--
type family ElemRepr a :: *
type instance ElemRepr () = ()
type instance ElemRepr All = ((), ())
type instance ElemRepr Int = ((), Int)
type instance ElemRepr Int8 = ((), Int8)
type instance ElemRepr Int16 = ((), Int16)
type instance ElemRepr Int32 = ((), Int32)
type instance ElemRepr Int64 = ((), Int64)
type instance ElemRepr Word = ((), Word)
type instance ElemRepr Word8 = ((), Word8)
type instance ElemRepr Word16 = ((), Word16)
type instance ElemRepr Word32 = ((), Word32)
type instance ElemRepr Word64 = ((), Word64)
type instance ElemRepr CShort = ((), CShort)
type instance ElemRepr CUShort = ((), CUShort)
type instance ElemRepr CInt = ((), CInt)
type instance ElemRepr CUInt = ((), CUInt)
type instance ElemRepr CLong = ((), CLong)
type instance ElemRepr CULong = ((), CULong)
type instance ElemRepr CLLong = ((), CLLong)
type instance ElemRepr CULLong = ((), CULLong)
type instance ElemRepr Float = ((), Float)
type instance ElemRepr Double = ((), Double)
type instance ElemRepr CFloat = ((), CFloat)
type instance ElemRepr CDouble = ((), CDouble)
type instance ElemRepr Bool = ((), Bool)
type instance ElemRepr Char = ((), Char)
type instance ElemRepr CChar = ((), CChar)
type instance ElemRepr CSChar = ((), CSChar)
type instance ElemRepr CUChar = ((), CUChar)
type instance ElemRepr (a, b) = (ElemRepr a, ElemRepr' b)
type instance ElemRepr (a, b, c) = (ElemRepr (a, b), ElemRepr' c)
type instance ElemRepr (a, b, c, d) = (ElemRepr (a, b, c), ElemRepr' d)
type instance ElemRepr (a, b, c, d, e) = (ElemRepr (a, b, c, d), ElemRepr' e)

-- To avoid overly nested pairs, we use a flattened representation at the
-- leaves.
--
type family ElemRepr' a :: *
type instance ElemRepr' () = ()
type instance ElemRepr' All = ()
type instance ElemRepr' Int = Int
type instance ElemRepr' Int8 = Int8
type instance ElemRepr' Int16 = Int16
type instance ElemRepr' Int32 = Int32
type instance ElemRepr' Int64 = Int64
type instance ElemRepr' Word = Word
type instance ElemRepr' Word8 = Word8
type instance ElemRepr' Word16 = Word16
type instance ElemRepr' Word32 = Word32
type instance ElemRepr' Word64 = Word64
type instance ElemRepr' CShort = CShort
type instance ElemRepr' CUShort = CUShort
type instance ElemRepr' CInt = CInt
type instance ElemRepr' CUInt = CUInt
type instance ElemRepr' CLong = CLong
type instance ElemRepr' CULong = CULong
type instance ElemRepr' CLLong = CLLong
type instance ElemRepr' CULLong = CULLong
type instance ElemRepr' Float = Float
type instance ElemRepr' Double = Double
type instance ElemRepr' CFloat = CFloat
type instance ElemRepr' CDouble = CDouble
type instance ElemRepr' Bool = Bool
type instance ElemRepr' Char = Char
type instance ElemRepr' CChar = CChar
type instance ElemRepr' CSChar = CSChar
type instance ElemRepr' CUChar = CUChar
type instance ElemRepr' (a, b) = (ElemRepr a, ElemRepr' b)
type instance ElemRepr' (a, b, c) = (ElemRepr (a, b), ElemRepr' c)
type instance ElemRepr' (a, b, c, d) = (ElemRepr (a, b, c), ElemRepr' d)
type instance ElemRepr' (a, b, c, d, e) = (ElemRepr (a, b, c, d), ElemRepr' e)


-- Array elements (tuples of scalars)
-- ----------------------------------

-- |Identifier for entire dimensions in slice descriptors
--
data All = All deriving (Typeable, Show)

-- |Class that characterises the types of values that can be array elements.
--
class (Show a, Typeable a, 
#ifdef ACCELERATE_CUDA_BACKEND
       CUDA.ArrayElem (ElemRepr a), CUDA.ArrayElem (ElemRepr' a),
#endif
       Typeable  (ElemRepr a), Typeable  (ElemRepr' a),
       ArrayElem (ElemRepr a), ArrayElem (ElemRepr' a))
      => Elem a where
  elemType  :: {-dummy-} a -> TupleType (ElemRepr a)
  fromElem  :: a -> ElemRepr a
  toElem    :: ElemRepr a -> a

  elemType' :: {-dummy-} a -> TupleType (ElemRepr' a)
  fromElem' :: a -> ElemRepr' a
  toElem'   :: ElemRepr' a -> a

instance Elem () where
  elemType _ = UnitTuple
  fromElem = id
  toElem   = id

  elemType' _ = UnitTuple
  fromElem' = id
  toElem'   = id

instance Elem All where
  elemType _      = PairTuple UnitTuple UnitTuple
  fromElem All    = ((), ())
  toElem ((), ()) = All

  elemType' _      = UnitTuple
  fromElem' All    = ()
  toElem' ()       = All

instance Elem Int where
  elemType       = singletonScalarType
  fromElem v     = ((), v)
  toElem ((), v) = v

  elemType' _    = SingleTuple scalarType
  fromElem'      = id
  toElem'        = id

instance Elem Int8 where
  elemType       = singletonScalarType
  fromElem v     = ((), v)
  toElem ((), v) = v

  elemType' _    = SingleTuple scalarType
  fromElem'      = id
  toElem'        = id

instance Elem Int16 where
  elemType       = singletonScalarType
  fromElem v     = ((), v)
  toElem ((), v) = v

  elemType' _    = SingleTuple scalarType
  fromElem'      = id
  toElem'        = id

instance Elem Int32 where
  elemType       = singletonScalarType
  fromElem v     = ((), v)
  toElem ((), v) = v

  elemType' _    = SingleTuple scalarType
  fromElem'      = id
  toElem'        = id

instance Elem Int64 where
  elemType       = singletonScalarType
  fromElem v     = ((), v)
  toElem ((), v) = v

  elemType' _    = SingleTuple scalarType
  fromElem'      = id
  toElem'        = id

instance Elem Word where
  elemType       = singletonScalarType
  fromElem v     = ((), v)
  toElem ((), v) = v

  elemType' _    = SingleTuple scalarType
  fromElem'      = id
  toElem'        = id

instance Elem Word8 where
  elemType       = singletonScalarType
  fromElem v     = ((), v)
  toElem ((), v) = v

  elemType' _    = SingleTuple scalarType
  fromElem'      = id
  toElem'        = id

instance Elem Word16 where
  elemType       = singletonScalarType
  fromElem v     = ((), v)
  toElem ((), v) = v

  elemType' _    = SingleTuple scalarType
  fromElem'      = id
  toElem'        = id

instance Elem Word32 where
  elemType       = singletonScalarType
  fromElem v     = ((), v)
  toElem ((), v) = v

  elemType' _    = SingleTuple scalarType
  fromElem'      = id
  toElem'        = id

instance Elem Word64 where
  elemType       = singletonScalarType
  fromElem v     = ((), v)
  toElem ((), v) = v

  elemType' _    = SingleTuple scalarType
  fromElem'      = id
  toElem'        = id

{-
instance Elem CShort where
  --elemType       = singletonScalarType
  fromElem v     = ((), v)
  toElem ((), v) = v

  --elemType' _    = SingleTuple scalarType
  fromElem'      = id
  toElem'        = id

instance Elem CUShort where
  --elemType       = singletonScalarType
  fromElem v     = ((), v)
  toElem ((), v) = v

  --elemType' _    = SingleTuple scalarType
  fromElem'      = id
  toElem'        = id

instance Elem CInt where
  --elemType       = singletonScalarType
  fromElem v     = ((), v)
  toElem ((), v) = v

  --elemType' _    = SingleTuple scalarType
  fromElem'      = id
  toElem'        = id

instance Elem CUInt where
  --elemType       = singletonScalarType
  fromElem v     = ((), v)
  toElem ((), v) = v

  --elemType' _    = SingleTuple scalarType
  fromElem'      = id
  toElem'        = id

instance Elem CLong where
  --elemType       = singletonScalarType
  fromElem v     = ((), v)
  toElem ((), v) = v

  --elemType' _    = SingleTuple scalarType
  fromElem'      = id
  toElem'        = id

instance Elem CULong where
  --elemType       = singletonScalarType
  fromElem v     = ((), v)
  toElem ((), v) = v

  --elemType' _    = SingleTuple scalarType
  fromElem'      = id
  toElem'        = id

instance Elem CLLong where
  --elemType       = singletonScalarType
  fromElem v     = ((), v)
  toElem ((), v) = v

  --elemType' _    = SingleTuple scalarType
  fromElem'      = id
  toElem'        = id

instance Elem CULLong where
  --elemType       = singletonScalarType
  fromElem v     = ((), v)
  toElem ((), v) = v

  --elemType' _    = SingleTuple scalarType
  fromElem'      = id
  toElem'        = id
-}

instance Elem Float where
  elemType       = singletonScalarType
  fromElem v     = ((), v)
  toElem ((), v) = v

  elemType' _    = SingleTuple scalarType
  fromElem'      = id
  toElem'        = id

instance Elem Double where
  elemType       = singletonScalarType
  fromElem v     = ((), v)
  toElem ((), v) = v

  elemType' _    = SingleTuple scalarType
  fromElem'      = id
  toElem'        = id

{-
instance Elem CFloat where
  --elemType       = singletonScalarType
  fromElem v     = ((), v)
  toElem ((), v) = v

  --elemType' _    = SingleTuple scalarType
  fromElem'      = id
  toElem'        = id

instance Elem CDouble where
  --elemType       = singletonScalarType
  fromElem v     = ((), v)
  toElem ((), v) = v

  --elemType' _    = SingleTuple scalarType
  fromElem'      = id
  toElem'        = id
-}

instance Elem Bool where
  elemType       = singletonScalarType
  fromElem v     = ((), v)
  toElem ((), v) = v

  elemType' _    = SingleTuple scalarType
  fromElem'      = id
  toElem'        = id

instance Elem Char where
  elemType       = singletonScalarType
  fromElem v     = ((), v)
  toElem ((), v) = v

  elemType' _    = SingleTuple scalarType
  fromElem'      = id
  toElem'        = id

{-
instance Elem CChar where
  --elemType       = singletonScalarType
  fromElem v     = ((), v)
  toElem ((), v) = v

  --elemType' _    = SingleTuple scalarType
  fromElem'      = id
  toElem'        = id

instance Elem CSChar where
  --elemType       = singletonScalarType
  fromElem v     = ((), v)
  toElem ((), v) = v

  --elemType' _    = SingleTuple scalarType
  fromElem'      = id
  toElem'        = id

instance Elem CUChar where
  --elemType       = singletonScalarType
  fromElem v     = ((), v)
  toElem ((), v) = v

  --elemType' _    = SingleTuple scalarType
  fromElem'      = id
  toElem'        = id
-}

instance (Elem a, Elem b) => Elem (a, b) where
  elemType (_::(a, b)) 
    = PairTuple (elemType (undefined :: a)) (elemType' (undefined :: b))
  fromElem (a, b)  = (fromElem a, fromElem' b)
  toElem (a, b)  = (toElem a, toElem' b)

  elemType' (_::(a, b)) 
    = PairTuple (elemType (undefined :: a)) (elemType' (undefined :: b))
  fromElem' (a, b) = (fromElem a, fromElem' b)
  toElem' (a, b) = (toElem a, toElem' b)

instance (Elem a, Elem b, Elem c) => Elem (a, b, c) where
  elemType (_::(a, b, c)) 
    = PairTuple (elemType (undefined :: (a, b))) (elemType' (undefined :: c))
  fromElem (a, b, c) = (fromElem (a, b), fromElem' c)
  toElem (ab, c) = let (a, b) = toElem ab in (a, b, toElem' c)
  
  elemType' (_::(a, b, c)) 
    = PairTuple (elemType (undefined :: (a, b))) (elemType' (undefined :: c))
  fromElem' (a, b, c) = (fromElem (a, b), fromElem' c)
  toElem' (ab, c) = let (a, b) = toElem ab in (a, b, toElem' c)
  
instance (Elem a, Elem b, Elem c, Elem d) => Elem (a, b, c, d) where
  elemType (_::(a, b, c, d)) 
    = PairTuple (elemType (undefined :: (a, b, c))) (elemType' (undefined :: d))
  fromElem (a, b, c, d) = (fromElem (a, b, c), fromElem' d)
  toElem (abc, d) = let (a, b, c) = toElem abc in (a, b, c, toElem' d)

  elemType' (_::(a, b, c, d)) 
    = PairTuple (elemType (undefined :: (a, b, c))) (elemType' (undefined :: d))
  fromElem' (a, b, c, d) = (fromElem (a, b, c), fromElem' d)
  toElem' (abc, d) = let (a, b, c) = toElem abc in (a, b, c, toElem' d)

instance (Elem a, Elem b, Elem c, Elem d, Elem e) => Elem (a, b, c, d, e) where
  elemType (_::(a, b, c, d, e)) 
    = PairTuple (elemType (undefined :: (a, b, c, d))) 
                (elemType' (undefined :: e))
  fromElem (a, b, c, d, e) = (fromElem (a, b, c, d), fromElem' e)
  toElem (abcd, e) = let (a, b, c, d) = toElem abcd in (a, b, c, d, toElem' e)

  elemType' (_::(a, b, c, d, e)) 
    = PairTuple (elemType (undefined :: (a, b, c, d))) 
                (elemType' (undefined :: e))
  fromElem' (a, b, c, d, e) = (fromElem (a, b, c, d), fromElem' e)
  toElem' (abcd, e) = let (a, b, c, d) = toElem abcd in (a, b, c, d, toElem' e)

-- |Convenience functions
-- -

singletonScalarType :: IsScalar a => a -> TupleType ((), a)
singletonScalarType _ = PairTuple UnitTuple (SingleTuple scalarType)

liftToElem :: (Elem a, Elem b) 
           => (ElemRepr a -> ElemRepr b)
           -> (a -> b)
{-# INLINE liftToElem #-}
liftToElem f = toElem . f . fromElem

liftToElem2 :: (Elem a, Elem b, Elem c) 
           => (ElemRepr a -> ElemRepr b -> ElemRepr c)
           -> (a -> b -> c)
{-# INLINE liftToElem2 #-}
liftToElem2 f = \x y -> toElem $ f (fromElem x) (fromElem y)

sinkFromElem :: (Elem a, Elem b) 
             => (a -> b)
             -> (ElemRepr a -> ElemRepr b)
{-# INLINE sinkFromElem #-}
sinkFromElem f = fromElem . f . toElem

sinkFromElem2 :: (Elem a, Elem b, Elem c) 
             => (a -> b -> c)
             -> (ElemRepr a -> ElemRepr b -> ElemRepr c)
{-# INLINE sinkFromElem2 #-}
sinkFromElem2 f = \x y -> fromElem $ f (toElem x) (toElem y)

{-# RULES

"fromElem/toElem" forall e.
  fromElem (toElem e) = e

-- FIXME: Won't type like that (it's ambiguous due to the ElemRepr family):  
-- "toElem/fromElem" forall e.
--   toElem (fromElem e) = e
  
  #-}


-- Surface arrays
-- --------------

-- |Multi-dimensional arrays for array processing
--
-- * If device and host memory are separate, arrays will be transferred to the
--   device when necessary (if possible asynchronously and in parallel with
--   other tasks) and cached on the device if sufficient memory is available.
--
data Array dim e where
  Array :: (Ix dim, Elem e) 
        => ElemRepr dim               -- extent of dimensions = shape
        -> ArrayData (ElemRepr e)     -- data, same layout as in
        -> Array dim e

-- |Scalars
--
type Scalar e = Array DIM0 e

-- |Vectors
--
type Vector e = Array DIM1 e

-- |Segment descriptor
--
type Segments = Vector Int

-- Shorthand for common shape types
--
type DIM0 = ()
type DIM1 = (Int)
type DIM2 = (Int, Int)
type DIM3 = (Int, Int, Int)
type DIM4 = (Int, Int, Int, Int)
type DIM5 = (Int, Int, Int, Int, Int)

-- Shape constraints and indexing
-- 

-- Shape elements
--
class Elem shb => ShapeBase shb
instance ShapeBase Int
instance ShapeBase All

class Elem sh => Shape sh

instance Shape ()
instance Shape Int
instance Shape All
instance (ShapeBase a, ShapeBase b) => Shape (a, b)
instance (ShapeBase a, ShapeBase b, ShapeBase c) => Shape (a, b, c)
instance (ShapeBase a, ShapeBase b, ShapeBase c, ShapeBase d) 
  => Shape (a, b, c, d)
instance (ShapeBase a, ShapeBase b, ShapeBase c, ShapeBase d, ShapeBase e) 
  => Shape (a, b, c, d, e)

type family FromShapeBase shb :: *
type instance FromShapeBase Int = Int
type instance FromShapeBase ()  = All

type family FromShapeRepr shr :: *
type instance FromShapeRepr ()           = ()
type instance FromShapeRepr ((), a)      = FromShapeBase a
type instance FromShapeRepr (((), a), b) = (FromShapeBase a, FromShapeBase b)
type instance FromShapeRepr ((((), a), b), c) 
  = (FromShapeBase a, FromShapeBase b, FromShapeBase c)
type instance FromShapeRepr (((((), a), b), c), d) 
  = (FromShapeBase a, FromShapeBase b, FromShapeBase c, FromShapeBase d)
type instance FromShapeRepr ((((((), a), b), c), d), e) 
  = (FromShapeBase a, FromShapeBase b, FromShapeBase c, FromShapeBase d, 
     FromShapeBase e)

-- |Shapes and indices of multi-dimensional arrays
--
class (Shape ix, Repr.Ix (ElemRepr ix)) => Ix ix where

  -- |Number of dimensions of a /shape/ or /index/ (>= 0)
  dim    :: ix -> Int

  -- Total number of elements in an array of the given /shape/
  size   :: ix -> Int

  -- |Magic value identifing elements ignored in 'permute'
  ignore :: ix
  
  -- |Map a multi-dimensional index into one in a linear, row-major 
  -- representation of the array (first argument is the /shape/, second 
  -- argument is the index)
  index  :: ix -> ix -> Int

  -- |Iterate through the entire shape, applying the function; third argument
  -- combines results and fourth is returned in case of an empty iteration
  -- space; the index space is traversed in row-major order
  iter  :: ix -> (ix -> a) -> (a -> a -> a) -> a -> a

  -- |Convert a minpoint-maxpoint index into a /shape/
  rangeToShape ::  (ix, ix) -> ix
  
  -- |Convert a /shape/ into a minpoint-maxpoint index
  shapeToRange ::  ix -> (ix, ix)

  dim         = Repr.dim . fromElem
  size        = Repr.size . fromElem
  ignore      = toElem Repr.ignore
  index sh ix = Repr.index (fromElem sh) (fromElem ix)
  
  iter sh f c r = Repr.iter (fromElem sh) (f . toElem) c r
  
  rangeToShape (low, high) 
    = toElem (Repr.rangeToShape (fromElem low, fromElem high))
  shapeToRange ix
    = let (low, high) = Repr.shapeToRange (fromElem ix)
      in
      (toElem low, toElem high)

instance Ix ()
instance Ix (Int)
instance Ix (Int, Int)
instance Ix (Int, Int, Int)
instance Ix (Int, Int, Int, Int)
instance Ix (Int, Int, Int, Int, Int)

-- |Slices -aka generalised indices- as n-tuples and mappings of slice
-- indicies to slices, co-slices, and slice dimensions
--
class (Shape sl, 
       Repr.SliceIx (ElemRepr sl), 
       Ix (Slice sl), Ix (CoSlice sl), Ix (SliceDim sl), 
       SliceIxConv sl) 
  => SliceIx sl where
  type Slice    sl :: *
  type CoSlice  sl :: *
  type SliceDim sl :: *
  sliceIndex :: sl -> Repr.SliceIndex (ElemRepr sl)
                                      (Repr.Slice (ElemRepr    sl))
                                      (Repr.CoSlice (ElemRepr  sl))
                                      (Repr.SliceDim (ElemRepr sl))

instance (Shape sl, 
          Repr.SliceIx (ElemRepr sl), 
          Ix (Slice sl), Ix (CoSlice sl), Ix (SliceDim sl), 
          SliceIxConv sl)
  => SliceIx sl where
  type Slice    sl = FromShapeRepr (Repr.Slice    (ElemRepr sl))
  type CoSlice  sl = FromShapeRepr (Repr.CoSlice  (ElemRepr sl))
  type SliceDim sl = FromShapeRepr (Repr.SliceDim (ElemRepr sl))
  sliceIndex = Repr.sliceIndex . fromElem

class SliceIxConv slix where
  convertSliceIndex :: slix {- dummy to fix the type variable -}
                    -> Repr.SliceIndex (ElemRepr slix)
                                       (Repr.Slice (ElemRepr    slix))
                                       (Repr.CoSlice (ElemRepr  slix))
                                       (Repr.SliceDim (ElemRepr slix))
                    -> Repr.SliceIndex (ElemRepr slix)
                                       (ElemRepr (Slice slix))
                                       (ElemRepr (CoSlice slix))
                                       (ElemRepr (SliceDim slix))

instance SliceIxConv slix where
  convertSliceIndex _ = unsafeCoerce
    -- FIXME: the coercion is safe given the definition of the involved
    --   families, but we really ought to code a proof for that instead


-- Array operations
-- ----------------

-- |Yield an array's shape
--
shape :: Ix dim => Array dim e -> dim
shape (Array sh _) = toElem sh

-- |Array indexing
--
infixl 9 !
(!) :: Array dim e -> dim -> e
{-# INLINE (!) #-}
-- (Array sh adata) ! ix = toElem (adata `indexArrayData` index sh ix)
-- FIXME: using this due to a bug in 6.10.x
(!) (Array sh adata) ix = toElem (adata `indexArrayData` index (toElem sh) ix)

-- |Create an array from its representation function
--
newArray :: (Ix dim, Elem e) => dim -> (dim -> e) -> Array dim e
{-# INLINE newArray #-}
newArray sh f 
  = adata `seq` Array (fromElem sh) adata
  where 
    (adata, _) = runArrayData $ do
                   arr <- newArrayData (1024 `max` size sh)
                   let write ix = writeArrayData arr (index sh ix) 
                                                     (fromElem (f ix))
                   iter sh write (>>) (return ())
                   return (arr, undefined)

-- |Convert an 'IArray' to an accelerated array.
--
fromIArray :: (IArray a e, IArray.Ix dim, Ix dim, Elem e) 
           => a dim e -> Array dim e
fromIArray iarr = newArray sh (iarr IArray.!)
  where
    sh = rangeToShape (IArray.bounds iarr)

-- |Convert an accelerated array to an 'IArray'
-- 
toIArray :: (IArray a e, IArray.Ix dim, Ix dim, Elem e) 
         => Array dim e -> a dim e
toIArray arr@(Array sh _) 
  = let bnds = shapeToRange (toElem sh)
    in
    IArray.array bnds [(ix, arr!ix) | ix <- IArray.range bnds]
    
-- |Convert a list (with elements in row-major order) to an accelerated array.
--
fromList :: (Ix dim, Elem e) => dim -> [e] -> Array dim e
fromList sh l = newArray sh indexIntoList 
  where
    indexIntoList ix = l!!index sh ix

-- |Convert an accelerated array to a list in row-major order.
--
toList :: forall dim e. Array dim e -> [e]
toList (Array sh adata) = iter sh' idx (.) id []
  where
    sh'    = toElem sh :: dim
    idx ix = \l -> toElem (adata `indexArrayData` index sh' ix) : l

-- Convert an array to a string
--
instance Show (Array dim e) where
  show arr@(Array sh _adata) 
    = "Array " ++ show (toElem sh :: dim) ++ " " ++ show (toList arr)


-- Array analysis
-- --------------

-- |Reify the element type of an array.
--
arrayType :: forall dim e. Array dim e -> TupleType (ElemRepr e)
arrayType (Array _ _) = elemType (undefined::e)
