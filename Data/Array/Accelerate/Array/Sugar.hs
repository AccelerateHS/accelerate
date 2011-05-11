{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators, GADTs, TypeFamilies, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, StandaloneDeriving, TupleSections #-}
-- |
-- Module      : Data.Array.Accelerate.Array.Sugar
-- Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Array.Sugar (

  -- * Array representation
  Array(..), Scalar, Vector, Segments,

  -- * Class of supported surface element types and their mapping to representation types
  Elt(..), EltRepr, EltRepr',
  
  -- * Derived functions
  liftToElt, liftToElt2, sinkFromElt, sinkFromElt2,

  -- * Array shapes
  DIM0, DIM1, DIM2, DIM3, DIM4, DIM5, DIM6, DIM7, DIM8, DIM9,

  -- * Array indexing and slicing
  Z(..), (:.)(..), All(..), Any(..), Shape(..), Slice(..),
  
  -- * Array shape query, indexing, and conversions
  shape, (!), newArray, allocateArray, fromIArray, toIArray, fromList, toList,

) where

-- standard library
import Data.Array.IArray (IArray)
import qualified Data.Array.IArray as IArray
import Data.Typeable

-- friends
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Data
import qualified Data.Array.Accelerate.Array.Representation as Repr


-- |Surface types representing array indices and slices
-- ----------------------------------------------------

-- |Array indices are snoc type lists
--
-- For example, the type of a rank-2 array index is 'Z :.Int :. Int'.

-- |Rank-0 index
--
data Z = Z
  deriving (Typeable, Show)

-- |Increase an index rank by one dimension
--
infixl 3 :.
data tail :. head = tail :. head
  deriving (Typeable, Show)

-- |Marker for entire dimensions in slice descriptors
--
data All = All 
  deriving (Typeable, Show)

-- |Marker for arbitrary shapes in slice descriptors
--
data Any sh = Any
  deriving (Typeable, Show)

-- |Representation change for array element types
-- ----------------------------------------------

-- |Type representation mapping
--
-- We represent tuples by using '()' and '(,)' as type-level nil and snoc to construct 
-- snoc-lists of types.
--
type family EltRepr a :: *
type instance EltRepr () = ()
type instance EltRepr Z = ()
type instance EltRepr (t:.h) = (EltRepr t, EltRepr' h)
type instance EltRepr All = ((), ())
type instance EltRepr (Any Z) = ()
type instance EltRepr (Any (sh:.Int)) = (EltRepr (Any sh), ())
type instance EltRepr Int = ((), Int)
type instance EltRepr Int8 = ((), Int8)
type instance EltRepr Int16 = ((), Int16)
type instance EltRepr Int32 = ((), Int32)
type instance EltRepr Int64 = ((), Int64)
type instance EltRepr Word = ((), Word)
type instance EltRepr Word8 = ((), Word8)
type instance EltRepr Word16 = ((), Word16)
type instance EltRepr Word32 = ((), Word32)
type instance EltRepr Word64 = ((), Word64)
type instance EltRepr CShort = ((), CShort)
type instance EltRepr CUShort = ((), CUShort)
type instance EltRepr CInt = ((), CInt)
type instance EltRepr CUInt = ((), CUInt)
type instance EltRepr CLong = ((), CLong)
type instance EltRepr CULong = ((), CULong)
type instance EltRepr CLLong = ((), CLLong)
type instance EltRepr CULLong = ((), CULLong)
type instance EltRepr Float = ((), Float)
type instance EltRepr Double = ((), Double)
type instance EltRepr CFloat = ((), CFloat)
type instance EltRepr CDouble = ((), CDouble)
type instance EltRepr Bool = ((), Bool)
type instance EltRepr Char = ((), Char)
type instance EltRepr CChar = ((), CChar)
type instance EltRepr CSChar = ((), CSChar)
type instance EltRepr CUChar = ((), CUChar)
type instance EltRepr (a, b) = (EltRepr a, EltRepr' b)
type instance EltRepr (a, b, c) = (EltRepr (a, b), EltRepr' c)
type instance EltRepr (a, b, c, d) = (EltRepr (a, b, c), EltRepr' d)
type instance EltRepr (a, b, c, d, e) = (EltRepr (a, b, c, d), EltRepr' e)
type instance EltRepr (a, b, c, d, e, f) = (EltRepr (a, b, c, d, e), EltRepr' f)
type instance EltRepr (a, b, c, d, e, f, g) = (EltRepr (a, b, c, d, e, f), EltRepr' g)
type instance EltRepr (a, b, c, d, e, f, g, h) = (EltRepr (a, b, c, d, e, f, g), EltRepr' h)
type instance EltRepr (a, b, c, d, e, f, g, h, i) 
  = (EltRepr (a, b, c, d, e, f, g, h), EltRepr' i)

-- To avoid overly nested pairs, we use a flattened representation at the
-- leaves.
--
type family EltRepr' a :: *
type instance EltRepr' () = ()
type instance EltRepr' Z = ()
type instance EltRepr' (t:.h) = (EltRepr t, EltRepr' h)
type instance EltRepr' All = ()
type instance EltRepr' (Any Z) = ()
type instance EltRepr' (Any (sh:.Int)) = (EltRepr' (Any sh), ())
type instance EltRepr' Int = Int
type instance EltRepr' Int8 = Int8
type instance EltRepr' Int16 = Int16
type instance EltRepr' Int32 = Int32
type instance EltRepr' Int64 = Int64
type instance EltRepr' Word = Word
type instance EltRepr' Word8 = Word8
type instance EltRepr' Word16 = Word16
type instance EltRepr' Word32 = Word32
type instance EltRepr' Word64 = Word64
type instance EltRepr' CShort = CShort
type instance EltRepr' CUShort = CUShort
type instance EltRepr' CInt = CInt
type instance EltRepr' CUInt = CUInt
type instance EltRepr' CLong = CLong
type instance EltRepr' CULong = CULong
type instance EltRepr' CLLong = CLLong
type instance EltRepr' CULLong = CULLong
type instance EltRepr' Float = Float
type instance EltRepr' Double = Double
type instance EltRepr' CFloat = CFloat
type instance EltRepr' CDouble = CDouble
type instance EltRepr' Bool = Bool
type instance EltRepr' Char = Char
type instance EltRepr' CChar = CChar
type instance EltRepr' CSChar = CSChar
type instance EltRepr' CUChar = CUChar
type instance EltRepr' (a, b) = (EltRepr a, EltRepr' b)
type instance EltRepr' (a, b, c) = (EltRepr (a, b), EltRepr' c)
type instance EltRepr' (a, b, c, d) = (EltRepr (a, b, c), EltRepr' d)
type instance EltRepr' (a, b, c, d, e) = (EltRepr (a, b, c, d), EltRepr' e)
type instance EltRepr' (a, b, c, d, e, f) = (EltRepr (a, b, c, d, e), EltRepr' f)
type instance EltRepr' (a, b, c, d, e, f, g) = (EltRepr (a, b, c, d, e, f), EltRepr' g)
type instance EltRepr' (a, b, c, d, e, f, g, h) = (EltRepr (a, b, c, d, e, f, g), EltRepr' h)
type instance EltRepr' (a, b, c, d, e, f, g, h, i) 
  = (EltRepr (a, b, c, d, e, f, g, h), EltRepr' i)


-- Array elements (tuples of scalars)
-- ----------------------------------

-- |Class that characterises the types of values that can be array elements, and hence, appear in
-- scalar Accelerate expressions.
--
class (Show a, Typeable a, 
       Typeable (EltRepr a), Typeable (EltRepr' a),
       ArrayElt (EltRepr a), ArrayElt (EltRepr' a))
      => Elt a where
  eltType  :: {-dummy-} a -> TupleType (EltRepr a)
  fromElt  :: a -> EltRepr a
  toElt    :: EltRepr a -> a

  eltType' :: {-dummy-} a -> TupleType (EltRepr' a)
  fromElt' :: a -> EltRepr' a
  toElt'   :: EltRepr' a -> a
  
instance Elt () where
  eltType _ = UnitTuple
  fromElt = id
  toElt   = id

  eltType' _ = UnitTuple
  fromElt' = id
  toElt'   = id

instance Elt Z where
  eltType _ = UnitTuple
  fromElt Z = ()
  toElt ()  = Z

  eltType' _ = UnitTuple
  fromElt' Z = ()
  toElt' ()  = Z

instance (Elt t, Elt h) => Elt (t:.h) where
  eltType (_::(t:.h)) = PairTuple (eltType (undefined :: t)) (eltType' (undefined :: h))
  fromElt (t:.h)      = (fromElt t, fromElt' h)
  toElt (t, h)        = toElt t :. toElt' h

  eltType' (_::(t:.h)) = PairTuple (eltType (undefined :: t)) (eltType' (undefined :: h))
  fromElt' (t:.h)      = (fromElt t, fromElt' h)
  toElt' (t, h)        = toElt t :. toElt' h

instance Elt All where
  eltType _      = PairTuple UnitTuple UnitTuple
  fromElt All    = ((), ())
  toElt ((), ()) = All

  eltType' _      = UnitTuple
  fromElt' All    = ()
  toElt' ()       = All

instance Elt (Any Z) where
  eltType _ = UnitTuple
  fromElt _ = ()
  toElt _ = Any
  
  eltType' _ = UnitTuple
  fromElt' _ = ()
  toElt' _ = Any

instance Shape sh => Elt (Any (sh:.Int)) where
  eltType _ = PairTuple (eltType (undefined::Any sh)) UnitTuple
  fromElt _ = (fromElt (undefined :: Any sh), ())
  toElt _ = Any

  eltType' _ = PairTuple (eltType' (undefined::Any sh)) UnitTuple
  fromElt' _ = (fromElt' (undefined :: Any sh), ())
  toElt' _ = Any

instance Elt Int where
  eltType       = singletonScalarType
  fromElt v     = ((), v)
  toElt ((), v) = v

  eltType' _    = SingleTuple scalarType
  fromElt'      = id
  toElt'        = id

instance Elt Int8 where
  eltType       = singletonScalarType
  fromElt v     = ((), v)
  toElt ((), v) = v

  eltType' _    = SingleTuple scalarType
  fromElt'      = id
  toElt'        = id

instance Elt Int16 where
  eltType       = singletonScalarType
  fromElt v     = ((), v)
  toElt ((), v) = v

  eltType' _    = SingleTuple scalarType
  fromElt'      = id
  toElt'        = id

instance Elt Int32 where
  eltType       = singletonScalarType
  fromElt v     = ((), v)
  toElt ((), v) = v

  eltType' _    = SingleTuple scalarType
  fromElt'      = id
  toElt'        = id

instance Elt Int64 where
  eltType       = singletonScalarType
  fromElt v     = ((), v)
  toElt ((), v) = v

  eltType' _    = SingleTuple scalarType
  fromElt'      = id
  toElt'        = id

instance Elt Word where
  eltType       = singletonScalarType
  fromElt v     = ((), v)
  toElt ((), v) = v

  eltType' _    = SingleTuple scalarType
  fromElt'      = id
  toElt'        = id

instance Elt Word8 where
  eltType       = singletonScalarType
  fromElt v     = ((), v)
  toElt ((), v) = v

  eltType' _    = SingleTuple scalarType
  fromElt'      = id
  toElt'        = id

instance Elt Word16 where
  eltType       = singletonScalarType
  fromElt v     = ((), v)
  toElt ((), v) = v

  eltType' _    = SingleTuple scalarType
  fromElt'      = id
  toElt'        = id

instance Elt Word32 where
  eltType       = singletonScalarType
  fromElt v     = ((), v)
  toElt ((), v) = v

  eltType' _    = SingleTuple scalarType
  fromElt'      = id
  toElt'        = id

instance Elt Word64 where
  eltType       = singletonScalarType
  fromElt v     = ((), v)
  toElt ((), v) = v

  eltType' _    = SingleTuple scalarType
  fromElt'      = id
  toElt'        = id

{-
instance Elt CShort where
  --eltType       = singletonScalarType
  fromElt v     = ((), v)
  toElt ((), v) = v

  --eltType' _    = SingleTuple scalarType
  fromElt'      = id
  toElt'        = id

instance Elt CUShort where
  --eltType       = singletonScalarType
  fromElt v     = ((), v)
  toElt ((), v) = v

  --eltType' _    = SingleTuple scalarType
  fromElt'      = id
  toElt'        = id

instance Elt CInt where
  --eltType       = singletonScalarType
  fromElt v     = ((), v)
  toElt ((), v) = v

  --eltType' _    = SingleTuple scalarType
  fromElt'      = id
  toElt'        = id

instance Elt CUInt where
  --eltType       = singletonScalarType
  fromElt v     = ((), v)
  toElt ((), v) = v

  --eltType' _    = SingleTuple scalarType
  fromElt'      = id
  toElt'        = id

instance Elt CLong where
  --eltType       = singletonScalarType
  fromElt v     = ((), v)
  toElt ((), v) = v

  --eltType' _    = SingleTuple scalarType
  fromElt'      = id
  toElt'        = id

instance Elt CULong where
  --eltType       = singletonScalarType
  fromElt v     = ((), v)
  toElt ((), v) = v

  --eltType' _    = SingleTuple scalarType
  fromElt'      = id
  toElt'        = id

instance Elt CLLong where
  --eltType       = singletonScalarType
  fromElt v     = ((), v)
  toElt ((), v) = v

  --eltType' _    = SingleTuple scalarType
  fromElt'      = id
  toElt'        = id

instance Elt CULLong where
  --eltType       = singletonScalarType
  fromElt v     = ((), v)
  toElt ((), v) = v

  --eltType' _    = SingleTuple scalarType
  fromElt'      = id
  toElt'        = id
-}

instance Elt Float where
  eltType       = singletonScalarType
  fromElt v     = ((), v)
  toElt ((), v) = v

  eltType' _    = SingleTuple scalarType
  fromElt'      = id
  toElt'        = id

instance Elt Double where
  eltType       = singletonScalarType
  fromElt v     = ((), v)
  toElt ((), v) = v

  eltType' _    = SingleTuple scalarType
  fromElt'      = id
  toElt'        = id

{-
instance Elt CFloat where
  --eltType       = singletonScalarType
  fromElt v     = ((), v)
  toElt ((), v) = v

  --eltType' _    = SingleTuple scalarType
  fromElt'      = id
  toElt'        = id

instance Elt CDouble where
  --eltType       = singletonScalarType
  fromElt v     = ((), v)
  toElt ((), v) = v

  --eltType' _    = SingleTuple scalarType
  fromElt'      = id
  toElt'        = id
-}

instance Elt Bool where
  eltType       = singletonScalarType
  fromElt v     = ((), v)
  toElt ((), v) = v

  eltType' _    = SingleTuple scalarType
  fromElt'      = id
  toElt'        = id

instance Elt Char where
  eltType       = singletonScalarType
  fromElt v     = ((), v)
  toElt ((), v) = v

  eltType' _    = SingleTuple scalarType
  fromElt'      = id
  toElt'        = id

{-
instance Elt CChar where
  --eltType       = singletonScalarType
  fromElt v     = ((), v)
  toElt ((), v) = v

  --eltType' _    = SingleTuple scalarType
  fromElt'      = id
  toElt'        = id

instance Elt CSChar where
  --eltType       = singletonScalarType
  fromElt v     = ((), v)
  toElt ((), v) = v

  --eltType' _    = SingleTuple scalarType
  fromElt'      = id
  toElt'        = id

instance Elt CUChar where
  --eltType       = singletonScalarType
  fromElt v     = ((), v)
  toElt ((), v) = v

  --eltType' _    = SingleTuple scalarType
  fromElt'      = id
  toElt'        = id
-}

instance (Elt a, Elt b) => Elt (a, b) where
  eltType (_::(a, b)) 
    = PairTuple (eltType (undefined :: a)) (eltType' (undefined :: b))
  fromElt (a, b)  = (fromElt a, fromElt' b)
  toElt (a, b)  = (toElt a, toElt' b)

  eltType' (_::(a, b)) 
    = PairTuple (eltType (undefined :: a)) (eltType' (undefined :: b))
  fromElt' (a, b) = (fromElt a, fromElt' b)
  toElt' (a, b) = (toElt a, toElt' b)

instance (Elt a, Elt b, Elt c) => Elt (a, b, c) where
  eltType (_::(a, b, c)) 
    = PairTuple (eltType (undefined :: (a, b))) (eltType' (undefined :: c))
  fromElt (a, b, c) = (fromElt (a, b), fromElt' c)
  toElt (ab, c) = let (a, b) = toElt ab in (a, b, toElt' c)
  
  eltType' (_::(a, b, c)) 
    = PairTuple (eltType (undefined :: (a, b))) (eltType' (undefined :: c))
  fromElt' (a, b, c) = (fromElt (a, b), fromElt' c)
  toElt' (ab, c) = let (a, b) = toElt ab in (a, b, toElt' c)
  
instance (Elt a, Elt b, Elt c, Elt d) => Elt (a, b, c, d) where
  eltType (_::(a, b, c, d)) 
    = PairTuple (eltType (undefined :: (a, b, c))) (eltType' (undefined :: d))
  fromElt (a, b, c, d) = (fromElt (a, b, c), fromElt' d)
  toElt (abc, d) = let (a, b, c) = toElt abc in (a, b, c, toElt' d)

  eltType' (_::(a, b, c, d)) 
    = PairTuple (eltType (undefined :: (a, b, c))) (eltType' (undefined :: d))
  fromElt' (a, b, c, d) = (fromElt (a, b, c), fromElt' d)
  toElt' (abc, d) = let (a, b, c) = toElt abc in (a, b, c, toElt' d)

instance (Elt a, Elt b, Elt c, Elt d, Elt e) => Elt (a, b, c, d, e) where
  eltType (_::(a, b, c, d, e)) 
    = PairTuple (eltType (undefined :: (a, b, c, d))) 
                (eltType' (undefined :: e))
  fromElt (a, b, c, d, e) = (fromElt (a, b, c, d), fromElt' e)
  toElt (abcd, e) = let (a, b, c, d) = toElt abcd in (a, b, c, d, toElt' e)

  eltType' (_::(a, b, c, d, e)) 
    = PairTuple (eltType (undefined :: (a, b, c, d))) 
                (eltType' (undefined :: e))
  fromElt' (a, b, c, d, e) = (fromElt (a, b, c, d), fromElt' e)
  toElt' (abcd, e) = let (a, b, c, d) = toElt abcd in (a, b, c, d, toElt' e)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f) => Elt (a, b, c, d, e, f) where
  eltType (_::(a, b, c, d, e, f)) 
    = PairTuple (eltType (undefined :: (a, b, c, d, e))) 
                (eltType' (undefined :: f))
  fromElt (a, b, c, d, e, f) = (fromElt (a, b, c, d, e), fromElt' f)
  toElt (abcde, f) = let (a, b, c, d, e) = toElt abcde in (a, b, c, d, e, toElt' f)

  eltType' (_::(a, b, c, d, e, f)) 
    = PairTuple (eltType (undefined :: (a, b, c, d, e))) 
                (eltType' (undefined :: f))
  fromElt' (a, b, c, d, e, f) = (fromElt (a, b, c, d, e), fromElt' f)
  toElt' (abcde, f) = let (a, b, c, d, e) = toElt abcde in (a, b, c, d, e, toElt' f)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g) 
  => Elt (a, b, c, d, e, f, g) where
  eltType (_::(a, b, c, d, e, f, g)) 
    = PairTuple (eltType (undefined :: (a, b, c, d, e, f))) 
                (eltType' (undefined :: g))
  fromElt (a, b, c, d, e, f, g) = (fromElt (a, b, c, d, e, f), fromElt' g)
  toElt (abcdef, g) = let (a, b, c, d, e, f) = toElt abcdef in (a, b, c, d, e, f, toElt' g)

  eltType' (_::(a, b, c, d, e, f, g)) 
    = PairTuple (eltType (undefined :: (a, b, c, d, e, f))) 
                (eltType' (undefined :: g))
  fromElt' (a, b, c, d, e, f, g) = (fromElt (a, b, c, d, e, f), fromElt' g)
  toElt' (abcdef, g) = let (a, b, c, d, e, f) = toElt abcdef in (a, b, c, d, e, f, toElt' g)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h) 
  => Elt (a, b, c, d, e, f, g, h) where
  eltType (_::(a, b, c, d, e, f, g, h)) 
    = PairTuple (eltType (undefined :: (a, b, c, d, e, f, g))) 
                (eltType' (undefined :: h))
  fromElt (a, b, c, d, e, f, g, h) = (fromElt (a, b, c, d, e, f, g), fromElt' h)
  toElt (abcdefg, h) = let (a, b, c, d, e, f, g) = toElt abcdefg 
                        in (a, b, c, d, e, f, g, toElt' h)

  eltType' (_::(a, b, c, d, e, f, g, h)) 
    = PairTuple (eltType (undefined :: (a, b, c, d, e, f, g))) 
                (eltType' (undefined :: h))
  fromElt' (a, b, c, d, e, f, g, h) = (fromElt (a, b, c, d, e, f, g), fromElt' h)
  toElt' (abcdefg, h) = let (a, b, c, d, e, f, g) = toElt abcdefg 
                         in (a, b, c, d, e, f, g, toElt' h)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i) 
  => Elt (a, b, c, d, e, f, g, h, i) where
  eltType (_::(a, b, c, d, e, f, g, h, i)) 
    = PairTuple (eltType (undefined :: (a, b, c, d, e, f, g, h))) 
                (eltType' (undefined :: i))
  fromElt (a, b, c, d, e, f, g, h, i) = (fromElt (a, b, c, d, e, f, g, h), fromElt' i)
  toElt (abcdefgh, i) = let (a, b, c, d, e, f, g, h) = toElt abcdefgh
                        in (a, b, c, d, e, f, g, h, toElt' i)

  eltType' (_::(a, b, c, d, e, f, g, h, i)) 
    = PairTuple (eltType (undefined :: (a, b, c, d, e, f, g, h))) 
                (eltType' (undefined :: i))
  fromElt' (a, b, c, d, e, f, g, h, i) = (fromElt (a, b, c, d, e, f, g, h), fromElt' i)
  toElt' (abcdefgh, i) = let (a, b, c, d, e, f, g, h) = toElt abcdefgh
                         in (a, b, c, d, e, f, g, h, toElt' i)

-- |Convenience functions
--

singletonScalarType :: IsScalar a => a -> TupleType ((), a)
singletonScalarType _ = PairTuple UnitTuple (SingleTuple scalarType)

liftToElt :: (Elt a, Elt b) 
          => (EltRepr a -> EltRepr b)
          -> (a -> b)
{-# INLINE liftToElt #-}
liftToElt f = toElt . f . fromElt

liftToElt2 :: (Elt a, Elt b, Elt c) 
           => (EltRepr a -> EltRepr b -> EltRepr c)
           -> (a -> b -> c)
{-# INLINE liftToElt2 #-}
liftToElt2 f = \x y -> toElt $ f (fromElt x) (fromElt y)

sinkFromElt :: (Elt a, Elt b) 
            => (a -> b)
            -> (EltRepr a -> EltRepr b)
{-# INLINE sinkFromElt #-}
sinkFromElt f = fromElt . f . toElt

sinkFromElt2 :: (Elt a, Elt b, Elt c) 
             => (a -> b -> c)
             -> (EltRepr a -> EltRepr b -> EltRepr c)
{-# INLINE sinkFromElt2 #-}
sinkFromElt2 f = \x y -> fromElt $ f (toElt x) (toElt y)

{-# RULES

"fromElt/toElt" forall e.
  fromElt (toElt e) = e

  #-}

-- Surface arrays
-- --------------

-- |Multi-dimensional arrays for array processing
--
-- * If device and host memory are separate, arrays will be transferred to the
--   device when necessary (if possible asynchronously and in parallel with
--   other tasks) and cached on the device if sufficient memory is available.
--
data Array sh e where
  Array :: (Shape sh, Elt e) 
        => EltRepr sh                 -- extent of dimensions = shape
        -> ArrayData (EltRepr e)      -- array payload
        -> Array sh e

deriving instance Typeable2 Array 

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
type DIM0 = Z
type DIM1 = DIM0:.Int
type DIM2 = DIM1:.Int
type DIM3 = DIM2:.Int
type DIM4 = DIM3:.Int
type DIM5 = DIM4:.Int
type DIM6 = DIM5:.Int
type DIM7 = DIM6:.Int
type DIM8 = DIM7:.Int
type DIM9 = DIM8:.Int

-- Shape constraints and indexing
-- 

-- |Shapes and indices of multi-dimensional arrays
--
class (Elt sh, Elt (Any sh), Repr.Shape (EltRepr sh)) => Shape sh where

  -- |Number of dimensions of a /shape/ or /index/ (>= 0).
  dim    :: sh -> Int
  
  -- |Total number of elements in an array of the given /shape/.
  size   :: sh -> Int

  -- |Magic value identifying elements ignored in 'permute'.
  ignore :: sh
  
  -- |Map a multi-dimensional index into one in a linear, row-major 
  -- representation of the array (first argument is the /shape/, second 
  -- argument is the index).
  index  :: sh -> sh -> Int

  -- |Apply a boundary condition to an index.
  bound  :: sh -> sh -> Boundary a -> Either a sh

  -- |Iterate through the entire shape, applying the function; third argument
  -- combines results and fourth is returned in case of an empty iteration
  -- space; the index space is traversed in row-major order.
  iter  :: sh -> (sh -> a) -> (a -> a -> a) -> a -> a

  -- |Convert a minpoint-maxpoint index into a /shape/.
  rangeToShape ::  (sh, sh) -> sh
  
  -- |Convert a /shape/ into a minpoint-maxpoint index.
  shapeToRange ::  sh -> (sh, sh)

  -- |Convert a shape to a list of dimensions.
  shapeToList :: sh -> [Int]

  -- |Convert a list of dimensions into a shape.
  listToShape :: [Int] -> sh

  -- | The slice index for slice specifier 'Any sh'
  sliceAnyIndex :: sh -> Repr.SliceIndex (EltRepr (Any sh)) (EltRepr sh) () (EltRepr sh)

  dim              = Repr.dim . fromElt
  size             = Repr.size . fromElt
  -- (#) must be individually defined, as it only hold for all instances *except* the one with the
  -- largest arity

  ignore           = toElt Repr.ignore
  index sh ix      = Repr.index (fromElt sh) (fromElt ix)
  bound sh ix bndy = case Repr.bound (fromElt sh) (fromElt ix) bndy of
                       Left v    -> Left v
                       Right ix' -> Right $ toElt ix'

  iter sh f c r = Repr.iter (fromElt sh) (f . toElt) c r

  rangeToShape (low, high) 
    = toElt (Repr.rangeToShape (fromElt low, fromElt high))
  shapeToRange ix
    = let (low, high) = Repr.shapeToRange (fromElt ix)
      in
      (toElt low, toElt high)

  shapeToList = Repr.shapeToList . fromElt
  listToShape = toElt . Repr.listToShape

instance Shape Z where
  sliceAnyIndex _ = Repr.SliceNil
  
instance Shape sh => Shape (sh:.Int) where
  sliceAnyIndex _ = Repr.SliceAll (sliceAnyIndex (undefined :: sh))

-- |Slices -aka generalised indices- as n-tuples and mappings of slice
-- indicies to slices, co-slices, and slice dimensions
--
class (Elt sl, Shape (SliceShape sl), Shape (CoSliceShape sl), Shape (FullShape sl)) 
       => Slice sl where
  type SliceShape   sl :: *
  type CoSliceShape sl :: *
  type FullShape    sl :: *
  sliceIndex :: sl -> Repr.SliceIndex (EltRepr sl)
                        (EltRepr (SliceShape   sl))
                        (EltRepr (CoSliceShape sl))
                        (EltRepr (FullShape    sl))

instance Slice Z where
  type SliceShape   Z = Z
  type CoSliceShape Z = Z
  type FullShape    Z = Z
  sliceIndex _ = Repr.SliceNil

instance Slice sl => Slice (sl:.All) where
  type SliceShape   (sl:.All) = SliceShape sl :. Int
  type CoSliceShape (sl:.All) = CoSliceShape sl
  type FullShape    (sl:.All) = FullShape sl :. Int
  sliceIndex _ = Repr.SliceAll (sliceIndex (undefined::sl))

instance Slice sl => Slice (sl:.Int) where
  type SliceShape   (sl:.Int) = SliceShape sl
  type CoSliceShape (sl:.Int) = CoSliceShape sl :. Int
  type FullShape    (sl:.Int) = FullShape sl :. Int
  sliceIndex _ = Repr.SliceFixed (sliceIndex (undefined::sl))

instance Shape sh => Slice (Any sh) where
  type SliceShape   (Any sh) = sh
  type CoSliceShape (Any sh) = Z
  type FullShape    (Any sh) = sh
  sliceIndex _ = sliceAnyIndex (undefined :: sh)

-- Array operations
-- ----------------

-- |Yield an array's shape
--
shape :: Shape sh => Array sh e -> sh
shape (Array sh _) = toElt sh

-- |Array indexing
--
infixl 9 !
(!) :: Array sh e -> sh -> e
{-# INLINE (!) #-}
-- (Array sh adata) ! ix = toElt (adata `indexArrayData` index sh ix)
-- FIXME: using this due to a bug in 6.10.x
(!) (Array sh adata) ix = toElt (adata `indexArrayData` index (toElt sh) ix)

-- |Create an array from its representation function
--
newArray :: (Shape sh, Elt e) => sh -> (sh -> e) -> Array sh e
{-# INLINE newArray #-}
newArray sh f = adata `seq` Array (fromElt sh) adata
  where 
    (adata, _) = runArrayData $ do
                   arr <- newArrayData (1024 `max` size sh)
                   let write ix = writeArrayData arr (index sh ix) 
                                                     (fromElt (f ix))
                   iter sh write (>>) (return ())
                   return (arr, undefined)

-- | Creates a new, uninitialized Accelerate array.
--
allocateArray :: (Shape sh, Elt e) => sh -> Array sh e
{-# INLINE allocateArray #-}
allocateArray sh = adata `seq` Array (fromElt sh) adata
  where
    (adata, _) = runArrayData $ (,undefined) `fmap` newArrayData (1024 `max` size sh)


-- |Convert an 'IArray' to an accelerated array.
--
fromIArray :: (EltRepr ix ~ EltRepr sh, IArray a e, IArray.Ix ix, Shape sh, Elt ix, Elt e)
           => a ix e -> Array sh e
fromIArray iarr = newArray (toElt sh) (\ix -> iarr IArray.! toElt (fromElt ix))
  where
    (lo,hi) = IArray.bounds iarr
    sh      = Repr.rangeToShape (fromElt lo, fromElt hi)

-- |Convert an accelerated array to an 'IArray'
-- 
toIArray :: (EltRepr ix ~ EltRepr sh, IArray a e, IArray.Ix ix, Shape sh, Elt ix, Elt e) 
         => Array sh e -> a ix e
toIArray arr = IArray.array bnds [(ix, arr ! toElt (fromElt ix)) | ix <- IArray.range bnds]
  where
    (lo,hi) = Repr.shapeToRange (fromElt (shape arr))
    bnds    = (toElt lo, toElt hi)

-- |Convert a list (with elements in row-major order) to an accelerated array.
--
fromList :: (Shape sh, Elt e) => sh -> [e] -> Array sh e
fromList sh l = newArray sh indexIntoList 
  where
    indexIntoList ix = l!!index sh ix

-- |Convert an accelerated array to a list in row-major order.
--
toList :: forall sh e. Array sh e -> [e]
toList (Array sh adata) = iter sh' idx (.) id []
  where
    sh'    = toElt sh :: sh
    idx ix = \l -> toElt (adata `indexArrayData` index sh' ix) : l

-- Convert an array to a string
--
instance Show (Array sh e) where
  show arr@(Array sh _adata) 
    = "Array " ++ show (toElt sh :: sh) ++ " " ++ show (toList arr)

