{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Type
-- Copyright   : [2008..2017] Manuel M T Chakravarty, Gabriele Keller
--               [2009..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
--  /Scalar types supported in array computations/
--
--  Integral types:
--    * Int
--    * Int8
--    * Int16
--    * Int32
--    * Int64
--    * Word
--    * Word8
--    * Word16
--    * Word32
--    * Word64
--    * CShort
--    * CUShort
--    * CInt
--    * CUInt
--    * CLong
--    * CULong
--    * CLLong
--    * CULLong
--
--  Floating types:
--    * Half
--    * Float
--    * Double
--    * CFloat
--    * CDouble
--
--  Non-numeric types:
--    * Bool
--    * Char
--    * CChar
--    * CSChar
--    * CUChar
--
--  SIMD vector types:
--    * V2
--    * V3
--    * V4
--    * V8
--    * V16
--
-- Note that 'Int' has the same bit width as in plain Haskell computations.
-- 'Float' and 'Double' represent IEEE single and double precision floating
-- point numbers, respectively.
--

module Data.Array.Accelerate.Type (
  Half(..), Float, Double, Char, Bool(..),
  module Data.Int,
  module Data.Word,
  module Foreign.C.Types,
  module Data.Array.Accelerate.Type
) where


import Data.Orphans ()    -- orphan instances for 8-tuples and beyond

-- standard libraries
import Data.Bits
import Data.Int
import Data.Type.Equality
import Data.Typeable
import Data.Word
import GHC.TypeLits
import Numeric.Half
import Text.Printf
import Foreign.Storable
import Foreign.C.Types
    (CChar, CSChar, CUChar, CShort, CUShort, CInt, CUInt, CLong, CULong, CLLong, CULLong, CFloat, CDouble)


-- Scalar types
-- ------------

-- Reified dictionaries
--

data IntegralDict a where
  IntegralDict :: ( Bounded a, Eq a, Ord a, Show a
                  , Bits a, FiniteBits a, Integral a, Num a, Real a, Storable a )
               => IntegralDict a

data FloatingDict a where
  FloatingDict :: ( Eq a, Ord a, Show a
                  , Floating a, Fractional a, Num a, Real a, RealFrac a
                  , RealFloat a, Storable a )
               => FloatingDict a

data NonNumDict a where
  NonNumDict :: ( Bounded a, Eq a, Ord a, Show a, Storable a )
             => NonNumDict a


-- Scalar type representation
--

-- | Integral types supported in array computations.
--
data IntegralType a where
  TypeInt     :: IntegralDict Int     -> IntegralType Int
  TypeInt8    :: IntegralDict Int8    -> IntegralType Int8
  TypeInt16   :: IntegralDict Int16   -> IntegralType Int16
  TypeInt32   :: IntegralDict Int32   -> IntegralType Int32
  TypeInt64   :: IntegralDict Int64   -> IntegralType Int64
  TypeWord    :: IntegralDict Word    -> IntegralType Word
  TypeWord8   :: IntegralDict Word8   -> IntegralType Word8
  TypeWord16  :: IntegralDict Word16  -> IntegralType Word16
  TypeWord32  :: IntegralDict Word32  -> IntegralType Word32
  TypeWord64  :: IntegralDict Word64  -> IntegralType Word64
  TypeCShort  :: IntegralDict CShort  -> IntegralType CShort
  TypeCUShort :: IntegralDict CUShort -> IntegralType CUShort
  TypeCInt    :: IntegralDict CInt    -> IntegralType CInt
  TypeCUInt   :: IntegralDict CUInt   -> IntegralType CUInt
  TypeCLong   :: IntegralDict CLong   -> IntegralType CLong
  TypeCULong  :: IntegralDict CULong  -> IntegralType CULong
  TypeCLLong  :: IntegralDict CLLong  -> IntegralType CLLong
  TypeCULLong :: IntegralDict CULLong -> IntegralType CULLong

-- | Floating-point types supported in array computations.
--
data FloatingType a where
  TypeHalf    :: FloatingDict Half    -> FloatingType Half
  TypeFloat   :: FloatingDict Float   -> FloatingType Float
  TypeDouble  :: FloatingDict Double  -> FloatingType Double
  TypeCFloat  :: FloatingDict CFloat  -> FloatingType CFloat
  TypeCDouble :: FloatingDict CDouble -> FloatingType CDouble

-- | Non-numeric types supported in array computations.
--
data NonNumType a where
  TypeBool    :: NonNumDict Bool      -> NonNumType Bool   --  marshalled to Word8
  TypeChar    :: NonNumDict Char      -> NonNumType Char
  TypeCChar   :: NonNumDict CChar     -> NonNumType CChar
  TypeCSChar  :: NonNumDict CSChar    -> NonNumType CSChar
  TypeCUChar  :: NonNumDict CUChar    -> NonNumType CUChar

-- | Numeric element types implement Num & Real
--
data NumType a where
  IntegralNumType :: IntegralType a -> NumType a
  FloatingNumType :: FloatingType a -> NumType a

-- | Bounded element types implement Bounded
--
data BoundedType a where
  IntegralBoundedType :: IntegralType a -> BoundedType a
  NonNumBoundedType   :: NonNumType a   -> BoundedType a

-- | All scalar element types implement Eq & Ord
--
data ScalarType a where
  SingleScalarType :: SingleType a     -> ScalarType a
  VectorScalarType :: VectorType (v a) -> ScalarType (v a)

data SingleType a where
  NumSingleType    :: NumType a    -> SingleType a
  NonNumSingleType :: NonNumType a -> SingleType a

data VectorType v where
  Vector2Type   :: SingleType a -> VectorType (V2 a)
  Vector3Type   :: SingleType a -> VectorType (V3 a)
  Vector4Type   :: SingleType a -> VectorType (V4 a)
  Vector8Type   :: SingleType a -> VectorType (V8 a)
  Vector16Type  :: SingleType a -> VectorType (V16 a)

-- Showing type names
--

instance Show (IntegralType a) where
  show (TypeInt _)     = "Int"
  show (TypeInt8 _)    = "Int8"
  show (TypeInt16 _)   = "Int16"
  show (TypeInt32 _)   = "Int32"
  show (TypeInt64 _)   = "Int64"
  show (TypeWord _)    = "Word"
  show (TypeWord8 _)   = "Word8"
  show (TypeWord16 _)  = "Word16"
  show (TypeWord32 _)  = "Word32"
  show (TypeWord64 _)  = "Word64"
  show (TypeCShort _)  = "CShort"
  show (TypeCUShort _) = "CUShort"
  show (TypeCInt _)    = "CInt"
  show (TypeCUInt _)   = "CUInt"
  show (TypeCLong _)   = "CLong"
  show (TypeCULong _)  = "CULong"
  show (TypeCLLong _)  = "CLLong"
  show (TypeCULLong _) = "CULLong"

instance Show (FloatingType a) where
  show (TypeHalf _)    = "Half"
  show (TypeFloat _)   = "Float"
  show (TypeDouble _)  = "Double"
  show (TypeCFloat _)  = "CFloat"
  show (TypeCDouble _) = "CDouble"

instance Show (NonNumType a) where
  show (TypeBool _)   = "Bool"
  show (TypeChar _)   = "Char"
  show (TypeCChar _)  = "CChar"
  show (TypeCSChar _) = "CSChar"
  show (TypeCUChar _) = "CUChar"

instance Show (NumType a) where
  show (IntegralNumType ty) = show ty
  show (FloatingNumType ty) = show ty

instance Show (BoundedType a) where
  show (IntegralBoundedType ty) = show ty
  show (NonNumBoundedType ty)   = show ty

instance Show (SingleType a) where
  show (NumSingleType ty)    = show ty
  show (NonNumSingleType ty) = show ty

instance Show (VectorType a) where
  show (Vector2Type t)  = printf "<2 x %s>" (show t)
  show (Vector3Type t)  = printf "<3 x %s>" (show t)
  show (Vector4Type t)  = printf "<4 x %s>" (show t)
  show (Vector8Type t)  = printf "<8 x %s>" (show t)
  show (Vector16Type t) = printf "<16 x %s>" (show t)

instance Show (ScalarType a) where
  show (SingleScalarType ty) = show ty
  show (VectorScalarType ty) = show ty


-- Querying scalar type representations
--
-- TODO: generate these automatically using TH. It would be better if we could
-- use the digItOut trick (see FromIntegral.hs), but then we'd have to shuffle
-- things around to different modules.
--

-- | Integral types
--
class (IsSingle a, IsNum a, IsBounded a) => IsIntegral a where
  integralType :: IntegralType a

instance IsIntegral Int where
  integralType = TypeInt IntegralDict

instance IsIntegral Int8 where
  integralType = TypeInt8 IntegralDict

instance IsIntegral Int16 where
  integralType = TypeInt16 IntegralDict

instance IsIntegral Int32 where
  integralType = TypeInt32 IntegralDict

instance IsIntegral Int64 where
  integralType = TypeInt64 IntegralDict

instance IsIntegral Word where
  integralType = TypeWord IntegralDict

instance IsIntegral Word8 where
  integralType = TypeWord8 IntegralDict

instance IsIntegral Word16 where
  integralType = TypeWord16 IntegralDict

instance IsIntegral Word32 where
  integralType = TypeWord32 IntegralDict

instance IsIntegral Word64 where
  integralType = TypeWord64 IntegralDict

instance IsIntegral CShort where
  integralType = TypeCShort IntegralDict

instance IsIntegral CUShort where
  integralType = TypeCUShort IntegralDict

instance IsIntegral CInt where
  integralType = TypeCInt IntegralDict

instance IsIntegral CUInt where
  integralType = TypeCUInt IntegralDict

instance IsIntegral CLong where
  integralType = TypeCLong IntegralDict

instance IsIntegral CULong where
  integralType = TypeCULong IntegralDict

instance IsIntegral CLLong where
  integralType = TypeCLLong IntegralDict

instance IsIntegral CULLong where
  integralType = TypeCULLong IntegralDict

-- | Floating types
--
class (Floating a, IsSingle a, IsNum a) => IsFloating a where
  floatingType :: FloatingType a

instance IsFloating Half where
  floatingType = TypeHalf FloatingDict

instance IsFloating Float where
  floatingType = TypeFloat FloatingDict

instance IsFloating Double where
  floatingType = TypeDouble FloatingDict

instance IsFloating CFloat where
  floatingType = TypeCFloat FloatingDict

instance IsFloating CDouble where
  floatingType = TypeCDouble FloatingDict

-- | Non-numeric types
--
class IsNonNum a where
  nonNumType :: NonNumType a

instance IsNonNum Bool where
  nonNumType = TypeBool NonNumDict

instance IsNonNum Char where
  nonNumType = TypeChar NonNumDict

instance IsNonNum CChar where
  nonNumType = TypeCChar NonNumDict

instance IsNonNum CSChar where
  nonNumType = TypeCSChar NonNumDict

instance IsNonNum CUChar where
  nonNumType = TypeCUChar NonNumDict

-- | Numeric types
--
class (Num a, IsSingle a) => IsNum a where
  numType :: NumType a

instance IsNum Int where
  numType = IntegralNumType integralType

instance IsNum Int8 where
  numType = IntegralNumType integralType

instance IsNum Int16 where
  numType = IntegralNumType integralType

instance IsNum Int32 where
  numType = IntegralNumType integralType

instance IsNum Int64 where
  numType = IntegralNumType integralType

instance IsNum Word where
  numType = IntegralNumType integralType

instance IsNum Word8 where
  numType = IntegralNumType integralType

instance IsNum Word16 where
  numType = IntegralNumType integralType

instance IsNum Word32 where
  numType = IntegralNumType integralType

instance IsNum Word64 where
  numType = IntegralNumType integralType

instance IsNum CShort where
  numType = IntegralNumType integralType

instance IsNum CUShort where
  numType = IntegralNumType integralType

instance IsNum CInt where
  numType = IntegralNumType integralType

instance IsNum CUInt where
  numType = IntegralNumType integralType

instance IsNum CLong where
  numType = IntegralNumType integralType

instance IsNum CULong where
  numType = IntegralNumType integralType

instance IsNum CLLong where
  numType = IntegralNumType integralType

instance IsNum CULLong where
  numType = IntegralNumType integralType

instance IsNum Half where
  numType = FloatingNumType floatingType

instance IsNum Float where
  numType = FloatingNumType floatingType

instance IsNum Double where
  numType = FloatingNumType floatingType

instance IsNum CFloat where
  numType = FloatingNumType floatingType

instance IsNum CDouble where
  numType = FloatingNumType floatingType

-- | Bounded types
--
class IsBounded a where
  boundedType :: BoundedType a

instance IsBounded Int where
  boundedType = IntegralBoundedType integralType

instance IsBounded Int8 where
  boundedType = IntegralBoundedType integralType

instance IsBounded Int16 where
  boundedType = IntegralBoundedType integralType

instance IsBounded Int32 where
  boundedType = IntegralBoundedType integralType

instance IsBounded Int64 where
  boundedType = IntegralBoundedType integralType

instance IsBounded Word where
  boundedType = IntegralBoundedType integralType

instance IsBounded Word8 where
  boundedType = IntegralBoundedType integralType

instance IsBounded Word16 where
  boundedType = IntegralBoundedType integralType

instance IsBounded Word32 where
  boundedType = IntegralBoundedType integralType

instance IsBounded Word64 where
  boundedType = IntegralBoundedType integralType

instance IsBounded CShort where
  boundedType = IntegralBoundedType integralType

instance IsBounded CUShort where
  boundedType = IntegralBoundedType integralType

instance IsBounded CInt where
  boundedType = IntegralBoundedType integralType

instance IsBounded CUInt where
  boundedType = IntegralBoundedType integralType

instance IsBounded CLong where
  boundedType = IntegralBoundedType integralType

instance IsBounded CULong where
  boundedType = IntegralBoundedType integralType

instance IsBounded CLLong where
  boundedType = IntegralBoundedType integralType

instance IsBounded CULLong where
  boundedType = IntegralBoundedType integralType

instance IsBounded Bool where
  boundedType = NonNumBoundedType nonNumType

instance IsBounded Char where
  boundedType = NonNumBoundedType nonNumType

instance IsBounded CChar where
  boundedType = NonNumBoundedType nonNumType

instance IsBounded CSChar where
  boundedType = NonNumBoundedType nonNumType

instance IsBounded CUChar where
  boundedType = NonNumBoundedType nonNumType

-- | All single value types
--
class IsScalar a => IsSingle a where
  singleType :: SingleType a

instance IsSingle Int where
  singleType = NumSingleType numType

instance IsSingle Int8 where
  singleType = NumSingleType numType

instance IsSingle Int16 where
  singleType = NumSingleType numType

instance IsSingle Int32 where
  singleType = NumSingleType numType

instance IsSingle Int64 where
  singleType = NumSingleType numType

instance IsSingle Word where
  singleType = NumSingleType numType

instance IsSingle Word8 where
  singleType = NumSingleType numType

instance IsSingle Word16 where
  singleType = NumSingleType numType

instance IsSingle Word32 where
  singleType = NumSingleType numType

instance IsSingle Word64 where
  singleType = NumSingleType numType

instance IsSingle CShort where
  singleType = NumSingleType numType

instance IsSingle CUShort where
  singleType = NumSingleType numType

instance IsSingle CInt where
  singleType = NumSingleType numType

instance IsSingle CUInt where
  singleType = NumSingleType numType

instance IsSingle CLong where
  singleType = NumSingleType numType

instance IsSingle CULong where
  singleType = NumSingleType numType

instance IsSingle CLLong where
  singleType = NumSingleType numType

instance IsSingle CULLong where
  singleType = NumSingleType numType

instance IsSingle Half where
  singleType = NumSingleType numType

instance IsSingle Float where
  singleType = NumSingleType numType

instance IsSingle Double where
  singleType = NumSingleType numType

instance IsSingle CFloat where
  singleType = NumSingleType numType

instance IsSingle CDouble where
  singleType = NumSingleType numType

instance IsSingle Bool where
  singleType = NonNumSingleType nonNumType

instance IsSingle Char where
  singleType = NonNumSingleType nonNumType

instance IsSingle CChar where
  singleType = NonNumSingleType nonNumType

instance IsSingle CSChar where
  singleType = NonNumSingleType nonNumType

instance IsSingle CUChar where
  singleType = NonNumSingleType nonNumType


-- | All scalar types
--
class Typeable a => IsScalar a where
  scalarType :: ScalarType a

instance IsScalar Int where
  scalarType = SingleScalarType singleType

instance IsScalar Int8 where
  scalarType = SingleScalarType singleType

instance IsScalar Int16 where
  scalarType = SingleScalarType singleType

instance IsScalar Int32 where
  scalarType = SingleScalarType singleType

instance IsScalar Int64 where
  scalarType = SingleScalarType singleType

instance IsScalar Word where
  scalarType = SingleScalarType singleType

instance IsScalar Word8 where
  scalarType = SingleScalarType singleType

instance IsScalar Word16 where
  scalarType = SingleScalarType singleType

instance IsScalar Word32 where
  scalarType = SingleScalarType singleType

instance IsScalar Word64 where
  scalarType = SingleScalarType singleType

instance IsScalar CShort where
  scalarType = SingleScalarType singleType

instance IsScalar CUShort where
  scalarType = SingleScalarType singleType

instance IsScalar CInt where
  scalarType = SingleScalarType singleType

instance IsScalar CUInt where
  scalarType = SingleScalarType singleType

instance IsScalar CLong where
  scalarType = SingleScalarType singleType

instance IsScalar CULong where
  scalarType = SingleScalarType singleType

instance IsScalar CLLong where
  scalarType = SingleScalarType singleType

instance IsScalar CULLong where
  scalarType = SingleScalarType singleType

instance IsScalar Half where
  scalarType = SingleScalarType singleType

instance IsScalar Float where
  scalarType = SingleScalarType singleType

instance IsScalar Double where
  scalarType = SingleScalarType singleType

instance IsScalar CFloat where
  scalarType = SingleScalarType singleType

instance IsScalar CDouble where
  scalarType = SingleScalarType singleType

instance IsScalar Bool where
  scalarType = SingleScalarType singleType

instance IsScalar Char where
  scalarType = SingleScalarType singleType

instance IsScalar CChar where
  scalarType = SingleScalarType singleType

instance IsScalar CSChar where
  scalarType = SingleScalarType singleType

instance IsScalar CUChar where
  scalarType = SingleScalarType singleType


-- XXX: Temporary to build D.A.A.Data.Complex module.
--      Replace with auto-generated instances.
instance IsScalar (V2 Float) where
  scalarType = VectorScalarType (Vector2Type singleType)

instance IsScalar (V2 Double) where
  scalarType = VectorScalarType (Vector2Type singleType)


-- Extract reified dictionaries
--

integralDict :: IntegralType a -> IntegralDict a
integralDict (TypeInt     dict) = dict
integralDict (TypeInt8    dict) = dict
integralDict (TypeInt16   dict) = dict
integralDict (TypeInt32   dict) = dict
integralDict (TypeInt64   dict) = dict
integralDict (TypeWord    dict) = dict
integralDict (TypeWord8   dict) = dict
integralDict (TypeWord16  dict) = dict
integralDict (TypeWord32  dict) = dict
integralDict (TypeWord64  dict) = dict
integralDict (TypeCShort  dict) = dict
integralDict (TypeCUShort dict) = dict
integralDict (TypeCInt    dict) = dict
integralDict (TypeCUInt   dict) = dict
integralDict (TypeCLong   dict) = dict
integralDict (TypeCULong  dict) = dict
integralDict (TypeCLLong  dict) = dict
integralDict (TypeCULLong dict) = dict

floatingDict :: FloatingType a -> FloatingDict a
floatingDict (TypeHalf dict)    = dict
floatingDict (TypeFloat dict)   = dict
floatingDict (TypeDouble dict)  = dict
floatingDict (TypeCFloat dict)  = dict
floatingDict (TypeCDouble dict) = dict

nonNumDict :: NonNumType a -> NonNumDict a
nonNumDict (TypeBool   dict) = dict
nonNumDict (TypeChar   dict) = dict
nonNumDict (TypeCChar  dict) = dict
nonNumDict (TypeCSChar dict) = dict
nonNumDict (TypeCUChar dict) = dict


-- Type representation
-- -------------------
--
-- Representation of product types, consisting of:
--
--   * unit (void)
--
--   * scalar types: values which go in registers. These may be single value
--     types such as int and float, or SIMD vectors of single value types such
--     as <4 * float>. We do not allow vectors-of-vectors.
--
--   * pairs: representing compound values (i.e. tuples) where each component
--     will be stored in a separate array.
--
data TupleType a where
  TypeRunit   ::                               TupleType ()
  TypeRscalar :: ScalarType a               -> TupleType a
  TypeRpair   :: TupleType a -> TupleType b -> TupleType (a, b)

instance Show (TupleType a) where
  show TypeRunit        = "()"
  show (TypeRscalar t)  = show t
  show (TypeRpair a b)  = printf "(%s,%s)" (show a) (show b)


-- Type-level bit sizes
-- --------------------

-- |Constraint that values of these two types have the same bit width
--
type BitSizeEq a b = (BitSize a == BitSize b) ~ 'True

type family BitSize a :: Nat

type instance BitSize Int8    = 8
type instance BitSize Int16   = 16
type instance BitSize Int32   = 32
type instance BitSize Int64   = 64
type instance BitSize Word8   = 8
type instance BitSize Word16  = 16
type instance BitSize Word32  = 32
type instance BitSize Word64  = 64
type instance BitSize Char    = 32
type instance BitSize Bool    = 1      -- but actually stored as Word8 |:

type instance BitSize CShort  = 16
type instance BitSize CUShort = 16
type instance BitSize CInt    = 32
type instance BitSize CUInt   = 32
type instance BitSize CLLong  = 64
type instance BitSize CULLong = 64
type instance BitSize CChar   = 8
type instance BitSize CUChar  = 8
type instance BitSize CSChar  = 8

type instance BitSize Half    = 16
type instance BitSize Float   = 32
type instance BitSize CFloat  = 32
type instance BitSize Double  = 64
type instance BitSize CDouble = 64

type instance BitSize (V2 a)  = 2  * BitSize a
type instance BitSize (V3 a)  = 3  * BitSize a
type instance BitSize (V4 a)  = 4  * BitSize a
type instance BitSize (V8 a)  = 8  * BitSize a
type instance BitSize (V16 a) = 16 * BitSize a

type instance BitSize Int    = $( case finiteBitSize (undefined::Int) of
                                    32 -> [t| 32 |]
                                    64 -> [t| 64 |]
                                    _  -> error "I don't know what architecture I am"  )

type instance BitSize Word   = $( case finiteBitSize (undefined::Word) of
                                    32 -> [t| 32 |]
                                    64 -> [t| 64 |]
                                    _  -> error "I don't know what architecture I am"  )

type instance BitSize CLong  = $( case finiteBitSize (undefined::CLong) of
                                    32 -> [t| 32 |]
                                    64 -> [t| 64 |]
                                    _  -> error "I don't know what architecture I am"  )

type instance BitSize CULong = $( case finiteBitSize (undefined::CULong) of
                                    32 -> [t| 32 |]
                                    64 -> [t| 64 |]
                                    _  -> error "I don't know what architecture I am"  )


-- SIMD vector types
-- -----------------

data V2 a  = V2 !a !a
  deriving (Typeable, Eq, Ord)

data V3 a  = V3 !a !a !a
  deriving (Typeable, Eq, Ord)

data V4 a  = V4 !a !a !a !a
  deriving (Typeable, Eq, Ord)

data V8 a  = V8 !a !a !a !a !a !a !a !a
  deriving (Typeable, Eq, Ord)

data V16 a = V16 !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a
  deriving (Typeable, Eq, Ord)

instance Show a => Show (V2 a) where
  show (V2 a b) = printf "<%s,%s>" (show a) (show b)

instance Show a => Show (V3 a) where
  show (V3 a b c) = printf "<%s,%s,%s>" (show a) (show b) (show c)

instance Show a => Show (V4 a) where
  show (V4 a b c d) = printf "<%s,%s,%s,%s>" (show a) (show b) (show c) (show d)

instance Show a => Show (V8 a) where
  show (V8 a b c d e f g h) =
    printf "<%s,%s,%s,%s,%s,%s,%s,%s>"
      (show a) (show b) (show c) (show d) (show e) (show f) (show g) (show h)

instance Show a => Show (V16 a) where
  show (V16 a b c d e f g h i j k l m n o p) =
    printf "<%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s>"
      (show a) (show b) (show c) (show d) (show e) (show f) (show g) (show h)
      (show i) (show j) (show k) (show l) (show m) (show n) (show o) (show p)

