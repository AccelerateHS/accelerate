{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Type
-- Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
--               [2009..2012] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
--  /Scalar types supported in array computations/
--
--  Integral types: Int, Int8, Int16, Int32, Int64, Word, Word8, Word16, Word32,
--    Word64, CShort, CUShort, CInt, CUInt, CLong, CULong, CLLong, CULLong
--
--  Floating types: Float, Double, CFloat, CDouble
--
--  Non-numeric types: Bool, Char, CChar, CSChar, CUChar
--
--  'Int' has the same bitwidth as in plain Haskell computations, and 'Float'
--  and 'Double' represent IEEE single and double precision floating point
--  numbers, respectively.
--

module Data.Array.Accelerate.Type (
  module Data.Int,
  module Data.Word,
  module Foreign.C.Types,
  module Data.Array.Accelerate.Type
) where

-- standard libraries
import Data.Bits
import Data.Int
import Data.Typeable
import Data.Word
import Foreign.Storable
import Foreign.C.Types (
  CChar, CSChar, CUChar, CShort, CUShort, CInt, CUInt, CLong, CULong,
  CLLong, CULLong, CFloat, CDouble)
  -- in the future, CHalf


-- Extend Typeable support for 8- and 9-tuple
-- ------------------------------------------

myMkTyCon :: String -> TyCon
myMkTyCon = mkTyCon3 "accelerate" "Data.Array.Accelerate.Type"

class Typeable8 t where
  typeOf8 :: t a b c d e f g h -> TypeRep

instance Typeable8 (,,,,,,,) where
  typeOf8 _ = myMkTyCon "(,,,,,,,)" `mkTyConApp` []

typeOf7Default :: (Typeable8 t, Typeable a) => t a b c d e f g h -> TypeRep
typeOf7Default x = typeOf8 x `mkAppTy` typeOf (argType x)
 where
   argType :: t a b c d e f g h -> a
   argType =  undefined

instance (Typeable8 s, Typeable a)
       => Typeable7 (s a) where
  typeOf7 = typeOf7Default
  
class Typeable9 t where
  typeOf9 :: t a b c d e f g h i -> TypeRep

instance Typeable9 (,,,,,,,,) where
  typeOf9 _ = myMkTyCon "(,,,,,,,,)" `mkTyConApp` []

typeOf8Default :: (Typeable9 t, Typeable a) => t a b c d e f g h i -> TypeRep
typeOf8Default x = typeOf9 x `mkAppTy` typeOf (argType x)
 where
   argType :: t a b c d e f g h i -> a
   argType =  undefined

instance (Typeable9 s, Typeable a)
       => Typeable8 (s a) where
  typeOf8 = typeOf8Default



-- Scalar types
-- ------------

-- Reified dictionaries
-- 

data IntegralDict a where
  IntegralDict :: ( Bounded a, Enum a, Eq a, Ord a, Show a
                  , Bits a, Integral a, Num a, Real a, Storable a)
               => IntegralDict a

data FloatingDict a where
  FloatingDict :: ( Enum a, Eq a, Ord a, Show a
                  , Floating a, Fractional a, Num a, Real a, RealFrac a
                  , RealFloat a, Storable a)
               => FloatingDict a

data NonNumDict a where
  NonNumDict :: (Bounded a, Enum a, Eq a, Ord a, Show a, Storable a)
             => NonNumDict a

-- Scalar type representation
-- 

-- |Integral types supported in array computations.
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

-- |Floating-point types supported in array computations.
--
data FloatingType a where
  TypeFloat   :: FloatingDict Float   -> FloatingType Float
  TypeDouble  :: FloatingDict Double  -> FloatingType Double
  TypeCFloat  :: FloatingDict CFloat  -> FloatingType CFloat
  TypeCDouble :: FloatingDict CDouble -> FloatingType CDouble

-- |Non-numeric types supported in array computations.
--
data NonNumType a where
  TypeBool    :: NonNumDict Bool      -> NonNumType Bool   --  marshalled to CInt
  TypeChar    :: NonNumDict Char      -> NonNumType Char
  TypeCChar   :: NonNumDict CChar     -> NonNumType CChar
  TypeCSChar  :: NonNumDict CSChar    -> NonNumType CSChar
  TypeCUChar  :: NonNumDict CUChar    -> NonNumType CUChar

-- |Numeric element types implement Num & Real
--
data NumType a where
  IntegralNumType :: IntegralType a -> NumType a
  FloatingNumType :: FloatingType a -> NumType a

-- |Bounded element types implement Bounded
--
data BoundedType a where
  IntegralBoundedType :: IntegralType a -> BoundedType a
  NonNumBoundedType   :: NonNumType a   -> BoundedType a

-- |All scalar element types implement Eq, Ord & Enum
--
data ScalarType a where
  NumScalarType    :: NumType a    -> ScalarType a
  NonNumScalarType :: NonNumType a -> ScalarType a

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

instance Show (ScalarType a) where
  show (NumScalarType ty)    = show ty
  show (NonNumScalarType ty) = show ty

instance Show (TupleType a) where 
  show UnitTuple = "()"
  show (SingleTuple scalarTy) = show scalarTy
  show (PairTuple a b) = "("++show a++", "++show b++")"

-- Querying scalar type representations
-- 

-- |Integral types
--
class (IsScalar a, IsNum a, IsBounded a) => IsIntegral a where
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

-- |Floating types
--
class (Floating a, IsScalar a, IsNum a) => IsFloating a where
  floatingType :: FloatingType a

instance IsFloating Float where
  floatingType = TypeFloat FloatingDict

instance IsFloating Double where
  floatingType = TypeDouble FloatingDict

instance IsFloating CFloat where
  floatingType = TypeCFloat FloatingDict

instance IsFloating CDouble where
  floatingType = TypeCDouble FloatingDict

-- |Non-numeric types
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

-- |Numeric types
--
class (Num a, IsScalar a) => IsNum a where
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

instance IsNum Float where
  numType = FloatingNumType floatingType

instance IsNum Double where
  numType = FloatingNumType floatingType

instance IsNum CFloat where
  numType = FloatingNumType floatingType

instance IsNum CDouble where
  numType = FloatingNumType floatingType

-- |Bounded types
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

-- |All scalar type
--
class Typeable a => IsScalar a where
  scalarType :: ScalarType a

instance IsScalar Int where
  scalarType = NumScalarType numType

instance IsScalar Int8 where
  scalarType = NumScalarType numType

instance IsScalar Int16 where
  scalarType = NumScalarType numType

instance IsScalar Int32 where
  scalarType = NumScalarType numType

instance IsScalar Int64 where
  scalarType = NumScalarType numType

instance IsScalar Word where
  scalarType = NumScalarType numType

instance IsScalar Word8 where
  scalarType = NumScalarType numType

instance IsScalar Word16 where
  scalarType = NumScalarType numType

instance IsScalar Word32 where
  scalarType = NumScalarType numType

instance IsScalar Word64 where
  scalarType = NumScalarType numType

instance IsScalar CShort where
  scalarType = NumScalarType numType

instance IsScalar CUShort where
  scalarType = NumScalarType numType

instance IsScalar CInt where
  scalarType = NumScalarType numType

instance IsScalar CUInt where
  scalarType = NumScalarType numType

instance IsScalar CLong where
  scalarType = NumScalarType numType

instance IsScalar CULong where
  scalarType = NumScalarType numType

instance IsScalar CLLong where
  scalarType = NumScalarType numType

instance IsScalar CULLong where
  scalarType = NumScalarType numType

instance IsScalar Float where
  scalarType = NumScalarType numType

instance IsScalar Double where
  scalarType = NumScalarType numType

instance IsScalar CFloat where
  scalarType = NumScalarType numType

instance IsScalar CDouble where
  scalarType = NumScalarType numType

instance IsScalar Bool where
  scalarType = NonNumScalarType nonNumType

instance IsScalar Char where
  scalarType = NonNumScalarType nonNumType

instance IsScalar CChar where
  scalarType = NonNumScalarType nonNumType

instance IsScalar CSChar where
  scalarType = NonNumScalarType nonNumType

instance IsScalar CUChar where
  scalarType = NonNumScalarType nonNumType

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
floatingDict (TypeFloat dict) = dict
floatingDict (TypeDouble dict) = dict
floatingDict (TypeCFloat dict) = dict
floatingDict (TypeCDouble dict) = dict

nonNumDict :: NonNumType a -> NonNumDict a
nonNumDict (TypeBool   dict) = dict
nonNumDict (TypeChar   dict) = dict
nonNumDict (TypeCChar  dict) = dict
nonNumDict (TypeCSChar dict) = dict
nonNumDict (TypeCUChar dict) = dict


-- Tuple type
-- ----------

data TupleType a where
  UnitTuple   ::                               TupleType ()
  SingleTuple :: ScalarType a               -> TupleType a
  PairTuple   :: TupleType a -> TupleType b -> TupleType (a, b)

-- Stencil support
-- ---------------

-- |Boundary condition specification for stencil operations.
--
data Boundary a = Clamp               -- ^clamp coordinates to the extent of the array
                | Mirror              -- ^mirror coordinates beyond the array extent
                | Wrap                -- ^wrap coordinates around on each dimension
                | Constant a          -- ^use a constant value for outlying coordinates 
                deriving (Show, Read)

{-
-- Vector GPU data types
-- ---------------------

data CChar1 = CChar1 CChar
data CChar2 = CChar2 CChar CChar
data CChar3 = CChar3 CChar CChar CChar
data CChar4 = CChar4 CChar CChar CChar CChar
data CSChar1 = CSChar1 CSChar
data CSChar2 = CSChar2 CSChar CSChar
data CSChar3 = CSChar3 CSChar CSChar CSChar
data CSChar4 = CSChar4 CSChar CSChar CSChar CSChar
data CUChar1 = CUChar1 CUChar
data CUChar2 = CUChar2 CUChar CUChar
data CUChar3 = CUChar3 CUChar CUChar CUChar
data CUChar4 = CUChar4 CUChar CUChar CUChar CUChar
data CShort1 = CShort1 CShort
data CShort2 = CShort2 CShort CShort
data CShort3 = CShort3 CShort CShort CShort
data CShort4 = CShort4 CShort CShort CShort CShort
data CUShort1 = CUShort1 CUShort
data CUShort2 = CUShort2 CUShort CUShort
data CUShort3 = CUShort3 CUShort CUShort CUShort
data CUShort4 = CUShort4 CUShort CUShort CUShort CUShort
data CInt1 = CInt1 CInt
data CInt2 = CInt2 CInt CInt
data CInt3 = CInt3 CInt CInt CInt
data CInt4 = CInt4 CInt CInt CInt CInt
data CUInt1 = CUInt1 CUInt
data CUInt2 = CUInt2 CUInt CUInt
data CUInt3 = CUInt3 CUInt CUInt CUInt
data CUInt4 = CUInt4 CUInt CUInt CUInt CUInt
data CLong1 = CLong1 CLong
data CLong2 = CLong2 CLong CLong
data CLong3 = CLong3 CLong CLong CLong
data CLong4 = CLong4 CLong CLong CLong CLong
data CULong1 = CULong1 CULong
data CULong2 = CULong2 CULong CULong
data CULong3 = CULong3 CULong CULong CULong
data CULong4 = CULong4 CULong CULong CULong CULong
data CLLong1 = CLLong1 CLLong
data CLLong2 = CLLong2 CLLong CLLong
data CLLong3 = CLLong3 CLLong CLLong CLLong
data CLLong4 = CLLong4 CLLong CLLong CLLong CLLong
data CULLong1 = CULLong1 CULLong
data CULLong2 = CULLong2 CULLong CULLong
data CULLong3 = CULLong3 CULLong CULLong CULLong
data CULLong4 = CULLong4 CULLong CULLong CULLong CULLong
data CFloat1 = CFloat1 CFloat
data CFloat2 = CFloat2 CFloat CFloat
data CFloat3 = CFloat3 CFloat CFloat CFloat
data CFloat4 = CFloat4 CFloat CFloat CFloat CFloat
data CDouble1 = CDouble1 CDouble
data CDouble2 = CDouble2 CDouble CDouble
data CDouble3 = CDouble3 CDouble CDouble CDouble
data CDouble4 = CDouble4 CDouble CDouble CDouble CDouble
-- in the future, vector types for CHalf
 -}

