{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Type
-- Copyright   : [2008..2018] Manuel M T Chakravarty, Gabriele Keller
--               [2009..2018] Trevor L. McDonell
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
import Language.Haskell.TH
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

-- | Integral types
--
class (IsSingle a, IsNum a, IsBounded a) => IsIntegral a where
  integralType :: IntegralType a

-- | Floating types
--
class (Floating a, IsSingle a, IsNum a) => IsFloating a where
  floatingType :: FloatingType a

-- | Non-numeric types
--
class IsNonNum a where
  nonNumType :: NonNumType a

-- | Numeric types
--
class (Num a, IsSingle a) => IsNum a where
  numType :: NumType a

-- | Bounded types
--
class IsBounded a where
  boundedType :: BoundedType a

-- | All single value types
--
class IsScalar a => IsSingle a where
  singleType :: SingleType a

-- | All scalar types
--
class Typeable a => IsScalar a where
  scalarType :: ScalarType a


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


-- Instances
-- ---------
--
-- Generate instances for the IsX classes. It would be preferable to do this
-- automatically based on the members of the IntegralType (etc.) representations
-- (see for example FromIntegral.hs) but TH phase restrictions would require us
-- to split this into a separate module.
--

$( runQ $ do
  let
      bits :: FiniteBits b => b -> Integer
      bits = toInteger . finiteBitSize

      integralTypes :: [(Name, Integer)]
      integralTypes =
        [ (''Int,     bits (undefined::Int))
        , (''Int8,    8)
        , (''Int16,   16)
        , (''Int32,   32)
        , (''Int64,   64)
        , (''Word,    bits (undefined::Word))
        , (''Word8,   8)
        , (''Word16,  16)
        , (''Word32,  32)
        , (''Word64,  64)
        , (''CShort,  16)
        , (''CUShort, 16)
        , (''CInt,    32)
        , (''CUInt,   32)
        , (''CLong,   bits (undefined::CLong))
        , (''CULong,  bits (undefined::CULong))
        , (''CLLong,  64)
        , (''CULLong, 64)
        ]

      floatingTypes :: [(Name, Integer)]
      floatingTypes =
        [ (''Half,    16)
        , (''Float,   32)
        , (''Double,  64)
        , (''CFloat,  32)
        , (''CDouble, 64)
        ]

      nonNumTypes :: [(Name, Integer)]
      nonNumTypes =
        [ (''Bool,   8)    -- stored as Word8
        , (''Char,   32)
        , (''CChar,  8)
        , (''CSChar, 8)
        , (''CUChar, 8)
        ]

      mkIntegral :: Name -> Integer -> Q [Dec]
      mkIntegral t n =
        [d| instance IsIntegral $(conT t) where
              integralType = $(conE (mkName ("Type" ++ nameBase t))) IntegralDict

            instance IsNum $(conT t) where
              numType = IntegralNumType integralType

            instance IsBounded $(conT t) where
              boundedType = IntegralBoundedType integralType

            instance IsSingle $(conT t) where
              singleType = NumSingleType numType

            instance IsScalar $(conT t) where
              scalarType = SingleScalarType singleType

            type instance BitSize $(conT t) = $(litT (numTyLit n))
          |]

      mkFloating :: Name -> Integer -> Q [Dec]
      mkFloating t n =
        [d| instance IsFloating $(conT t) where
              floatingType = $(conE (mkName ("Type" ++ nameBase t))) FloatingDict

            instance IsNum $(conT t) where
              numType = FloatingNumType floatingType

            instance IsSingle $(conT t) where
              singleType = NumSingleType numType

            instance IsScalar $(conT t) where
              scalarType = SingleScalarType singleType

            type instance BitSize $(conT t) = $(litT (numTyLit n))
          |]

      mkNonNum :: Name -> Integer -> Q [Dec]
      mkNonNum t n =
        [d| instance IsNonNum $(conT t) where
              nonNumType = $(conE (mkName ("Type" ++ nameBase t))) NonNumDict

            instance IsBounded $(conT t) where
              boundedType = NonNumBoundedType nonNumType

            instance IsSingle $(conT t) where
              singleType = NonNumSingleType nonNumType

            instance IsScalar $(conT t) where
              scalarType = SingleScalarType singleType

            type instance BitSize $(conT t) = $(litT (numTyLit n))
          |]

      mkVector :: Name -> Integer -> Q [Dec]
      mkVector t n =
        [d| instance IsScalar (V2 $(conT t)) where
              scalarType = VectorScalarType (Vector2Type singleType)

            instance IsScalar (V3 $(conT t)) where
              scalarType = VectorScalarType (Vector3Type singleType)

            instance IsScalar (V4 $(conT t)) where
              scalarType = VectorScalarType (Vector4Type singleType)

            instance IsScalar (V8 $(conT t)) where
              scalarType = VectorScalarType (Vector8Type singleType)

            instance IsScalar (V16 $(conT t)) where
              scalarType = VectorScalarType (Vector16Type singleType)

            type instance BitSize (V2 $(conT t))  = $(litT (numTyLit (2*n)))
            type instance BitSize (V3 $(conT t))  = $(litT (numTyLit (3*n)))
            type instance BitSize (V4 $(conT t))  = $(litT (numTyLit (4*n)))
            type instance BitSize (V8 $(conT t))  = $(litT (numTyLit (8*n)))
            type instance BitSize (V16 $(conT t)) = $(litT (numTyLit (16*n)))
          |]
      --
  is <- mapM (uncurry mkIntegral) integralTypes
  fs <- mapM (uncurry mkFloating) floatingTypes
  ns <- mapM (uncurry mkNonNum)   nonNumTypes
  vs <- mapM (uncurry mkVector)  (integralTypes ++ floatingTypes ++ nonNumTypes)
  --
  return (concat is ++ concat fs ++ concat ns ++ concat vs)
 )

