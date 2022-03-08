{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RoleAnnotations     #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Type
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
--  Primitive scalar types supported by Accelerate
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
--
--  Floating types:
--    * Half
--    * Float
--    * Double
--
--  SIMD vector types of the above:
--    * Vec2
--    * Vec3
--    * Vec4
--    * Vec8
--    * Vec16
--
-- Note that 'Int' has the same bit width as in plain Haskell computations.
-- 'Float' and 'Double' represent IEEE single and double precision floating
-- point numbers, respectively.
--

module Data.Array.Accelerate.Type (

  Half(..), Float, Double,
  module Data.Int,
  module Data.Word,
  module Foreign.C.Types,
  module Data.Array.Accelerate.Type,

) where

import Data.Array.Accelerate.Orphans () -- Prim Half
import Data.Array.Accelerate.Representation.POS
import Data.Primitive.Vec

import Data.Bits
import Data.Int
import Data.Primitive.Types
import Data.Type.Equality
import Data.Word
import Data.Kind
import Foreign.C.Types
import Foreign.Storable                                             ( Storable )
import Formatting
import Language.Haskell.TH.Extra hiding (Type)
import Numeric.Half
import Text.Printf

import GHC.Prim
import GHC.TypeLits

import Unsafe.Coerce


type family POStoEltR (cs :: Nat) fs :: Type where
  POStoEltR 1 '[ '[x]] = x -- singletontypes
  POStoEltR 1 x = FlattenProduct x -- tagless types (could / should be represented without Sums in the Product)
  POStoEltR n x = (Finite n, FlattenProduct x) -- all other types

type family FlattenProduct (xss :: f (g a)) = (r :: Type) where
  FlattenProduct '[] = ()
  FlattenProduct (x ': xs) = (ScalarType (FlattenSum x), FlattenProduct xs)

type family FlattenSum (xss :: f a) = (r :: Type) | r -> f where
  FlattenSum '[] = ()
  FlattenSum (x ': xs) = (x, FlattenSum xs)

flattenProduct :: Product a -> FlattenProduct a
flattenProduct Nil = ()
flattenProduct (Cons x xs) = (SumScalarType x, flattenProduct xs)

-- unFlattenProduct :: FlattenProduct a -> Product a
-- unFlattenProduct () = Nil
-- unFlattenProduct (SumScalarType x, xs) = Cons x (unFlattenProduct xs)

mkEltR :: forall a . (POSable a) => a -> POStoEltR (Choices a) (Fields a)
mkEltR x = case natVal cs of
             -- This distinction is hard to express in a type-correct way,
             -- hence the unsafeCoerce's
             1 -> case emptyFields @a of
                    PTCons (STSucc _ STZero) PTNil | Cons (Pick f) Nil <- fields x -> unsafeCoerce f
                    _ -> unsafeCoerce fs
             _ -> unsafeCoerce (cs, fs)
  where
    cs = choices x
    fs = flattenProduct (fields x)


fromEltR :: forall a . (POSable a) => POStoEltR (Choices a) (Fields a) -> a
fromEltR x = case natVal (emptyChoices @a) of
              1 -> case emptyFields @a of
                PTCons (STSucc _ STZero) PTNil -> unsafeCoerce x
                _ -> fromPOSable 0 (unsafeCoerce x)
              _ -> uncurry fromPOSable (unsafeCoerce x)

-- Scalar types
-- ------------

-- Reified dictionaries
--
data SingleDict a where
  SingleDict :: ( Eq a, Ord a, Show a, Storable a, Prim a )
             => SingleDict a

data IntegralDict a where
  IntegralDict :: ( Eq a, Ord a, Show a
                  , Bounded a, Bits a, FiniteBits a, Integral a, Num a, Real a, Storable a )
               => IntegralDict a

data FloatingDict a where
  FloatingDict :: ( Eq a, Ord a, Show a
                  , Floating a, Fractional a, Num a, Real a, RealFrac a, RealFloat a, Storable a )
               => FloatingDict a


-- Scalar type representation
--

-- | Integral types supported in array computations.
--
data IntegralType a where
  TypeInt     :: IntegralType Int
  TypeInt8    :: IntegralType Int8
  TypeInt16   :: IntegralType Int16
  TypeInt32   :: IntegralType Int32
  TypeInt64   :: IntegralType Int64
  TypeWord    :: IntegralType Word
  TypeWord8   :: IntegralType Word8
  TypeWord16  :: IntegralType Word16
  TypeWord32  :: IntegralType Word32
  TypeWord64  :: IntegralType Word64
  TypeSingletonType :: IntegralType (SingletonType a)
  TypeTAG     :: IntegralType (Finite n)


type SingletonType x = (ScalarType (Int, ()), ())

-- | Floating-point types supported in array computations.
--
data FloatingType a where
  TypeHalf    :: FloatingType Half
  TypeFloat   :: FloatingType Float
  TypeDouble  :: FloatingType Double
  TypeFloatingSingletonType :: FloatingType (SingletonType a)

-- | Numeric element types implement Num & Real
--
data NumType a where
  IntegralNumType :: IntegralType a -> NumType a
  FloatingNumType :: FloatingType a -> NumType a

-- | Bounded element types implement Bounded
--
data BoundedType a where
  IntegralBoundedType :: IntegralType a -> BoundedType a

-- | All scalar element types implement Eq & Ord
--
data ScalarType a where
  SumScalarType    :: Sum a                -> ScalarType (FlattenSum a)
  TagScalarType    :: Finite n             -> ScalarType (Finite n)
  SingleScalarType :: SingleType a         -> ScalarType a
  VectorScalarType :: VectorType (Vec n a) -> ScalarType (Vec n a)

data SingleType a where
  NumSingleType :: NumType a -> SingleType a

data VectorType a where
  VectorType :: KnownNat n => {-# UNPACK #-} !Int -> SingleType a -> VectorType (Vec n a)

instance Show (IntegralType a) where
  show TypeInt    = "Int"
  show TypeInt8   = "Int8"
  show TypeInt16  = "Int16"
  show TypeInt32  = "Int32"
  show TypeInt64  = "Int64"
  show TypeWord   = "Word"
  show TypeWord8  = "Word8"
  show TypeWord16 = "Word16"
  show TypeWord32 = "Word32"
  show TypeWord64 = "Word64"

instance Show (FloatingType a) where
  show TypeHalf   = "Half"
  show TypeFloat  = "Float"
  show TypeDouble = "Double"

instance Show (NumType a) where
  show (IntegralNumType ty) = show ty
  show (FloatingNumType ty) = show ty

instance Show (BoundedType a) where
  show (IntegralBoundedType ty) = show ty

instance Show (SingleType a) where
  show (NumSingleType ty) = show ty

instance Show (VectorType a) where
  show (VectorType n ty) = printf "<%d x %s>" n (show ty)

instance Show (ScalarType a) where
  show (SingleScalarType ty) = show ty
  show (VectorScalarType ty) = show ty

formatIntegralType :: Format r (IntegralType a -> r)
formatIntegralType = later $ \case
  TypeInt    -> "Int"
  TypeInt8   -> "Int8"
  TypeInt16  -> "Int16"
  TypeInt32  -> "Int32"
  TypeInt64  -> "Int64"
  TypeWord   -> "Word"
  TypeWord8  -> "Word8"
  TypeWord16 -> "Word16"
  TypeWord32 -> "Word32"
  TypeWord64 -> "Word64"

formatFloatingType :: Format r (FloatingType a -> r)
formatFloatingType = later $ \case
  TypeHalf   -> "Half"
  TypeFloat  -> "Float"
  TypeDouble -> "Double"

formatNumType :: Format r (NumType a -> r)
formatNumType = later $ \case
  IntegralNumType ty -> bformat formatIntegralType ty
  FloatingNumType ty -> bformat formatFloatingType ty

formatBoundedType :: Format r (BoundedType a -> r)
formatBoundedType = later $ \case
  IntegralBoundedType ty -> bformat formatIntegralType ty

formatSingleType :: Format r (SingleType a -> r)
formatSingleType = later $ \case
  NumSingleType ty -> bformat formatNumType ty

formatVectorType :: Format r (VectorType a -> r)
formatVectorType = later $ \case
  VectorType n ty -> bformat (angled (int % " x " % formatSingleType)) n ty

formatScalarType :: Format r (ScalarType a -> r)
formatScalarType = later $ \case
  SingleScalarType ty -> bformat formatSingleType ty
  VectorScalarType ty -> bformat formatVectorType ty


-- | Querying Integral types
--
class (IsSingle a, IsNum a, IsBounded a) => IsIntegral a where
  integralType :: IntegralType a

-- | Querying Floating types
--
class (Floating a, IsSingle a, IsNum a) => IsFloating a where
  floatingType :: FloatingType a

-- | Querying Numeric types
--
class (Num a, IsSingle a) => IsNum a where
  numType :: NumType a

-- | Querying Bounded types
--
class IsBounded a where
  boundedType :: BoundedType a

-- | Querying single value types
--
class IsScalar a => IsSingle a where
  singleType :: SingleType a

-- | Querying all scalar types
--
class IsScalar a where
  scalarType :: ScalarType a


integralDict :: IntegralType a -> IntegralDict a
integralDict TypeInt    = IntegralDict
integralDict TypeInt8   = IntegralDict
integralDict TypeInt16  = IntegralDict
integralDict TypeInt32  = IntegralDict
integralDict TypeInt64  = IntegralDict
integralDict TypeWord   = IntegralDict
integralDict TypeWord8  = IntegralDict
integralDict TypeWord16 = IntegralDict
integralDict TypeWord32 = IntegralDict
integralDict TypeWord64 = IntegralDict

floatingDict :: FloatingType a -> FloatingDict a
floatingDict TypeHalf   = FloatingDict
floatingDict TypeFloat  = FloatingDict
floatingDict TypeDouble = FloatingDict

singleDict :: SingleType a -> SingleDict a
singleDict = single
  where
    single :: SingleType a -> SingleDict a
    single (NumSingleType t) = num t

    num :: NumType a -> SingleDict a
    num (IntegralNumType t) = integral t
    num (FloatingNumType t) = floating t

    integral :: IntegralType a -> SingleDict a
    integral TypeInt    = SingleDict
    integral TypeInt8   = SingleDict
    integral TypeInt16  = SingleDict
    integral TypeInt32  = SingleDict
    integral TypeInt64  = SingleDict
    integral TypeWord   = SingleDict
    integral TypeWord8  = SingleDict
    integral TypeWord16 = SingleDict
    integral TypeWord32 = SingleDict
    integral TypeWord64 = SingleDict

    floating :: FloatingType a -> SingleDict a
    floating TypeHalf   = SingleDict
    floating TypeFloat  = SingleDict
    floating TypeDouble = SingleDict


scalarTypeInt :: ScalarType Int
scalarTypeInt = SingleScalarType $ NumSingleType $ IntegralNumType TypeInt

scalarTypeWord :: ScalarType Word
scalarTypeWord = SingleScalarType $ NumSingleType $ IntegralNumType TypeWord

scalarTypeInt32 :: ScalarType Int32
scalarTypeInt32 = SingleScalarType $ NumSingleType $ IntegralNumType TypeInt32

scalarTypeWord8 :: ScalarType Word8
scalarTypeWord8 = SingleScalarType $ NumSingleType $ IntegralNumType TypeWord8

scalarTypeWord32 :: ScalarType Word32
scalarTypeWord32 = SingleScalarType $ NumSingleType $ IntegralNumType TypeWord32

rnfScalarType :: ScalarType t -> ()
rnfScalarType (SingleScalarType t) = rnfSingleType t
rnfScalarType (VectorScalarType t) = rnfVectorType t

rnfSingleType :: SingleType t -> ()
rnfSingleType (NumSingleType t) = rnfNumType t

rnfVectorType :: VectorType t -> ()
rnfVectorType (VectorType !_ t) = rnfSingleType t

rnfBoundedType :: BoundedType t -> ()
rnfBoundedType (IntegralBoundedType t) = rnfIntegralType t

rnfNumType :: NumType t -> ()
rnfNumType (IntegralNumType t) = rnfIntegralType t
rnfNumType (FloatingNumType t) = rnfFloatingType t

rnfIntegralType :: IntegralType t -> ()
rnfIntegralType TypeInt    = ()
rnfIntegralType TypeInt8   = ()
rnfIntegralType TypeInt16  = ()
rnfIntegralType TypeInt32  = ()
rnfIntegralType TypeInt64  = ()
rnfIntegralType TypeWord   = ()
rnfIntegralType TypeWord8  = ()
rnfIntegralType TypeWord16 = ()
rnfIntegralType TypeWord32 = ()
rnfIntegralType TypeWord64 = ()

rnfFloatingType :: FloatingType t -> ()
rnfFloatingType TypeHalf   = ()
rnfFloatingType TypeFloat  = ()
rnfFloatingType TypeDouble = ()


liftScalar :: ScalarType t -> t -> CodeQ t
liftScalar (SingleScalarType t) = liftSingle t
liftScalar (VectorScalarType t) = liftVector t

liftSingle :: SingleType t -> t -> CodeQ t
liftSingle (NumSingleType t) = liftNum t

liftVector :: VectorType t -> t -> CodeQ t
liftVector VectorType{} = liftVec

liftNum :: NumType t -> t -> CodeQ t
liftNum (IntegralNumType t) = liftIntegral t
liftNum (FloatingNumType t) = liftFloating t

liftIntegral :: IntegralType t -> t -> CodeQ t
liftIntegral TypeInt    x = [|| x ||]
liftIntegral TypeInt8   x = [|| x ||]
liftIntegral TypeInt16  x = [|| x ||]
liftIntegral TypeInt32  x = [|| x ||]
liftIntegral TypeInt64  x = [|| x ||]
liftIntegral TypeWord   x = [|| x ||]
liftIntegral TypeWord8  x = [|| x ||]
liftIntegral TypeWord16 x = [|| x ||]
liftIntegral TypeWord32 x = [|| x ||]
liftIntegral TypeWord64 x = [|| x ||]

liftFloating :: FloatingType t -> t -> CodeQ t
liftFloating TypeHalf   x = [|| x ||]
liftFloating TypeFloat  x = [|| x ||]
liftFloating TypeDouble x = [|| x ||]


liftScalarType :: ScalarType t -> CodeQ (ScalarType t)
liftScalarType (SingleScalarType t) = [|| SingleScalarType $$(liftSingleType t) ||]
liftScalarType (VectorScalarType t) = [|| VectorScalarType $$(liftVectorType t) ||]

liftSingleType :: SingleType t -> CodeQ (SingleType t)
liftSingleType (NumSingleType t) = [|| NumSingleType $$(liftNumType t) ||]

liftVectorType :: VectorType t -> CodeQ (VectorType t)
liftVectorType (VectorType n t) = [|| VectorType n $$(liftSingleType t) ||]

liftNumType :: NumType t -> CodeQ (NumType t)
liftNumType (IntegralNumType t) = [|| IntegralNumType $$(liftIntegralType t) ||]
liftNumType (FloatingNumType t) = [|| FloatingNumType $$(liftFloatingType t) ||]

liftBoundedType :: BoundedType t -> CodeQ (BoundedType t)
liftBoundedType (IntegralBoundedType t) = [|| IntegralBoundedType $$(liftIntegralType t) ||]

liftIntegralType :: IntegralType t -> CodeQ (IntegralType t)
liftIntegralType TypeInt    = [|| TypeInt ||]
liftIntegralType TypeInt8   = [|| TypeInt8 ||]
liftIntegralType TypeInt16  = [|| TypeInt16 ||]
liftIntegralType TypeInt32  = [|| TypeInt32 ||]
liftIntegralType TypeInt64  = [|| TypeInt64 ||]
liftIntegralType TypeWord   = [|| TypeWord ||]
liftIntegralType TypeWord8  = [|| TypeWord8 ||]
liftIntegralType TypeWord16 = [|| TypeWord16 ||]
liftIntegralType TypeWord32 = [|| TypeWord32 ||]
liftIntegralType TypeWord64 = [|| TypeWord64 ||]

liftFloatingType :: FloatingType t -> CodeQ (FloatingType t)
liftFloatingType TypeHalf   = [|| TypeHalf ||]
liftFloatingType TypeFloat  = [|| TypeFloat ||]
liftFloatingType TypeDouble = [|| TypeDouble ||]


-- Type-level bit sizes
-- --------------------

-- | Constraint that values of these two types have the same bit width
--
type BitSizeEq a b = (BitSize a == BitSize b) ~ 'True
type family BitSize a :: Nat


-- Instances
-- ---------
--
-- Generate instances for the IsX classes. It would be preferable to do this
-- automatically based on the members of the IntegralType (etc.) representations
-- (see for example FromIntegral.hs) but TH phase restrictions would require us
-- to split this into a separate module.
--

runQ $ do
  let
      bits :: FiniteBits b => b -> Integer
      bits = toInteger . finiteBitSize

      integralTypes :: [(Name, Integer)]
      integralTypes =
        [ (''Int,    bits (undefined::Int))
        , (''Int8,   8)
        , (''Int16,  16)
        , (''Int32,  32)
        , (''Int64,  64)
        , (''Word,   bits (undefined::Word))
        , (''Word8,  8)
        , (''Word16, 16)
        , (''Word32, 32)
        , (''Word64, 64)
        ]

      floatingTypes :: [(Name, Integer)]
      floatingTypes =
        [ (''Half,   16)
        , (''Float,  32)
        , (''Double, 64)
        ]

      vectorTypes :: [(Name, Integer)]
      vectorTypes = integralTypes ++ floatingTypes

      mkIntegral :: Name -> Integer -> Q [Dec]
      mkIntegral t n =
        [d| instance IsIntegral $(conT t) where
              integralType = $(conE (mkName ("Type" ++ nameBase t)))

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
              floatingType = $(conE (mkName ("Type" ++ nameBase t)))

            instance IsNum $(conT t) where
              numType = FloatingNumType floatingType

            instance IsSingle $(conT t) where
              singleType = NumSingleType numType

            instance IsScalar $(conT t) where
              scalarType = SingleScalarType singleType

            type instance BitSize $(conT t) = $(litT (numTyLit n))
          |]

      mkVector :: Name -> Integer -> Q [Dec]
      mkVector t n =
        [d| instance KnownNat n => IsScalar (Vec n $(conT t)) where
              scalarType = VectorScalarType (VectorType (fromIntegral (natVal' (proxy# :: Proxy# n))) singleType)

            type instance BitSize (Vec w $(conT t)) = w GHC.TypeLits.* $(litT (numTyLit n))
          |]
      --
  is <- mapM (uncurry mkIntegral) integralTypes
  fs <- mapM (uncurry mkFloating) floatingTypes
  vs <- mapM (uncurry mkVector)   vectorTypes
  --
  return (concat is ++ concat fs ++ concat vs)

