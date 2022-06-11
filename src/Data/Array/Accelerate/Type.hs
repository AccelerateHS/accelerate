{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RoleAnnotations     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
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
--    * Int8
--    * Int16
--    * Int32
--    * Int64
--    * Int128
--    * Word8
--    * Word16
--    * Word32
--    * Word64
--    * Word128
--
--  Floating types (IEEE):
--    * Half
--    * Float
--    * Double
--    * Float128
--
--  A single bit
--
--  and SIMD vector types of all of the above
--

module Data.Array.Accelerate.Type (

  Bit(..), Half(..), Float, Double, Float128(..),
  module Data.Int, Int128(..),
  module Data.Word, Word128(..),
  module Data.Array.Accelerate.Type,

) where

import Data.Array.Accelerate.Orphans () -- Prim Half

import Data.Primitive.Bit
import Data.Primitive.Vec
import Data.Numeric.Float128

import Data.Bits
import Data.Int
import Data.Type.Equality
import Data.WideWord.Int128
import Data.WideWord.Word128
import Data.Word
import Formatting
import Language.Haskell.TH.Extra
import Numeric.Half
import Text.Printf

import GHC.Prim
import GHC.TypeLits

type Float16 = Half
type Float32 = Float
type Float64 = Double

-- Scalar types
-- ------------

-- | Scalar element types are values that can be stored in machine
-- registers: ground types (int32, float64, etc.) and SIMD vectors of these
--
data ScalarType a where
  NumScalarType :: NumType a -> ScalarType a
  BitScalarType :: BitType a -> ScalarType a
  -- Void?

data BitType a where
  TypeBit  ::                           BitType Bit
  TypeMask :: KnownNat n => Proxy# n -> BitType (Vec n Bit)

data NumType a where
  IntegralNumType :: IntegralType a -> NumType a
  FloatingNumType :: FloatingType a -> NumType a

data IntegralType a where
  SingleIntegralType ::                           SingleIntegralType a -> IntegralType a
  VectorIntegralType :: KnownNat n => Proxy# n -> SingleIntegralType a -> IntegralType (Vec n a)

data SingleIntegralType a where
  TypeInt8    :: SingleIntegralType Int8
  TypeInt16   :: SingleIntegralType Int16
  TypeInt32   :: SingleIntegralType Int32
  TypeInt64   :: SingleIntegralType Int64
  TypeInt128  :: SingleIntegralType Int128
  TypeWord8   :: SingleIntegralType Word8
  TypeWord16  :: SingleIntegralType Word16
  TypeWord32  :: SingleIntegralType Word32
  TypeWord64  :: SingleIntegralType Word64
  TypeWord128 :: SingleIntegralType Word128

data FloatingType a where
  SingleFloatingType ::                           SingleFloatingType a -> FloatingType a
  VectorFloatingType :: KnownNat n => Proxy# n -> SingleFloatingType a -> FloatingType (Vec n a)

data SingleFloatingType a where
  -- TypeFloat8   :: SingleFloatingType Float8
  -- TypeBFloat16 :: SingleFloatingType BFloat16
  TypeFloat16  :: SingleFloatingType Float16
  TypeFloat32  :: SingleFloatingType Float32
  TypeFloat64  :: SingleFloatingType Float64
  TypeFloat128 :: SingleFloatingType Float128

instance Show (IntegralType a) where
  show (SingleIntegralType t)   = show t
  show (VectorIntegralType n t) = printf "<%d x %s>" (natVal' n) (show t)

instance Show (FloatingType a) where
  show (SingleFloatingType t)   = show t
  show (VectorFloatingType n t) = printf "<%d x %s>" (natVal' n) (show t)

instance Show (SingleIntegralType a) where
  show TypeInt8    = "Int8"
  show TypeInt16   = "Int16"
  show TypeInt32   = "Int32"
  show TypeInt64   = "Int64"
  show TypeInt128  = "Int128"
  show TypeWord8   = "Word8"
  show TypeWord16  = "Word16"
  show TypeWord32  = "Word32"
  show TypeWord64  = "Word64"
  show TypeWord128 = "Word128"

instance Show (SingleFloatingType a) where
  show TypeFloat16  = "Float16"
  show TypeFloat32  = "Float32"
  show TypeFloat64  = "Float64"
  show TypeFloat128 = "Float128"

instance Show (NumType a) where
  show (IntegralNumType t) = show t
  show (FloatingNumType t) = show t

instance Show (BitType t) where
  show (TypeBit)    = "Bit"
  show (TypeMask n) = printf "<%d x Bit>" (natVal' n)

instance Show (ScalarType a) where
  show (NumScalarType t) = show t
  show (BitScalarType t) = show t

formatIntegralType :: Format r (IntegralType a -> r)
formatIntegralType = later $ \case
  SingleIntegralType t   -> bformat formatSingleIntegralType t
  VectorIntegralType n t -> bformat (angled (int % " x " % formatSingleIntegralType)) (natVal' n) t

formatSingleIntegralType :: Format r (SingleIntegralType a -> r)
formatSingleIntegralType = later $ \case
  TypeInt8    -> "Int8"
  TypeInt16   -> "Int16"
  TypeInt32   -> "Int32"
  TypeInt64   -> "Int64"
  TypeInt128  -> "Int128"
  TypeWord8   -> "Word8"
  TypeWord16  -> "Word16"
  TypeWord32  -> "Word32"
  TypeWord64  -> "Word64"
  TypeWord128 -> "Word128"

formatFloatingType :: Format r (FloatingType a -> r)
formatFloatingType = later $ \case
  SingleFloatingType t   -> bformat formatSingleFloatingType t
  VectorFloatingType n t -> bformat (angled (int % " x " % formatSingleFloatingType)) (natVal' n) t

formatSingleFloatingType :: Format r (SingleFloatingType a -> r)
formatSingleFloatingType = later $ \case
  TypeFloat16  -> "Float16"
  TypeFloat32  -> "Float32"
  TypeFloat64  -> "Float64"
  TypeFloat128 -> "Float128"

formatNumType :: Format r (NumType a -> r)
formatNumType = later $ \case
  IntegralNumType t -> bformat formatIntegralType t
  FloatingNumType t -> bformat formatFloatingType t

formatBitType :: Format r (BitType t -> r)
formatBitType = later $ \case
  TypeBit    -> "Bit"
  TypeMask n -> bformat (angled (int % " x Bit")) (natVal' n)

formatScalarType :: Format r (ScalarType a -> r)
formatScalarType = later $ \case
  NumScalarType t -> bformat formatNumType t
  BitScalarType t -> bformat formatBitType t


rnfScalarType :: ScalarType t -> ()
rnfScalarType (NumScalarType t) = rnfNumType t
rnfScalarType (BitScalarType t) = rnfBitType t

rnfBitType :: BitType t -> ()
rnfBitType TypeBit       = ()
rnfBitType (TypeMask !_) = ()

rnfNumType :: NumType t -> ()
rnfNumType (IntegralNumType t) = rnfIntegralType t
rnfNumType (FloatingNumType t) = rnfFloatingType t

rnfIntegralType :: IntegralType t -> ()
rnfIntegralType (SingleIntegralType t)    = rnfSingleIntegralType t
rnfIntegralType (VectorIntegralType !_ t) = rnfSingleIntegralType t

rnfSingleIntegralType :: SingleIntegralType t -> ()
rnfSingleIntegralType TypeInt8    = ()
rnfSingleIntegralType TypeInt16   = ()
rnfSingleIntegralType TypeInt32   = ()
rnfSingleIntegralType TypeInt64   = ()
rnfSingleIntegralType TypeInt128  = ()
rnfSingleIntegralType TypeWord8   = ()
rnfSingleIntegralType TypeWord16  = ()
rnfSingleIntegralType TypeWord32  = ()
rnfSingleIntegralType TypeWord64  = ()
rnfSingleIntegralType TypeWord128 = ()

rnfFloatingType :: FloatingType t -> ()
rnfFloatingType (SingleFloatingType t)    = rnfSingleFloatingType t
rnfFloatingType (VectorFloatingType !_ t) = rnfSingleFloatingType t

rnfSingleFloatingType :: SingleFloatingType t -> ()
rnfSingleFloatingType TypeFloat16  = ()
rnfSingleFloatingType TypeFloat32  = ()
rnfSingleFloatingType TypeFloat64  = ()
rnfSingleFloatingType TypeFloat128 = ()


liftScalar :: ScalarType t -> t -> CodeQ t
liftScalar (NumScalarType t) = liftNum t
liftScalar (BitScalarType t) = liftBit t

liftBit :: BitType t -> t -> CodeQ t
liftBit TypeBit    (Bit x) = [|| Bit x ||]
liftBit TypeMask{} x       = liftVec x

liftNum :: NumType t -> t -> CodeQ t
liftNum (IntegralNumType t) = liftIntegral t
liftNum (FloatingNumType t) = liftFloating t

liftIntegral :: IntegralType t -> t -> CodeQ t
liftIntegral (SingleIntegralType t)   = liftSingleIntegral t
liftIntegral (VectorIntegralType _ _) = liftVec

liftSingleIntegral :: SingleIntegralType t -> t -> CodeQ t
liftSingleIntegral TypeInt8    x = [|| x ||]
liftSingleIntegral TypeInt16   x = [|| x ||]
liftSingleIntegral TypeInt32   x = [|| x ||]
liftSingleIntegral TypeInt64   x = [|| x ||]
liftSingleIntegral TypeWord8   x = [|| x ||]
liftSingleIntegral TypeWord16  x = [|| x ||]
liftSingleIntegral TypeWord32  x = [|| x ||]
liftSingleIntegral TypeWord64  x = [|| x ||]
liftSingleIntegral TypeInt128  (Int128 x y)  = [|| Int128 x y ||]
liftSingleIntegral TypeWord128 (Word128 x y) = [|| Word128 x y ||]

liftFloating :: FloatingType t -> t -> CodeQ t
liftFloating (SingleFloatingType t)   = liftSingleFloating t
liftFloating (VectorFloatingType _ _) = liftVec

liftSingleFloating :: SingleFloatingType t -> t -> CodeQ t
liftSingleFloating TypeFloat16  x = [|| x ||]
liftSingleFloating TypeFloat32  x = [|| x ||]
liftSingleFloating TypeFloat64  x = [|| x ||]
liftSingleFloating TypeFloat128 (Float128 x y) = [|| Float128 x y ||]


liftScalarType :: ScalarType t -> CodeQ (ScalarType t)
liftScalarType (NumScalarType t) = [|| NumScalarType $$(liftNumType t) ||]
liftScalarType (BitScalarType t) = [|| BitScalarType $$(liftBitType t) ||]

liftBitType :: BitType t -> CodeQ (BitType t)
liftBitType TypeBit    = [|| TypeBit ||]
liftBitType TypeMask{} = [|| TypeMask proxy# ||]

liftNumType :: NumType t -> CodeQ (NumType t)
liftNumType (IntegralNumType t) = [|| IntegralNumType $$(liftIntegralType t) ||]
liftNumType (FloatingNumType t) = [|| FloatingNumType $$(liftFloatingType t) ||]

liftIntegralType :: IntegralType t -> CodeQ (IntegralType t)
liftIntegralType (SingleIntegralType t)   = [|| SingleIntegralType $$(liftSingleIntegralType t) ||]
liftIntegralType (VectorIntegralType _ t) = [|| VectorIntegralType proxy# $$(liftSingleIntegralType t) ||]

liftSingleIntegralType :: SingleIntegralType t -> CodeQ (SingleIntegralType t)
liftSingleIntegralType TypeInt8    = [|| TypeInt8 ||]
liftSingleIntegralType TypeInt16   = [|| TypeInt16 ||]
liftSingleIntegralType TypeInt32   = [|| TypeInt32 ||]
liftSingleIntegralType TypeInt64   = [|| TypeInt64 ||]
liftSingleIntegralType TypeInt128  = [|| TypeInt128 ||]
liftSingleIntegralType TypeWord8   = [|| TypeWord8 ||]
liftSingleIntegralType TypeWord16  = [|| TypeWord16 ||]
liftSingleIntegralType TypeWord32  = [|| TypeWord32 ||]
liftSingleIntegralType TypeWord64  = [|| TypeWord64 ||]
liftSingleIntegralType TypeWord128 = [|| TypeWord128 ||]

liftFloatingType :: FloatingType t -> CodeQ (FloatingType t)
liftFloatingType (SingleFloatingType t)   = [|| SingleFloatingType $$(liftSingleFloatingType t) ||]
liftFloatingType (VectorFloatingType _ t) = [|| VectorFloatingType proxy# $$(liftSingleFloatingType t) ||]

liftSingleFloatingType :: SingleFloatingType t -> CodeQ (SingleFloatingType t)
liftSingleFloatingType TypeFloat16  = [|| TypeFloat16 ||]
liftSingleFloatingType TypeFloat32  = [|| TypeFloat32 ||]
liftSingleFloatingType TypeFloat64  = [|| TypeFloat64 ||]
liftSingleFloatingType TypeFloat128 = [|| TypeFloat128 ||]


-- Querying types
-- --------------

class IsScalar a where
  scalarType :: ScalarType a

class IsBit a where
  bitType :: BitType a

class IsNum a where
  numType :: NumType a

class IsIntegral a where
  integralType :: IntegralType a

class IsFloating a where
  floatingType :: FloatingType a

class IsSingleIntegral a where
  singleIntegralType :: SingleIntegralType a

class IsSingleFloating a where
  singleFloatingType :: SingleFloatingType a

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
      integralTypes :: [Integer]
      integralTypes = [8,16,32,64,128]

      floatingTypes :: [(Name, Integer)]
      floatingTypes =
        [ (''Half,     16)
        , (''Float,    32)
        , (''Double,   64)
        , (''Float128, 128)
        ]

      mkIntegral :: String -> Integer -> Q [Dec]
      mkIntegral name bits =
        let t = conT $ mkName $ printf "%s%d" name bits
            c = conE $ mkName $ printf "Type%s%d" name bits
        in
        [d| instance IsScalar $t where
              scalarType = NumScalarType numType

            instance IsNum $t where
              numType = IntegralNumType integralType

            instance IsIntegral $t where
              integralType = SingleIntegralType singleIntegralType

            instance IsSingleIntegral $t where
              singleIntegralType = $c

            instance KnownNat n => IsIntegral (Vec n $t) where
              integralType = VectorIntegralType proxy# $c

            instance KnownNat n => IsNum (Vec n $t) where
              numType = IntegralNumType integralType

            instance KnownNat n => IsScalar (Vec n $t) where
              scalarType = NumScalarType numType

            type instance BitSize $t = $(litT (numTyLit bits))
            type instance BitSize (Vec n $t) = n GHC.TypeLits.* $(litT (numTyLit bits))
          |]

      mkFloating :: Name -> Integer -> Q [Dec]
      mkFloating name bits =
        let t = conT name
            c = conE $ mkName $ printf "TypeFloat%d" bits
        in
        [d| instance IsScalar $t where
              scalarType = NumScalarType numType

            instance IsNum $t where
              numType = FloatingNumType floatingType

            instance IsFloating $t where
              floatingType = SingleFloatingType singleFloatingType

            instance IsSingleFloating $t where
              singleFloatingType = $c

            instance KnownNat n => IsFloating (Vec n $t) where
              floatingType = VectorFloatingType proxy# $c

            instance KnownNat n => IsNum (Vec n $t) where
              numType = FloatingNumType floatingType

            instance KnownNat n => IsScalar (Vec n $t) where
              scalarType = NumScalarType numType

            type instance BitSize $t = $(litT (numTyLit bits))
            type instance BitSize (Vec n $t) = n GHC.TypeLits.* $(litT (numTyLit bits))
          |]

  ss <- mapM (mkIntegral "Int") integralTypes
  us <- mapM (mkIntegral "Word") integralTypes
  fs <- mapM (uncurry mkFloating) floatingTypes
  --
  return (concat ss ++ concat us ++ concat fs)

type instance BitSize Bit = 1
type instance BitSize (Vec n Bit) = n

instance IsScalar Bit where
  scalarType = BitScalarType bitType

instance KnownNat n => IsScalar (Vec n Bit) where
  scalarType = BitScalarType bitType

instance IsBit Bit where
  bitType = TypeBit

instance KnownNat n => IsBit (Vec n Bit) where
  bitType = TypeMask proxy#


-- Determine the underlying type of a Haskell Int and Word
--
runQ [d| type INT = $(
              case finiteBitSize (undefined::Int) of
                8  -> [t| Int8  |]
                16 -> [t| Int16 |]
                32 -> [t| Int32 |]
                64 -> [t| Int64 |]
                _  -> error "I don't know what architecture I am" ) |]

runQ [d| type WORD = $(
              case finiteBitSize (undefined::Word) of
                8  -> [t| Word8  |]
                16 -> [t| Word16 |]
                32 -> [t| Word32 |]
                64 -> [t| Word64 |]
                _  -> error "I don't know what architecture I am" ) |]

