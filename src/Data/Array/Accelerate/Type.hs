{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
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
import Data.Kind
import Data.Type.Equality
import Data.WideWord.Int128
import Data.WideWord.Word128
import Data.Word
import Formatting
import Language.Haskell.TH.Extra                                    hiding ( Type )
import Numeric.Half
import Text.Printf
import Unsafe.Coerce

import GHC.Prim
import GHC.TypeNats

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

-- Note: [Arbitrary width integers]
--
-- We support arbitrary width signed and unsigned integers, but for almost all
-- cases you should use the type synonyms generated below.
--
data SingleIntegralType :: Type -> Type where
  TypeInt  :: {-# UNPACK #-} !Int -> SingleIntegralType a
  TypeWord :: {-# UNPACK #-} !Int -> SingleIntegralType a

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


typeIntBits :: SingleIntegralType a -> Maybe Int
typeIntBits (TypeInt x) = Just x
typeIntBits _           = Nothing

typeWordBits :: SingleIntegralType a -> Maybe Int
typeWordBits (TypeWord x) = Just x
typeWordBits _            = Nothing

-- Generate pattern synonyms for fixed sized signed and unsigned integers. In
-- practice this is what we'll use most of the time, but occasionally we need to
-- convert via an arbitrary width integer (e.g. coercing between a BitMask and
-- an integral value).
--
-- SEE: [Arbitrary width integers]
--
runQ $ do
  let
      integralTypes :: [Integer]
      integralTypes = [8,16,32,64,128]

      mkIntegral :: String -> Integer -> Q [Dec]
      mkIntegral name bits = do
        let t = conT $ mkName $ printf "%s%d" name bits
            e = varE $ mkName $ printf "type%sBits" name
            c = mkName $ printf "Type%s%d" name bits
         --
        a <- newName "a"
        sequence [ patSynSigD c (forallT [plainInvisTV a specifiedSpec] (return [TupleT 0]) [t| () => ($(varT a) ~ $t) => SingleIntegralType $(varT a) |])
                 , patSynD c (prefixPatSyn []) (explBidir [clause [] (normalB [| $(conE (mkName (printf "Type%s" name))) $(litE (integerL bits)) |]) [] ])
                     [p| (\x -> ($e x, unsafeCoerce Refl) -> (Just $(litP (integerL bits)), Refl :: $(varT a) :~: $t)) |]
                 ]
  --
  cs <- pragCompleteD [ mkName (printf "Type%s%d" name bits) | name <- ["Int", "Word" :: String], bits <- integralTypes ] Nothing
  ss <- mapM (mkIntegral "Int") integralTypes
  us <- mapM (mkIntegral "Word") integralTypes
  return (cs : concat ss ++ concat us)

instance Show (IntegralType a) where
  show (SingleIntegralType t)   = show t
  show (VectorIntegralType n t) = printf "<%d x %s>" (natVal' n) (show t)

instance Show (FloatingType a) where
  show (SingleFloatingType t)   = show t
  show (VectorFloatingType n t) = printf "<%d x %s>" (natVal' n) (show t)

instance Show (SingleIntegralType a) where
  show (TypeInt  n) = printf "Int%d" n
  show (TypeWord n) = printf "Word%d" n

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
  TypeInt  n -> bformat ("Int" % int) n
  TypeWord n -> bformat ("Word" % int) n

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
rnfSingleIntegralType (TypeInt  !_) = ()
rnfSingleIntegralType (TypeWord !_) = ()

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
liftSingleIntegralType (TypeInt  n) = [|| TypeInt n ||]
liftSingleIntegralType (TypeWord n) = [|| TypeWord n ||]

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

-- | Constraint that values of these two types have the same bit width
--
type BitSizeEq a b = (BitSize a == BitSize b) ~ 'True
type family BitSize a :: Nat

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
            type instance BitSize (Vec n $t) = n GHC.TypeNats.* $(litT (numTyLit bits))
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
            type instance BitSize (Vec n $t) = n GHC.TypeNats.* $(litT (numTyLit bits))
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

