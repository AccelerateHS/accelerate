{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
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
#if __GLASGOW_HASKELL__ <= 800
{-# OPTIONS_GHC -fno-warn-unrecognised-pragmas #-}
#endif
-- |
-- Module      : Data.Array.Accelerate.Type
-- Copyright   : [2008..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
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
--
--  Floating types:
--    * Half
--    * Float
--    * Double
--
--  Non-numeric types:
--    * Bool
--    * Char
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
  module Data.Array.Accelerate.Type,

) where

import Data.Orphans ()    -- orphan instances for 8-tuples and beyond

import Control.Monad.ST
import Data.Bits
import Data.Int
import Data.Primitive.ByteArray
import Data.Primitive.Types
import Data.Text.Prettyprint.Doc
import Data.Type.Equality
import Data.Typeable
import Data.Word
import Foreign.C.Types
import Foreign.Storable                                             ( Storable )
import Language.Haskell.TH
import Numeric.Half
import Text.Printf

import GHC.Base                                                     ( isTrue# )
import GHC.Int
import GHC.Prim
import GHC.TypeLits


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

-- | Floating-point types supported in array computations.
--
data FloatingType a where
  TypeHalf    :: FloatingDict Half    -> FloatingType Half
  TypeFloat   :: FloatingDict Float   -> FloatingType Float
  TypeDouble  :: FloatingDict Double  -> FloatingType Double

-- | Non-numeric types supported in array computations.
--
data NonNumType a where
  TypeBool  :: NonNumDict Bool  -> NonNumType Bool   --  marshalled to Word8
  TypeChar  :: NonNumDict Char  -> NonNumType Char

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
  SingleScalarType :: SingleType a         -> ScalarType a
  VectorScalarType :: VectorType (Vec n a) -> ScalarType (Vec n a)

data SingleType a where
  NumSingleType    :: NumType a    -> SingleType a
  NonNumSingleType :: NonNumType a -> SingleType a

data VectorType a where
  VectorType       :: {-# UNPACK #-} !Int -> SingleType a -> VectorType (Vec n a)

-- Showing type names
--

instance Show (IntegralType a) where
  show TypeInt{}     = "Int"
  show TypeInt8{}    = "Int8"
  show TypeInt16{}   = "Int16"
  show TypeInt32{}   = "Int32"
  show TypeInt64{}   = "Int64"
  show TypeWord{}    = "Word"
  show TypeWord8{}   = "Word8"
  show TypeWord16{}  = "Word16"
  show TypeWord32{}  = "Word32"
  show TypeWord64{}  = "Word64"

instance Show (FloatingType a) where
  show TypeHalf{}    = "Half"
  show TypeFloat{}   = "Float"
  show TypeDouble{}  = "Double"

instance Show (NonNumType a) where
  show TypeBool{}   = "Bool"
  show TypeChar{}   = "Char"

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
  show (VectorType n ty)     = printf "<%d x %s>" n (show ty)

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
integralDict (TypeInt    dict) = dict
integralDict (TypeInt8   dict) = dict
integralDict (TypeInt16  dict) = dict
integralDict (TypeInt32  dict) = dict
integralDict (TypeInt64  dict) = dict
integralDict (TypeWord   dict) = dict
integralDict (TypeWord8  dict) = dict
integralDict (TypeWord16 dict) = dict
integralDict (TypeWord32 dict) = dict
integralDict (TypeWord64 dict) = dict

floatingDict :: FloatingType a -> FloatingDict a
floatingDict (TypeHalf   dict) = dict
floatingDict (TypeFloat  dict) = dict
floatingDict (TypeDouble dict) = dict

nonNumDict :: NonNumType a -> NonNumDict a
nonNumDict (TypeBool dict) = dict
nonNumDict (TypeChar dict) = dict


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

-- | Constraint that values of these two types have the same bit width
--
type BitSizeEq a b = (BitSize a == BitSize b) ~ 'True

type family BitSize a :: Nat


-- SIMD vector types
-- -----------------

-- Note: [Representing SIMD vector types]
--
-- A simple polymorphic representation of SIMD types such as the following:
--
-- > data V2 a = V2 !a !a
--
-- is not able to unpack the values into the constructor, meaning that 'V2' is
-- storing pointers to (strict) values on the heap, which is a very inefficient
-- representation.
--
-- We might try defining a data family instead so that we can get efficient
-- unboxed representations, and even make use of the unlifted SIMD types GHC
-- knows about:
--
-- > data family V2 a :: *
-- > data instance V2 Float    = V2_Float Float# Float#   -- reasonable
-- > data instance V2 Double   = V2_Double DoubleX2#      -- built in!
--
-- However, this runs into the problem that GHC stores all values as word sized
-- entities:
--
-- > data instance V2 Int      = V2_Int Int# Int#
-- > data instance V2 Int8     = V2_Int8 Int8# Int8#      -- Int8# does not exist; requires a full Int#
--
-- which, again, is very memory inefficient.
--
-- So, as a last resort, we'll just use a ByteArray# to ensure an efficient
-- packed representation.
--
-- One inefficiency of this approach is that the byte array does track its size,
-- which redundant for our use case (derivable from type level information).
--
data Vec (n::Nat) a = Vec ByteArray#
  deriving Typeable

type role Vec nominal representational

instance (Show a, Prim a, KnownNat n) => Show (Vec n a) where
  show (Vec ba#) = vec (go 0#)
    where
      vec :: [a] -> String
      vec = show
          . group . encloseSep (flatAlt "< " "<") (flatAlt " >" ">") ", "
          . map viaShow
      --
      go :: Int# -> [a]
      go i# | isTrue# (i# <# n#)  = indexByteArray# ba# i# : go (i# +# 1#)
            | otherwise           = []
      --
      !(I# n#)  = fromIntegral (natVal' (proxy# :: Proxy# n))

instance Eq (Vec n a) where
  Vec ba1# == Vec ba2# = ByteArray ba1# == ByteArray ba2#


-- Type synonyms for common SIMD vector types
--
type V2 a  = Vec 2 a
type V3 a  = Vec 3 a  -- XXX: dubious?
type V4 a  = Vec 4 a
type V8 a  = Vec 8 a
type V16 a = Vec 16 a

pattern V2 :: Prim a => a -> a -> V2 a
pattern V2 a b <- (unpackV2 -> (a,b))
  where V2 = packV2
{-# COMPLETE V2 #-}

pattern V3 :: Prim a => a -> a -> a -> V3 a
pattern V3 a b c <- (unpackV3 -> (a,b,c))
  where V3 = packV3
{-# COMPLETE V3 #-}

pattern V4 :: Prim a => a -> a -> a -> a -> V4 a
pattern V4 a b c d <- (unpackV4 -> (a,b,c,d))
  where V4 = packV4
{-# COMPLETE V4 #-}

pattern V8 :: Prim a => a -> a -> a -> a -> a -> a -> a -> a -> V8 a
pattern V8 a b c d e f g h <- (unpackV8 -> (a,b,c,d,e,f,g,h))
  where V8 = packV8
{-# COMPLETE V8 #-}

pattern V16 :: Prim a => a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> V16 a
pattern V16 a b c d e f g h i j k l m n o p <- (unpackV16 -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p))
  where V16 = packV16
{-# COMPLETE V16 #-}

unpackV2 :: Prim a => V2 a -> (a,a)
unpackV2 (Vec ba#) =
  ( indexByteArray# ba# 0#
  , indexByteArray# ba# 1#
  )

unpackV3 :: Prim a => V3 a -> (a,a,a)
unpackV3 (Vec ba#) =
  ( indexByteArray# ba# 0#
  , indexByteArray# ba# 1#
  , indexByteArray# ba# 2#
  )

unpackV4 :: Prim a => V4 a -> (a,a,a,a)
unpackV4 (Vec ba#) =
  ( indexByteArray# ba# 0#
  , indexByteArray# ba# 1#
  , indexByteArray# ba# 2#
  , indexByteArray# ba# 3#
  )

unpackV8 :: Prim a => V8 a -> (a,a,a,a,a,a,a,a)
unpackV8 (Vec ba#) =
  ( indexByteArray# ba# 0#
  , indexByteArray# ba# 1#
  , indexByteArray# ba# 2#
  , indexByteArray# ba# 3#
  , indexByteArray# ba# 4#
  , indexByteArray# ba# 5#
  , indexByteArray# ba# 6#
  , indexByteArray# ba# 7#
  )

unpackV16 :: Prim a => V16 a -> (a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a)
unpackV16 (Vec ba#) =
  ( indexByteArray# ba# 0#
  , indexByteArray# ba# 1#
  , indexByteArray# ba# 2#
  , indexByteArray# ba# 3#
  , indexByteArray# ba# 4#
  , indexByteArray# ba# 5#
  , indexByteArray# ba# 6#
  , indexByteArray# ba# 7#
  , indexByteArray# ba# 8#
  , indexByteArray# ba# 9#
  , indexByteArray# ba# 10#
  , indexByteArray# ba# 11#
  , indexByteArray# ba# 12#
  , indexByteArray# ba# 13#
  , indexByteArray# ba# 14#
  , indexByteArray# ba# 15#
  )

packV2 :: Prim a => a -> a -> V2 a
packV2 a b = runST $ do
  mba <- newByteArray (2 * sizeOf a)
  writeByteArray mba 0 a
  writeByteArray mba 1 b
  ByteArray ba# <- unsafeFreezeByteArray mba
  return $! Vec ba#

packV3 :: Prim a => a -> a -> a -> V3 a
packV3 a b c = runST $ do
  mba <- newByteArray (3 * sizeOf a)
  writeByteArray mba 0 a
  writeByteArray mba 1 b
  writeByteArray mba 2 c
  ByteArray ba# <- unsafeFreezeByteArray mba
  return $! Vec ba#

packV4 :: Prim a => a -> a -> a -> a -> V4 a
packV4 a b c d = runST $ do
  mba <- newByteArray (4 * sizeOf a)
  writeByteArray mba 0 a
  writeByteArray mba 1 b
  writeByteArray mba 2 c
  writeByteArray mba 3 d
  ByteArray ba# <- unsafeFreezeByteArray mba
  return $! Vec ba#

packV8 :: Prim a => a -> a -> a -> a -> a -> a -> a -> a -> V8 a
packV8 a b c d e f g h = runST $ do
  mba <- newByteArray (8 * sizeOf a)
  writeByteArray mba 0 a
  writeByteArray mba 1 b
  writeByteArray mba 2 c
  writeByteArray mba 3 d
  writeByteArray mba 4 e
  writeByteArray mba 5 f
  writeByteArray mba 6 g
  writeByteArray mba 7 h
  ByteArray ba# <- unsafeFreezeByteArray mba
  return $! Vec ba#

packV16 :: Prim a => a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> V16 a
packV16 a b c d e f g h i j k l m n o p = runST $ do
  mba <- newByteArray (16 * sizeOf a)
  writeByteArray mba 0 a
  writeByteArray mba 1 b
  writeByteArray mba 2 c
  writeByteArray mba 3 d
  writeByteArray mba 4 e
  writeByteArray mba 5 f
  writeByteArray mba 6 g
  writeByteArray mba 7 h
  writeByteArray mba 8 i
  writeByteArray mba 9 j
  writeByteArray mba 10 k
  writeByteArray mba 11 l
  writeByteArray mba 12 m
  writeByteArray mba 13 n
  writeByteArray mba 14 o
  writeByteArray mba 15 p
  ByteArray ba# <- unsafeFreezeByteArray mba
  return $! Vec ba#


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

      nonNumTypes :: [(Name, Integer)]
      nonNumTypes =
        [ (''Bool, 8)     -- stored as Word8
        , (''Char, 32)
        ]

      vectorTypes :: [(Name, Integer)]
      vectorTypes = integralTypes ++ floatingTypes ++ tail nonNumTypes  -- not Bool, no ArrayElt instances

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
        [d| instance KnownNat n => IsScalar (Vec n $(conT t)) where
              scalarType = VectorScalarType (VectorType (fromIntegral (natVal' (proxy# :: Proxy# n))) singleType)

            type instance BitSize (Vec w $(conT t)) = w GHC.TypeLits.* $(litT (numTyLit n))
          |]
      --
  is <- mapM (uncurry mkIntegral) integralTypes
  fs <- mapM (uncurry mkFloating) floatingTypes
  ns <- mapM (uncurry mkNonNum)   nonNumTypes
  vs <- mapM (uncurry mkVector)   vectorTypes
  --
  return (concat is ++ concat fs ++ concat ns ++ concat vs)
 )

