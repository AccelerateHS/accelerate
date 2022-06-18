{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Sugar.Vec
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Sugar.Vec (

  Vec(..), KnownNat,
  Vec2,
  Vec3,
  Vec4,
  Vec8,
  Vec16,
  SIMD(..),

) where

import Data.Array.Accelerate.Representation.Tag
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Sugar.Shape
import Data.Array.Accelerate.Type
import qualified Data.Array.Accelerate.Representation.Vec           as R
import qualified Data.Primitive.Vec                                 as Prim

import Data.Kind
import Prettyprinter
import Language.Haskell.TH.Extra                                    hiding ( Type )

import GHC.TypeLits
import GHC.Generics
import qualified GHC.Exts                                           as GHC


-- | SIMD vectors of fixed width
--
data Vec n a = Vec (VecR n a)

-- Synonyms for common vector sizes
--
type Vec2  = Vec 2
type Vec3  = Vec 3
type Vec4  = Vec 4
type Vec8  = Vec 8
type Vec16 = Vec 16

instance (Show a, Elt a, SIMD n a) => Show (Vec n a) where
  show = vec . toList
    where
      vec :: [a] -> String
      vec = show
          . group . encloseSep (flatAlt "< " "<") (flatAlt " >" ">") ", "
          . map viaShow

instance (Eq a, SIMD n a) => Eq (Vec n a) where
  Vec x == Vec y = tuple (vecR @n @a) x y
    where
      tuple :: TypeR v -> v -> v -> Bool
      tuple TupRunit         ()      ()      = True
      tuple (TupRpair aR bR) (a1,b1) (a2,b2) = tuple aR a1 a2 && tuple bR b1 b2
      tuple (TupRsingle t)   a       b       = scalar t a b

      scalar :: ScalarType v -> v -> v -> Bool
      scalar (NumScalarType t) = num t
      scalar (BitScalarType t) = bit t

      bit :: BitType v -> v -> v -> Bool
      bit TypeBit    = (==)
      bit TypeMask{} = (==)

      num :: NumType v -> v -> v -> Bool
      num (IntegralNumType t) = integral t
      num (FloatingNumType t) = floating t

      integral :: IntegralType v -> v -> v -> Bool
      integral (SingleIntegralType t) = case t of
        TypeInt8    -> (==)
        TypeInt16   -> (==)
        TypeInt32   -> (==)
        TypeInt64   -> (==)
        TypeInt128  -> (==)
        TypeWord8   -> (==)
        TypeWord16  -> (==)
        TypeWord32  -> (==)
        TypeWord64  -> (==)
        TypeWord128 -> (==)
      integral (VectorIntegralType _ t) = case t of
        TypeInt8    -> (==)
        TypeInt16   -> (==)
        TypeInt32   -> (==)
        TypeInt64   -> (==)
        TypeInt128  -> (==)
        TypeWord8   -> (==)
        TypeWord16  -> (==)
        TypeWord32  -> (==)
        TypeWord64  -> (==)
        TypeWord128 -> (==)

      floating :: FloatingType v -> v -> v -> Bool
      floating (SingleFloatingType t) = case t of
        TypeFloat16  -> (==)
        TypeFloat32  -> (==)
        TypeFloat64  -> (==)
        TypeFloat128 -> (==)
      floating (VectorFloatingType _ t) = case t of
        TypeFloat16  -> (==)
        TypeFloat32  -> (==)
        TypeFloat64  -> (==)
        TypeFloat128 -> (==)


instance (Elt a, SIMD n a) => GHC.IsList (Vec n a) where
  type Item (Vec n a) = a
  toList   = toList
  fromList = fromList

toList :: forall n a. (Elt a, SIMD n a) => Vec n a -> [a]
toList (Vec vs) = map toElt $ R.toList (vecR @n @a) (eltR @a) vs

fromList :: forall n a. (Elt a, SIMD n a) => [a] -> Vec n a
fromList = Vec . R.fromList (vecR @n @a) (eltR @a) . map fromElt

instance SIMD n a => Elt (Vec n a) where
  type EltR (Vec n a) = VecR n a
  eltR            = vecR @n @a
  tagsR           = vtagsR @n @a
  toElt           = Vec
  fromElt (Vec a) = a

-- | The 'SIMD' class characterises the subset of scalar element types from
-- 'Elt' that can be packed into SIMD vectors.
--
-- @since 1.4.0.0
--
class KnownNat n => SIMD n a where
  type VecR n a :: Type
  type VecR n a = GVecR () n (Rep a)

  vecR    :: TypeR (VecR n a)
  vtagsR  :: [TagR (VecR n a)]

  default vecR
      :: (GVec n (Rep a), VecR n a ~ GVecR () n (Rep a))
      => TypeR (VecR n a)
  vecR = gvecR @n @(Rep a) TupRunit

  -- default vtagsR
  --     :: (GVec n (Rep a), VecR n a ~ GVecR () n (Rep a))
  --     => [TagR (VecR n a)]
  -- vtagsR = gvtagsR @n @(Rep a) TagRunit
  vtagsR = [tagOfType (vecR @n @a)]

class KnownNat n => GVec n (f :: Type -> Type) where
  type GVecR t n f
  gvecR   :: TypeR t -> TypeR (GVecR t n f)
  gvtagsR :: TagR t -> [TagR (GVecR t n f)]

instance KnownNat n => GVec n U1 where
  type GVecR t n U1 = t
  gvecR t   = t
  gvtagsR t = [t]

instance GVec n a => GVec n (M1 i c a) where
  type GVecR t n (M1 i c a) = GVecR t n a
  gvecR   = gvecR @n @a
  gvtagsR = gvtagsR @n @a

instance SIMD n a => GVec n (K1 i a) where
  type GVecR t n (K1 i a) = (t, VecR n a)
  gvecR t   = TupRpair t (vecR @n @a)
  gvtagsR t = TagRpair t <$> vtagsR @n @a

instance (GVec n a, GVec n b) => GVec n (a :*: b) where
  type GVecR t n (a :*: b) = GVecR (GVecR t n a) n b
  gvecR   = gvecR @n @b . gvecR @n @a
  gvtagsR = concatMap (gvtagsR @n @b) . gvtagsR @n @a

instance (GVec n a, GVec n b, GSumVec n (a :+: b)) => GVec n (a :+: b) where
  type GVecR t n (a :+: b) = (Prim.Vec n TAG, GSumVecR t n (a :+: b))
  gvecR t = TupRpair (TupRsingle scalarType) (gsumvecR @n @(a :+: b) t)
  -- gvtagsR t = let zero = Prim.fromList (replicate (fromInteger (natVal' (proxy# @n))) 0)
  --              in uncurry TagRtag <$> gsumvtagsR @n @(a :+: b) zero t
  gvtagsR _ = error "TODO: gvtagsR (:+:)"

class KnownNat n => GSumVec n (f :: Type -> Type) where
  type GSumVecR t n f
  gsumvecR :: TypeR t -> TypeR (GSumVecR t n f)
  -- gsumvtagsR :: Prim.Vec n TAG -> TagR t -> [(Prim.Vec n TAG, TagR (GSumVecR t n f))]

instance KnownNat n => GSumVec n U1 where
  type GSumVecR t n U1 = t
  gsumvecR t = t

instance GSumVec n a => GSumVec n (M1 i c a) where
  type GSumVecR t n (M1 i c a) = GSumVecR t n a
  gsumvecR = gsumvecR @n @a

instance (KnownNat n, SIMD n a) => GSumVec n (K1 i a) where
  type GSumVecR t n (K1 i a) = (t, VecR n a)
  gsumvecR t = TupRpair t (vecR @n @a)

instance (GVec n a, GVec n b) => GSumVec n (a :*: b) where
  type GSumVecR t n (a :*: b) = GVecR t n (a :*: b)
  gsumvecR = gvecR @n @(a :*: b)

instance (GSumVec n a, GSumVec n b) => GSumVec n (a :+: b) where
  type GSumVecR t n (a :+: b) = GSumVecR (GSumVecR t n a) n b
  gsumvecR = gsumvecR @n @b . gsumvecR @n @a

tagOfType :: TypeR a -> TagR a
tagOfType TupRunit       = TagRunit
tagOfType (TupRpair s t) = TagRpair (tagOfType s) (tagOfType t)
tagOfType (TupRsingle t) = TagRsingle t

instance KnownNat n => SIMD n Z
instance KnownNat n => SIMD n ()
instance KnownNat n => SIMD n Ordering
instance SIMD n a => SIMD n (Maybe a)
instance SIMD n sh => SIMD n (sh :. Int)
instance (SIMD n a, SIMD n b) => SIMD n (Either a b)

instance KnownNat n => SIMD n Bool where
  type VecR n Bool = Prim.Vec n Bit
  vecR             = TupRsingle scalarType
  vtagsR           = [TagRsingle scalarType]

instance KnownNat n => SIMD n Int where
  type VecR n Int = Prim.Vec n (EltR Int)
  vecR            = TupRsingle scalarType
  vtagsR          = [TagRsingle scalarType]

instance KnownNat n => SIMD n Word where
  type VecR n Word = Prim.Vec n (EltR Word)
  vecR            = TupRsingle scalarType
  vtagsR          = [TagRsingle scalarType]

instance KnownNat n => SIMD n Char where
  type VecR n Char = Prim.Vec n (EltR Char)
  vecR             = TupRsingle scalarType
  vtagsR           = [TagRsingle scalarType]

runQ $ do
  let
      integralTypes :: [Name]
      integralTypes =
        [ ''Int8
        , ''Int16
        , ''Int32
        , ''Int64
        , ''Int128
        , ''Word8
        , ''Word16
        , ''Word32
        , ''Word64
        , ''Word128
        ]

      floatingTypes :: [Name]
      floatingTypes =
        [ ''Half
        , ''Float
        , ''Double
        , ''Float128
        ]

      numTypes :: [Name]
      numTypes = integralTypes ++ floatingTypes

      mkPrim :: Name -> Q [Dec]
      mkPrim name =
        let t = conT name
        in
        [d| instance KnownNat n => SIMD n $t where
              type VecR n $t = Prim.Vec n $t
              vecR   = TupRsingle scalarType
              vtagsR = [TagRsingle scalarType]
          |]

      mkTuple :: Int -> Q Dec
      mkTuple n = do
        w <- newName "w"
        let
            xs  = [ mkName ('x' : show i) | i <- [0 .. n-1] ]
            ts  = map varT xs
            res = tupT ts
            ctx = mapM (appT [t| SIMD $(varT w) |]) ts
        --
        instanceD ctx [t| SIMD $(varT w) $res |] []
  --
  ps <- mapM mkPrim numTypes
  ts <- mapM mkTuple [2..16]
  return (concat ps ++ ts)


{--
type NoTypeError = (() :: Constraint)

type family NoNestedVec (f :: k) :: Constraint where
  NoNestedVec t = If (HasNestedVec (Rep t))
                       (TypeError (NestedVecError t))
                       NoTypeError

type family HasNestedVec (f :: k -> Type) :: Bool where
  HasNestedVec (f :+: g)        = HasNestedVec f || HasNestedVec g
  HasNestedVec (f :*: g)        = HasNestedVec f || HasNestedVec g
  HasNestedVec (M1 _ _ a)       = HasNestedVec a
  HasNestedVec U1               = 'False
  -- HasNestedVec (Rec0 Int)       = 'False
  -- HasNestedVec (Rec0 Int8)      = 'False
  -- HasNestedVec (Rec0 Int16)     = 'False
  -- HasNestedVec (Rec0 Int32)     = 'False
  -- HasNestedVec (Rec0 Int64)     = 'False
  -- HasNestedVec (Rec0 Int128)    = 'False
  -- HasNestedVec (Rec0 Word)      = 'False
  -- HasNestedVec (Rec0 Word8)     = 'False
  -- HasNestedVec (Rec0 Word16)    = 'False
  -- HasNestedVec (Rec0 Word32)    = 'False
  -- HasNestedVec (Rec0 Word64)    = 'False
  -- HasNestedVec (Rec0 Word128)   = 'False
  -- HasNestedVec (Rec0 Half)      = 'False
  -- HasNestedVec (Rec0 Float)     = 'False
  -- HasNestedVec (Rec0 Double)    = 'False
  -- HasNestedVec (Rec0 Float128)  = 'False
  -- HasNestedVec (Rec0 (Vec _ _)) = 'True
  -- HasNestedVec (Rec0 a)         = 'False
  HasNestedVec (Rec0 a)         = Stuck (Rep a) 'False (HasNestedVec (Rep a))

-- type family NoNestedVec (f :: k -> Type) :: Constraint where
--   NoNestedVec (f :+: g)        = (NoNestedVec f, NoNestedVec g)
--   NoNestedVec (f :*: g)        = (NoNestedVec f, NoNestedVec g)
--   NoNestedVec (M1 _ _ a)       = NoNestedVec a
--   NoNestedVec U1               = NoTypeError
--   NoNestedVec (Rec0 Int)       = NoTypeError
--   NoNestedVec (Rec0 Int8)      = NoTypeError
--   NoNestedVec (Rec0 Int16)     = NoTypeError
--   NoNestedVec (Rec0 Int32)     = NoTypeError
--   NoNestedVec (Rec0 Int64)     = NoTypeError
--   NoNestedVec (Rec0 Int128)    = NoTypeError
--   NoNestedVec (Rec0 Word)      = NoTypeError
--   NoNestedVec (Rec0 Word8)     = NoTypeError
--   NoNestedVec (Rec0 Word16)    = NoTypeError
--   NoNestedVec (Rec0 Word32)    = NoTypeError
--   NoNestedVec (Rec0 Word64)    = NoTypeError
--   NoNestedVec (Rec0 Word128)   = NoTypeError
--   NoNestedVec (Rec0 Half)      = NoTypeError
--   NoNestedVec (Rec0 Float)     = NoTypeError
--   NoNestedVec (Rec0 Double)    = NoTypeError
--   NoNestedVec (Rec0 Float128)  = NoTypeError
--   NoNestedVec (Rec0 (Vec _ _)) = TypeError (NestedVecError Int)
--   NoNestedVec (Rec0 a)         = Stuck (Rep a) NoTypeError (NoNestedVec (Rep a))

--   NoNestedVec _                = NoTypeError
  -- NoNestedVec (K1 R a)         = NoNestedVec (Rep a)
  -- NoNestedVec (K1 R a)         = Stuck (Rep a) (NoGeneric a) (NoNestedVec (Rep a))

-- type family IsVec a :: Bool where
--   IsVec (Vec n a) = 'True
--   IsVec _         = 'False

-- foo :: forall a. Stuck (Rep a) (NoGeneric a) NoTypeError => ()
foo :: forall a. NoNestedVec a => ()
foo = ()

data T x
type family Any :: k

type family Stuck (f :: Type -> Type) (c :: Bool) (a :: k) :: k where
  Stuck T _ _ = Any
  Stuck _ _ k = k

type family NoGeneric t where
  NoGeneric x = TypeError ('Text "No instance for " ':<>: 'ShowType (Generic x))

-- type family NoSIMD n t where
--   NoSIMD n t = TypeError ('Text "No instance for " ':<>: 'ShowType (SIMD n t))

type family NestedVecError (t :: k) :: ErrorMessage where
  NestedVecError t = 'Text "Can not derive SIMD class"
               ':$$: 'Text "Because '" ':<>: 'ShowType t ':<>: 'Text "' already contains a SIMD vector."
--}

