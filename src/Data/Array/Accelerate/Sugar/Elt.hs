{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE GADTs                #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -ddump-splices #-}
-- |
-- Module      : Data.Array.Accelerate.Sugar.Elt
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Sugar.Elt ( Elt(..), eltRType, EltRType(..) )
  where

import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Representation.POS
import Data.Array.Accelerate.Representation.Tag
import Data.Array.Accelerate.Sugar.POS ()
import Data.Array.Accelerate.Representation.POS
import Data.Array.Accelerate.Type

import Data.Char
import Data.Kind
import Language.Haskell.TH.Extra                                    hiding ( Type )

import GHC.TypeLits
import Unsafe.Coerce
import Data.Type.Equality
import Data.Proxy
import Data.Typeable
import Data.Finite.Internal (Finite(..))

-- | The 'Elt' class characterises the allowable array element types, and
-- hence the types which can appear in scalar Accelerate expressions of
-- type 'Data.Array.Accelerate.Exp'.
--
-- Accelerate arrays consist of simple atomic types as well as nested
-- tuples thereof, stored efficiently in memory as consecutive unpacked
-- elements without pointers. It roughly consists of:
--
--  * Signed and unsigned integers (8, 16, 32, and 64-bits wide)
--  * Floating point numbers (half, single, and double precision)
--  * 'Char'
--  * 'Bool'
--  * ()
--  * Shapes formed from 'Z' and (':.')
--  * Nested tuples of all of these, currently up to 16-elements wide
--
-- Adding new instances for 'Elt' consists of explaining to Accelerate how
-- to map between your data type and a (tuple of) primitive values. For
-- examples see:
--
--  * "Data.Array.Accelerate.Data.Complex"
--  * "Data.Array.Accelerate.Data.Monoid"
--  * <https://hackage.haskell.org/package/linear-accelerate linear-accelerate>
--  * <https://hackage.haskell.org/package/colour-accelerate colour-accelerate>
--
-- For simple types it is possible to derive 'Elt' automatically, for
-- example:
--
-- > data Point = Point Int Float
-- >   deriving (Generic, Elt)
--
-- > data Option a = None | Just a
-- >   deriving (Generic, Elt)
--
-- See the function 'Data.Array.Accelerate.match' for details on how to use
-- sum types in embedded code.
--
class (KnownNat (EltChoices a)) => Elt a where
  -- | Type representation mapping, which explains how to convert a type
  -- from the surface type into the internal representation type consisting
  -- only of simple primitive types, unit '()', and pair '(,)'.
  --
  type EltR a :: Type
  type EltR a = POStoEltR (Choices a) (Fields a)

  type EltChoices a :: Nat
  type EltChoices a = Choices a

  --
  eltR    :: TypeR (EltR a)
  tagsR   :: [TagR (EltR a)]
  fromElt :: a -> EltR a
  toElt   :: EltR a -> a

  default eltR :: (POSable a, POStoEltR (Choices a) (Fields a) ~ EltR a) => TypeR (EltR a)
  eltR = mkEltRT @a

  default fromElt :: (POSable a, POStoEltR (Choices a) (Fields a) ~ EltR a) => a -> EltR a
  fromElt = mkEltR

  default toElt :: (POSable a,  POStoEltR (Choices a) (Fields a) ~ EltR a) => EltR a -> a
  toElt = fromEltR

  default tagsR :: (POSable a) => [TagR (EltR a)]
  tagsR = f 0 (map fromInteger (tags @a))
    where
      f :: TAG -> [TAG] -> [TagR (EltR a)]
      f n l = case l of
        [] -> []
        x : xs -> (TagR n (n + x)) : f (n + x) xs


-- function to bring the contraints in scope that are needed to work with EltR,
-- without needing to inspect how POS2EltR works
data EltRType x where
  SingletonType :: (EltR x ~ POStoEltR (Choices x) (Fields x), EltR x ~ x, Fields x ~ '[ '[x]]) => EltRType x
  TaglessType   :: (EltR x ~ POStoEltR (Choices x) (Fields x), EltR x ~ FlattenProduct (Fields x)) => EltRType x
  TaggedType    :: (EltR x ~ POStoEltR (Choices x) (Fields x), EltR x ~ (TAG, FlattenProduct (Fields x))) => EltRType x

eltRType :: forall x . POSable x => EltRType x
eltRType = case sameNat (Proxy :: Proxy (Choices x)) (Proxy :: Proxy 1) of
  Just Refl -> case emptyFields @x of
    PTCons (STSucc _ STZero) PTNil
      | Refl :: (EltR x :~: x) <- unsafeCoerce Refl
      , Refl :: (Fields x :~: '[ '[x]]) <- unsafeCoerce Refl
      -> SingletonType
    _
      | Refl :: (EltR x :~: FlattenProduct (Fields x)) <- unsafeCoerce Refl
      , Refl :: (POStoEltR 1 (Fields x) :~: EltR x) <- unsafeCoerce Refl
      -> TaglessType
  Nothing
    | Refl :: (EltR x :~: (TAG, FlattenProduct (Fields x))) <- unsafeCoerce Refl
    , Refl :: (POStoEltR (Choices x) (Fields x) :~: (TAG, FlattenProduct (Fields x))) <- unsafeCoerce Refl
    -> TaggedType


flattenProductType :: ProductType a -> TypeR (FlattenProduct a)
flattenProductType PTNil = TupRunit
flattenProductType (PTCons x xs) = TupRpair (TupRsingle (flattenSumType x)) (flattenProductType xs)

flattenSumType :: SumType a -> ScalarType (UnionScalar (FlattenSum a))
flattenSumType STZero = UnionScalarType ZeroScalarType
flattenSumType (STSucc x xs) = case flattenSumType xs of
  UnionScalarType xs' -> UnionScalarType (SuccScalarType (mkSingleType x) xs')

-- This is an unsafe conversion, and should be kept strictly in sync with the
-- set of types that implement Ground
mkScalarType :: forall a . (Typeable a, Ground a) => a -> ScalarType a
mkScalarType _
  | Just Refl <- eqT @a @Int
   = scalarType @a
mkScalarType _
  | Just Refl <- eqT @a @Int8
   = scalarType @a
mkScalarType _
  | Just Refl <- eqT @a @Int16
  = scalarType @a
mkScalarType _
  | Just Refl <- eqT @a @Int32
  = scalarType @a
mkScalarType _
  | Just Refl <- eqT @a @Int64
  = scalarType @a
mkScalarType _
  | Just Refl <- eqT @a @Word
    = scalarType @a
mkScalarType _
  | Just Refl <- eqT @a @Word8
    = scalarType @a
mkScalarType _
  | Just Refl <- eqT @a @Word16
  = scalarType @a
mkScalarType _
  | Just Refl <- eqT @a @Word32
  = scalarType @a
mkScalarType _
  | Just Refl <- eqT @a @Word64
  = scalarType @a
mkScalarType _
  | Just Refl <- eqT @a @Half
  = scalarType @a
mkScalarType _
  | Just Refl <- eqT @a @Float
  = scalarType @a
mkScalarType _
  | Just Refl <- eqT @a @Double
  = scalarType @a
mkScalarType _
  | Just Refl <- eqT @a @Undef
  = scalarType @a


-- This is an unsafe conversion, and should be kept strictly in sync with the
-- set of types that implement Ground
mkSingleType :: forall a . (Typeable a, Ground a) => a -> SingleType a
mkSingleType _
  | Just Refl <- eqT @a @Int
   = singleType @a
mkSingleType _
  | Just Refl <- eqT @a @Int8
   = singleType @a
mkSingleType _
  | Just Refl <- eqT @a @Int16
  = singleType @a
mkSingleType _
  | Just Refl <- eqT @a @Int32
  = singleType @a
mkSingleType _
  | Just Refl <- eqT @a @Int64
  = singleType @a
mkSingleType _
  | Just Refl <- eqT @a @Word
    = singleType @a
mkSingleType _
  | Just Refl <- eqT @a @Word8
    = singleType @a
mkSingleType _
  | Just Refl <- eqT @a @Word16
  = singleType @a
mkSingleType _
  | Just Refl <- eqT @a @Word32
  = singleType @a
mkSingleType _
  | Just Refl <- eqT @a @Word64
  = singleType @a
mkSingleType _
  | Just Refl <- eqT @a @Half
  = singleType @a
mkSingleType _
  | Just Refl <- eqT @a @Float
  = singleType @a
mkSingleType _
  | Just Refl <- eqT @a @Double
  = singleType @a
mkSingleType _
  | Just Refl <- eqT @a @Undef
  = singleType @a


mkEltRT :: forall a . (POSable a) => TypeR (POStoEltR (Choices a) (Fields a))
mkEltRT = case eltRType @a of
  SingletonType | PTCons (STSucc x STZero) PTNil <- emptyFields @a -> TupRsingle (mkScalarType x)
  TaglessType   -> flattenProductType (emptyFields @a)
  TaggedType    -> TupRpair (TupRsingle (SingleScalarType (NumSingleType (IntegralNumType TypeTAG)))) (flattenProductType (emptyFields @a))


mkEltR :: forall a . (POSable a) => a -> POStoEltR (Choices a) (Fields a)
mkEltR x = case eltRType @a of
  SingletonType | Cons (Pick f) Nil <- fields x -> f
  TaglessType   -> fs
  TaggedType    -> (cs, fs)
  where
    cs = fromInteger @TAG $ toInteger $ choices x
    fs = flattenProduct (fields x)

fromEltR :: forall a . (POSable a) => POStoEltR (Choices a) (Fields a) -> a
fromEltR x = case eltRType @a of
  SingletonType -> x
  TaglessType   -> fromPOSable 0 (unFlattenProduct (emptyFields @a) x)
  TaggedType    | (t, fs) <- x -> fromPOSable (Finite $ toInteger t) (unFlattenProduct (emptyFields @a) fs)

unFlattenProduct :: ProductType a -> FlattenProduct a -> Product a
unFlattenProduct PTNil () = Nil
unFlattenProduct (PTCons x xs) (y, ys) = Cons (unFlattenSum x y) (unFlattenProduct xs ys)

unFlattenSum :: SumType a -> UnionScalar (FlattenSum a) -> Sum a
unFlattenSum (STSucc x xs) (PickScalar y) = Pick y
unFlattenSum (STSucc x xs) (SkipScalar ys) = Skip $ unFlattenSum xs ys


flattenProduct :: Product a -> FlattenProduct a
flattenProduct Nil = ()
flattenProduct (Cons x xs) = (flattenSum x, flattenProduct xs)

flattenSum :: Sum a -> UnionScalar (FlattenSum a)
flattenSum (Pick x) = PickScalar x
flattenSum (Skip xs) = SkipScalar (flattenSum xs)
              
-- untag :: TypeR t -> TagR t
-- untag TupRunit         = TagRunit
-- untag (TupRsingle t)   = TagRundef t
-- untag (TupRpair ta tb) = TagRpair (untag ta) (untag tb)


-- Note: [Deriving Elt]
--
-- We can't use the cunning generalised newtype deriving mechanism, because
-- the generated 'eltR function does not type check. For example, it will
-- generate the following implementation for 'CShort':
--
-- > eltR
-- >   = coerce
-- >       @(TypeR (EltR Int16))
-- >       @(TypeR (EltR CShort))
-- >       (eltR :: TypeR (EltR CShort))
--
-- Which yields the error "couldn't match type 'EltR a0' with 'Int16'".
-- Since this function returns a type family type, the type signature on the
-- result is not enough to fix the type 'a'. Instead, we require the use of
-- (visible) type applications:
--
-- > eltR
-- >   = coerce
-- >       @(TypeR (EltR Int16))
-- >       @(TypeR (EltR CShort))
-- >       (eltR @(EltR CShort))
--
-- Note that this does not affect deriving instances via 'Generic'
--
-- Instances for basic types are generated at the end of this module.
--

instance Elt ()
instance Elt Bool
instance Elt Ordering
instance (POSable (Maybe a), Elt a) => Elt (Maybe a)
instance (POSable (Either a b), Elt a, Elt b) => Elt (Either a b)

instance Elt Char where
  type EltR Char = Word32
  type EltChoices Char = 1
  eltR    = TupRsingle scalarType
  tagsR   = [TagR 0 1]
  toElt   = chr . fromIntegral
  fromElt = fromIntegral . ord

-- Anything that has a POS instance has a default Elt instance
-- TODO: build instances for the sections of newtypes
runQ $ do
  let
      -- XXX: we might want to do the digItOut trick used by FromIntegral?
      --
      integralTypes :: [Name]
      integralTypes =
        [ ''Int
        , ''Int8
        , ''Int16
        , ''Int32
        , ''Int64
        , ''Word
        , ''Word8
        , ''Word16
        , ''Word32
        , ''Word64
        ]

      floatingTypes :: [Name]
      floatingTypes =
        [ ''Half
        , ''Float
        , ''Double
        ]

      newtypes :: [Name]
      newtypes =
        [ ''CShort
        , ''CUShort
        , ''CInt
        , ''CUInt
        , ''CLong
        , ''CULong
        , ''CLLong
        , ''CULLong
        , ''CFloat
        , ''CDouble
        , ''CChar
        , ''CSChar
        , ''CUChar
        ]

      mkSimple :: Name -> Q [Dec]
      mkSimple name =
        let t = conT name
        in
        [d| instance Elt $t
          |]

      mkTuple :: Int -> Q Dec
      mkTuple n =
        let
            xs  = [ mkName ('x' : show i) | i <- [0 .. n-1] ]
            ts  = map varT xs
            res = tupT ts
            ctx = mapM (appT [t| Elt |]) ts
        in
        instanceD ctx [t| Elt $res |] []

      -- mkVecElt :: Name -> Integer -> Q [Dec]
      -- mkVecElt name n =
      --   let t = conT name
      --       v = [t| Vec $(litT (numTyLit n)) $t |]
      --    in
      --    [d| instance Elt $v where
      --          type EltR $v = $v
      --          eltR    = TupRsingle scalarType
      --          fromElt = id
      --          toElt   = id
      --      |]

      -- ghci> $( stringE . show =<< reify ''CFloat )
      -- TyConI (NewtypeD [] Foreign.C.Types.CFloat [] Nothing (NormalC Foreign.C.Types.CFloat [(Bang NoSourceUnpackedness NoSourceStrictness,ConT GHC.Types.Float)]) [])
      --
      mkNewtype :: Name -> Q [Dec]
      mkNewtype name =
        let t = conT name
        in
        [d| instance Elt $t
          |]
  --
  ss <- mapM mkSimple (integralTypes ++ floatingTypes)
  -- TODO:
  ns <- mapM mkNewtype newtypes
  -- ts <- mapM mkTuple [2..8]
  -- vs <- sequence [ mkVecElt t n | t <- integralTypes ++ floatingTypes, n <- [2,3,4,8,16] ]
  return (concat ss ++ concat ns)

instance Elt Undef

-- TODO: bring this back into TH
instance (POSable a, POSable b) => Elt (a, b)
instance (POSable a, POSable b, POSable c) => Elt (a, b, c)
instance (POSable a, POSable b, POSable c, POSable d) => Elt (a, b, c, d)
instance (POSable a, POSable b, POSable c, POSable d, POSable e) => Elt (a, b, c, d, e)
