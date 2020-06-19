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
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Sugar.Elt
-- Copyright   : [2008..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Sugar.Elt ( Elt(..) )
  where

import Data.Array.Accelerate.Representation.Elt
import Data.Array.Accelerate.Representation.Tag
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Type

import Data.Bits
import Data.Char
import Data.Kind
import Language.Haskell.TH                                          hiding ( Type )
import Language.Haskell.TH.Extra

import GHC.Generics


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
class Elt a where
  -- | Type representation mapping, which explains how to convert a type
  -- from the surface type into the internal representation type consisting
  -- only of simple primitive types, unit '()', and pair '(,)'.
  --
  type EltR a :: Type
  type EltR a = GEltR () (Rep a)
  --
  eltR    :: TypeR (EltR a)
  tagsR   :: [TagR (EltR a)]
  fromElt :: a -> EltR a
  toElt   :: EltR a -> a

  default eltR
      :: (GElt (Rep a), EltR a ~ GEltR () (Rep a))
      => TypeR (EltR a)
  eltR = geltR @(Rep a) TupRunit

  default tagsR
      :: (Generic a, GElt (Rep a), EltR a ~ GEltR () (Rep a))
      => [TagR (EltR a)]
  tagsR = gtagsR @(Rep a) TagRunit

  default fromElt
      :: (Generic a, GElt (Rep a), EltR a ~ GEltR () (Rep a))
      => a
      -> EltR a
  fromElt = gfromElt () . from

  default toElt
      :: (Generic a, GElt (Rep a), EltR a ~ GEltR () (Rep a))
      => EltR a
      -> a
  toElt = to . snd . gtoElt @(Rep a) @()


class GElt f where
  type GEltR t f
  geltR    :: TypeR t -> TypeR (GEltR t f)
  gtagsR   :: TagR t -> [TagR (GEltR t f)]
  gfromElt :: t -> f a -> GEltR t f
  gtoElt   :: GEltR t f -> (t, f a)
  --
  gundef   :: t -> GEltR t f
  guntag   :: TagR t -> TagR (GEltR t f)

instance GElt U1 where
  type GEltR t U1 = t
  geltR t       = t
  gtagsR t      = [t]
  gfromElt t U1 = t
  gtoElt t      = (t, U1)
  gundef t      = t
  guntag t      = t

instance GElt a => GElt (M1 i c a) where
  type GEltR t (M1 i c a) = GEltR t a
  geltR             = geltR @a
  gtagsR            = gtagsR @a
  gfromElt t (M1 x) = gfromElt t x
  gtoElt         x  = let (t, x1) = gtoElt x in (t, M1 x1)
  gundef            = gundef @a
  guntag            = guntag @a

instance Elt a => GElt (K1 i a) where
  type GEltR t (K1 i a) = (t, EltR a)
  geltR t           = TupRpair t (eltR @a)
  gtagsR t          = TagRpair t <$> tagsR @a
  gfromElt t (K1 x) = (t, fromElt x)
  gtoElt     (t, x) = (t, K1 (toElt x))
  gundef t          = (t, undefElt (eltR @a))
  guntag t          = TagRpair t (untag (eltR @a))

instance (GElt a, GElt b) => GElt (a :*: b) where
  type GEltR t (a :*: b) = GEltR (GEltR t a) b
  geltR  = geltR @b . geltR @a
  gtagsR = concatMap (gtagsR @b) . gtagsR @a
  gfromElt t (a :*: b) = gfromElt (gfromElt t a) b
  gtoElt t =
    let (t1, b) = gtoElt t
        (t2, a) = gtoElt t1
    in
    (t2, a :*: b)
  gundef t = gundef @b (gundef @a t)
  guntag t = guntag @b (guntag @a t)

instance (GElt a, GElt b, GSumElt (a :+: b)) => GElt (a :+: b) where
  type GEltR t (a :+: b) = (TAG, GSumEltR t (a :+: b))
  geltR t      = TupRpair (TupRsingle scalarType) (gsumEltR @(a :+: b) t)
  gtagsR t     = uncurry TagRtag <$> gsumTagsR @(a :+: b) 0 t
  gfromElt     = gsumFromElt 0
  gtoElt (k,x) = gsumToElt k x
  gundef t     = (0xff, gsumUndef @(a :+: b) t)
  guntag t     = TagRpair (TagRundef scalarType) (gsumUntag @(a :+: b) t)


class GSumElt f where
  type GSumEltR t f
  gsumEltR     :: TypeR t -> TypeR (GSumEltR t f)
  gsumTagsR    :: TAG -> TagR t -> [(TAG, TagR (GSumEltR t f))]
  gsumFromElt  :: TAG -> t -> f a -> (TAG, GSumEltR t f)
  gsumToElt    :: TAG -> GSumEltR t f -> (t, f a)
  gsumUndef    :: t -> GSumEltR t f
  gsumUntag    :: TagR t -> TagR (GSumEltR t f)

instance GSumElt U1 where
  type GSumEltR t U1 = t
  gsumEltR t         = t
  gsumTagsR n t      = [(n, t)]
  gsumFromElt n t U1 = (n, t)
  gsumToElt _ t      = (t, U1)
  gsumUndef t        = t
  gsumUntag t        = t

instance GSumElt a => GSumElt (M1 i c a) where
  type GSumEltR t (M1 i c a) = GSumEltR t a
  gsumEltR               = gsumEltR @a
  gsumTagsR              = gsumTagsR @a
  gsumFromElt n t (M1 x) = gsumFromElt n t x
  gsumToElt k x          = let (t, x') = gsumToElt k x in (t, M1 x')
  gsumUntag              = gsumUntag @a
  gsumUndef              = gsumUndef @a

instance Elt a => GSumElt (K1 i a) where
  type GSumEltR t (K1 i a) = (t, EltR a)
  gsumEltR t             = TupRpair t (eltR @a)
  gsumTagsR n t          = (n,) . TagRpair t <$> tagsR @a
  gsumFromElt n t (K1 x) = (n, (t, fromElt x))
  gsumToElt _ (t, x)     = (t, K1 (toElt x))
  gsumUntag t            = TagRpair t (untag (eltR @a))
  gsumUndef t            = (t, undefElt (eltR @a))

instance (GElt a, GElt b) => GSumElt (a :*: b) where
  type GSumEltR t (a :*: b) = GEltR t (a :*: b)
  gsumEltR                  = geltR @(a :*: b)
  gsumTagsR n t             = (n,) <$> gtagsR @(a :*: b) t
  gsumFromElt n t (a :*: b) = (n, gfromElt (gfromElt t a) b)
  gsumToElt _ t0 =
    let (t1, b) = gtoElt t0
        (t2, a) = gtoElt t1
     in
     (t2, a :*: b)
  gsumUndef       = gundef @(a :*: b)
  gsumUntag       = guntag @(a :*: b)

instance (GSumElt a, GSumElt b) => GSumElt (a :+: b) where
  type GSumEltR t (a :+: b) = GSumEltR (GSumEltR t a) b
  gsumEltR = gsumEltR @b . gsumEltR @a

  gsumFromElt n t (L1 a) = let (m,r) = gsumFromElt n t a
                            in (shiftL m 1, gsumUndef @b r)
  gsumFromElt n t (R1 b) = let (m,r) = gsumFromElt n (gsumUndef @a t) b
                            in (setBit (m `shiftL` 1) 0, r)

  gsumToElt k t0 =
    let (t1, b) = gsumToElt (shiftR k 1) t0
        (t2, a) = gsumToElt (shiftR k 1) t1
     in
     if testBit k 0
        then (t2, R1 b)
        else (t2, L1 a)

  gsumTagsR k t =
    let a = gsumTagsR @a k t
        b = gsumTagsR @b k (gsumUntag @a t)
     in
     map (\(x,y) ->         (x `shiftL` 1, gsumUntag @b y)) a ++
     map (\(x,y) -> (setBit (x `shiftL` 1) 0, y)) b

  gsumUndef t = gsumUndef @b (gsumUndef @a t)
  gsumUntag t = gsumUntag @b (gsumUntag @a t)


class GTags (f :: Type -> Type) where
  gtags :: TAG -> [(String, TAG)]

instance GTags a => GTags (D1 c a) where
  gtags = gtags @a

instance Constructor c => GTags (C1 c a) where
  gtags k = [ (conName (undefined :: D1 c a ()), k) ]

instance (GTags a, GTags b) => GTags (a :+: b) where
  gtags k =
    let as = gtags @a k
        bs = gtags @b k
     in
     map (\(x,y) -> (x,         y `shiftL` 1)   ) as ++
     map (\(x,y) -> (x, setBit (y `shiftL` 1) 0)) bs


untag :: TypeR t -> TagR t
untag TupRunit         = TagRunit
untag (TupRsingle t)   = TagRundef t
untag (TupRpair ta tb) = TagRpair (untag ta) (untag tb)


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

instance Elt Char where
  type EltR Char = Int
  eltR    = TupRsingle scalarType
  tagsR   = [TagRsingle scalarType]
  toElt   = chr
  fromElt = ord

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
        [d| instance Elt $t where
              type EltR $t = $t
              eltR    = TupRsingle scalarType
              tagsR   = [TagRsingle scalarType]
              fromElt = id
              toElt   = id
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
      mkNewtype name = do
        r    <- reify name
        base <- case r of
                  TyConI (NewtypeD _ _ _ _ (NormalC _ [(_, ConT b)]) _) -> return b
                  _                                                     -> error "unexpected case generating newtype Elt instance"
        --
        [d| instance Elt $(conT name) where
              type EltR $(conT name) = $(conT base)
              eltR = TupRsingle scalarType
              tagsR = [TagRsingle scalarType]
              fromElt $(conP (mkName (nameBase name)) [varP (mkName "x")]) = x
              toElt = $(conE (mkName (nameBase name)))
          |]
  --
  ss <- mapM mkSimple (integralTypes ++ floatingTypes)
  ns <- mapM mkNewtype newtypes
  ts <- mapM mkTuple [2..16]
  -- vs <- sequence [ mkVecElt t n | t <- integralTypes ++ floatingTypes, n <- [2,3,4,8,16] ]
  return (concat ss ++ concat ns ++ ts)

