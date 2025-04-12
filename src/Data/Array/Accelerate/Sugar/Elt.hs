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
-- Copyright   : [2008..2020] The Accelerate Team
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

import Control.Applicative                                          ( (<|>) )
import qualified Control.Monad.State.Lazy                           as LState
import Control.Monad.Fix                                            ( mfix )
import Data.Char
import Data.Kind
import Data.Maybe                                                   ( fromMaybe )
import Data.Proxy
import Language.Haskell.TH.Extra                                    hiding ( Type )

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
-- See the function 'Data.Array.Accelerate.match' for details on how to use
-- sum types in embedded code.
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


-- | Given:
--
-- > data Test = A Int | B Int Float | C Float | D Double deriving (Show, Generic, Elt)
--
-- the call `geltR @(Rep Test) t` produces:
--
-- > TupRsingle Word8 :* (t :* tInt :* tInt :* tFloat :* tFloat :* tDouble)
--
-- where we abbreviate:
--
-- * TupRpair as (:*), infixl (i.e. "a :* b :* c" means "(a :* b) :* c")
-- * TupRsingle Int as tInt
-- * TupRsingle Float as tFloat
--
-- The first Word8 is the sum type tag.
--
-- This tag is not generated if there is only one constructor, so for example, given:
--
-- > data Test2 = T Int Float deriving (Show, Generic, Elt)
--
-- the call `geltR @Test2 t` produces: (using the same abbreviations)
--
-- > t :* tInt :* tFloat
--
-- To make sure the tag gets included conditionally, but only once if there are
-- many levels of (:+:) in the Generics representation -- and to furthermore
-- avoid needing a third type class -- we do a bit of a strange dance.
--
-- * The Elt default methods invoke GElt at the Rep of the data type. This may
--   be a (:+:) if it has more than 1 constructor, and something else
--   otherwise.
-- * GElt generates the tag in its instance for (:+:), and calls out to GSumElt
--   to actually handle the sum type constructors. Whenever GSumElt reaches a
--   non-(:+:), it calls back into GElt, which, from now on, will never
--   encounter (:+:) any more.
-- * GElt then handles the products (i.e. the data type constructor fields) and
--   finishes.
--
-- So to understand the full, precise control flow, one should start reading in
-- Elt, proceed with the GElt class and its (:+:) instance, then the full
-- definition of GSumElt, and finally the remaining instances of GElt.
class GElt f where
  -- | The @t@ variable is an additional uninterpreted type that's paired up
  -- all the way at the left of the produced (left-associated) product. The
  -- functions here are written in "CPS-style", in order to produce a long list
  -- instead of a tree structure.
  type GEltR t f
  geltR    :: TypeR t -> TypeR (GEltR t f)
  -- | This expands all sum types recursively; see the doc comment on 'TagR'.
  gtagsR   :: TagR t -> [TagR (GEltR t f)]
  gfromElt :: t -> f a -> GEltR t f
  gtoElt   :: GEltR t f -> (t, f a)
  --
  gundef   :: t -> GEltR t f
  guntag   :: TagR t -> TagR (GEltR t f)  -- ^ generate TagRundef for all leaves

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
  gtagsR t     = uncurry TagRtag <$> LState.evalState (gsumTagsR @(a :+: b) t) 0
  gfromElt t x = LState.evalState (gsumFromElt t x) 0
  gtoElt (k,x) = let (t, x') = LState.evalState (gsumToElt k x) 0
                 in (t, fromMaybe (error err) x')
    where err = "Elt: no sum type tag matched (k=" ++ show k ++ ")"
  gundef t     = (0xff, gsumUndef @(a :+: b) t)
  guntag t     = TagRpair (TagRundef scalarType) (gsumUntag @(a :+: b) t)


class GSumElt f where
  type GSumEltR t f
  gsumEltR     :: TypeR t -> TypeR (GSumEltR t f)
  -- | The state monad here is a /lazy/ state monad. We need this for 'gsumToElt' of '(:+:)'.
  gsumTagsR    :: TagR t -> LState.State TAG [(TAG, TagR (GSumEltR t f))]
  gsumFromElt  :: t -> f a -> LState.State TAG (TAG, GSumEltR t f)
  gsumCountTags :: Proxy f -> LState.State TAG ()
  gsumToElt    :: TAG -> GSumEltR t f -> LState.State TAG (t, Maybe (f a))
  gsumUndef    :: t -> GSumEltR t f
  gsumUntag    :: TagR t -> TagR (GSumEltR t f)

genTag :: LState.State TAG TAG
genTag = LState.state (\s -> (s, s + 1))

instance GSumElt U1 where
  type GSumEltR t U1 = t
  gsumEltR t         = t
  gsumTagsR t        = do n <- genTag; pure [(n, t)]
  gsumFromElt t U1   = do n <- genTag; pure (n, t)
  gsumCountTags _    = () <$ genTag
  gsumToElt k t      = do n <- genTag
                          pure (t, if n == k then Just U1 else Nothing)
  gsumUndef t        = t
  gsumUntag t        = t

instance GSumElt a => GSumElt (M1 i c a) where
  type GSumEltR t (M1 i c a) = GSumEltR t a
  gsumEltR               = gsumEltR @a
  gsumTagsR              = gsumTagsR @a
  gsumFromElt t (M1 x)   = gsumFromElt t x
  gsumCountTags _        = gsumCountTags (Proxy @a)
  gsumToElt k x          = (\(t, x') -> (t, fmap M1 x')) <$> gsumToElt k x
  gsumUndef              = gsumUndef @a
  gsumUntag              = gsumUntag @a

instance Elt a => GSumElt (K1 i a) where
  type GSumEltR t (K1 i a) = (t, EltR a)
  gsumEltR t             = TupRpair t (eltR @a)
  gsumTagsR t            = do n <- genTag; pure ((n,) . TagRpair t <$> tagsR @a)
  gsumFromElt t (K1 x)   = do n <- genTag; pure (n, (t, fromElt x))
  gsumCountTags _        = () <$ genTag
  gsumToElt k ~(t, x)    = do n <- genTag
                              pure (t, if n == k then Just (K1 (toElt x)) else Nothing)
  gsumUndef t            = (t, undefElt (eltR @a))
  gsumUntag t            = TagRpair t (untag (eltR @a))

instance (GElt a, GElt b) => GSumElt (a :*: b) where
  type GSumEltR t (a :*: b) = GEltR t (a :*: b)
  gsumEltR                  = geltR @(a :*: b)
  gsumTagsR t               = do n <- genTag; pure ((n,) <$> gtagsR @(a :*: b) t)
  gsumFromElt t (a :*: b)   = do n <- genTag; pure (n, gfromElt t (a :*: b))
  gsumCountTags _           = () <$ genTag
  gsumToElt k t0            = do n <- genTag
                                 -- Note that gtoElt produces x lazily, so this
                                 -- does not read any spurious undefs.
                                 let (t, x) = gtoElt t0
                                 pure (t, if n == k then Just x else Nothing)
  gsumUndef       = gundef @(a :*: b)
  gsumUntag       = guntag @(a :*: b)

instance (GSumElt a, GSumElt b) => GSumElt (a :+: b) where
  type GSumEltR t (a :+: b) = GSumEltR (GSumEltR t a) b
  gsumEltR = gsumEltR @b . gsumEltR @a

  gsumTagsR t = do
    a <- gsumTagsR @a t
    -- join b (filled with undefs) to the TagR's for 'a':
    let a' = map (\(tag, tagrA) -> (tag, gsumUntag @b tagrA)) a
    b <- gsumTagsR @b (gsumUntag @a t)
    pure (a' ++ b)

  gsumFromElt t (L1 a) = do
    (n, reprA) <- gsumFromElt t a
    pure (n, gsumUndef @b reprA)  -- join undef-filled b to this rep of A
  gsumFromElt t (R1 b) = do
    gsumCountTags (Proxy @a)  -- skip the tags in the left alternatives
    gsumFromElt (gsumUndef @a t) b

  gsumCountTags _ = gsumCountTags (Proxy @a) >> gsumCountTags (Proxy @b)

  gsumToElt k t0 = do
    -- The starting tag for the traversal of 'b' depends on the number of tags
    -- generated when traversing 'a'. However, in t0, the data for 'b' is
    -- on the outside, meaning that we can only get the initial 't' for the 'a'
    -- traversal by traversing 'b' first. We solve this circular dependency by
    -- observing that gsumToElt can count the tags before it needs its 't'
    -- argument, so we can tie a knot.
    -- We could also escape the State monad here with evalState, and explicitly
    -- gsumCountTags first, then traverse 'b', and finally traverse 'a'. But
    -- that would be ugly.
    (_t1, t2, mres1, mres2) <- mfix $ \ ~(t1, _t2, _mres1, _mres2) ->do
      (t2', mres2) <- gsumToElt @a k t1
      (t1', mres1) <- gsumToElt @b k t0
      pure (t1', t2', mres1, mres2)
    pure (t2, (R1 <$> mres1) <|> (L1 <$> mres2))

  gsumUndef t = gsumUndef @b (gsumUndef @a t)
  gsumUntag t = gsumUntag @b (gsumUntag @a t)


untag :: TypeR t -> TagR t
untag TupRunit         = TagRunit
untag (TupRsingle t)   = TagRundef t
untag (TupRpair ta tb) = TagRpair (untag ta) (untag tb)


-- Note: [Deriving Elt]
--
-- We can't use the cunning generalised newtype deriving mechanism, because
-- the generated 'eltR' function does not type check. For example, it will
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
instance Elt a => Elt (Maybe a)
instance (Elt a, Elt b) => Elt (Either a b)

instance Elt Char where
  type EltR Char = Word32
  eltR    = TupRsingle scalarType
  tagsR   = [TagRsingle scalarType]
  toElt   = chr . fromIntegral
  fromElt = fromIntegral . ord

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

