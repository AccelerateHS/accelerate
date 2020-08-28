{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Product
-- Copyright   : [2008..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Our representation of products are heterogenous snoc lists, which are typed
-- by type lists, where '()' and '(,)' are type-level nil and snoc,
-- respectively. The components may only be drawn from types that can be used as
-- array elements.
--

module Data.Array.Accelerate.Product (

  -- * Product types
  TupleIdx(..), IsProduct(..), ProdR(..),

) where

import GHC.Generics
import Data.Primitive.Types
import Language.Haskell.TH
import Language.Haskell.TH.Extra

import Data.Array.Accelerate.Type


-- | Type-safe projection indices for tuples.
--
-- NB: We index tuples by starting to count from the *right*!
--
data TupleIdx t e where
  ZeroTupIdx ::                 TupleIdx (t, s) s
  SuccTupIdx :: TupleIdx t e -> TupleIdx (t, s) e

-- | Product reification
--
data ProdR cst t where
  ProdRunit   :: ProdR cst ()
  ProdRsnoc   :: cst e => ProdR cst t -> ProdR cst (t,e)

-- | Conversion between surface product types and our product representation.
--
-- We parameterise our products by a constraint on their elements (the 'cst'
-- argument). Every element in the product must obey this constraint, but the
-- products themselves do not necessarily have to.
--
class IsProduct cst tup where
  type ProdRepr tup
  type ProdRepr tup = GProdRepr () (Rep tup)
  --
  fromProd :: tup -> ProdRepr tup
  toProd   :: ProdRepr tup -> tup
  prod     :: ProdR cst (ProdRepr tup)

  {-# INLINE fromProd #-}
  default fromProd
    :: (Generic tup, ProdRepr tup ~ GProdRepr () (Rep tup), GIsProduct cst (Rep tup))
    => tup
    -> ProdRepr tup
  fromProd = gfromProd @cst @(Rep tup) () . from

  {-# INLINE toProd #-}
  default toProd
    :: (Generic tup, ProdRepr tup ~ GProdRepr () (Rep tup), GIsProduct cst (Rep tup))
    => ProdRepr tup
    -> tup
  toProd = to . snd . gtoProd @cst @(Rep tup) @()

  {-# INLINE prod #-}
  default prod
    :: (ProdRepr tup ~ GProdRepr () (Rep tup), GIsProduct cst (Rep tup))
    => ProdR cst (ProdRepr tup)
  prod = gprod @cst @(Rep tup) ProdRunit


class GIsProduct cst f where
  type GProdRepr t f
  gfromProd :: t -> f a -> GProdRepr t f
  gtoProd   :: GProdRepr t f -> (t, f a)
  gprod     :: ProdR cst t -> ProdR cst (GProdRepr t f)

instance GIsProduct cst U1 where
  type GProdRepr t U1 = t
  gfromProd t U1 = t
  gtoProd   t    = (t, U1)
  gprod     t    = t

instance GIsProduct cst a => GIsProduct cst (M1 i c a) where
  type GProdRepr t (M1 i c a) = GProdRepr t a
  gfromProd t (M1 x) = gfromProd @cst t x
  gtoProd         x  = let (t, x1) = gtoProd @cst x in (t, M1 x1)
  gprod              = gprod @cst @a

instance cst a => GIsProduct cst (K1 i a) where
  type GProdRepr t (K1 i a) = (t, a)
  gfromProd t (K1 x) = (t, x)
  gtoProd     (t, x) = (t, K1 x)
  gprod     t        = ProdRsnoc t

instance (GIsProduct cst a, GIsProduct cst b) => GIsProduct cst (a :*: b) where
  type GProdRepr t (a :*: b) = GProdRepr (GProdRepr t a) b
  gfromProd t (a :*: b) = gfromProd @cst (gfromProd @cst t a) b
  gtoProd t =
    let (t1, b) = gtoProd @cst t
        (t2, a) = gtoProd @cst t1
    in
    (t2, a :*: b)
  gprod t = gprod @cst @b (gprod @cst @a t)

instance IsProduct cst () where
  type ProdRepr ()   = ()
  fromProd           = id
  toProd             = id
  prod               = ProdRunit

instance (Prim a, cst a) => IsProduct cst (V2 a) where
  type ProdRepr (V2 a) = (((), a), a)
  fromProd (V2 a b)    = (((), a), b)
  toProd (((), a), b)  = V2 a b
  prod                 = prod @cst @(a,a)

instance (Prim a, cst a) => IsProduct cst (V3 a) where
  type ProdRepr (V3 a)     = ((((), a), a), a)
  fromProd (V3 a b c)      = ((((), a), b), c)
  toProd ((((), a), b), c) = V3 a b c
  prod                     = prod @cst @(a,a,a)

instance (Prim a, cst a) => IsProduct cst (V4 a) where
  type ProdRepr (V4 a)          = (((((), a), a), a), a)
  fromProd (V4 a b c d)         = (((((), a), b), c), d)
  toProd (((((), a), b), c), d) = V4 a b c d
  prod                          = prod @cst @(a,a,a,a)

instance (Prim a, cst a) => IsProduct cst (V8 a) where
  type ProdRepr (V8 a)          = (((((((((), a), a), a), a), a), a), a), a)
  fromProd (V8 a b c d e f g h) = (((((((((), a), b), c), d), e), f), g), h)
  toProd (((((((((), a), b), c), d), e), f), g), h)
    = V8 a b c d e f g h
  prod
    = prod @cst @(a,a,a,a,a,a,a,a)

instance (Prim a, cst a) => IsProduct cst (V16 a) where
  type ProdRepr (V16 a) = (((((((((((((((((), a), a), a), a), a), a), a), a), a), a), a), a), a), a), a), a)
  fromProd (V16 a b c d e f g h i j k l m n o p)
    = (((((((((((((((((), a), b), c), d), e), f), g), h), i), j), k), l), m), n), o), p)
  toProd (((((((((((((((((), a), b), c), d), e), f), g), h), i), j), k), l), m), n), o), p)
    = V16 a b c d e f g h i j k l m n o p
  prod
    = prod @cst @(a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a)

$(runQ $ do
    let
        mkIsProduct :: Int -> Q [Dec]
        mkIsProduct n = do
          cst <- newName "cst"
          let
              xs    = [ mkName ('x' : show i) | i <- [0 .. n-1] ]
              ts    = map varT xs
              lhs   = tupT (map (varT cst `appT`) ts)
              flat  = tupT ts
              --
              prod' 0 = [| ProdRunit |]
              prod' i = [| ProdRsnoc $(prod' (i-1)) |]
          --
          [d| instance $lhs => IsProduct $(varT cst) $flat where
                type ProdRepr $flat = $(foldl (\s t -> [t| ($s, $t) |]) [t| () |] ts)
                fromProd $(tupP (map varP xs)) = $(foldl (\vs v -> [| ($vs, $(varE v)) |]) [|()|] xs)
                toProd $(foldl (\ps p -> tupP [ps, varP p]) (tupP []) xs) = $(tupE (map varE xs))
                prod = $(prod' n)
            |]

    concat <$> mapM mkIsProduct [2..16]
 )

