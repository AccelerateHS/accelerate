{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Product
-- Copyright   : [2008..2017] Manuel M T Chakravarty, Gabriele Keller
--               [2009..2017] Trevor L. McDonell
--               [2013..2017] Robert Clifton-Everest
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Our representation of products are heterogenous snoc lists, which are typed by
-- type lists, where '()' and '(,)' are type-level nil and snoc, respectively.
-- The components may only be drawn from types that can be used as array
-- elements.
--

module Data.Array.Accelerate.Product (

  -- * Tuple representation
  TupleIdx(..), IsProduct(..), ProdR(..)

) where

import Data.Array.Accelerate.Type

import GHC.Generics


-- |Type-safe projection indices for tuples.
--
-- NB: We index tuples by starting to count from the *right*!
--
data TupleIdx t e where
  ZeroTupIdx ::                 TupleIdx (t, s) s
  SuccTupIdx :: TupleIdx t e -> TupleIdx (t, s) e

-- |Product reification
--
data ProdR cst t where
  ProdRunit   :: ProdR cst ()
  ProdRsnoc   :: cst e => ProdR cst t -> ProdR cst (t,e)

-- |Conversion between surface product types and our product representation.
--
-- We parameterise our products by a constraint on their elements (the 'cst' argument). Every element
-- in the product must obey this constraint, but the products themselves do necessarily not have to.
--
class IsProduct cst tup where
  type ProdRepr tup
  fromProd :: proxy cst -> tup -> ProdRepr tup
  toProd   :: proxy cst -> ProdRepr tup -> tup
  prod     :: proxy cst -> {- dummy -} tup -> ProdR cst (ProdRepr tup)

  type ProdRepr tup = GProdRepr () (Rep tup)

  default fromProd
    :: (Generic tup)
    => proxy cst -> tup -> ProdRepr tup
  fromProd = undefined

  default toProd
    :: (Generic tup)
    => proxy cst -> ProdRepr tup -> tup
  toProd = undefined

  default prod
    :: (Generic tup, ProdRepr tup ~ GProdRepr () (Rep tup), GIsProduct cst (Rep tup))
    => proxy cst -> {- dummy -} tup -> ProdR cst (ProdRepr tup)
  prod _ _ = gprod @cst @(Rep tup) ProdRunit


class GIsProduct cst (f :: * -> *) where
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
  fromProd _         = id
  toProd _           = id
  prod _ _           = ProdRunit

instance (cst a, cst b) => IsProduct cst (a, b) where
  type ProdRepr (a, b)   = (((), a), b)
  fromProd _ (a, b)      = (((), a), b)
  toProd _ (((), a), b)  = (a, b)
  prod _ _               = ProdRsnoc $ ProdRsnoc ProdRunit

instance (cst a, cst b, cst c) => IsProduct cst (a, b, c) where
  type ProdRepr (a, b, c)    = (ProdRepr (a, b), c)
  fromProd _ (a, b, c)       = ((((), a), b), c)
  toProd _ ((((), a), b), c) = (a, b, c)
  prod p _                   = ProdRsnoc (prod p (undefined :: (a,b)))

instance (cst a, cst b, cst c, cst d) => IsProduct cst (a, b, c, d) where
  type ProdRepr (a, b, c, d)      = (ProdRepr (a, b, c), d)
  fromProd _ (a, b, c, d)         = (((((), a), b), c), d)
  toProd _ (((((), a), b), c), d) = (a, b, c, d)
  prod p _                        = ProdRsnoc (prod p (undefined :: (a,b,c)))

instance (cst a, cst b, cst c, cst d, cst e) => IsProduct cst (a, b, c, d, e) where
  type ProdRepr (a, b, c, d, e)        = (ProdRepr (a, b, c, d), e)
  fromProd _ (a, b, c, d, e)           = ((((((), a), b), c), d), e)
  toProd _ ((((((), a), b), c), d), e) = (a, b, c, d, e)
  prod p _                             = ProdRsnoc (prod p (undefined :: (a,b,c,d)))

instance (cst a, cst b, cst c, cst d, cst e, cst f) => IsProduct cst (a, b, c, d, e, f) where
  type ProdRepr (a, b, c, d, e, f)          = (ProdRepr (a, b, c, d, e), f)
  fromProd _ (a, b, c, d, e, f)             = (((((((), a), b), c), d), e), f)
  toProd _ (((((((), a), b), c), d), e), f) = (a, b, c, d, e, f)
  prod p _                                  = ProdRsnoc (prod p (undefined :: (a,b,c,d,e)))

instance (cst a, cst b, cst c, cst d, cst e, cst f, cst g)
  => IsProduct cst (a, b, c, d, e, f, g) where
  type ProdRepr (a, b, c, d, e, f, g)            = (ProdRepr (a, b, c, d, e, f), g)
  fromProd _ (a, b, c, d, e, f, g)               = ((((((((), a), b), c), d), e), f), g)
  toProd _ ((((((((), a), b), c), d), e), f), g) = (a, b, c, d, e, f, g)
  prod p _                                       = ProdRsnoc (prod p (undefined :: (a,b,c,d,e,f)))

instance (cst a, cst b, cst c, cst d, cst e, cst f, cst g, cst h)
  => IsProduct cst (a, b, c, d, e, f, g, h) where
  type ProdRepr (a, b, c, d, e, f, g, h)              = (ProdRepr (a, b, c, d, e, f, g), h)
  fromProd _ (a, b, c, d, e, f, g, h)                 = (((((((((), a), b), c), d), e), f), g), h)
  toProd _ (((((((((), a), b), c), d), e), f), g), h) = (a, b, c, d, e, f, g, h)
  prod p _                                            = ProdRsnoc (prod p (undefined :: (a,b,c,d,e,f,g)))

instance (cst a, cst b, cst c, cst d, cst e, cst f, cst g, cst h, cst i)
  => IsProduct cst (a, b, c, d, e, f, g, h, i) where
  type ProdRepr (a, b, c, d, e, f, g, h, i) = (ProdRepr (a, b, c, d, e, f, g, h), i)
  fromProd _ (a, b, c, d, e, f, g, h, i)
    = ((((((((((), a), b), c), d), e), f), g), h), i)
  toProd _ ((((((((((), a), b), c), d), e), f), g), h), i)
    = (a, b, c, d, e, f, g, h, i)
  prod p _
    = ProdRsnoc (prod p (undefined :: (a,b,c,d,e,f,g,h)))

instance (cst a, cst b, cst c, cst d, cst e, cst f, cst g, cst h, cst i, cst j)
  => IsProduct cst (a, b, c, d, e, f, g, h, i, j) where
  type ProdRepr (a, b, c, d, e, f, g, h, i, j) = (ProdRepr (a, b, c, d, e, f, g, h, i), j)
  fromProd _ (a, b, c, d, e, f, g, h, i, j)
    = (((((((((((), a), b), c), d), e), f), g), h), i), j)
  toProd _ (((((((((((), a), b), c), d), e), f), g), h), i), j)
    = (a, b, c, d, e, f, g, h, i, j)
  prod p _
    = ProdRsnoc (prod p (undefined :: (a,b,c,d,e,f,g,h,i)))

instance (cst a, cst b, cst c, cst d, cst e, cst f, cst g, cst h, cst i, cst j, cst k)
  => IsProduct cst (a, b, c, d, e, f, g, h, i, j, k) where
  type ProdRepr (a, b, c, d, e, f, g, h, i, j, k) = (ProdRepr (a, b, c, d, e, f, g, h, i, j), k)
  fromProd _ (a, b, c, d, e, f, g, h, i, j, k)
    = ((((((((((((), a), b), c), d), e), f), g), h), i), j), k)
  toProd _ ((((((((((((), a), b), c), d), e), f), g), h), i), j), k)
    = (a, b, c, d, e, f, g, h, i, j, k)
  prod p _
    = ProdRsnoc (prod p (undefined :: (a,b,c,d,e,f,g,h,i,j)))

instance (cst a, cst b, cst c, cst d, cst e, cst f, cst g, cst h, cst i, cst j, cst k, cst l)
  => IsProduct cst (a, b, c, d, e, f, g, h, i, j, k, l) where
  type ProdRepr (a, b, c, d, e, f, g, h, i, j, k, l) = (ProdRepr (a, b, c, d, e, f, g, h, i, j, k), l)
  fromProd _ (a, b, c, d, e, f, g, h, i, j, k, l)
    = (((((((((((((), a), b), c), d), e), f), g), h), i), j), k), l)
  toProd _ (((((((((((((), a), b), c), d), e), f), g), h), i), j), k), l)
    = (a, b, c, d, e, f, g, h, i, j, k, l)
  prod p _
    = ProdRsnoc (prod p (undefined :: (a,b,c,d,e,f,g,h,i,j,k)))

instance (cst a, cst b, cst c, cst d, cst e, cst f, cst g, cst h, cst i, cst j, cst k, cst l, cst m)
  => IsProduct cst (a, b, c, d, e, f, g, h, i, j, k, l, m) where
  type ProdRepr (a, b, c, d, e, f, g, h, i, j, k, l, m) = (ProdRepr (a, b, c, d, e, f, g, h, i, j, k, l), m)
  fromProd _ (a, b, c, d, e, f, g, h, i, j, k, l, m)
    = ((((((((((((((), a), b), c), d), e), f), g), h), i), j), k), l), m)
  toProd _ ((((((((((((((), a), b), c), d), e), f), g), h), i), j), k), l), m)
    = (a, b, c, d, e, f, g, h, i, j, k, l, m)
  prod p _
    = ProdRsnoc (prod p (undefined :: (a,b,c,d,e,f,g,h,i,j,k,l)))

instance (cst a, cst b, cst c, cst d, cst e, cst f, cst g, cst h, cst i, cst j, cst k, cst l, cst m, cst n)
  => IsProduct cst (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
  type ProdRepr (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = (ProdRepr (a, b, c, d, e, f, g, h, i, j, k, l, m), n)
  fromProd _ (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
    = (((((((((((((((), a), b), c), d), e), f), g), h), i), j), k), l), m), n)
  toProd _ (((((((((((((((), a), b), c), d), e), f), g), h), i), j), k), l), m), n)
    = (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
  prod p _
    = ProdRsnoc (prod p (undefined :: (a,b,c,d,e,f,g,h,i,j,k,l,m)))

instance (cst a, cst b, cst c, cst d, cst e, cst f, cst g, cst h, cst i, cst j, cst k, cst l, cst m, cst n, cst o)
  => IsProduct cst (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
  type ProdRepr (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = (ProdRepr (a, b, c, d, e, f, g, h, i, j, k, l, m, n), o)
  fromProd _ (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
    = ((((((((((((((((), a), b), c), d), e), f), g), h), i), j), k), l), m), n), o)
  toProd _ ((((((((((((((((), a), b), c), d), e), f), g), h), i), j), k), l), m), n), o)
    = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
  prod p _
    = ProdRsnoc (prod p (undefined :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n)))

instance (cst a, cst b, cst c, cst d, cst e, cst f, cst g, cst h, cst i, cst j, cst k, cst l, cst m, cst n, cst o, cst p)
  => IsProduct cst (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) where
  type ProdRepr (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) = (ProdRepr (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o), p)
  fromProd _ (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
    = (((((((((((((((((), a), b), c), d), e), f), g), h), i), j), k), l), m), n), o), p)
  toProd _ (((((((((((((((((), a), b), c), d), e), f), g), h), i), j), k), l), m), n), o), p)
    = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
  prod p _
    = ProdRsnoc (prod p (undefined :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)))

instance cst a => IsProduct cst (V2 a) where
  type ProdRepr (V2 a)  = ProdRepr (a, a)
  fromProd cst (V2 a b) = fromProd cst (a, b)
  toProd cst p          = let (a, b) = toProd cst p in V2 a b
  prod cst _            = prod cst (undefined :: (a,a))

instance cst a => IsProduct cst (V3 a) where
  type ProdRepr (V3 a)    = ProdRepr (a, a, a)
  fromProd cst (V3 a b c) = fromProd cst (a, b, c)
  toProd cst p            = let (a, b, c) = toProd cst p in V3 a b c
  prod cst _              = prod cst (undefined :: (a,a,a))

instance cst a => IsProduct cst (V4 a) where
  type ProdRepr (V4 a)      = ProdRepr (a, a, a, a)
  fromProd cst (V4 a b c d) = fromProd cst (a, b, c, d)
  toProd cst p              = let (a, b, c, d) = toProd cst p in V4 a b c d
  prod cst _                = prod cst (undefined :: (a,a,a,a))

instance cst a => IsProduct cst (V8 a) where
  type ProdRepr (V8 a) = ProdRepr (a, a, a, a, a, a, a, a)
  fromProd cst (V8 a b c d e f g h)
    = fromProd cst (a, b, c, d, e, f, g, h)
  toProd cst p
    = let (a, b, c, d, e, f, g, h) = toProd cst p
      in  V8 a b c d e f g h
  prod cst _
    = prod cst (undefined :: (a,a,a,a,a,a,a,a))

instance cst a => IsProduct cst (V16 a) where
  type ProdRepr (V16 a) = ProdRepr (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  fromProd cst (V16 a b c d e f g h i j k l m n o p)
    = fromProd cst (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
  toProd cst x
    = let (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) = toProd cst x
      in  V16 a b c d e f g h i j k l m n o p
  prod cst _
    = prod cst (undefined :: (a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a))

