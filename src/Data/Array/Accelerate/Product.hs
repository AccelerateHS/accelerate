{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
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
-- Our representation of products are heterogenous snoc lists, which are typed
-- by type lists, where '()' and '(,)' are type-level nil and snoc,
-- respectively. The components may only be drawn from types that can be used as
-- array elements.
--

module Data.Array.Accelerate.Product (

  -- * Product types
  TupleIdx(..), IsProduct(..), ProdR(..),

) where


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
  fromProd :: tup -> ProdRepr tup
  toProd   :: ProdRepr tup -> tup
  prod     :: ProdR cst (ProdRepr tup)

instance IsProduct cst () where
  type ProdRepr ()   = ()
  fromProd           = id
  toProd             = id
  prod               = ProdRunit

instance (cst a, cst b) => IsProduct cst (a, b) where
  type ProdRepr (a, b) = (((), a), b)
  fromProd (a, b)      = (((), a), b)
  toProd (((), a), b)  = (a, b)
  prod                 = ProdRsnoc $ ProdRsnoc ProdRunit

instance (cst a, cst b, cst c) => IsProduct cst (a, b, c) where
  type ProdRepr (a, b, c)  = (ProdRepr (a, b), c)
  fromProd (a, b, c)       = ((((), a), b), c)
  toProd ((((), a), b), c) = (a, b, c)
  prod                     = ProdRsnoc (prod @cst @(a,b))

instance (cst a, cst b, cst c, cst d) => IsProduct cst (a, b, c, d) where
  type ProdRepr (a, b, c, d)    = (ProdRepr (a, b, c), d)
  fromProd (a, b, c, d)         = (((((), a), b), c), d)
  toProd (((((), a), b), c), d) = (a, b, c, d)
  prod                          = ProdRsnoc (prod @cst @(a,b,c))

instance (cst a, cst b, cst c, cst d, cst e) => IsProduct cst (a, b, c, d, e) where
  type ProdRepr (a, b, c, d, e)      = (ProdRepr (a, b, c, d), e)
  fromProd (a, b, c, d, e)           = ((((((), a), b), c), d), e)
  toProd ((((((), a), b), c), d), e) = (a, b, c, d, e)
  prod                               = ProdRsnoc (prod @cst @(a,b,c,d))

instance (cst a, cst b, cst c, cst d, cst e, cst f) => IsProduct cst (a, b, c, d, e, f) where
  type ProdRepr (a, b, c, d, e, f)        = (ProdRepr (a, b, c, d, e), f)
  fromProd (a, b, c, d, e, f)             = (((((((), a), b), c), d), e), f)
  toProd (((((((), a), b), c), d), e), f) = (a, b, c, d, e, f)
  prod                                    = ProdRsnoc (prod @cst @(a,b,c,d,e))

instance (cst a, cst b, cst c, cst d, cst e, cst f, cst g)
  => IsProduct cst (a, b, c, d, e, f, g) where
  type ProdRepr (a, b, c, d, e, f, g)          = (ProdRepr (a, b, c, d, e, f), g)
  fromProd (a, b, c, d, e, f, g)               = ((((((((), a), b), c), d), e), f), g)
  toProd ((((((((), a), b), c), d), e), f), g) = (a, b, c, d, e, f, g)
  prod                                         = ProdRsnoc (prod @cst @(a,b,c,d,e,f))

instance (cst a, cst b, cst c, cst d, cst e, cst f, cst g, cst h)
  => IsProduct cst (a, b, c, d, e, f, g, h) where
  type ProdRepr (a, b, c, d, e, f, g, h)            = (ProdRepr (a, b, c, d, e, f, g), h)
  fromProd (a, b, c, d, e, f, g, h)                 = (((((((((), a), b), c), d), e), f), g), h)
  toProd (((((((((), a), b), c), d), e), f), g), h) = (a, b, c, d, e, f, g, h)
  prod                                              = ProdRsnoc (prod @cst @(a,b,c,d,e,f,g))

instance (cst a, cst b, cst c, cst d, cst e, cst f, cst g, cst h, cst i)
  => IsProduct cst (a, b, c, d, e, f, g, h, i) where
  type ProdRepr (a, b, c, d, e, f, g, h, i) = (ProdRepr (a, b, c, d, e, f, g, h), i)
  fromProd (a, b, c, d, e, f, g, h, i)
    = ((((((((((), a), b), c), d), e), f), g), h), i)
  toProd ((((((((((), a), b), c), d), e), f), g), h), i)
    = (a, b, c, d, e, f, g, h, i)
  prod
    = ProdRsnoc (prod @cst @(a,b,c,d,e,f,g,h))

instance (cst a, cst b, cst c, cst d, cst e, cst f, cst g, cst h, cst i, cst j)
  => IsProduct cst (a, b, c, d, e, f, g, h, i, j) where
  type ProdRepr (a, b, c, d, e, f, g, h, i, j) = (ProdRepr (a, b, c, d, e, f, g, h, i), j)
  fromProd (a, b, c, d, e, f, g, h, i, j)
    = (((((((((((), a), b), c), d), e), f), g), h), i), j)
  toProd (((((((((((), a), b), c), d), e), f), g), h), i), j)
    = (a, b, c, d, e, f, g, h, i, j)
  prod
    = ProdRsnoc (prod @cst @(a,b,c,d,e,f,g,h,i))

instance (cst a, cst b, cst c, cst d, cst e, cst f, cst g, cst h, cst i, cst j, cst k)
  => IsProduct cst (a, b, c, d, e, f, g, h, i, j, k) where
  type ProdRepr (a, b, c, d, e, f, g, h, i, j, k) = (ProdRepr (a, b, c, d, e, f, g, h, i, j), k)
  fromProd (a, b, c, d, e, f, g, h, i, j, k)
    = ((((((((((((), a), b), c), d), e), f), g), h), i), j), k)
  toProd ((((((((((((), a), b), c), d), e), f), g), h), i), j), k)
    = (a, b, c, d, e, f, g, h, i, j, k)
  prod
    = ProdRsnoc (prod @cst @(a,b,c,d,e,f,g,h,i,j))

instance (cst a, cst b, cst c, cst d, cst e, cst f, cst g, cst h, cst i, cst j, cst k, cst l)
  => IsProduct cst (a, b, c, d, e, f, g, h, i, j, k, l) where
  type ProdRepr (a, b, c, d, e, f, g, h, i, j, k, l) = (ProdRepr (a, b, c, d, e, f, g, h, i, j, k), l)
  fromProd (a, b, c, d, e, f, g, h, i, j, k, l)
    = (((((((((((((), a), b), c), d), e), f), g), h), i), j), k), l)
  toProd (((((((((((((), a), b), c), d), e), f), g), h), i), j), k), l)
    = (a, b, c, d, e, f, g, h, i, j, k, l)
  prod
    = ProdRsnoc (prod @cst @(a,b,c,d,e,f,g,h,i,j,k))

instance (cst a, cst b, cst c, cst d, cst e, cst f, cst g, cst h, cst i, cst j, cst k, cst l, cst m)
  => IsProduct cst (a, b, c, d, e, f, g, h, i, j, k, l, m) where
  type ProdRepr (a, b, c, d, e, f, g, h, i, j, k, l, m) = (ProdRepr (a, b, c, d, e, f, g, h, i, j, k, l), m)
  fromProd (a, b, c, d, e, f, g, h, i, j, k, l, m)
    = ((((((((((((((), a), b), c), d), e), f), g), h), i), j), k), l), m)
  toProd ((((((((((((((), a), b), c), d), e), f), g), h), i), j), k), l), m)
    = (a, b, c, d, e, f, g, h, i, j, k, l, m)
  prod
    = ProdRsnoc (prod @cst @(a,b,c,d,e,f,g,h,i,j,k,l))

instance (cst a, cst b, cst c, cst d, cst e, cst f, cst g, cst h, cst i, cst j, cst k, cst l, cst m, cst n)
  => IsProduct cst (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
  type ProdRepr (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = (ProdRepr (a, b, c, d, e, f, g, h, i, j, k, l, m), n)
  fromProd (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
    = (((((((((((((((), a), b), c), d), e), f), g), h), i), j), k), l), m), n)
  toProd (((((((((((((((), a), b), c), d), e), f), g), h), i), j), k), l), m), n)
    = (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
  prod
    = ProdRsnoc (prod @cst @(a,b,c,d,e,f,g,h,i,j,k,l,m))

instance (cst a, cst b, cst c, cst d, cst e, cst f, cst g, cst h, cst i, cst j, cst k, cst l, cst m, cst n, cst o)
  => IsProduct cst (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
  type ProdRepr (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = (ProdRepr (a, b, c, d, e, f, g, h, i, j, k, l, m, n), o)
  fromProd (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
    = ((((((((((((((((), a), b), c), d), e), f), g), h), i), j), k), l), m), n), o)
  toProd ((((((((((((((((), a), b), c), d), e), f), g), h), i), j), k), l), m), n), o)
    = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
  prod
    = ProdRsnoc (prod @cst @(a,b,c,d,e,f,g,h,i,j,k,l,m,n))

instance (cst a, cst b, cst c, cst d, cst e, cst f, cst g, cst h, cst i, cst j, cst k, cst l, cst m, cst n, cst o, cst p)
  => IsProduct cst (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) where
  type ProdRepr (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) = (ProdRepr (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o), p)
  fromProd (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
    = (((((((((((((((((), a), b), c), d), e), f), g), h), i), j), k), l), m), n), o), p)
  toProd (((((((((((((((((), a), b), c), d), e), f), g), h), i), j), k), l), m), n), o), p)
    = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
  prod
    = ProdRsnoc (prod @cst @(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o))

