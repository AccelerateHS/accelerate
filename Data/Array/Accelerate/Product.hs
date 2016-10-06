{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Product
-- Copyright   : [2008..2014] Manuel M T Chakravarty, Gabriele Keller
--               [2008..2009] Sean Lee
--               [2009..2014] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
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

