{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Tuple
-- Copyright   : [2008..2014] Manuel M T Chakravarty, Gabriele Keller
--               [2008..2009] Sean Lee
--               [2009..2014] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Our representation of tuples are heterogenous snoc lists, which are typed by
-- type lists, where '()' and '(,)' are type-level nil and snoc, respectively.
-- The components may only be drawn from types that can be used as array
-- elements.
--

module Data.Array.Accelerate.Tuple (

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
-- in the tuple must obey this constraint, but the tuples themselves do necessarily not have to.
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
  fromProd _ (x, y)      = (((), x), y)
  toProd _ (((), x), y)  = (x, y)
  prod _ _               = ProdRsnoc $ ProdRsnoc ProdRunit

instance (cst a, cst b, cst c) => IsProduct cst (a, b, c) where
  type ProdRepr (a, b, c)    = (ProdRepr (a, b), c)
  fromProd _ (x, y, z)       = ((((), x), y), z)
  toProd _ ((((), x), y), z) = (x, y, z)
  prod p _                   = ProdRsnoc (prod p (undefined :: (a,b)))

instance (cst a, cst b, cst c, cst d) => IsProduct cst (a, b, c, d) where
  type ProdRepr (a, b, c, d)      = (ProdRepr (a, b, c), d)
  fromProd _ (x, y, z, v)         = (((((), x), y), z), v)
  toProd _ (((((), x), y), z), v) = (x, y, z, v)
  prod p _                        = ProdRsnoc (prod p (undefined :: (a,b,c)))

instance (cst a, cst b, cst c, cst d, cst e) => IsProduct cst (a, b, c, d, e) where
  type ProdRepr (a, b, c, d, e)        = (ProdRepr (a, b, c, d), e)
  fromProd _ (x, y, z, v, w)           = ((((((), x), y), z), v), w)
  toProd _ ((((((), x), y), z), v), w) = (x, y, z, v, w)
  prod p _                             = ProdRsnoc (prod p (undefined :: (a,b,c,d)))

instance (cst a, cst b, cst c, cst d, cst e, cst f) => IsProduct cst (a, b, c, d, e, f) where
  type ProdRepr (a, b, c, d, e, f)          = (ProdRepr (a, b, c, d, e), f)
  fromProd _ (x, y, z, v, w, r)             = (((((((), x), y), z), v), w), r)
  toProd _ (((((((), x), y), z), v), w), r) = (x, y, z, v, w, r)
  prod p _                                  = ProdRsnoc (prod p (undefined :: (a,b,c,d,e)))

instance (cst a, cst b, cst c, cst d, cst e, cst f, cst g)
  => IsProduct cst (a, b, c, d, e, f, g) where
  type ProdRepr (a, b, c, d, e, f, g)            = (ProdRepr (a, b, c, d, e, f), g)
  fromProd _ (x, y, z, v, w, r, s)               = ((((((((), x), y), z), v), w), r), s)
  toProd _ ((((((((), x), y), z), v), w), r), s) = (x, y, z, v, w, r, s)
  prod p _                                       = ProdRsnoc (prod p (undefined :: (a,b,c,d,e,f)))

instance (cst a, cst b, cst c, cst d, cst e, cst f, cst g, cst h)
  => IsProduct cst (a, b, c, d, e, f, g, h) where
  type ProdRepr (a, b, c, d, e, f, g, h)              = (ProdRepr (a, b, c, d, e, f, g), h)
  fromProd _ (x, y, z, v, w, r, s, t)                 = (((((((((), x), y), z), v), w), r), s), t)
  toProd _ (((((((((), x), y), z), v), w), r), s), t) = (x, y, z, v, w, r, s, t)
  prod p _                                            = ProdRsnoc (prod p (undefined :: (a,b,c,d,e,f,g)))

instance (cst a, cst b, cst c, cst d, cst e, cst f, cst g, cst h, cst i)
  => IsProduct cst (a, b, c, d, e, f, g, h, i) where
  type ProdRepr (a, b, c, d, e, f, g, h, i) = (ProdRepr (a, b, c, d, e, f, g, h), i)
  fromProd _ (x, y, z, v, w, r, s, t, u)
    = ((((((((((), x), y), z), v), w), r), s), t), u)
  toProd _ ((((((((((), x), y), z), v), w), r), s), t), u)
    = (x, y, z, v, w, r, s, t, u)
  prod p _
    = ProdRsnoc (prod p (undefined :: (a,b,c,d,e,f,g,h)))

