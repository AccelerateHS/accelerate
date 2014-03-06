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
-- Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
--               [2009..2012] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
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
  TupleIdx(..), IsConstrainedTuple(..), TupleR(..), Base

) where

import GHC.Prim           ( Constraint )

-- |Type-safe projection indices for tuples.
--
-- NB: We index tuples by starting to count from the *right*!
--
data TupleIdx t e where
  ZeroTupIdx ::                 TupleIdx (t, s) s
  SuccTupIdx :: TupleIdx t e -> TupleIdx (t, s) e

-- |Tuple reification
--
data TupleR cst t where
  TupleRunit   :: TupleR cst ()
  TupleRsnoc   :: cst e => TupleR cst t -> TupleR cst (t,e)

-- |Conversion between surface n-tuples and our tuple representation.
--
-- We parameterise our tuples by a constraint on their elements (the 'cst' argument). Every element
-- in the tuple must obey this constraint, but the tuples themselves do not have to.
--
-- If one-tuples, tuples with only a single component, are needed then an instance of the `Base`
-- type family can be provided for that constraint. The `Base` constraint is a sub-constraint that is
-- undivisable -- i.e. it has no components that can be extracted out of it.
class IsConstrainedTuple cst tup where
  type TupleRepr tup
  fromTuple :: tup -> TupleRepr tup
  toTuple   :: TupleRepr tup -> tup
  tuple     :: {- dummy -} tup -> TupleR cst (TupleRepr tup)

instance IsConstrainedTuple cst () where
  type TupleRepr () = ()
  fromTuple         = id
  toTuple           = id
  tuple _           = TupleRunit

instance (cst a, cst b) => IsConstrainedTuple cst (a, b) where
  type TupleRepr (a, b) = (((), a), b)
  fromTuple (x, y)      = (((), x), y)
  toTuple (((), x), y)  = (x, y)
  tuple _               = TupleRsnoc $ TupleRsnoc TupleRunit

instance (cst a, cst b, cst c) => IsConstrainedTuple cst (a, b, c) where
  type TupleRepr (a, b, c)  = (TupleRepr (a, b), c)
  fromTuple (x, y, z)       = ((((), x), y), z)
  toTuple ((((), x), y), z) = (x, y, z)
  tuple _                   = TupleRsnoc (tuple (undefined :: (a,b)))

instance (cst a, cst b, cst c, cst d) => IsConstrainedTuple cst (a, b, c, d) where
  type TupleRepr (a, b, c, d)    = (TupleRepr (a, b, c), d)
  fromTuple (x, y, z, v)         = (((((), x), y), z), v)
  toTuple (((((), x), y), z), v) = (x, y, z, v)
  tuple _                        = TupleRsnoc (tuple (undefined :: (a,b,c)))

instance (cst a, cst b, cst c, cst d, cst e) => IsConstrainedTuple cst (a, b, c, d, e) where
  type TupleRepr (a, b, c, d, e)      = (TupleRepr (a, b, c, d), e)
  fromTuple (x, y, z, v, w)           = ((((((), x), y), z), v), w)
  toTuple ((((((), x), y), z), v), w) = (x, y, z, v, w)
  tuple _                             = TupleRsnoc (tuple (undefined :: (a,b,c,d)))

instance (cst a, cst b, cst c, cst d, cst e, cst f) => IsConstrainedTuple cst (a, b, c, d, e, f) where
  type TupleRepr (a, b, c, d, e, f)        = (TupleRepr (a, b, c, d, e), f)
  fromTuple (x, y, z, v, w, r)             = (((((((), x), y), z), v), w), r)
  toTuple (((((((), x), y), z), v), w), r) = (x, y, z, v, w, r)
  tuple _                                  = TupleRsnoc (tuple (undefined :: (a,b,c,d,e)))

instance (cst a, cst b, cst c, cst d, cst e, cst f, cst g)
  => IsConstrainedTuple cst (a, b, c, d, e, f, g) where
  type TupleRepr (a, b, c, d, e, f, g)          = (TupleRepr (a, b, c, d, e, f), g)
  fromTuple (x, y, z, v, w, r, s)               = ((((((((), x), y), z), v), w), r), s)
  toTuple ((((((((), x), y), z), v), w), r), s) = (x, y, z, v, w, r, s)
  tuple _                                       = TupleRsnoc (tuple (undefined :: (a,b,c,d,e,f)))

instance (cst a, cst b, cst c, cst d, cst e, cst f, cst g, cst h)
  => IsConstrainedTuple cst (a, b, c, d, e, f, g, h) where
  type TupleRepr (a, b, c, d, e, f, g, h)            = (TupleRepr (a, b, c, d, e, f, g), h)
  fromTuple (x, y, z, v, w, r, s, t)                 = (((((((((), x), y), z), v), w), r), s), t)
  toTuple (((((((((), x), y), z), v), w), r), s), t) = (x, y, z, v, w, r, s, t)
  tuple _                                            = TupleRsnoc (tuple (undefined :: (a,b,c,d,e,f,g)))

instance (cst a, cst b, cst c, cst d, cst e, cst f, cst g, cst h, cst i)
  => IsConstrainedTuple cst (a, b, c, d, e, f, g, h, i) where
  type TupleRepr (a, b, c, d, e, f, g, h, i) = (TupleRepr (a, b, c, d, e, f, g, h), i)
  fromTuple (x, y, z, v, w, r, s, t, u)
    = ((((((((((), x), y), z), v), w), r), s), t), u)
  toTuple ((((((((((), x), y), z), v), w), r), s), t), u)
    = (x, y, z, v, w, r, s, t, u)
  tuple _
    = TupleRsnoc (tuple (undefined :: (a,b,c,d,e,f,g,h)))

type family Base (cst :: * -> Constraint) t :: Constraint

