{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
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
  Tuple(..), Atuple(..), TupleIdx(..), IsTuple(..)

) where

-- friends
import Data.Array.Accelerate.Array.Sugar


-- Tuple representation
-- --------------------

-- |We represent tuples as heterogenous lists, typed by a type list.
--
data Tuple c t where
  NilTup  ::                              Tuple c ()
  SnocTup :: Elt t => Tuple c s -> c t -> Tuple c (s, t)

-- TLM: It is irritating that we need a separate data type for tuples of scalars
--   vs. arrays, purely to carry the class constraint.
--
-- | Tuples of Arrays.  Note that this carries the `Arrays` class
--   constraint rather than `Elt` in the case of tuples of scalars.
data Atuple c t where
  NilAtup  ::                                  Atuple c ()
  SnocAtup :: Arrays a => Atuple c s -> c a -> Atuple c (s, a)

-- |Type-safe projection indices for tuples.
--
-- NB: We index tuples by starting to count from the *right*!
--
data TupleIdx t e where
  ZeroTupIdx ::                 TupleIdx (t, s) s
  SuccTupIdx :: TupleIdx t e -> TupleIdx (t, s) e

-- |Conversion between surface n-tuples and our tuple representation.
--
class IsTuple tup where
  type TupleRepr tup
  fromTuple :: tup -> TupleRepr tup
  toTuple   :: TupleRepr tup -> tup

instance IsTuple () where
  type TupleRepr () = ()
  fromTuple         = id
  toTuple           = id

instance IsTuple (a, b) where
  type TupleRepr (a, b) = (((), a), b)
  fromTuple (x, y)      = (((), x), y)
  toTuple (((), x), y)  = (x, y)

instance IsTuple (a, b, c) where
  type TupleRepr (a, b, c)  = (TupleRepr (a, b), c)
  fromTuple (x, y, z)       = ((((), x), y), z)
  toTuple ((((), x), y), z) = (x, y, z)

instance IsTuple (a, b, c, d) where
  type TupleRepr (a, b, c, d)    = (TupleRepr (a, b, c), d)
  fromTuple (x, y, z, v)         = (((((), x), y), z), v)
  toTuple (((((), x), y), z), v) = (x, y, z, v)

instance IsTuple (a, b, c, d, e) where
  type TupleRepr (a, b, c, d, e)      = (TupleRepr (a, b, c, d), e)
  fromTuple (x, y, z, v, w)           = ((((((), x), y), z), v), w)
  toTuple ((((((), x), y), z), v), w) = (x, y, z, v, w)

instance IsTuple (a, b, c, d, e, f) where
  type TupleRepr (a, b, c, d, e, f)        = (TupleRepr (a, b, c, d, e), f)
  fromTuple (x, y, z, v, w, r)             = (((((((), x), y), z), v), w), r)
  toTuple (((((((), x), y), z), v), w), r) = (x, y, z, v, w, r)

instance IsTuple (a, b, c, d, e, f, g) where
  type TupleRepr (a, b, c, d, e, f, g)          = (TupleRepr (a, b, c, d, e, f), g)
  fromTuple (x, y, z, v, w, r, s)               = ((((((((), x), y), z), v), w), r), s)
  toTuple ((((((((), x), y), z), v), w), r), s) = (x, y, z, v, w, r, s)

instance IsTuple (a, b, c, d, e, f, g, h) where
  type TupleRepr (a, b, c, d, e, f, g, h)            = (TupleRepr (a, b, c, d, e, f, g), h)
  fromTuple (x, y, z, v, w, r, s, t)                 = (((((((((), x), y), z), v), w), r), s), t)
  toTuple (((((((((), x), y), z), v), w), r), s), t) = (x, y, z, v, w, r, s, t)

instance IsTuple (a, b, c, d, e, f, g, h, i) where
  type TupleRepr (a, b, c, d, e, f, g, h, i) = (TupleRepr (a, b, c, d, e, f, g, h), i)
  fromTuple (x, y, z, v, w, r, s, t, u)
    = ((((((((((), x), y), z), v), w), r), s), t), u)
  toTuple ((((((((((), x), y), z), v), w), r), s), t), u)
    = (x, y, z, v, w, r, s, t, u)

