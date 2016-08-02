{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Data.Array.Accelerate.Array.Lifted
-- Copyright   : [2012..2013] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell, Robert Clifton-Everest
-- License     : BSD3
--
-- Maintainer  : Robert Clifton-Everest <robertce@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lifted array representation. Vector of arrays represented as segmented vectors.
--

module Data.Array.Accelerate.Array.Lifted (

  Nested(..), NestedTupleRepr, Segments, VectorisedForeign(..), isVectorised,

) where

import Prelude                                                  hiding ( concat )
import Data.Typeable

-- friends
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Array.Sugar                        hiding ( Segments )

-- Nested arrays
-- ----------------
--
-- We specify a special new type of surface tuple to represent the nested
-- version of members of the `Arrays' class. We do this in order to convince the
-- type checker that the nested arrays or tuples of arrays, are still members of
-- the 'Arrays' class.

newtype Nested a = Nested (Nested' a (ArrRepr a)) deriving Typeable

-- Segment descriptors. The segments descriptors of a nested array capture the
-- sizes of the subarrays. They are either regular or irregular, as indicated by
-- the flag.
--
-- For regular nested arrays, we track the total number of arrays as well as the
-- extent of each of the subarrays.
--
-- For irregular arrays, each segment of a lifted array is defined by its index
-- into its flattend representation as well as its shape. We split this up into
-- two vectors as opposed to a vector of pairs to aid in fusion.
--
-- In both cases we also keep track of the total size of the lifted array the
-- segment descriptors refer to. This aids in fusion.
--
type Segments sh = ( Scalar Bool -- Irregular?
                   , Scalar Int  -- Total size in scalar elements
                   , Scalar Int  -- If regular, number of subarrays
                   , Scalar sh   -- If regular, extent of subarrays
                   , Vector Int  -- If irregular, offsets
                   , Vector sh   -- If irregular, extents
                   )

type family Nested' a a' where
  Nested' ()           ()           = ((),Scalar Int)
  Nested' (Array sh e) (Array sh e) = (((),Segments sh), Vector e)
  Nested' a            (l,r)        = NestedTupleRepr (TupleRepr a)

type family NestedTupleRepr t where
  NestedTupleRepr ()     = ()
  NestedTupleRepr (b, a) = (NestedTupleRepr b, Nested a)

instance Arrays a => IsProduct Arrays (Nested a) where
  type ProdRepr (Nested a) = Nested' a (ArrRepr a)
  fromProd _ (Nested a) = a
  toProd   _               = Nested
  prod     _ _             =
    case flavour (undefined :: a) of
      ArraysFunit  -> ProdRsnoc ProdRunit
      ArraysFarray -> ProdRsnoc (ProdRsnoc ProdRunit)
      ArraysFtuple -> tup (prod arraysP (undefined :: a))
        where
          tup :: ProdR Arrays t -> ProdR Arrays (NestedTupleRepr t)
          tup ProdRunit      = ProdRunit
          tup (ProdRsnoc pr) = ProdRsnoc (tup pr)

type instance ArrRepr (Nested a) = ArrRepr (Nested' a (ArrRepr a))

instance Arrays a => Arrays (Nested a) where
  arrays _ = tup (prod arraysP (undefined :: Nested a))
    where
      tup :: forall t. ProdR Arrays t -> ArraysR (ArrRepr t)
      tup ProdRunit      = ArraysRunit
      tup (ProdRsnoc pr) = ArraysRpair (ArraysRpair ArraysRunit (tup pr)) ar
        where ar :: forall e l. t ~ (l,e) => ArraysR (ArrRepr e)
              ar = arrays (undefined :: e)
  flavour _ = case flavour (undefined :: a) of
    ArraysFunit  -> ArraysFtuple
    ArraysFarray -> ArraysFtuple
    ArraysFtuple ->
      case arrays (undefined :: Nested a) of
        ArraysRpair _ _ -> ArraysFtuple
        _               -> error "Unreachable"

  toArr a = Nested (tA (prod arraysP (undefined :: Nested a)) a)
    where
      tA :: ProdR Arrays t -> ArrRepr t -> t
      tA ProdRunit      ()          = ()
      tA (ProdRsnoc pr) (((),ar),a) = (tA pr ar, toArr a)

  fromArr (Nested a) = fA (prod arraysP (undefined :: Nested a)) a
    where
      fA :: ProdR Arrays t -> t -> ArrRepr t
      fA ProdRunit      ()    = ()
      fA (ProdRsnoc pr) (l,a) = (((),fA pr l), fromArr a)

arraysP :: Proxy Arrays
arraysP = Proxy

data VectorisedForeign a b = forall f.  Foreign f => VectorisedForeign (f (Nested a) (Nested b))

instance Foreign VectorisedForeign where
  strForeign (VectorisedForeign f) = strForeign f

isVectorised :: (Typeable as, Typeable bs, Foreign f) => f as bs -> Maybe (VectorisedForeign as bs)
isVectorised = cast
