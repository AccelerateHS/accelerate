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
-- Flattened array representation
--

module Data.Array.Accelerate.Array.Lifted (

  Regular(..), RegularTupleRepr,
  Irregular(..), IrregularTupleRepr, Segments,

  VectorisedRegularForeign(..), isVectorisedRegular,

) where

import Prelude                                                  hiding ( concat )
import Data.Typeable

-- friends
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Array.Sugar                        hiding ( Segments )

-- Nested arrays
-- ----------------
--
-- We specify special new types of surface tuple to represent the sequence chunk
-- versions of members of the `Arrays' class. We do this in order to convince
-- the type checker that the nested arrays or tuples of arrays, are still
-- members of the 'Arrays' class.

-- Regular chunks. All subarray are of the same extent.
--
newtype Regular a = Regular (Regular' a (ArrRepr a)) deriving Typeable

-- Irregular chunks. Subarrays can vary in extent.
--
newtype Irregular a = Irregular (Irregular' a (ArrRepr a)) deriving Typeable

-- Segment descriptors. The segments descriptors of an irregular chunk capture
-- the offset and extent of each subarray. We split this up into two vectors as
-- opposed to a vector of pairs to aid in fusion.
--
-- We also keep track of the total size of the chunk (in terms of scalar
-- elements) the segment descriptors refer to. If we do not do this, we are
-- forced get the last offset and the last size and add them together. By
-- indexing the offsets and the extents in this way we could force them both to
-- be manifest, even though we may only care about one of their elements.
--
type Segments sh = ( Scalar Int  -- Total size in scalar elements
                   , Vector Int  -- Offsets
                   , Vector sh   -- Extents
                   )

-- For both flattened representations we have to distribute over tuples.
--
type family Irregular' a a' where
  Irregular' ()           ()           = ((),Scalar Int)
  Irregular' (Array sh e) (Array sh e) = (((),Segments sh), Vector e)
  Irregular' a            (l,r)        = IrregularTupleRepr (TupleRepr a)

type family Regular' a a' where
  Regular' ()           ()           = ((),Scalar Int)
  Regular' (Array sh e) (Array sh e) = ((),Array (sh:.Int) e)
  Regular' a            (l,r)        = RegularTupleRepr (TupleRepr a)

type family IrregularTupleRepr t where
  IrregularTupleRepr ()     = ()
  IrregularTupleRepr (b, a) = (IrregularTupleRepr b, Irregular a)

type family RegularTupleRepr t where
  RegularTupleRepr ()     = ()
  RegularTupleRepr (b, a) = (RegularTupleRepr b, Regular a)


-- Instances for both IsProduct and Arrays. By being careful in our
-- construction there are no additional constraints on 'a' in order to produce
-- an Arrays instance for 'Regular a' and 'Irregular a'.
--
instance Arrays a => IsProduct Arrays (Regular a) where
  type ProdRepr (Regular a) = Regular' a (ArrRepr a)
  fromProd _ (Regular a) = a
  toProd   _               = Regular
  prod     _ _             =
    case flavour (undefined :: a) of
      ArraysFunit  -> ProdRsnoc ProdRunit
      ArraysFarray -> ProdRsnoc ProdRunit
      ArraysFtuple -> tup (prod arraysP (undefined :: a))
        where
          tup :: ProdR Arrays t -> ProdR Arrays (RegularTupleRepr t)
          tup ProdRunit      = ProdRunit
          tup (ProdRsnoc pr) = ProdRsnoc (tup pr)

type instance ArrRepr (Regular a) = ArrRepr (Regular' a (ArrRepr a))

instance Arrays a => Arrays (Regular a) where
  arrays _ = tup (prod arraysP (undefined :: Regular a))
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
      case arrays (undefined :: Regular a) of
        ArraysRpair _ _ -> ArraysFtuple
        _               -> error "Unreachable"

  toArr a = Regular (tA (prod arraysP (undefined :: Regular a)) a)
    where
      tA :: ProdR Arrays t -> ArrRepr t -> t
      tA ProdRunit      ()          = ()
      tA (ProdRsnoc pr) (((),ar),a) = (tA pr ar, toArr a)

  fromArr (Regular a) = fA (prod arraysP (undefined :: Regular a)) a
    where
      fA :: ProdR Arrays t -> t -> ArrRepr t
      fA ProdRunit      ()    = ()
      fA (ProdRsnoc pr) (l,a) = (((),fA pr l), fromArr a)

instance Arrays a => IsProduct Arrays (Irregular a) where
  type ProdRepr (Irregular a) = Irregular' a (ArrRepr a)
  fromProd _ (Irregular a) = a
  toProd   _               = Irregular
  prod     _ _             =
    case flavour (undefined :: a) of
      ArraysFunit  -> ProdRsnoc ProdRunit
      ArraysFarray -> ProdRsnoc (ProdRsnoc ProdRunit)
      ArraysFtuple -> tup (prod arraysP (undefined :: a))
        where
          tup :: ProdR Arrays t -> ProdR Arrays (IrregularTupleRepr t)
          tup ProdRunit      = ProdRunit
          tup (ProdRsnoc pr) = ProdRsnoc (tup pr)

type instance ArrRepr (Irregular a) = ArrRepr (Irregular' a (ArrRepr a))

instance Arrays a => Arrays (Irregular a) where
  arrays _ = tup (prod arraysP (undefined :: Irregular a))
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
      case arrays (undefined :: Irregular a) of
        ArraysRpair _ _ -> ArraysFtuple
        _               -> error "Unreachable"

  toArr a = Irregular (tA (prod arraysP (undefined :: Irregular a)) a)
    where
      tA :: ProdR Arrays t -> ArrRepr t -> t
      tA ProdRunit      ()          = ()
      tA (ProdRsnoc pr) (((),ar),a) = (tA pr ar, toArr a)

  fromArr (Irregular a) = fA (prod arraysP (undefined :: Irregular a)) a
    where
      fA :: ProdR Arrays t -> t -> ArrRepr t
      fA ProdRunit      ()    = ()
      fA (ProdRsnoc pr) (l,a) = (((),fA pr l), fromArr a)

arraysP :: Proxy Arrays
arraysP = Proxy

data VectorisedRegularForeign asm where
  VectorisedRegularForeign :: Foreign f
                           => f (Regular a -> Regular b)
                           -> VectorisedRegularForeign (a -> b)
  deriving Typeable

instance Foreign VectorisedRegularForeign where
  strForeign (VectorisedRegularForeign f) = strForeign f

isVectorisedRegular :: (Typeable asm, Foreign f)
                    => f asm
                    -> Maybe (VectorisedRegularForeign asm)
isVectorisedRegular = cast
