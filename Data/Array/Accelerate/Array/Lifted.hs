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

  Irregular, IrregularTupleRepr, Segments

) where

import Prelude                                                  hiding ( concat )
import Data.Typeable

-- friends
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Array.Sugar                        hiding ( Segments )

-- Lifted arrays
-- ----------------
--
-- We specify a special new type of surface tuple to represent the lifted version of members of the
-- `Arrays' class. We do this in order to convince the type checker that the lifted arrays or tuples
-- of arrays, are still members of the 'Arrays' class.

newtype Irregular a = Irregular (Irregular' a (ArrRepr a)) deriving Typeable

-- Segment descriptors. Each segment of a lifted array is defined by its index
-- into its flattend representation as well as its shape. We also keep track of
-- the total size of the lifted array the segment descriptors refer to, in order
-- to aid in fusion.
--
type Segments sh = (Scalar Int, Vector Int, Vector sh)

type family Irregular' a a' where
  Irregular' ()           ()           = ((),Scalar Int)
  Irregular' (Array sh e) (Array sh e) = (((),Segments sh), Vector e)
  Irregular' a            (l,r)        = IrregularTupleRepr (TupleRepr a)

type family IrregularTupleRepr t where
  IrregularTupleRepr ()     = ()
  IrregularTupleRepr (b, a) = (IrregularTupleRepr b, Irregular a)

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
