{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Data.Monoid
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Monoid instances for Accelerate
--

module Data.Array.Accelerate.Data.Monoid (

  Monoid(..), (<>),

  Sum(..),
  Product(..),

) where

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Type                                   as A
import Data.Array.Accelerate.Smart                                  as A ( Exp(..), PreExp(..) )
import Data.Array.Accelerate.Product                                as A
import Data.Array.Accelerate.Array.Sugar                            as A

import Data.Function
import Data.Monoid                                                  hiding ( mconcat )
import qualified Prelude                                            as P


-- Sum: Monoid under addition
-- --------------------------

type instance EltRepr (Sum a) = ((), EltRepr a)

instance Elt a => Elt (Sum a) where
  eltType _       = PairTuple UnitTuple (eltType (undefined::a))
  toElt ((),x)    = Sum (toElt x)
  fromElt (Sum x) = ((), fromElt x)
  eltFlavour _    = EltTuple

instance Elt a => IsProduct Elt (Sum a) where
  type ProdRepr (Sum a) = ((), a)
  toProd _ ((),a)    = Sum a
  fromProd _ (Sum a) = ((),a)
  prod _ _           = ProdRsnoc ProdRunit

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Sum a) where
  type Plain (Sum a) = Sum (Plain a)
  lift (Sum a)       = Exp $ Tuple $ NilTup `SnocTup` lift a

instance Elt a => Unlift Exp (Sum (Exp a)) where
  unlift t = Sum . Exp $ ZeroTupIdx `Prj` t

instance A.Num a => Monoid (Exp (Sum a)) where
  mempty  = 0
  mappend = lift2 (mappend :: Sum (Exp a) -> Sum (Exp a) -> Sum (Exp a))

instance A.Num a => P.Num (Exp (Sum a)) where
  (+)             = lift2 ((+) :: Sum (Exp a) -> Sum (Exp a) -> Sum (Exp a))
  (-)             = lift2 ((-) :: Sum (Exp a) -> Sum (Exp a) -> Sum (Exp a))
  (*)             = lift2 ((*) :: Sum (Exp a) -> Sum (Exp a) -> Sum (Exp a))
  negate          = lift1 (negate :: Sum (Exp a) -> Sum (Exp a))
  signum          = lift1 (signum :: Sum (Exp a) -> Sum (Exp a))
  abs             = lift1 (signum :: Sum (Exp a) -> Sum (Exp a))
  fromInteger x   = lift (P.fromInteger x :: Sum (Exp a))

instance A.Eq a => A.Eq (Sum a) where
  (==) = lift2 ((==) `on` getSum)
  (/=) = lift2 ((/=) `on` getSum)

instance A.Ord a => A.Ord (Sum a) where
  (<)     = lift2 ((<) `on` getSum)
  (>)     = lift2 ((>) `on` getSum)
  (<=)    = lift2 ((<=) `on` getSum)
  (>=)    = lift2 ((>=) `on` getSum)
  min x y = lift . Sum $ lift2 (min `on` getSum) x y
  max x y = lift . Sum $ lift2 (max `on` getSum) x y


-- Product: Monoid under multiplication
-- ------------------------------------

type instance EltRepr (Product a) = ((), EltRepr a)

instance Elt a => Elt (Product a) where
  eltType _       = PairTuple UnitTuple (eltType (undefined::a))
  toElt ((),x)    = Product (toElt x)
  fromElt (Product x) = ((), fromElt x)
  eltFlavour _        = EltTuple

instance Elt a => IsProduct Elt (Product a) where
  type ProdRepr (Product a) = ((), a)
  toProd _ ((),a)        = Product a
  fromProd _ (Product a) = ((),a)
  prod _ _               = ProdRsnoc ProdRunit

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Product a) where
  type Plain (Product a) = Product (Plain a)
  lift (Product a)       = Exp $ Tuple $ NilTup `SnocTup` lift a

instance Elt a => Unlift Exp (Product (Exp a)) where
  unlift t = Product . Exp $ ZeroTupIdx `Prj` t

instance A.Num a => Monoid (Exp (Product a)) where
  mempty  = 1
  mappend = lift2 (mappend :: Product (Exp a) -> Product (Exp a) -> Product (Exp a))

instance A.Num a => P.Num (Exp (Product a)) where
  (+)             = lift2 ((+) :: Product (Exp a) -> Product (Exp a) -> Product (Exp a))
  (-)             = lift2 ((-) :: Product (Exp a) -> Product (Exp a) -> Product (Exp a))
  (*)             = lift2 ((*) :: Product (Exp a) -> Product (Exp a) -> Product (Exp a))
  negate          = lift1 (negate :: Product (Exp a) -> Product (Exp a))
  signum          = lift1 (signum :: Product (Exp a) -> Product (Exp a))
  abs             = lift1 (signum :: Product (Exp a) -> Product (Exp a))
  fromInteger x   = lift (P.fromInteger x :: Product (Exp a))

instance A.Eq a => A.Eq (Product a) where
  (==) = lift2 ((==) `on` getProduct)
  (/=) = lift2 ((/=) `on` getProduct)

instance A.Ord a => A.Ord (Product a) where
  (<)     = lift2 ((<) `on` getProduct)
  (>)     = lift2 ((>) `on` getProduct)
  (<=)    = lift2 ((<=) `on` getProduct)
  (>=)    = lift2 ((>=) `on` getProduct)
  min x y = lift . Product $ lift2 (min `on` getProduct) x y
  max x y = lift . Product $ lift2 (max `on` getProduct) x y


-- Instances for unit and tuples
-- -----------------------------

instance Monoid (Exp ()) where
  mempty      = constant ()
  mappend _ _ = constant ()

instance (Elt a, Elt b, Monoid (Exp a), Monoid (Exp b)) => Monoid (Exp (a,b)) where
  mempty      = lift (mempty :: Exp a, mempty :: Exp b)
  mappend x y = let (a1,b1) = unlift x  :: (Exp a, Exp b)
                    (a2,b2) = unlift y
                in
                lift (a1<>a2, b1<>b2)

instance (Elt a, Elt b, Elt c, Monoid (Exp a), Monoid (Exp b), Monoid (Exp c)) => Monoid (Exp (a,b,c)) where
  mempty      = lift (mempty :: Exp a, mempty :: Exp b, mempty :: Exp c)
  mappend x y = let (a1,b1,c1) = unlift x  :: (Exp a, Exp b, Exp c)
                    (a2,b2,c2) = unlift y
                in
                lift (a1<>a2, b1<>b2, c1<>c2)

instance (Elt a, Elt b, Elt c, Elt d, Monoid (Exp a), Monoid (Exp b), Monoid (Exp c), Monoid (Exp d)) => Monoid (Exp (a,b,c,d)) where
  mempty      = lift (mempty :: Exp a, mempty :: Exp b, mempty :: Exp c, mempty :: Exp d)
  mappend x y = let (a1,b1,c1,d1) = unlift x  :: (Exp a, Exp b, Exp c, Exp d)
                    (a2,b2,c2,d2) = unlift y
                in
                lift (a1<>a2, b1<>b2, c1<>c2, d1<>d2)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Monoid (Exp a), Monoid (Exp b), Monoid (Exp c), Monoid (Exp d), Monoid (Exp e)) => Monoid (Exp (a,b,c,d,e)) where
  mempty      = lift (mempty :: Exp a, mempty :: Exp b, mempty :: Exp c, mempty :: Exp d, mempty :: Exp e)
  mappend x y = let (a1,b1,c1,d1,e1) = unlift x  :: (Exp a, Exp b, Exp c, Exp d, Exp e)
                    (a2,b2,c2,d2,e2) = unlift y
                in
                lift (a1<>a2, b1<>b2, c1<>c2, d1<>d2, e1<>e2)

