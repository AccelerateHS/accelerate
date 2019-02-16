{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE UndecidableInstances  #-}
#endif
-- |
-- Module      : Data.Array.Accelerate.Data.Semigroup
-- Copyright   : [2018..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Semigroup instances for Accelerate
--
-- @since 1.2.0.0
--

module Data.Array.Accelerate.Data.Semigroup (

  Semigroup(..),

  Min(..),
  Max(..),

) where

import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Classes.Bounded
import Data.Array.Accelerate.Classes.Eq
import Data.Array.Accelerate.Classes.Num
import Data.Array.Accelerate.Classes.Ord
import Data.Array.Accelerate.Lift
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type

import Data.Function
import Data.Monoid                                                  ( Monoid(..) )
import Data.Semigroup
import qualified Prelude                                            as P


instance Elt a => Elt (Min a) where
  type EltRepr (Min a) = ((), EltRepr a)
  {-# INLINE eltType     #-}
  {-# INLINE [1] toElt   #-}
  {-# INLINE [1] fromElt #-}
  eltType         = TypeRpair TypeRunit (eltType @a)
  toElt ((),x)    = Min (toElt x)
  fromElt (Min x) = ((), fromElt x)

instance Elt a => IsProduct Elt (Min a) where
  type ProdRepr (Min a) = ((), a)
  toProd ((),a)    = Min a
  fromProd (Min a) = ((),a)
  prod             = ProdRsnoc ProdRunit

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Min a) where
  type Plain (Min a) = Min (Plain a)
  lift (Min a)       = Exp $ Tuple $ NilTup `SnocTup` lift a

instance Elt a => Unlift Exp (Min (Exp a)) where
  unlift t = Min . Exp $ ZeroTupIdx `Prj` t

instance Bounded a => P.Bounded (Exp (Min a)) where
  minBound = lift $ Min (minBound :: Exp a)
  maxBound = lift $ Min (maxBound :: Exp a)

instance Num a => P.Num (Exp (Min a)) where
  (+)           = lift2 ((+) :: Min (Exp a) -> Min (Exp a) -> Min (Exp a))
  (-)           = lift2 ((-) :: Min (Exp a) -> Min (Exp a) -> Min (Exp a))
  (*)           = lift2 ((*) :: Min (Exp a) -> Min (Exp a) -> Min (Exp a))
  negate        = lift1 (negate :: Min (Exp a) -> Min (Exp a))
  signum        = lift1 (signum :: Min (Exp a) -> Min (Exp a))
  abs           = lift1 (signum :: Min (Exp a) -> Min (Exp a))
  fromInteger x = lift (P.fromInteger x :: Min (Exp a))

instance Eq a => Eq (Min a) where
  (==) = lift2 ((==) `on` getMin)
  (/=) = lift2 ((/=) `on` getMin)

instance Ord a => Ord (Min a) where
  (<)     = lift2 ((<) `on` getMin)
  (>)     = lift2 ((>) `on` getMin)
  (<=)    = lift2 ((<=) `on` getMin)
  (>=)    = lift2 ((>=) `on` getMin)
  min x y = lift . Min $ lift2 (min `on` getMin) x y
  max x y = lift . Min $ lift2 (max `on` getMin) x y

instance Ord a => Semigroup (Exp (Min a)) where
  x <> y  = lift . Min $ lift2 (min `on` getMin) x y
  stimes  = stimesIdempotent

instance (Ord a, Bounded a) => Monoid (Exp (Min a)) where
  mempty  = maxBound
  mappend = (<>)


instance Elt a => Elt (Max a) where
  type EltRepr (Max a) = ((), EltRepr a)
  {-# INLINE eltType     #-}
  {-# INLINE [1] toElt   #-}
  {-# INLINE [1] fromElt #-}
  eltType         = TypeRpair TypeRunit (eltType @a)
  toElt ((),x)    = Max (toElt x)
  fromElt (Max x) = ((), fromElt x)

instance Elt a => IsProduct Elt (Max a) where
  type ProdRepr (Max a) = ((), a)
  toProd ((),a)    = Max a
  fromProd (Max a) = ((),a)
  prod             = ProdRsnoc ProdRunit

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Max a) where
  type Plain (Max a) = Max (Plain a)
  lift (Max a)       = Exp $ Tuple $ NilTup `SnocTup` lift a

instance Elt a => Unlift Exp (Max (Exp a)) where
  unlift t = Max . Exp $ ZeroTupIdx `Prj` t

instance Bounded a => P.Bounded (Exp (Max a)) where
  minBound = lift $ Max (minBound :: Exp a)
  maxBound = lift $ Max (maxBound :: Exp a)

instance Num a => P.Num (Exp (Max a)) where
  (+)           = lift2 ((+) :: Max (Exp a) -> Max (Exp a) -> Max (Exp a))
  (-)           = lift2 ((-) :: Max (Exp a) -> Max (Exp a) -> Max (Exp a))
  (*)           = lift2 ((*) :: Max (Exp a) -> Max (Exp a) -> Max (Exp a))
  negate        = lift1 (negate :: Max (Exp a) -> Max (Exp a))
  signum        = lift1 (signum :: Max (Exp a) -> Max (Exp a))
  abs           = lift1 (signum :: Max (Exp a) -> Max (Exp a))
  fromInteger x = lift (P.fromInteger x :: Max (Exp a))

instance Eq a => Eq (Max a) where
  (==) = lift2 ((==) `on` getMax)
  (/=) = lift2 ((/=) `on` getMax)

instance Ord a => Ord (Max a) where
  (<)     = lift2 ((<) `on` getMax)
  (>)     = lift2 ((>) `on` getMax)
  (<=)    = lift2 ((<=) `on` getMax)
  (>=)    = lift2 ((>=) `on` getMax)
  min x y = lift . Max $ lift2 (min `on` getMax) x y
  max x y = lift . Max $ lift2 (max `on` getMax) x y

instance Ord a => Semigroup (Exp (Max a)) where
  x <> y  = lift . Max $ lift2 (max `on` getMax) x y
  stimes  = stimesIdempotent

instance (Ord a, Bounded a) => Monoid (Exp (Max a)) where
  mempty  = minBound
  mappend = (<>)


-- Instances for unit and tuples
-- -----------------------------

instance Semigroup (Exp ()) where
  _ <> _     = constant ()
  sconcat _  = constant ()
  stimes _ _ = constant ()

instance (Elt a, Elt b, Semigroup (Exp a), Semigroup (Exp b)) => Semigroup (Exp (a,b)) where
  (<>) = lift2 ((<>) :: (Exp a, Exp b) -> (Exp a, Exp b) -> (Exp a, Exp b))
  stimes n (unlift -> (a,b) :: (Exp a, Exp b)) = lift (stimes n a, stimes n b)

instance (Elt a, Elt b, Elt c, Semigroup (Exp a), Semigroup (Exp b), Semigroup (Exp c)) => Semigroup (Exp (a,b,c)) where
  (<>) = lift2 ((<>) :: (Exp a, Exp b, Exp c) -> (Exp a, Exp b, Exp c) -> (Exp a, Exp b, Exp c))
  stimes n (unlift -> (a,b,c) :: (Exp a, Exp b, Exp c)) = lift (stimes n a, stimes n b, stimes n c)

instance (Elt a, Elt b, Elt c, Elt d, Semigroup (Exp a), Semigroup (Exp b), Semigroup (Exp c), Semigroup (Exp d)) => Semigroup (Exp (a,b,c,d)) where
  (<>) = lift2 ((<>) :: (Exp a, Exp b, Exp c, Exp d) -> (Exp a, Exp b, Exp c, Exp d) -> (Exp a, Exp b, Exp c, Exp d))
  stimes n (unlift -> (a,b,c,d) :: (Exp a, Exp b, Exp c, Exp d)) = lift (stimes n a, stimes n b, stimes n c, stimes n d)

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Semigroup (Exp a), Semigroup (Exp b), Semigroup (Exp c), Semigroup (Exp d), Semigroup (Exp e)) => Semigroup (Exp (a,b,c,d,e)) where
  (<>) = lift2 ((<>) :: (Exp a, Exp b, Exp c, Exp d, Exp e) -> (Exp a, Exp b, Exp c, Exp d, Exp e) -> (Exp a, Exp b, Exp c, Exp d, Exp e))
  stimes n (unlift -> (a,b,c,d,e) :: (Exp a, Exp b, Exp c, Exp d, Exp e)) = lift (stimes n a, stimes n b, stimes n c, stimes n d, stimes n e)

