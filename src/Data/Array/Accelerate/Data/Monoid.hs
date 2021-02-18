{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE UndecidableInstances  #-}
#endif
-- |
-- Module      : Data.Array.Accelerate.Data.Monoid
-- Copyright   : [2016..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Monoid instances for Accelerate
--
-- @since 1.2.0.0
--

module Data.Array.Accelerate.Data.Monoid (

  Monoid(..), (<>),

  Sum(..), pattern Sum_,
  Product(..), pattern Product_,

) where

import Data.Array.Accelerate.Classes.Bounded
import Data.Array.Accelerate.Classes.Eq
import Data.Array.Accelerate.Classes.Num
import Data.Array.Accelerate.Classes.Ord
import Data.Array.Accelerate.Data.Semigroup                         ()
import Data.Array.Accelerate.Language
import Data.Array.Accelerate.Lift
import Data.Array.Accelerate.Pattern
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Type

import Data.Function
import Data.Monoid                                                  hiding ( (<>) )
import Data.Semigroup
import GHC.Stack
import qualified Prelude                                            as P


-- Sum: Monoid under addition
-- --------------------------

-- TODO: How does call stack freezing work here? Same for the other pattern
--       synonyms here.
pattern Sum_ :: (HasCallStack, Elt a) => Exp a -> Exp (Sum a)
pattern Sum_ x = Pattern x
{-# COMPLETE Sum_ #-}

instance Elt a => Elt (Sum a)

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Sum a) where
  type Plain (Sum a) = Sum (Plain a)
  lift (Sum a)       = withFrozenCallStack $ Sum_ (lift a)

instance Elt a => Unlift Exp (Sum (Exp a)) where
  unlift (Sum_ a) = withFrozenCallStack $ Sum a

-- TODO: Can we use call stacks in these prelude type classes?
instance Bounded a => P.Bounded (Exp (Sum a)) where
  minBound = Sum_ minBound
  maxBound = Sum_ maxBound

instance Num a => P.Num (Exp (Sum a)) where
  (+)             = lift2 ((+) :: Sum (Exp a) -> Sum (Exp a) -> Sum (Exp a))
  (-)             = lift2 ((-) :: Sum (Exp a) -> Sum (Exp a) -> Sum (Exp a))
  (*)             = lift2 ((*) :: Sum (Exp a) -> Sum (Exp a) -> Sum (Exp a))
  negate          = lift1 (negate :: Sum (Exp a) -> Sum (Exp a))
  signum          = lift1 (signum :: Sum (Exp a) -> Sum (Exp a))
  abs             = lift1 (signum :: Sum (Exp a) -> Sum (Exp a))
  fromInteger x   = lift (P.fromInteger x :: Sum (Exp a))

instance Eq a => Eq (Sum a) where
  (==) = withFrozenCallStack $ lift2 ((==) `on` getSum)
  (/=) = withFrozenCallStack $ lift2 ((/=) `on` getSum)

instance Ord a => Ord (Sum a) where
  (<)     = withFrozenCallStack $ lift2 ((<) `on` getSum)
  (>)     = withFrozenCallStack $ lift2 ((>) `on` getSum)
  (<=)    = withFrozenCallStack $ lift2 ((<=) `on` getSum)
  (>=)    = withFrozenCallStack $ lift2 ((>=) `on` getSum)
  min x y = withFrozenCallStack $ Sum_ $ lift2 (min `on` getSum) x y
  max x y = withFrozenCallStack $ Sum_ $ lift2 (max `on` getSum) x y

instance Num a => Monoid (Exp (Sum a)) where
  -- TODO: Does this work like you'd expect?
  mempty = withFrozenCallStack 0

-- | @since 1.2.0.0
instance Num a => Semigroup (Exp (Sum a)) where
  (<>)              = withFrozenCallStack (+)
  stimes n (Sum_ x) = withFrozenCallStack $ Sum_ $ P.fromIntegral n * x


-- Product: Monoid under multiplication
-- ------------------------------------

pattern Product_ :: Elt a => Exp a -> Exp (Product a)
pattern Product_ x = Pattern x
{-# COMPLETE Product_ #-}

instance Elt a => Elt (Product a)

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Product a) where
  type Plain (Product a) = Product (Plain a)
  lift (Product a)       = withFrozenCallStack $ Product_ (lift a)

instance Elt a => Unlift Exp (Product (Exp a)) where
  unlift (Product_ a) = withFrozenCallStack $ Product a

instance Bounded a => P.Bounded (Exp (Product a)) where
  minBound = Product_ minBound
  maxBound = Product_ maxBound

instance Num a => P.Num (Exp (Product a)) where
  (+)             = lift2 ((+) :: Product (Exp a) -> Product (Exp a) -> Product (Exp a))
  (-)             = lift2 ((-) :: Product (Exp a) -> Product (Exp a) -> Product (Exp a))
  (*)             = lift2 ((*) :: Product (Exp a) -> Product (Exp a) -> Product (Exp a))
  negate          = lift1 (negate :: Product (Exp a) -> Product (Exp a))
  signum          = lift1 (signum :: Product (Exp a) -> Product (Exp a))
  abs             = lift1 (signum :: Product (Exp a) -> Product (Exp a))
  fromInteger x   = lift (P.fromInteger x :: Product (Exp a))

instance Eq a => Eq (Product a) where
  (==) = withFrozenCallStack $ lift2 ((==) `on` getProduct)
  (/=) = withFrozenCallStack $ lift2 ((/=) `on` getProduct)

instance Ord a => Ord (Product a) where
  (<)     = withFrozenCallStack $ lift2 ((<) `on` getProduct)
  (>)     = withFrozenCallStack $ lift2 ((>) `on` getProduct)
  (<=)    = withFrozenCallStack $ lift2 ((<=) `on` getProduct)
  (>=)    = withFrozenCallStack $ lift2 ((>=) `on` getProduct)
  min x y = withFrozenCallStack $ Product_ $ lift2 (min `on` getProduct) x y
  max x y = withFrozenCallStack $ Product_ $ lift2 (max `on` getProduct) x y

instance Num a => Monoid (Exp (Product a)) where
  -- TODO: Does this work?
  mempty = withFrozenCallStack 1

-- | @since 1.2.0.0
instance Num a => Semigroup (Exp (Product a)) where
  (<>)                  = withFrozenCallStack (*)
  stimes n (Product_ x) = withFrozenCallStack $ Product_ $ x ^ (P.fromIntegral n :: Exp Int)


-- Instances for unit and tuples
-- -----------------------------

instance Monoid (Exp ()) where
  mempty = withFrozenCallStack $ constant ()

instance (Elt a, Elt b, Monoid (Exp a), Monoid (Exp b)) => Monoid (Exp (a,b)) where
  mempty = withFrozenCallStack $ T2 mempty mempty

instance (Elt a, Elt b, Elt c, Monoid (Exp a), Monoid (Exp b), Monoid (Exp c)) => Monoid (Exp (a,b,c)) where
  mempty = withFrozenCallStack $ T3 mempty mempty mempty

instance (Elt a, Elt b, Elt c, Elt d, Monoid (Exp a), Monoid (Exp b), Monoid (Exp c), Monoid (Exp d)) => Monoid (Exp (a,b,c,d)) where
  mempty = withFrozenCallStack $ T4 mempty mempty mempty mempty

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Monoid (Exp a), Monoid (Exp b), Monoid (Exp c), Monoid (Exp d), Monoid (Exp e)) => Monoid (Exp (a,b,c,d,e)) where
  mempty = withFrozenCallStack $ T5 mempty mempty mempty mempty mempty
