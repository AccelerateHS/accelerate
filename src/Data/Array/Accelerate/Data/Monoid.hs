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

import Data.Array.Accelerate.Annotations
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
  lift (Sum a)       = sourceMap $ Sum_ (lift a)

instance Elt a => Unlift Exp (Sum (Exp a)) where
  unlift = sourceMap $ \(Sum_ a) -> Sum a

instance Bounded a => P.Bounded (Exp (Sum a)) where
  minBound = sourceMapRuntime $ Sum_ minBound
  maxBound = sourceMapRuntime $ Sum_ maxBound

instance Num a => P.Num (Exp (Sum a)) where
  (+)             = sourceMapRuntime $ lift2 ((+) :: Sum (Exp a) -> Sum (Exp a) -> Sum (Exp a))
  (-)             = sourceMapRuntime $ lift2 ((-) :: Sum (Exp a) -> Sum (Exp a) -> Sum (Exp a))
  (*)             = sourceMapRuntime $ lift2 ((*) :: Sum (Exp a) -> Sum (Exp a) -> Sum (Exp a))
  negate          = sourceMapRuntime $ lift1 (negate :: Sum (Exp a) -> Sum (Exp a))
  signum          = sourceMapRuntime $ lift1 (signum :: Sum (Exp a) -> Sum (Exp a))
  abs             = sourceMapRuntime $ lift1 (signum :: Sum (Exp a) -> Sum (Exp a))
  fromInteger x   = sourceMapRuntime $ lift (P.fromInteger x :: Sum (Exp a))

instance Eq a => Eq (Sum a) where
  (==) = sourceMap $ lift2 ((==) `on` getSum)
  (/=) = sourceMap $ lift2 ((/=) `on` getSum)

instance Ord a => Ord (Sum a) where
  (<)     = sourceMap $ lift2 ((<) `on` getSum)
  (>)     = sourceMap $ lift2 ((>) `on` getSum)
  (<=)    = sourceMap $ lift2 ((<=) `on` getSum)
  (>=)    = sourceMap $ lift2 ((>=) `on` getSum)
  min x y = sourceMap $ Sum_ $ lift2 (min `on` getSum) x y
  max x y = sourceMap $ Sum_ $ lift2 (max `on` getSum) x y

instance Num a => Monoid (Exp (Sum a)) where
  mempty = sourceMapRuntime 0

-- | @since 1.2.0.0
instance Num a => Semigroup (Exp (Sum a)) where
  (<>)     = sourceMapRuntime (+)
  stimes n = sourceMapRuntime $ \(Sum_ x) -> Sum_ $ P.fromIntegral n * x


-- Product: Monoid under multiplication
-- ------------------------------------

pattern Product_ :: Elt a => Exp a -> Exp (Product a)
pattern Product_ x = Pattern x
{-# COMPLETE Product_ #-}

instance Elt a => Elt (Product a)

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Product a) where
  type Plain (Product a) = Product (Plain a)
  lift (Product a)       = sourceMap $ Product_ (lift a)

instance Elt a => Unlift Exp (Product (Exp a)) where
  unlift = sourceMap $ \(Product_ a) -> Product a

instance Bounded a => P.Bounded (Exp (Product a)) where
  minBound = sourceMapRuntime $ Product_ minBound
  maxBound = sourceMapRuntime $ Product_ maxBound

instance Num a => P.Num (Exp (Product a)) where
  (+)             = sourceMapRuntime $ lift2 ((+) :: Product (Exp a) -> Product (Exp a) -> Product (Exp a))
  (-)             = sourceMapRuntime $ lift2 ((-) :: Product (Exp a) -> Product (Exp a) -> Product (Exp a))
  (*)             = sourceMapRuntime $ lift2 ((*) :: Product (Exp a) -> Product (Exp a) -> Product (Exp a))
  negate          = sourceMapRuntime $ lift1 (negate :: Product (Exp a) -> Product (Exp a))
  signum          = sourceMapRuntime $ lift1 (signum :: Product (Exp a) -> Product (Exp a))
  abs             = sourceMapRuntime $ lift1 (signum :: Product (Exp a) -> Product (Exp a))
  fromInteger x   = sourceMapRuntime $ lift (P.fromInteger x :: Product (Exp a))

instance Eq a => Eq (Product a) where
  (==) = sourceMap $ lift2 ((==) `on` getProduct)
  (/=) = sourceMap $ lift2 ((/=) `on` getProduct)

instance Ord a => Ord (Product a) where
  (<)     = sourceMap $ lift2 ((<) `on` getProduct)
  (>)     = sourceMap $ lift2 ((>) `on` getProduct)
  (<=)    = sourceMap $ lift2 ((<=) `on` getProduct)
  (>=)    = sourceMap $ lift2 ((>=) `on` getProduct)
  min x y = sourceMap $ Product_ $ lift2 (min `on` getProduct) x y
  max x y = sourceMap $ Product_ $ lift2 (max `on` getProduct) x y

instance Num a => Monoid (Exp (Product a)) where
  mempty = sourceMapRuntime 1

-- | @since 1.2.0.0
instance Num a => Semigroup (Exp (Product a)) where
  (<>)     = sourceMap (*)
  stimes n = sourceMap $ \(Product_ x) -> Product_ $ x ^ (P.fromIntegral n :: Exp Int)


-- Instances for unit and tuples
-- -----------------------------

instance Monoid (Exp ()) where
  mempty = sourceMapRuntime $ constant ()

instance (Elt a, Elt b, Monoid (Exp a), Monoid (Exp b)) => Monoid (Exp (a,b)) where
  mempty = sourceMapRuntime $ T2 mempty mempty

instance (Elt a, Elt b, Elt c, Monoid (Exp a), Monoid (Exp b), Monoid (Exp c)) => Monoid (Exp (a,b,c)) where
  mempty = sourceMapRuntime $ T3 mempty mempty mempty

instance (Elt a, Elt b, Elt c, Elt d, Monoid (Exp a), Monoid (Exp b), Monoid (Exp c), Monoid (Exp d)) => Monoid (Exp (a,b,c,d)) where
  mempty = sourceMapRuntime $ T4 mempty mempty mempty mempty

instance (Elt a, Elt b, Elt c, Elt d, Elt e, Monoid (Exp a), Monoid (Exp b), Monoid (Exp c), Monoid (Exp d), Monoid (Exp e)) => Monoid (Exp (a,b,c,d,e)) where
  mempty = sourceMapRuntime $ T5 mempty mempty mempty mempty mempty
