{-# LANGUAGE CPP                    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE ViewPatterns           #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE UndecidableInstances   #-}
#endif
-- |
-- Module      : Data.Array.Accelerate.Data.Semigroup
-- Copyright   : [2018..2020] The Accelerate Team
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

  Min, pattern Min,
  Max, pattern Max,

) where

import Data.Array.Accelerate.Classes.Bounded
import Data.Array.Accelerate.Classes.Eq
import Data.Array.Accelerate.Classes.Num
import Data.Array.Accelerate.Classes.Ord
import Data.Array.Accelerate.Lift
import Data.Array.Accelerate.Pattern
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Elt

import Data.Function
import Data.Monoid                                                  ( Monoid(..) )
import Data.Semigroup                                               ( Semigroup(..), Min, Max )
import qualified Prelude                                            as P
import qualified Data.Semigroup                                     as P


pattern Min :: IsMin a b => a -> b
pattern Min x <- (matchMin -> x)
  where Min = buildMin
{-# COMPLETE Min :: Min #-}
{-# COMPLETE Min :: Exp #-}

class IsMin a b | b -> a where
  matchMin :: b -> a
  buildMin :: a -> b

instance IsMin a (Min a) where
  matchMin = P.getMin
  buildMin = P.Min

instance Elt a => IsMin (Exp a) (Exp (Min a)) where
  matchMin (Pattern x) = x
  buildMin x = Pattern x

instance Elt a => Elt (Min a)

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Min a) where
  type Plain (Min a) = Min (Plain a)
  lift (Min a)       = Min (lift a)

instance Elt a => Unlift Exp (Min (Exp a)) where
  unlift (Min a) = Min a

instance Bounded a => P.Bounded (Exp (Min a)) where
  minBound = Min minBound
  maxBound = Min maxBound

instance Num a => P.Num (Exp (Min a)) where
  (+)           = lift2 ((+) :: Min (Exp a) -> Min (Exp a) -> Min (Exp a))
  (-)           = lift2 ((-) :: Min (Exp a) -> Min (Exp a) -> Min (Exp a))
  (*)           = lift2 ((*) :: Min (Exp a) -> Min (Exp a) -> Min (Exp a))
  negate        = lift1 (negate :: Min (Exp a) -> Min (Exp a))
  signum        = lift1 (signum :: Min (Exp a) -> Min (Exp a))
  abs           = lift1 (signum :: Min (Exp a) -> Min (Exp a))
  fromInteger x = lift (P.fromInteger x :: Min (Exp a))

instance Eq a => Eq (Min a) where
  (==) = lift2 ((==) `on` P.getMin)
  (/=) = lift2 ((/=) `on` P.getMin)

instance Ord a => Ord (Min a) where
  (<)     = lift2 ((<) `on` P.getMin)
  (>)     = lift2 ((>) `on` P.getMin)
  (<=)    = lift2 ((<=) `on` P.getMin)
  (>=)    = lift2 ((>=) `on` P.getMin)
  min x y = Min $ lift2 (min `on` P.getMin) x y
  max x y = Min $ lift2 (max `on` P.getMin) x y

instance Ord a => Semigroup (Exp (Min a)) where
  x <> y  = Min $ lift2 (min `on` P.getMin) x y
  stimes  = P.stimesIdempotent

instance (Ord a, Bounded a) => Monoid (Exp (Min a)) where
  mempty  = maxBound
  mappend = (<>)


pattern Max :: IsMax a b => a -> b
pattern Max x <- (matchMax -> x)
  where Max = buildMax
{-# COMPLETE Max :: Max #-}
{-# COMPLETE Max :: Exp #-}

class IsMax a b | b -> a where
  matchMax :: b -> a
  buildMax :: a -> b

instance IsMax a (Max a) where
  matchMax = P.getMax
  buildMax = P.Max

instance Elt a => IsMax (Exp a) (Exp (Max a)) where
  matchMax (Pattern x) = x
  buildMax x = Pattern x

instance Elt a => Elt (Max a)

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Max a) where
  type Plain (Max a) = Max (Plain a)
  lift (Max a)       = Max (lift a)

instance Elt a => Unlift Exp (Max (Exp a)) where
  unlift (Max a) = Max a

instance Bounded a => P.Bounded (Exp (Max a)) where
  minBound = Max minBound
  maxBound = Max maxBound

instance Num a => P.Num (Exp (Max a)) where
  (+)           = lift2 ((+) :: Max (Exp a) -> Max (Exp a) -> Max (Exp a))
  (-)           = lift2 ((-) :: Max (Exp a) -> Max (Exp a) -> Max (Exp a))
  (*)           = lift2 ((*) :: Max (Exp a) -> Max (Exp a) -> Max (Exp a))
  negate        = lift1 (negate :: Max (Exp a) -> Max (Exp a))
  signum        = lift1 (signum :: Max (Exp a) -> Max (Exp a))
  abs           = lift1 (signum :: Max (Exp a) -> Max (Exp a))
  fromInteger x = lift (P.fromInteger x :: Max (Exp a))

instance Eq a => Eq (Max a) where
  (==) = lift2 ((==) `on` P.getMax)
  (/=) = lift2 ((/=) `on` P.getMax)

instance Ord a => Ord (Max a) where
  (<)     = lift2 ((<) `on` P.getMax)
  (>)     = lift2 ((>) `on` P.getMax)
  (<=)    = lift2 ((<=) `on` P.getMax)
  (>=)    = lift2 ((>=) `on` P.getMax)
  min x y = Max $ lift2 (min `on` P.getMax) x y
  max x y = Max $ lift2 (max `on` P.getMax) x y

instance Ord a => Semigroup (Exp (Max a)) where
  x <> y  = Max $ lift2 (max `on` P.getMax) x y
  stimes  = P.stimesIdempotent

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

