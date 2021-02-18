{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Data.Maybe
-- Copyright   : [2018..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- @since 1.2.0.0
--

module Data.Array.Accelerate.Data.Maybe (

  Maybe(..), pattern Nothing_, pattern Just_,
  maybe, isJust, isNothing, fromMaybe, fromJust, justs,

) where

import Data.Array.Accelerate.Annotations
import Data.Array.Accelerate.AST.Idx
import Data.Array.Accelerate.Language
import Data.Array.Accelerate.Lift
import Data.Array.Accelerate.Pattern.Maybe
import Data.Array.Accelerate.Prelude
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Array                            ( Array, Vector )
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Sugar.Shape                            ( Shape, Slice, (:.) )
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.Classes.Eq
import Data.Array.Accelerate.Classes.Ord

import Data.Array.Accelerate.Control.Monad
import Data.Array.Accelerate.Data.Functor
import Data.Array.Accelerate.Data.Monoid
import Data.Array.Accelerate.Data.Semigroup

import Data.Function                                                ( (&) )
import Data.Maybe                                                   ( Maybe(..) )
import Prelude                                                      ( ($), (.) )


-- | Returns 'True' if the argument is 'Nothing'
--
isNothing :: (HasCallStack, Elt a) => Exp (Maybe a) -> Exp Bool
isNothing = withFrozenCallStack $ not . isJust

-- | Returns 'True' if the argument is of the form @Just _@
--
isJust :: (HasCallStack, Elt a) => Exp (Maybe a) -> Exp Bool
isJust (Exp x) = withFrozenCallStack $ Exp $ SmartExp $ Pair mkAnn (SmartExp $ Prj PairIdxLeft x) (SmartExp (Nil mkAnn))
  -- TLM: This is a sneaky hack because we know that the tag bits for Just
  -- and True are identical.

-- | The 'fromMaybe' function takes a default value and a 'Maybe' value. If the
-- 'Maybe' is 'Nothing', the default value is returned; otherwise, it returns
-- the value contained in the 'Maybe'.
--
fromMaybe :: (HasCallStack, Elt a) => Exp a -> Exp (Maybe a) -> Exp a
fromMaybe d = withFrozenCallStack $ match \case
  Nothing_ -> d
  Just_ x  -> x

-- | The 'fromJust' function extracts the element out of the 'Just' constructor.
-- If the argument was actually 'Nothing', you will get an undefined value
-- instead.
--
fromJust :: (HasCallStack, Elt a) => Exp (Maybe a) -> Exp a
fromJust (Exp x) = withFrozenCallStack $ Exp $ SmartExp (PairIdxRight `Prj` SmartExp (PairIdxRight `Prj` x))

-- | The 'maybe' function takes a default value, a function, and a 'Maybe'
-- value. If the 'Maybe' value is nothing, the default value is returned;
-- otherwise, it applies the function to the value inside the 'Just' and returns
-- the result
--
maybe :: (HasCallStack, Elt a, Elt b) => Exp b -> (Exp a -> Exp b) -> Exp (Maybe a) -> Exp b
maybe d f = withFrozenCallStack $ match \case
  Nothing_ -> d
  Just_ x  -> f x

-- | Extract from an array all of the 'Just' values, together with a segment
-- descriptor indicating how many elements along each dimension were returned.
--
justs :: (HasCallStack, Shape sh, Slice sh, Elt a)
      => Acc (Array (sh:.Int) (Maybe a))
      -> Acc (Vector a, Array sh Int)
justs xs = withFrozenCallStack $ compact (map isJust xs) (map fromJust xs)


instance Functor Maybe where
  fmap f = withFrozenCallStack $ match \case
    Nothing_ -> Nothing_
    Just_ x  -> Just_ (f x)

instance Monad Maybe where
  return   = withFrozenCallStack Just_
  mx >>= f = withFrozenCallStack $ mx & match \case
    Nothing_ -> Nothing_
    Just_ x  -> f x

instance Eq a => Eq (Maybe a) where
  (==) = withFrozenCallStack $ match go
    where
      go :: HasCallStack => Exp (Maybe a) -> Exp (Maybe a) -> Exp Bool
      go Nothing_  Nothing_  = True_
      go (Just_ x) (Just_ y) = x == y
      go _         _         = False_

instance Ord a => Ord (Maybe a) where
  compare = withFrozenCallStack $ match go
    where
      go :: HasCallStack => Exp (Maybe a) -> Exp (Maybe a) -> Exp Ordering
      go (Just_ x) (Just_ y)  = compare x y
      go Nothing_  Nothing_   = EQ_
      go Nothing_  Just_{}    = LT_
      go Just_{}   Nothing_{} = GT_

instance (Monoid (Exp a), Elt a) => Monoid (Exp (Maybe a)) where
  mempty = withFrozenCallStack Nothing_

instance (Semigroup (Exp a), Elt a) => Semigroup (Exp (Maybe a)) where
  ma <> mb = withFrozenCallStack
           $ cond (isNothing ma) mb
           $ cond (isNothing mb) mb
           $ lift (Just (fromJust ma <> fromJust mb))

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Maybe a) where
  type Plain (Maybe a) = Maybe (Plain a)
  lift Nothing  = withFrozenCallStack Nothing_
  lift (Just a) = withFrozenCallStack $ Just_ (lift a)
