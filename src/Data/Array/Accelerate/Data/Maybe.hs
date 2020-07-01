{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
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
-- Copyright   : [2018..2019] The Accelerate Team
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

import Data.Array.Accelerate.AST.Idx
import Data.Array.Accelerate.Language                               hiding ( chr )
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

import Data.Array.Accelerate.Data.Functor
import Data.Array.Accelerate.Data.Monoid
#if __GLASGOW_HASKELL__ >= 800
import Data.Array.Accelerate.Data.Semigroup
#endif

import Data.Maybe                                                   ( Maybe(..) )
import Prelude                                                      ( ($) )


-- | Returns 'True' if the argument is 'Nothing'
--
isNothing :: Elt a => Exp (Maybe a) -> Exp Bool
isNothing x = tag x == 0

-- | Returns 'True' if the argument is of the form @Just _@
--
isJust :: Elt a => Exp (Maybe a) -> Exp Bool
isJust x = tag x == 1

-- | The 'fromMaybe' function takes a default value and a 'Maybe' value. If the
-- 'Maybe' is 'Nothing', the default value is returned; otherwise, it returns
-- the value contained in the 'Maybe'.
--
fromMaybe :: Elt a => Exp a -> Exp (Maybe a) -> Exp a
fromMaybe d x = cond (isNothing x) d (fromJust x)

-- | The 'fromJust' function extracts the element out of the 'Just' constructor.
-- If the argument was actually 'Nothing', you will get an undefined value
-- instead.
--
fromJust :: Elt a => Exp (Maybe a) -> Exp a
fromJust (Exp x) = Exp $ SmartExp (PairIdxRight `Prj` SmartExp (PairIdxRight `Prj` x))

-- | The 'maybe' function takes a default value, a function, and a 'Maybe'
-- value. If the 'Maybe' value is nothing, the default value is returned;
-- otherwise, it applies the function to the value inside the 'Just' and returns
-- the result
--
maybe :: (Elt a, Elt b) => Exp b -> (Exp a -> Exp b) -> Exp (Maybe a) -> Exp b
maybe d f x = cond (isNothing x) d (f (fromJust x))


-- | Extract from an array all of the 'Just' values, together with a segment
-- descriptor indicating how many elements along each dimension were returned.
--
justs :: (Shape sh, Slice sh, Elt a)
      => Acc (Array (sh:.Int) (Maybe a))
      -> Acc (Vector a, Array sh Int)
justs xs = compact (map isJust xs) (map fromJust xs)


instance Functor Maybe where
  fmap f x = cond (isNothing x) Nothing_ (Just_ (f (fromJust x)))

instance Eq a => Eq (Maybe a) where
  ma == mb = cond (isNothing ma && isNothing mb) True_
           $ cond (isJust ma    && isJust mb)    (fromJust ma == fromJust mb)
           $ False_

instance Ord a => Ord (Maybe a) where
  compare ma mb = cond (isJust ma && isJust mb)
                       (compare (fromJust ma) (fromJust mb))
                       (compare (tag ma) (tag mb))

instance (Monoid (Exp a), Elt a) => Monoid (Exp (Maybe a)) where
  mempty        = Nothing_
#if __GLASGOW_HASKELL__ < 804
  mappend ma mb = cond (isNothing ma) mb
                $ cond (isNothing mb) ma
                $ lift (Just (fromJust ma `mappend` fromJust mb))
#endif

#if __GLASGOW_HASKELL__ >= 800
instance (Semigroup (Exp a), Elt a) => Semigroup (Exp (Maybe a)) where
  ma <> mb = cond (isNothing ma) mb
           $ cond (isNothing mb) mb
           $ lift (Just (fromJust ma <> fromJust mb))
#endif


tag :: Elt a => Exp (Maybe a) -> Exp Word8
tag (Exp x) = Exp $ SmartExp $ Prj PairIdxLeft x

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Maybe a) where
  type Plain (Maybe a) = Maybe (Plain a)
  lift Nothing  = Nothing_
  lift (Just a) = Just_ (lift a)

