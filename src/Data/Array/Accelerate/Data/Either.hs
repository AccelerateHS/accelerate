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
-- Module      : Data.Array.Accelerate.Data.Either
-- Copyright   : [2018..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- @since 1.2.0.0
--

module Data.Array.Accelerate.Data.Either (

  Either(..), pattern Left_, pattern Right_,
  either, isLeft, isRight, fromLeft, fromRight, lefts, rights,

) where

import Data.Array.Accelerate.AST.Idx
import Data.Array.Accelerate.Language                               hiding ( chr )
import Data.Array.Accelerate.Lift
import Data.Array.Accelerate.Pattern.Either
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

import Data.Either                                                  ( Either(..) )
import Prelude                                                      ( (.), ($) )


-- | Lift a value into the 'Left' constructor
--
left :: forall a b. (Elt a, Elt b) => Exp a -> Exp (Either a b)
left a = lift (Left a :: Either (Exp a) (Exp b))

-- | Lift a value into the 'Right' constructor
--
right :: forall a b. (Elt a, Elt b) => Exp b -> Exp (Either a b)
right b = lift (Right b :: Either (Exp a) (Exp b))
--
-- See Note: [lifting Nothing]


-- | Return 'True' if the argument is a 'Left'-value
--
isLeft :: (Elt a, Elt b) => Exp (Either a b) -> Exp Bool
isLeft x = tag x == 0

-- | Return 'True' if the argument is a 'Right'-value
--
isRight :: (Elt a, Elt b) => Exp (Either a b) -> Exp Bool
isRight x = tag x == 1

-- | The 'fromLeft' function extracts the element out of the 'Left' constructor.
-- If the argument was actually 'Right', you will get an undefined value
-- instead.
--
fromLeft :: (Elt a, Elt b) => Exp (Either a b) -> Exp a
fromLeft (Exp e) = Exp $ SmartExp $ Prj PairIdxRight $ SmartExp $ Prj PairIdxLeft $ SmartExp $ Prj PairIdxRight e

-- | The 'fromRight' function extracts the element out of the 'Right'
-- constructor. If the argument was actually 'Left', you will get an undefined
-- value instead.
--
fromRight :: (Elt a, Elt b) => Exp (Either a b) -> Exp b
fromRight (Exp e) = Exp $ SmartExp $ Prj PairIdxRight $ SmartExp $ Prj PairIdxRight e

-- | The 'either' function performs case analysis on the 'Either' type. If the
-- value is @'Left' a@, apply the first function to @a@; if it is @'Right' b@,
-- apply the second function to @b@.
--
either :: (Elt a, Elt b, Elt c) => (Exp a -> Exp c) -> (Exp b -> Exp c) -> Exp (Either a b) -> Exp c
either f g x =
  cond (isLeft x) (f (fromLeft x)) (g (fromRight x))


-- | Extract from the array of 'Either' all of the 'Left' elements, together
-- with a segment descriptor indicating how many elements along each dimension
-- were returned.
--
lefts :: (Shape sh, Slice sh, Elt a, Elt b)
      => Acc (Array (sh:.Int) (Either a b))
      -> Acc (Vector a, Array sh Int)
lefts es = compact (map isLeft es) (map fromLeft es)

-- | Extract from the array of 'Either' all of the 'Right' elements, together
-- with a segment descriptor indicating how many elements along each dimension
-- were returned.
--
rights :: (Shape sh, Slice sh, Elt a, Elt b)
       => Acc (Array (sh:.Int) (Either a b))
       -> Acc (Vector b, Array sh Int)
rights es = compact (map isRight es) (map fromRight es)


instance Elt a => Functor (Either a) where
  fmap f = either left (right . f)

instance (Eq a, Eq b) => Eq (Either a b) where
  ex == ey = isLeft  ex && isLeft  ey ? ( fromLeft ex  == fromLeft ey
           , isRight ex && isRight ey ? ( fromRight ex == fromRight ey
           , {- else -}                   False_ ))

instance (Ord a, Ord b) => Ord (Either a b) where
  compare ex ey = isLeft  ex && isLeft  ey ? ( compare (fromLeft ex) (fromLeft ey)
                , isRight ex && isRight ey ? ( compare (fromRight ex) (fromRight ey)
                , {- else -}                   compare (tag ex) (tag ey) ))

#if __GLASGOW_HASKELL__ >= 800
instance (Elt a, Elt b) => Semigroup (Exp (Either a b)) where
  ex <> ey = isLeft ex ? ( ey, ex )
#endif

tag :: (Elt a, Elt b) => Exp (Either a b) -> Exp Word8
tag (Exp e) = Exp $ SmartExp $ Prj PairIdxLeft e

instance (Lift Exp a, Lift Exp b, Elt (Plain a), Elt (Plain b)) => Lift Exp (Either a b) where
  type Plain (Either a b) = Either (Plain a) (Plain b)
  lift (Left a)  = Left_ (lift a)
  lift (Right b) = Right_ (lift b)

