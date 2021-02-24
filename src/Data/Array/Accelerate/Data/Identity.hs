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
module Data.Array.Accelerate.Data.Identity  (

  Identity(..),
  identity

) where
import Prelude (Semigroup(..), Monoid(..))
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Data.Functor
import Data.Array.Accelerate.Control.Monad
import Data.Array.Accelerate.Classes.Eq
import Data.Array.Accelerate.Classes.Ord
import Data.Array.Accelerate.Pattern.Identity
import Data.Array.Accelerate.Sugar.Elt

identity :: (Elt a, Elt b) => (Exp a -> Exp b) -> Exp (Identity a) -> Exp (Identity b)
identity = fmap

instance Functor Identity where
  fmap f (Identity_ x) = Identity_ (f x)

instance Monad Identity where
  return = Identity_
  (Identity_ x) >>= f = f x

instance Eq a => Eq (Identity a) where
  (Identity_ x) == (Identity_ y) = x == y

instance Ord a => Ord (Identity a) where
  compare (Identity_ x) (Identity_ y) = compare x y

instance (Elt a, Semigroup (Exp a)) => Semigroup (Exp (Identity a)) where
  (Identity_ x) <> (Identity_ y) = Identity_ (x <> y)

instance (Elt a, Monoid (Exp a)) => Monoid (Exp (Identity a)) where
  mempty = Identity_ mempty
