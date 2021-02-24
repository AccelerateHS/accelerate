{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ConstraintKinds              #-}
{-# LANGUAGE TypeFamilies                 #-}
module Data.Array.Accelerate.Control.Monad.Trans.Functor where

import Data.Array.Accelerate.Smart ( Exp )
import Data.Array.Accelerate.Sugar.Elt ( Elt )
import Prelude ()
import GHC.Exts (Constraint)
import Data.Kind (Type)

class Functor f where
  type FunctorC f o :: Constraint
  fmap  :: (FunctorC f a, FunctorC f b) => (Exp a -> Exp b) -> f a -> f b

type Functor' :: (Type -> Type) -> Type -> Constraint
type Functor' f a = (Functor f, FunctorC f a)

(<$>) :: (Functor' f a, Functor' f b) => (Exp a -> Exp b) -> f a -> f b
(<$>) = fmap

instance Functor Exp where
  type FunctorC Exp a = Elt a
  fmap f = f
