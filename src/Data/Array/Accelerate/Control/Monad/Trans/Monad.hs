{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds              #-}
{-# LANGUAGE TypeFamilies                 #-}
module Data.Array.Accelerate.Control.Monad.Trans.Monad where

import Data.Array.Accelerate.Smart ( Exp )
import Prelude ()
import GHC.Exts (Constraint)

import Data.Array.Accelerate.Control.Monad.Trans.Functor
    ( Functor(FunctorC) )
import Data.Kind (Type)

-- | Note that @-XRebindableSyntax@ desugars to @fail@ even
-- on complete patterns. This means that you might need to 
-- use irrefutable patterns (@~(T2 x y) <- foo@) instead.
-- see https://gitlab.haskell.org/ghc/ghc/-/issues/13649,
-- and https://gitlab.haskell.org/ghc/ghc/-/issues/16618
-- TODO move this comment to some relevant place
class Functor m => Monad m where
  type MonadC m o :: Constraint
  return :: (MonadC m a) => Exp a -> m a
  (>>=) :: (MonadC m a, MonadC m b) => m a -> (Exp a -> m b) -> m b

type Monad' :: (Type -> Type) -> Type -> Constraint
type Monad' m a = (Monad m, MonadC m a)

(>>) :: (Monad' m a, Monad' m b) => m a -> m b -> m b
x >> y = x >>= \_ -> y

instance Monad Exp where
  type MonadC Exp a = FunctorC Exp a -- = Elt a 
  return x = x
  x >>= f = f x
