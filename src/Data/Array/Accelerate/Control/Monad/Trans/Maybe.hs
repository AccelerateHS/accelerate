{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RebindableSyntax #-}

--
-- The 'MaybeT' monad transformer extends a monad with the ability to exit
-- the computation without returning a value.
--
-- A sequence of actions produces a value only if all the actions in
-- the sequence do.  If one exits, the rest of the sequence is skipped
-- and the composite action exits.
--
-- For a variant allowing a range of exception values, see
-- "Data.Array.Accelerate.Control.Monad.Trans.Except".

module Data.Array.Accelerate.Control.Monad.Trans.Maybe
  ( MaybeT(..), ExpMaybe
  , mapMaybeT
  , maybeToExceptT, exceptToMaybeT
  ) where

import Data.Array.Accelerate.Control.Monad.Trans.Functor
import Data.Array.Accelerate.Control.Monad.Trans.Monad
import Data.Array.Accelerate.Control.Monad.Trans.Class
import Data.Array.Accelerate.Control.Monad.Trans.Except

import Data.Array.Accelerate.Prelude
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Elt

import Data.Array.Accelerate.Data.Either
import Data.Array.Accelerate.Data.Maybe
import Prelude ( ($), (.), const )
import Data.Function ( (&) )

-- | The parameterizable maybe monad, obtained by composing an arbitrary
-- monad with the 'Maybe' monad.
--
-- Computations are actions that may produce a value or exit.
--
-- The 'return' function yields a computation that produces that
-- value, while @>>=@ sequences two subcomputations, exiting if either
-- computation does.
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Elt (m (Maybe a))) => Elt (MaybeT m a) where
  type EltR (MaybeT m a) = EltR (m (Maybe a))
  eltR = eltR @(m (Maybe a))
  tagsR = tagsR @(m (Maybe a))
  fromElt = fromElt . runMaybeT
  toElt = MaybeT . toElt

instance (Matching (m (Maybe a))) => Matching (MaybeT m a) where
  type ResultT (MaybeT m a) = ResultT (m (Maybe a))
  type ReprT (MaybeT m a) = ReprT (m (Maybe a))
  mkMatch (MaybeT f) = mkMatch f
  mkFun f k = MaybeT $ mkFun f k

-- | The Transformers-Accelerate alias for @MaybeT Exp@: @ExpMaybe a@ ~ @Exp (Maybe a)@.
-- This type is an instance of Data.Array.Accelerate.Control.Monad.Trans.Monad.
type ExpMaybe = MaybeT Exp

instance (Functor m) => Functor (MaybeT m) where
  type FunctorC (MaybeT m) a = (Elt a, FunctorC m (Maybe a))
  fmap f = MaybeT . fmap (maybe Nothing_ (Just_ . f)) . runMaybeT

instance (Monad m) => Monad (MaybeT m) where
  type MonadC (MaybeT m) a = (FunctorC (MaybeT m) a, MonadC m (Maybe a), Matching (m (Maybe a)))
  return = MaybeT . return . Just_

  x >>= f = MaybeT $ do
    v <- runMaybeT x
    v & match \case
        Nothing_ -> return Nothing_
        Just_ y  -> runMaybeT (f y)

instance MonadTrans MaybeT where
  lift = MaybeT . fmap Just_



-- | Transform the computation inside a @MaybeT@.
--
-- * @'runMaybeT' ('mapMaybeT' f m) = f ('runMaybeT' m)@
mapMaybeT :: (m (Maybe a) -> n (Maybe b)) -> MaybeT m a -> MaybeT n b
mapMaybeT f = MaybeT . f . runMaybeT

-- | Convert a 'MaybeT' computation to 'ExceptT', with a default
-- exception value.
maybeToExceptT :: (Functor' m (Maybe a), Functor' m (Either e a), Elt a, Elt e) 
               => Exp e -> MaybeT m a -> ExceptT e m a
maybeToExceptT e (MaybeT m) = ExceptT $ fmap (maybe (Left_ e) Right_) m

-- | Convert a 'ExceptT' computation to 'MaybeT', discarding the
-- value of any exception.
exceptToMaybeT :: (Functor' m (Maybe a), Functor' m (Either e a), Elt e, Elt a) 
               => ExceptT e m a -> MaybeT m a
exceptToMaybeT (ExceptT m) = MaybeT $ fmap (either (const Nothing_) Just_) m
