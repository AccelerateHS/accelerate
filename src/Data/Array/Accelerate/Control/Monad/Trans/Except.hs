{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Array.Accelerate.Control.Monad.Trans.Except (
    -- * The Except monad
    Except,
    except,
    runExcept,
    mapExcept,
    withExcept,
    -- * The ExceptT monad transformer
    ExceptT(ExceptT),
    runExceptT,
    mapExceptT,
    withExceptT,
    -- * Exception operations
    throwE,
    catchE
  ) where

import Data.Array.Accelerate.Data.Either
import Data.Array.Accelerate.Smart ( Exp )
import Data.Array.Accelerate.Sugar.Elt ( Elt(..) )
import Data.Array.Accelerate.Prelude ( Matching(..), match )

import Prelude( ($), (.) )
import Data.Function ( (&) )

import Data.Array.Accelerate.Control.Monad.Trans.Class
    ( MonadTrans(..) )
import Data.Array.Accelerate.Control.Monad.Trans.Functor
    (Functor',  Functor(..) )
import Data.Array.Accelerate.Control.Monad.Trans.Monad
    (Monad',  Monad(..) )


-- | The parameterizable exception monad.
--
-- Computations are either exceptions or normal values.
--
-- The 'return' function returns a normal value, while @>>=@ exits on
-- the first exception.  For a variant that continues after an error
-- and collects all the errors, see 'Control.Applicative.Lift.Errors'.
type Except e = ExceptT e Exp

-- | Constructor for computations in the exception monad.
-- (The inverse of 'runExcept').
except :: (Monad' m (Either e a))
       => Exp (Either e a)
       -> ExceptT e m a
except m = ExceptT (return m)

-- | Extractor for computations in the exception monad.
-- (The inverse of 'except').
runExcept :: Except e a -> Exp (Either e a)
runExcept = runExceptT

-- | Map the unwrapped computation using the given function.
--
-- * @'runExcept' ('mapExcept' f m) = f ('runExcept' m)@
mapExcept :: (Exp (Either e a) -> Exp (Either e' b))
          -> Except e a
          -> Except e' b
mapExcept = mapExceptT

-- | Transform any exceptions thrown by the computation using the given
-- function (a specialization of 'withExceptT').
withExcept :: (Elt e, Elt e', Elt a) => (Exp e -> Exp e') -> Except e a -> Except e' a
withExcept = withExceptT

-- | A monad transformer that adds exceptions to other monads.
--
-- @ExceptT@ constructs a monad parameterized over two things:
--
-- * e - The exception type.
--
-- * m - The inner monad.
--
-- The 'return' function yields a computation that produces the given
-- value, while @>>=@ sequences two subcomputations, exiting on the
-- first exception.
newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a)}

-- | Map the unwrapped computation using the given function.
--
-- * @'runExceptT' ('mapExceptT' f m) = f ('runExceptT' m)@
mapExceptT :: (m (Either e a) -> n (Either e' b))
           -> ExceptT e m a
           -> ExceptT e' n b
mapExceptT f m = ExceptT $ f (runExceptT m)

-- | Transform any exceptions thrown by the computation using the
-- given function.
withExceptT :: (Functor m, Functor' (ExceptT e m) a, Functor' (ExceptT e' m) a) 
            => (Exp e -> Exp e') -> ExceptT e m a -> ExceptT e' m a
withExceptT f = mapExceptT $ fmap $ either (Left_ . f) Right_

instance (Elt (m (Either e a))) => Elt (ExceptT e m a) where
  type EltR (ExceptT e m a) = EltR (m (Either e a))
  eltR = eltR @(m (Either e a))
  tagsR = tagsR @(m (Either e a))
  fromElt = fromElt . runExceptT
  toElt = ExceptT . toElt

instance (Matching (m (Either e a))) => Matching (ExceptT e m a) where
  type ResultT (ExceptT e m a) = ResultT (m (Either e a))
  type ReprT (ExceptT e m a) = ReprT (m (Either e a))
  mkMatch (ExceptT f) = mkMatch f
  mkFun f k = ExceptT $ mkFun f k

instance (Functor m, Elt e) => Functor (ExceptT e m) where
  type FunctorC (ExceptT e m) a = (Elt e, Elt a, FunctorC m (Either e a))
  fmap f = ExceptT . fmap (match \case
    Left_ e -> Left_ e
    Right_ a -> Right_ (f a)) . runExceptT

instance (Monad m, Elt e) => Monad (ExceptT e m) where
  type MonadC (ExceptT e m) a = (Elt e, Elt a, Matching (m (Either e a)), MonadC m (Either e a), FunctorC (ExceptT e m) a)
  return a = ExceptT $ return (Right_ a)
  m >>= k = ExceptT $ do
      a <- runExceptT m
      case a of
          Left_ e -> return (Left_ e)
          Right_ x -> runExceptT (k x)


instance Elt e => MonadTrans (ExceptT e) where
  lift = ExceptT . fmap Right_

-- | Signal an exception value @e@.
--
-- * @'runExceptT' ('throwE' e) = 'return' ('Left' e)@
--
-- * @'throwE' e >>= m = 'throwE' e@
throwE :: (Monad m, Monad' (ExceptT e m) a) => Exp e -> ExceptT e m a
throwE = ExceptT . return . Left_

-- | Handle an exception.
--
-- If you run into the Matching constraint, make sure
-- that you have a concrete monad stack and the underlying
-- Monad is Exp. 
-- At least, I guess that's the idea! It's possible that
-- the `newtypes` mess things up, and we'll need to add some
-- deriving clauses to each transformer. 
-- TODO check
--
-- * @'catchE' ('lift' m) h = 'lift' m@
--
-- * @'catchE' ('throwE' e) h = h e@
catchE :: (Monad m, Monad' (ExceptT e m) a, Monad' (ExceptT e' m) a)
       => ExceptT e m a               -- ^ the inner computation
       -> (Exp e -> ExceptT e' m a)   -- ^ a handler for exceptions in the inner computation
       -> ExceptT e' m a
m `catchE` h = ExceptT $ do
    a <- runExceptT m
    a & match \case
      Right_ r -> return $ Right_ r
      Left_ l -> runExceptT (h l)
