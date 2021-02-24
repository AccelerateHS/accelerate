{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RebindableSyntax     #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE TypeFamilies     #-}

module Data.Array.Accelerate.Control.Monad.Trans.State
  ( StateT(..), State
  , get, put, modify, gets
  , state, runState
  , evalState,  execState,  mapState,  withState
  , evalStateT, execStateT, mapStateT, withStateT
  ) where

import Data.Array.Accelerate.Prelude ( fst, snd, Matching(..) )
import Data.Array.Accelerate.Pattern ( pattern T2 )
import Data.Array.Accelerate.Smart ( Exp, constant )
import Data.Array.Accelerate.Sugar.Elt ( Elt )
import Prelude ((.),  ($) )


import Data.Array.Accelerate.Control.Monad.Trans.Class   ( MonadTrans(..) )
import Data.Array.Accelerate.Control.Monad.Trans.Functor ( Functor(..) )
import Data.Array.Accelerate.Control.Monad.Trans.Monad   (Monad',  Monad(..) )

-- | A state transformer monad parameterized by:
--
--   * @s@ - The state.
--
--   * @m@ - The inner monad.
--
-- The 'return' function leaves the state unchanged, while @>>=@ uses
-- the final state of the first computation as the initial state of
-- the second.
newtype StateT s m a = StateT { runStateT :: Exp s -> m (a,s) }

instance (Matching (Exp s -> m (a,s))) => Matching (StateT s m a) where
  type ResultT (StateT s m a) = ResultT (Exp s -> m (a,s))
  type ReprT (StateT s m a) = ReprT (Exp s -> m (a,s))
  mkMatch (StateT f) = mkMatch f
  mkFun f k = StateT $ mkFun f k

type State s = StateT s Exp


instance (Functor m, Elt s) => Functor (StateT s m) where
  type FunctorC (StateT s m) a = (Elt a, FunctorC m (a, s))
  fmap f m = StateT $ \s ->
    fmap (\(T2 a s') -> T2 (f a) s') $ runStateT m s

instance (Monad m, Elt s) => Monad (StateT s m) where
  type MonadC (StateT s m) a = (FunctorC (StateT s m) a, MonadC m a, MonadC m (a,s), MonadC m s, Matching (Exp s -> m (a,s)))
  return a = StateT $ \s ->
    return $ T2 a s

  x >>= f = StateT $ \s ->
    runStateT x s >>= \(T2 a s') -> runStateT (f a) s'

instance Elt s => MonadTrans (StateT s) where
  lift m = StateT $ \s ->
    fmap (\a -> T2 a s) m


-- | Fetch the current value of the state within the monad.
get :: (Monad m, Monad' (StateT s m) s, Elt s) => StateT s m s
get = state $ \ s -> T2 s s

-- | @'put' s@ sets the state within the monad to @s@.
put :: (Monad m, Monad' (StateT s m) (), Elt s) => Exp s -> StateT s m ()
put s = state $ \ _ -> T2 (constant ()) s

-- | @'modify' f@ is an action that updates the state to the result of
-- applying @f@ to the current state.
--
-- * @'modify' f = 'get' >>= ('put' . f)@
modify :: (Monad m, Monad' (StateT s m) (), Elt s) => (Exp s -> Exp s) -> StateT s m ()
modify f = state $ \ s -> T2 (constant ()) (f s)

-- | Get a specific component of the state, using a projection function
-- supplied.
--
-- * @'gets' f = 'liftM' f 'get'@
gets :: (Monad m, Monad' (StateT s m) a, Elt s) => (Exp s -> Exp a) -> StateT s m a
gets f = state $ \ s -> T2 (f s) s

-- | Construct a state monad computation from a function.
-- (The inverse of 'runState'.)
state :: (Monad m, Monad' (StateT s m) a)
      => (Exp s -> Exp (a, s))  -- ^pure state transformer
      -> StateT s m a   -- ^equivalent state-passing computation
state f = StateT (return . f)

-- | Unwrap a state monad computation as a function.
-- (The inverse of 'state'.)
runState :: (Elt a, Elt s)
         => State s a   -- ^state-passing computation to execute
         -> Exp s       -- ^initial state
         -> Exp (a, s)  -- ^return value and final state
runState = runStateT

-- | Evaluate a state computation with the given initial state
-- and return the final value, discarding the final state.
--
-- * @'evalState' m s = 'fst' ('runState' m s)@
evalState :: (Elt a, Elt s)
          => State s a  -- ^state-passing computation to execute
          -> Exp s      -- ^initial value
          -> Exp a      -- ^return value of the state computation
evalState m s = fst (runState m s)

-- | Evaluate a state computation with the given initial state
-- and return the final state, discarding the final value.
--
-- * @'execState' m s = 'snd' ('runState' m s)@
execState :: (Elt a, Elt s)
          => State s a  -- ^state-passing computation to execute
          -> Exp s      -- ^initial value
          -> Exp s      -- ^final state
execState m s = snd (runState m s)

-- | Map both the return value and final state of a computation using
-- the given function.
--
-- * @'runState' ('mapState' f m) = f . 'runState' m@
mapState :: (Elt a, Elt s) => (Exp (a, s) -> Exp (b, s)) -> State s a -> State s b
mapState = mapStateT

-- | @'withState' f m@ executes action @m@ on a state modified by
-- applying @f@.
--
-- * @'withState' f m = 'modify' f >> m@
withState :: (Elt a, Elt s) => (Exp s -> Exp s) -> State s a -> State s a
withState = withStateT


-- | Evaluate a state computation with the given initial state
-- and return the final value, discarding the final state.
--
-- * @'evalStateT' m s = 'liftM' 'fst' ('runStateT' m s)@
evalStateT :: (Monad m, Monad' (StateT s m) a, Elt a, Elt s) => StateT s m a -> Exp s -> m a
evalStateT m s = do
    ~(T2 a _) <- runStateT m s
    return a

-- | Evaluate a state computation with the given initial state
-- and return the final state, discarding the final value.
--
-- * @'execStateT' m s = 'liftM' 'snd' ('runStateT' m s)@
execStateT :: (Monad m, Monad' (StateT s m) a, Elt a, Elt s) => StateT s m a -> Exp s -> m s
execStateT m s = do
    ~(T2 _ s') <- runStateT m s
    return s'

-- | Map both the return value and final state of a computation using
-- the given function.
--
-- * @'runStateT' ('mapStateT' f m) = f . 'runStateT' m@
mapStateT :: (m (a, s) -> n (b, s)) -> StateT s m a -> StateT s n b
mapStateT f m = StateT $ f . runStateT m

-- | @'withStateT' f m@ executes action @m@ on a state modified by
-- applying @f@.
--
-- * @'withStateT' f m = 'modify' f >> m@
withStateT :: Elt s => (Exp s -> Exp s) -> StateT s m a -> StateT s m a
withStateT f m = StateT $ runStateT m . f
