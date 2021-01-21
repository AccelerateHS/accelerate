{-# LANGUAGE RebindableSyntax #-}
-- |
-- Module      : Data.Array.Accelerate.Data.Functor
-- Copyright   : [2018..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- A functor performs a uniform action over a parameterised type
--
-- This is essentially the same as the standard Haskell 'Prelude.Functor' class,
-- lifted to Accelerate 'Exp' terms.
--
-- @since 1.2.0.0
--

module Data.Array.Accelerate.Data.Monad (

  Monad(..)

) where

import Data.Array.Accelerate.Data.Functor

import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Smart

import qualified Prelude as P


-- | The 'Functor' class is used for scalar types which can be mapped over.
-- Instances of 'Functor' should satisfy the following laws:
--
-- > fmap id      == id
-- > fmap (f . g) == fmap f . fmap g
--
class Functor m => Monad m where
  -- | Sequentially compose two actions, passing any value produced
  -- by the first as an argument to the second.
  --
  -- \'@as '>>=' bs@\' can be understood as the @do@ expression
  --
  -- @
  -- do a <- as
  --    bs a
  -- @
  (>>=) :: (Elt a, Elt b, Elt (m a), Elt (m b)) => Exp (m a) -> (Exp a -> Exp (m b)) -> Exp (m b)
  (>>=) = P.flip (=<<)

  (=<<) :: (Elt a, Elt b, Elt (m a), Elt (m b)) => (Exp a -> Exp (m b)) -> Exp (m a) -> Exp (m b)
  (=<<) = P.flip (>>=)

  -- | Sequentially compose two actions, discarding any value produced
  -- by the first, like sequencing operators (such as the semicolon)
  -- in imperative languages.
  --
  -- \'@as '>>' bs@\' can be understood as the @do@ expression
  --
  -- @
  -- do as
  --    bs
  -- @
  (>>) :: (Elt a, Elt b, Elt (m a), Elt (m b)) => Exp (m a) -> Exp (m b) -> Exp (m b)
  m >> k = m >>= \_ -> k
  {-# INLINE (>>) #-}

  -- | Inject a value into the monadic type.
  return :: (Elt a, Elt (m a)) => Exp a -> Exp (m a)
  return = pure

  -- | Inject a value into the monadic type.
  pure :: (Elt a, Elt (m a)) => Exp a -> Exp (m a)
  pure = return

  {-# MINIMAL ((>>=) | (=<<)), (return | pure) #-}