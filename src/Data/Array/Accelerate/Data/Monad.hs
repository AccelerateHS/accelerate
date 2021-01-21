-- |
-- Module      : Data.Array.Accelerate.Data.Monad
-- Copyright   : [2018..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- A monad sequences actions over a parametrised type.
--
-- This is essentially the same as the standard Haskell 'Control.Monad' class,
-- lifted to Accelerate 'Exp' terms.
--
-- @since 1.4.0.0
--

module Data.Array.Accelerate.Data.Monad (

  Monad(..)

) where

import Data.Array.Accelerate.Data.Functor

import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Smart

import qualified Prelude as P


-- | The 'Monad' class is used for scalar types which can be sequenced.
-- Instances of 'Monad' should satisfy the following laws:
--
-- [Left identity]  @'return' a '>>=' k  =  k a@
-- [Right identity] @m '>>=' 'return'  =  m@
-- [Associativity]  @m '>>=' (\\x -> k x '>>=' h)  =  (m '>>=' k) '>>=' h@
-- 
-- Furthermore, the 'Monad' and 'Functor' operations should relate as follows:
-- * @'fmap' f xs  =  xs '>>=' 'return' . f@
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