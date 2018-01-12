{-# LANGUAGE RebindableSyntax #-}
-- |
-- Module      : Data.Array.Accelerate.Data.Functor
-- Copyright   : [2018] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- A functor performs a uniform action over a parameterised type
--
-- This is essentially the same as the standard Haskell 'Prelude.Functor' class,
-- lifted to Accelarete 'Exp' terms.
--
-- @since 1.2.0.0
--

module Data.Array.Accelerate.Data.Functor (

  Functor(..),
  (<$>),
  ($>),
  void,

) where

import Data.Array.Accelerate

import Data.Monoid
import Data.Semigroup
import Prelude                                                      ( flip )


-- | The 'Functor' class is used for scalar types which can be mapped over.
-- Instances of 'Functor' should satisfy the following laws:
--
-- > fmap id      == id
-- > fmap (f . g) == fmap f . fmap g
--
class Functor f where
  fmap :: (Elt a, Elt b) => (Exp a -> Exp b) -> Exp (f a) -> Exp (f b)

  -- | Replace all locations in the input with the same value. The default
  -- definition is @fmap . const@, but this may be overridden with a more
  -- efficient version.
  --
  infixl 4 <$
  (<$) :: (Elt a, Elt b) => Exp a -> Exp (f b) -> Exp (f a)
  (<$) = fmap . const


-- | An infix synonym for 'fmap'
--
-- The name of this operator is an allusion to 'Prelude.$'. Note the
-- similarities between their types:
--
-- >  ($)  ::              (Exp a -> Exp b) -> Exp a     -> Exp b
-- > (<$>) :: Functor f => (Exp a -> Exp b) -> Exp (f a) -> Exp (f b)
--
-- Whereas 'Prelude.$' is function application, '<$>' is function application
-- lifted over a 'Functor'.
--
infixl 4 <$>
(<$>) :: (Functor f, Elt a, Elt b) => (Exp a -> Exp b) -> Exp (f a) -> Exp (f b)
(<$>) = fmap


-- | A flipped version of '(<$)'.
--
infixl 4 $>
($>) :: (Functor f, Elt a, Elt b) => Exp (f a) -> Exp b -> Exp (f b)
($>) = flip (<$)


-- | @'void' value@ discards or ignores the result of evaluation.
--
void :: (Functor f, Elt a) => Exp (f a) -> Exp (f ())
void x = constant () <$ x


instance Functor Sum where
  fmap f = lift1 (fmap f)

instance Functor Product where
  fmap f = lift1 (fmap f)

instance Functor Min where
  fmap f = lift1 (fmap f)

instance Functor Max where
  fmap f = lift1 (fmap f)

