{-# LANGUAGE RebindableSyntax #-}
-- |
-- Module      : Data.Array.Accelerate.Control.Monad
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

module Data.Array.Accelerate.Control.Monad (

  -- * Monad class
  Monad(..),

  -- * Functions
  -- ** Basic functions
  (=<<), (>>),
  (>=>), (<=<),

  -- ** Conditional execution of monadic expressions
  when, unless,

  -- ** Monadic lifting operations
  liftM, liftM2, liftM3, liftM4, liftM5,

) where

import Data.Array.Accelerate.Data.Functor
import Data.Array.Accelerate.Language

import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Smart

import Prelude                                                      ( Bool, flip )


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
  --
  infixl 1 >>=
  (>>=) :: (Elt a, Elt b, Elt (m a), Elt (m b))
        => Exp (m a)
        -> (Exp a -> Exp (m b))
        -> Exp (m b)

  -- | Inject a value into the monadic type
  --
  return :: (Elt a, Elt (m a)) => Exp a -> Exp (m a)


-- | Same as '>>=', but with the arguments interchanged
--
infixr 1 =<<
(=<<) :: (Monad m, Elt a, Elt b, Elt (m a), Elt (m b))
      => (Exp a -> Exp (m b))
      -> Exp (m a)
      -> Exp (m b)
(=<<) = flip (>>=)

-- | Sequentially compose two actions, discarding any value produced by the
-- first, like sequencing operators (such as the semicolon) in imperative
-- languages.
--
-- \'@as '>>' bs@\' can be understood as the @do@ expression
--
-- @
-- do as
--    bs
-- @
--
infixl 1 >>
(>>) :: (Monad m, Elt a, Elt b, Elt (m a), Elt (m b))
     => Exp (m a)
     -> Exp (m b)
     -> Exp (m b)
m >> k = m >>= \_ -> k


-- | Left-to-right composition of Kleisli arrows.
--
-- \'@(bs '>=>' cs) a@\' can be understood as the @do@ expression
--
-- @
-- do b <- bs a
--    cs b
-- @
--
infixr 1 >=>
(>=>) :: (Monad m, Elt a, Elt b, Elt c, Elt (m b), Elt (m c))
      => (Exp a -> Exp (m b))
      -> (Exp b -> Exp (m c))
      -> (Exp a -> Exp (m c))
f >=> g = \x -> f x >>= g

-- | Right-to-left composition of Kleisli arrows. @('>=>')@, with the arguments
-- flipped.
--
-- Note how this operator resembles function composition @('.')@:
--
-- > (.)   ::            (b ->   c) -> (a ->   b) -> a ->   c
-- > (<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
--
infixr 1 <=<
(<=<) :: (Monad m, Elt a, Elt b, Elt c, Elt (m b), Elt (m c))
      => (Exp b -> Exp (m c))
      -> (Exp a -> Exp (m b))
      -> (Exp a -> Exp (m c))
(<=<) = flip (>=>)


-- | Conditional execution of a monadic expression
--
when :: (Monad m, Elt (m ())) => Exp Bool -> Exp (m ()) -> Exp (m ())
when p s = cond p s (return (constant ()))

-- | The reverse of 'when'
--
unless :: (Monad m, Elt (m ())) => Exp Bool -> Exp (m ()) -> Exp (m ())
unless p s = cond p (return (constant ())) s

-- | Promote a function to a monad
--
liftM :: (Monad m, Elt a, Elt b, Elt (m a), Elt (m b)) => (Exp a -> Exp b) -> Exp (m a) -> Exp (m b)
liftM f m1 = do
  x1 <- m1
  return (f x1)

-- | Promote a function to a monad, scanning the monadic arguments from
-- left to right.
--
liftM2 :: (Monad m, Elt a, Elt b, Elt c, Elt (m a), Elt (m b), Elt (m c))
       => (Exp a -> Exp b -> Exp c)
       -> Exp (m a)
       -> Exp (m b)
       -> Exp (m c)
liftM2 f m1 m2 = do
  x1 <- m1
  x2 <- m2
  return (f x1 x2)

-- | Promote a function to a monad, scanning the monadic arguments from
-- left to right (cf. 'liftM2')
--
liftM3 :: (Monad m, Elt a, Elt b, Elt c, Elt d, Elt (m a), Elt (m b), Elt (m c), Elt (m d))
       => (Exp a -> Exp b -> Exp c -> Exp d)
       -> Exp (m a)
       -> Exp (m b)
       -> Exp (m c)
       -> Exp (m d)
liftM3 f m1 m2 m3 = do
  x1 <- m1
  x2 <- m2
  x3 <- m3
  return (f x1 x2 x3)

-- | Promote a function to a monad, scanning the monadic arguments from
-- left to right (cf. 'liftM2')
--
liftM4 :: (Monad m, Elt a, Elt b, Elt c, Elt d, Elt e, Elt (m a), Elt (m b), Elt (m c), Elt (m d), Elt (m e))
       => (Exp a -> Exp b -> Exp c -> Exp d -> Exp e)
       -> Exp (m a)
       -> Exp (m b)
       -> Exp (m c)
       -> Exp (m d)
       -> Exp (m e)
liftM4 f m1 m2 m3 m4 = do
  x1 <- m1
  x2 <- m2
  x3 <- m3
  x4 <- m4
  return (f x1 x2 x3 x4)

-- | Promote a function to a monad, scanning the monadic arguments from
-- left to right (cf. 'liftM2')
--
liftM5 :: ( Monad m
          , Elt a, Elt b, Elt c, Elt d, Elt e, Elt f
          , Elt (m a), Elt (m b), Elt (m c), Elt (m d), Elt (m e), Elt (m f)
          )
       => (Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f)
       -> Exp (m a)
       -> Exp (m b)
       -> Exp (m c)
       -> Exp (m d)
       -> Exp (m e)
       -> Exp (m f)
liftM5 f m1 m2 m3 m4 m5 = do
  x1 <- m1
  x2 <- m2
  x3 <- m3
  x4 <- m4
  x5 <- m5
  return (f x1 x2 x3 x4 x5)

