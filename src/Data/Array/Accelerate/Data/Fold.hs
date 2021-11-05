{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeOperators     #-}
-- |
-- Module      : Data.Array.Accelerate.Data.Fold
-- Copyright   : [2016..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Combine folds in 'Applicative' style to generate multiple results with
-- a single pass over the array. Based on Max Rabkin's "Beautiful Folding" [1]
-- and talks by Gabriel Gonzalez [2].
--
--  1. <http://squing.blogspot.com/2008/11/beautiful-folding.html>
--  2. <https://www.youtube.com/watch?v=6a5Ti0r8Q2s>
--

module Data.Array.Accelerate.Data.Fold (

  Fold(..), runFold,

) where

import Data.Array.Accelerate.Annotations
import Data.Array.Accelerate.Classes.Floating                       as A
import Data.Array.Accelerate.Classes.Fractional                     as A
import Data.Array.Accelerate.Classes.Num                            as A
import Data.Array.Accelerate.Data.Monoid
import Data.Array.Accelerate.Language                               as A
import Data.Array.Accelerate.Lift
import Data.Array.Accelerate.Smart                                  ( Acc, Exp, constant )
import Data.Array.Accelerate.Sugar.Array
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Sugar.Shape

import Prelude                                                      hiding ( sum, product, length )
import Control.Applicative                                          as P
import qualified Prelude                                            as P


-- | 'Fold' describes how to process data of some 'i'nput type into some
-- 'o'utput type, via a reduction using some intermediate Monoid 'w'. For
-- example, both 'sum' and 'length' below use the 'Sum' monoid:
--
-- > sum = Fold (lift . Sum) (getSum . unlift)
-- > length = Fold (\_ -> 1) (getSum . unlift)
--
-- The key is that 'Fold's can be combined using 'Applicative' in order to
-- produce multiple outputs from a /single/ reduction of the array. For example:
--
-- > average = (/) <$> sum <*> length
--
-- This computes both the sum of the array as well as its length in a single
-- traversal, then combines both results to compute the average.
--
-- Because 'Fold' has some numeric instances, this can also be defined more
-- succinctly as:
--
-- > average = sum / length
--
-- A more complex example:
--
-- > sumOfSquares = Fold (lift . Sum . (^2)) (getSum . unlift)
-- > standardDeviation = sqrt ((sumOfSquares / length) - (sum / length) ^ 2)
--
-- These will all execute with a single reduction kernel and a single map to
-- summarise (combine) the results.
--
data Fold i o where
  Fold :: (Elt w, Monoid (Exp w))
       => (i -> Exp w)              -- transform input element into internal monoid type
       -> (Exp w -> o)              -- summarise the reduction to retrieve the final result
       -> Fold i o

-- | Apply a 'Fold' to an array.
--
runFold
    :: (HasCallStack, Shape sh, Elt i, Elt o)
    => Fold (Exp i) (Exp o)
    -> Acc (Array (sh:.Int) i)
    -> Acc (Array sh o)
runFold (Fold tally summarise) is
  = sourceMap
  $ A.map summarise
  $ A.fold mappend mempty
  $ A.map tally is


-- sum :: A.Num e => Fold (Exp e) (Exp e)
-- sum = Fold (lift . Sum) (getSum . unlift)

-- product :: A.Num e => Fold (Exp e) (Exp e)
-- product = Fold (lift . Product) (getProduct . unlift)

-- length :: A.Num i => Fold (Exp e) (Exp i)
-- length = Fold (\_ -> 1) (getSum . unlift)


-- combine2 :: (Elt a, Elt b) => Exp a -> Exp b -> Exp (a,b)
-- combine2 a b = lift (a,b)

-- combine3 :: (Elt a, Elt b, Elt c) => Exp a -> Exp b -> Exp c -> Exp (a,b,c)
-- combine3 a b c = lift (a,b,c)

-- combine4 :: (Elt a, Elt b, Elt c, Elt d) => Exp a -> Exp b -> Exp c -> Exp d -> Exp (a,b,c,d)
-- combine4 a b c d = lift (a,b,c,d)

-- combine5 :: (Elt a, Elt b, Elt c, Elt d, Elt e) => Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp (a,b,c,d,e)
-- combine5 a b c d e = lift (a,b,c,d,e)


-- Instances for 'Fold'
-- --------------------

instance P.Functor (Fold i) where
  fmap k (Fold tally summarise) = sourceMapRuntime $ Fold tally (k . summarise)

instance P.Applicative (Fold i) where
  pure o                    = sourceMapRuntime $ Fold (\_ -> constant ()) (\_ -> o)
  Fold tF sF <*> Fold tX sX = sourceMapRuntime
    $ let tally i     = lift (tF i, tX i)
          summarise t = let (mF, mX) = unlift t
                        in sF mF (sX mX)
       in Fold tally summarise

instance A.Num b => P.Num (Fold a (Exp b)) where
  (+)           = sourceMapRuntime $ liftA2 (+)
  (-)           = sourceMapRuntime $ liftA2 (-)
  (*)           = sourceMapRuntime $ liftA2 (*)
  negate        = sourceMapRuntime $ fmap negate
  abs           = sourceMapRuntime $ fmap abs
  signum        = sourceMapRuntime $ fmap signum
  fromInteger n = sourceMapRuntime $ pure (A.fromInteger n)

instance A.Fractional b => P.Fractional (Fold a (Exp b)) where
  (/)            = sourceMapRuntime $ liftA2 (/)
  recip          = sourceMapRuntime $ fmap recip
  fromRational n = sourceMapRuntime $ pure (A.fromRational n)

instance A.Floating b => P.Floating (Fold a (Exp b)) where
  pi      = sourceMapRuntime $ pure pi
  sin     = sourceMapRuntime $ fmap sin
  cos     = sourceMapRuntime $ fmap cos
  tan     = sourceMapRuntime $ fmap tan
  asin    = sourceMapRuntime $ fmap asin
  acos    = sourceMapRuntime $ fmap acos
  atan    = sourceMapRuntime $ fmap atan
  sinh    = sourceMapRuntime $ fmap sinh
  cosh    = sourceMapRuntime $ fmap cosh
  tanh    = sourceMapRuntime $ fmap tanh
  asinh   = sourceMapRuntime $ fmap asinh
  acosh   = sourceMapRuntime $ fmap acosh
  atanh   = sourceMapRuntime $ fmap atanh
  exp     = sourceMapRuntime $ fmap exp
  sqrt    = sourceMapRuntime $ fmap sqrt
  log     = sourceMapRuntime $ fmap log
  (**)    = sourceMapRuntime $ liftA2 (**)
  logBase = sourceMapRuntime $ liftA2 logBase
