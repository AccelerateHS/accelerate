{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
-- Module      : Data.Array.Accelerate.Array.Delayed
-- Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
--               [2009..2013] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Delayed arrays are represented by their representation function, which enables the simple
-- composition of many array operations.
--

module Data.Array.Accelerate.Array.Delayed (

  -- * Delayed array interface
  Delayed, DelayedR(..), delay, force,

) where

-- friends
import Data.Array.Accelerate.Array.Sugar


type Delayed a = DelayedR (ArrRepr a)


delay :: Arrays a => a -> Delayed a
delay arr = go (arrays arr) (fromArr arr)
  where
    go :: ArraysR a -> a -> DelayedR a
    go ArraysRunit         ()       = DelayedRunit
    go ArraysRarray        a        = delayR a
    go (ArraysRpair r1 r2) (a1, a2) = DelayedRpair (go r1 a1) (go r2 a2)


force :: forall a. Arrays a => Delayed a -> a
force arr = toArr $ go (arrays (undefined::a)) arr
  where
    go :: ArraysR a' -> DelayedR a' -> a'
    go ArraysRunit         DelayedRunit         = ()
    go ArraysRarray        a                    = forceR a
    go (ArraysRpair r1 r2) (DelayedRpair d1 d2) = (go r1 d1, go r2 d2)


-- Delayed arrays are characterised by the domain of an array and its functional
-- representation
--
class Delayable a where
  data DelayedR a
  delayR :: a -> DelayedR a
  forceR :: DelayedR a -> a

instance Delayable () where
  data DelayedR () = DelayedRunit
  delayR ()           = DelayedRunit
  forceR DelayedRunit = ()

instance Delayable (Array sh e) where
  data DelayedR (Array sh e)
    = (Shape sh, Elt e) =>
      DelayedRarray { shapeDA :: EltRepr sh
                    , repfDA  :: EltRepr sh -> EltRepr e
                    }
  delayR arr@(Array sh _)    = DelayedRarray sh (fromElt . (arr!) . toElt)
  forceR (DelayedRarray sh f) = newArray (toElt sh) (toElt . f . fromElt)

instance (Delayable a1, Delayable a2) => Delayable (a1, a2) where
  data DelayedR (a1, a2) = DelayedRpair (DelayedR a1) (DelayedR a2)
  delayR (a1, a2) = DelayedRpair (delayR a1) (delayR a2)
  forceR (DelayedRpair a1 a2) = (forceR a1, forceR a2)

