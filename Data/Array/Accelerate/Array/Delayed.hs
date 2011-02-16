{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.Array.Accelerate
-- Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
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
  Delayable(delay, force), Delayed(..)

) where

-- friends
import Data.Array.Accelerate.Array.Sugar


-- Delayed arrays are characterised by the domain of an array and its functional
-- representation
-- 

class Delayable a where
  data Delayed a
  delay :: a -> Delayed a
  force :: Delayed a -> a
  
instance Delayable () where
  data Delayed () = DelayedUnit
  delay ()          = DelayedUnit
  force DelayedUnit = ()

instance Delayable (Array sh e) where
  data Delayed (Array sh e) 
    = (Shape sh, Elt e) => 
      DelayedArray { shapeDA :: EltRepr sh
                   , repfDA  :: EltRepr sh -> EltRepr e
                   }
  delay arr@(Array sh _)    = DelayedArray sh (fromElt . (arr!) . toElt)
  force (DelayedArray sh f) = newArray (toElt sh) (toElt . f . fromElt)
  
instance (Delayable a1, Delayable a2) => Delayable (a1, a2) where
  data Delayed (a1, a2) = DelayedPair (Delayed a1) (Delayed a2)
  delay (a1, a2) = DelayedPair (delay a1) (delay a2)
  force (DelayedPair a1 a2) = (force a1, force a2)

