{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-orphans         #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.Real
-- Copyright   : [2016..2017] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Classes.Real (

  Real,

) where

import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Classes.Num
import Data.Array.Accelerate.Classes.Ord

import qualified Prelude                                            as P


type Real a = (Num a, Ord a, P.Real (Exp a))

-- Instances of 'Real' don't make sense in Accelerate at the moment. These are
-- only provided to fulfil superclass constraints; e.g. Integral.
--
-- We won't need `toRational' until we support rational numbers in AP
-- computations.
--
instance (Num a, Ord a) => P.Real (Exp a) where
  toRational = P.error "Prelude.toRational not supported for Accelerate types"

