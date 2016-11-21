{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.Fractional
-- Copyright   : [2016] Manuel M T Chakravarty, Gabriele Keller
--               [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Classes.Fractional (

  Fractional,
  (P./), P.recip, P.fromRational,

) where

import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.Classes.Num

import Prelude                                                      ( (.), fromRational )
import qualified Prelude                                            as P


-- | Fractional numbers, supporting real division
--
type Fractional a = (Num a, P.Fractional (Exp a))


instance P.Fractional (Exp Float) where
  (/)          = mkFDiv
  recip        = mkRecip
  fromRational = constant . fromRational

instance P.Fractional (Exp Double) where
  (/)          = mkFDiv
  recip        = mkRecip
  fromRational = constant . fromRational

instance P.Fractional (Exp CFloat) where
  (/)          = mkFDiv
  recip        = mkRecip
  fromRational = constant . fromRational

instance P.Fractional (Exp CDouble) where
  (/)          = mkFDiv
  recip        = mkRecip
  fromRational = constant . fromRational

