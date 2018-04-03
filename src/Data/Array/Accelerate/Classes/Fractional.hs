{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.Fractional
-- Copyright   : [2016..2017] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Classes.Fractional (

  Fractional,
  (P./), P.recip, fromRational,

) where

import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.Classes.Num

import Prelude                                                      ( Rational, (.) )
import qualified Prelude                                            as P


-- | Conversion from a 'Rational'.
--
-- A floating point literal representations the application of the function
-- 'fromRational' to a value of type 'Rational'. We export this specialised
-- version where the return type is fixed to an 'Exp' term in order to improve
-- type checking in Accelerate modules when @RebindableSyntax@ is enabled.
--
fromRational :: Fractional a => Rational -> Exp a
fromRational = P.fromRational


-- | Fractional numbers, supporting real division
--
type Fractional a = (Num a, P.Fractional (Exp a))


instance P.Fractional (Exp Half) where
  (/)          = mkFDiv
  recip        = mkRecip
  fromRational = constant . P.fromRational

instance P.Fractional (Exp Float) where
  (/)          = mkFDiv
  recip        = mkRecip
  fromRational = constant . P.fromRational

instance P.Fractional (Exp Double) where
  (/)          = mkFDiv
  recip        = mkRecip
  fromRational = constant . P.fromRational

instance P.Fractional (Exp CFloat) where
  (/)          = mkFDiv
  recip        = mkRecip
  fromRational = constant . P.fromRational

instance P.Fractional (Exp CDouble) where
  (/)          = mkFDiv
  recip        = mkRecip
  fromRational = constant . P.fromRational

