{-# LANGUAGE TypeOperators        #-}
{-# OPTIONS_HADDOCK hide #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
-- This is needed to derive POSable for tuples of size more then 4
{-# OPTIONS_GHC -fconstraint-solver-iterations=16 #-}
-- |
-- Module      : Data.Array.Accelerate.Representation.POS
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Representation.POS (
  POSable(..), Product(..), Sum(..),
  Ground(..), Finite, ProductType(..), SumType(..), POSable.Generic, type (++),
  mkPOSableGround)
  where


import Data.Type.POSable.POSable as POSable
import Data.Type.POSable.Representation
import Data.Type.POSable.Instances ()
import Data.Type.POSable.TH
