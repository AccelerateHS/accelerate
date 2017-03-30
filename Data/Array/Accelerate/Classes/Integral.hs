{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Classes.Integral
-- Copyright   : [2016..2017] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Classes.Integral (

  Integral,
  P.quot,
  P.rem,
  P.div,
  P.mod,
  P.quotRem,
  P.divMod,

) where

import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.Classes.Enum
import Data.Array.Accelerate.Classes.Real

import Prelude                                                      ( error )
import qualified Prelude                                            as P


-- | Integral numbers, supporting integral division
--
type Integral a = (Enum a, Real a, P.Integral (Exp a))


instance P.Integral (Exp Int) where
  quot      = mkQuot
  rem       = mkRem
  div       = mkIDiv
  mod       = mkMod
  quotRem   = mkQuotRem
  divMod    = mkDivMod
  toInteger = error "Prelude.toInteger not supported for Accelerate types"

instance P.Integral (Exp Int8) where
  quot      = mkQuot
  rem       = mkRem
  div       = mkIDiv
  mod       = mkMod
  quotRem   = mkQuotRem
  divMod    = mkDivMod
  toInteger = error "Prelude.toInteger not supported for Accelerate types"

instance P.Integral (Exp Int16) where
  quot      = mkQuot
  rem       = mkRem
  div       = mkIDiv
  mod       = mkMod
  quotRem   = mkQuotRem
  divMod    = mkDivMod
  toInteger = error "Prelude.toInteger not supported for Accelerate types"

instance P.Integral (Exp Int32) where
  quot      = mkQuot
  rem       = mkRem
  div       = mkIDiv
  mod       = mkMod
  quotRem   = mkQuotRem
  divMod    = mkDivMod
  toInteger = error "Prelude.toInteger not supported for Accelerate types"

instance P.Integral (Exp Int64) where
  quot      = mkQuot
  rem       = mkRem
  div       = mkIDiv
  mod       = mkMod
  quotRem   = mkQuotRem
  divMod    = mkDivMod
  toInteger = error "Prelude.toInteger not supported for Accelerate types"

instance P.Integral (Exp Word) where
  quot      = mkQuot
  rem       = mkRem
  div       = mkIDiv
  mod       = mkMod
  quotRem   = mkQuotRem
  divMod    = mkDivMod
  toInteger = error "Prelude.toInteger not supported for Accelerate types"

instance P.Integral (Exp Word8) where
  quot      = mkQuot
  rem       = mkRem
  div       = mkIDiv
  mod       = mkMod
  quotRem   = mkQuotRem
  divMod    = mkDivMod
  toInteger = error "Prelude.toInteger not supported for Accelerate types"

instance P.Integral (Exp Word16) where
  quot      = mkQuot
  rem       = mkRem
  div       = mkIDiv
  mod       = mkMod
  quotRem   = mkQuotRem
  divMod    = mkDivMod
  toInteger = error "Prelude.toInteger not supported for Accelerate types"

instance P.Integral (Exp Word32) where
  quot      = mkQuot
  rem       = mkRem
  div       = mkIDiv
  mod       = mkMod
  quotRem   = mkQuotRem
  divMod    = mkDivMod
  toInteger = error "Prelude.toInteger not supported for Accelerate types"

instance P.Integral (Exp Word64) where
  quot      = mkQuot
  rem       = mkRem
  div       = mkIDiv
  mod       = mkMod
  quotRem   = mkQuotRem
  divMod    = mkDivMod
  toInteger = error "Prelude.toInteger not supported for Accelerate types"

instance P.Integral (Exp CInt) where
  quot      = mkQuot
  rem       = mkRem
  div       = mkIDiv
  mod       = mkMod
  quotRem   = mkQuotRem
  divMod    = mkDivMod
  toInteger = error "Prelude.toInteger not supported for Accelerate types"

instance P.Integral (Exp CUInt) where
  quot      = mkQuot
  rem       = mkRem
  div       = mkIDiv
  mod       = mkMod
  quotRem   = mkQuotRem
  divMod    = mkDivMod
  toInteger = error "Prelude.toInteger not supported for Accelerate types"

instance P.Integral (Exp CLong) where
  quot      = mkQuot
  rem       = mkRem
  div       = mkIDiv
  mod       = mkMod
  quotRem   = mkQuotRem
  divMod    = mkDivMod
  toInteger = error "Prelude.toInteger not supported for Accelerate types"

instance P.Integral (Exp CULong) where
  quot      = mkQuot
  rem       = mkRem
  div       = mkIDiv
  mod       = mkMod
  quotRem   = mkQuotRem
  divMod    = mkDivMod
  toInteger = error "Prelude.toInteger not supported for Accelerate types"

instance P.Integral (Exp CLLong) where
  quot      = mkQuot
  rem       = mkRem
  div       = mkIDiv
  mod       = mkMod
  quotRem   = mkQuotRem
  divMod    = mkDivMod
  toInteger = error "Prelude.toInteger not supported for Accelerate types"

instance P.Integral (Exp CULLong) where
  quot      = mkQuot
  rem       = mkRem
  div       = mkIDiv
  mod       = mkMod
  quotRem   = mkQuotRem
  divMod    = mkDivMod
  toInteger = error "Prelude.toInteger not supported for Accelerate types"

instance P.Integral (Exp CShort) where
  quot      = mkQuot
  rem       = mkRem
  div       = mkIDiv
  mod       = mkMod
  quotRem   = mkQuotRem
  divMod    = mkDivMod
  toInteger = error "Prelude.toInteger not supported for Accelerate types"

instance P.Integral (Exp CUShort) where
  quot      = mkQuot
  rem       = mkRem
  div       = mkIDiv
  mod       = mkMod
  quotRem   = mkQuotRem
  divMod    = mkDivMod
  toInteger = error "Prelude.toInteger not supported for Accelerate types"

