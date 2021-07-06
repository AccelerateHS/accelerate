{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Sugar.Foreign
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Sugar.Foreign
  where

import Data.Array.Accelerate.Error

import Data.Typeable
import Language.Haskell.TH.Extra


-- Class for backends to choose their own representation of foreign functions.
-- By default it has no instances. If a backend wishes to have an FFI it must
-- provide an instance.
--
class Typeable asm => Foreign asm where

  -- Backends should be able to produce a string representation of the foreign
  -- function for pretty printing, typically the name of the function.
  strForeign :: asm args -> String
  strForeign _ = "<foreign>"

  -- Backends which want to support compile-time embedding must be able to lift
  -- the foreign function into Template Haskell
  liftForeign :: HasCallStack => asm args -> CodeQ (asm args)
  liftForeign _ = internalError "not supported by this backend"

