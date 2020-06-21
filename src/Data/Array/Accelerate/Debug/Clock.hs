{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# OPTIONS_GHC -fobject-code #-}
-- |
-- Module      : Data.Array.Accelerate.Debug.Clock
-- Copyright   : [2016..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Debug.Clock
  where

import Language.Haskell.TH.Syntax

foreign import ccall unsafe "clock_gettime_monotonic_seconds" getMonotonicTime :: IO Double
foreign import ccall unsafe "clock_gettime_elapsed_seconds"   getProgramTime   :: IO Double

-- SEE: [linking to .c files]
--
runQ $ do
  addForeignFilePath LangC "cbits/clock.c"
  return []

