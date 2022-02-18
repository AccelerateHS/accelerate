{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# OPTIONS_GHC -fobject-code #-}
-- |
-- Module      : Data.Array.Accelerate.Debug.Internal.Clock
-- Copyright   : [2016..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Debug.Internal.Clock
  where

import Language.Haskell.TH.Syntax


-- SEE: [HLS and GHC IDE]
--
#ifndef __GHCIDE__

foreign import ccall unsafe "clock_gettime_monotonic_seconds" getMonotonicTime :: IO Double
foreign import ccall unsafe "clock_gettime_elapsed_seconds"   getProgramTime   :: IO Double

#else

getMonotonicTime :: IO Double
getMonotonicTime = undefined

getProgramTime :: IO Double
getProgramTime = undefined

#endif

-- SEE: [linking to .c files]
--
runQ $ do
  addForeignFilePath LangC "cbits/clock.c"
  return []
