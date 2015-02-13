{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Debug
-- Copyright   : [2008..2014] Manuel M T Chakravarty, Gabriele Keller
--               [2009..2014] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Embedded array processing language: debugging support (internal). This module
-- provides functionality that is useful for developers of the library. It is
-- not meant for library users.
--

module Data.Array.Accelerate.Debug ( module Debug, when )
  where

import Data.Array.Accelerate.Debug.Flags        as Debug
import Data.Array.Accelerate.Debug.Stats        as Debug
import Data.Array.Accelerate.Debug.Trace        as Debug

import Control.Monad.IO.Class

-- | Conditional execution of a monadic expression
--
when :: MonadIO m => Mode -> m () -> m ()
when f s = do
  yes <- liftIO $ queryFlag f
  if yes then s else return ()
