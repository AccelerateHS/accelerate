{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo.Config
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Trafo.Config (

  Config(..),
  Flag(..),
  defaultOptions,

) where

import Data.BitSet
import Data.Array.Accelerate.Debug.Internal.Flags                   as F

import Data.Word
import System.IO.Unsafe
import Foreign.Storable


data Config = Config
  { options                 :: {-# UNPACK #-} !(BitSet Word32 Flag)
  }
  deriving Show

{-# NOINLINE defaultOptions #-}
defaultOptions :: Config
defaultOptions = unsafePerformIO $!
  Config <$> BitSet <$> peek F.__cmd_line_flags

