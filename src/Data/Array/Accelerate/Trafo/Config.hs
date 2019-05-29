{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo.Config
-- Copyright   : [2008..2019] The Accelerate Team
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
import Data.Array.Accelerate.Debug.Flags                  as F

import Data.Word
import System.IO.Unsafe
import Foreign.Storable


data Config = Config
  { options                   :: {-# UNPACK #-} !(BitSet Word32 Flag)
  , unfolding_use_threshold   :: {-# UNPACK #-} !Int
  , max_simplifier_iterations :: {-# UNPACK #-} !Int
  }
  deriving Show

defaultOptions :: Config
defaultOptions = unsafePerformIO $!
  Config <$> BitSet <$> peek F.__cmd_line_flags
         <*> F.getValue F.unfolding_use_threshold
         <*> F.getValue F.max_simplifier_iterations

