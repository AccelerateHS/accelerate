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

  -- Other options not controlled by the command line flags
  -- float_out_acc,

) where

import Data.Bits
import Data.BitSet
import Data.Array.Accelerate.Debug.Internal.Flags                   as F

import Data.Word
import System.IO.Unsafe
import Foreign.Storable


data Config = Config
  { options                   :: {-# UNPACK #-} !(BitSet Word32 Flag)
  , unfolding_use_threshold   :: {-# UNPACK #-} !Int
  , max_simplifier_iterations :: {-# UNPACK #-} !Int
  }
  deriving Show

{-# NOINLINE defaultOptions #-}
defaultOptions :: Config
defaultOptions = unsafePerformIO $!
  Config <$> (BitSet . (0x80000000 .|.)) <$> peek F.__cmd_line_flags
         <*> (fromIntegral <$> F.getValue F.unfolding_use_threshold)
         <*> (fromIntegral <$> F.getValue F.max_simplifier_iterations)

-- Extra options not covered by command line flags
--
-- float_out_acc          = Flag 31

