-- |
-- Module      : Data.Array.Accelerate.Analysis.Hash.TH
-- Copyright   : [2017..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Analysis.Hash.TH (

  hashQ,
  hashWithSaltQ,

) where

import Data.Hashable
import Language.Haskell.TH


hashQ :: Hashable a => a -> ExpQ
hashQ = intE . hash

hashWithSaltQ :: Hashable a => Int -> a -> ExpQ
hashWithSaltQ s x = intE (hashWithSalt s x)

intE :: Int -> ExpQ
intE = litE . integerL . fromIntegral

