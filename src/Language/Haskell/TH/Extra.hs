{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      : Language.Haskell.TH.Extra
-- Copyright   : [2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Language.Haskell.TH.Extra
  where

import Language.Haskell.TH


tupT :: [TypeQ] -> TypeQ
tupT tup =
  let n = length tup
   in foldl (\ts t -> [t| $ts $t |]) (tupleT n) tup

