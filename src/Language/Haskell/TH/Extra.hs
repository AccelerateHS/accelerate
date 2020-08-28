{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      : Language.Haskell.TH.Extra
-- Copyright   : [2019..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Language.Haskell.TH.Extra
  where

import Language.Haskell.TH                                          hiding ( tupP, tupE )
import qualified Language.Haskell.TH                                as TH


tupT :: [TypeQ] -> TypeQ
tupT [t] = t
tupT tup =
  let n = length tup
   in foldl (\ts t -> [t| $ts $t |]) (tupleT n) tup

tupP :: [PatQ] -> PatQ
tupP [p] = p
tupP ps  = TH.tupP ps

tupE :: [ExpQ] -> ExpQ
tupE [t] = t
tupE ts  = TH.tupE ts

tyVarBndrName :: TyVarBndr -> Name
tyVarBndrName (PlainTV  n)   = n
tyVarBndrName (KindedTV n _) = n

