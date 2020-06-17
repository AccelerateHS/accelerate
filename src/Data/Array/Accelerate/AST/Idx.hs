{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.AST.Idx
-- Copyright   : [2008..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Typed de Bruijn indices
--

module Data.Array.Accelerate.AST.Idx
  where

import Language.Haskell.TH

-- De Bruijn variable index projecting a specific type from a type
-- environment.  Type environments are nested pairs (..((), t1), t2, ..., tn).
--
data Idx env t where
  ZeroIdx ::              Idx (env, t) t
  SuccIdx :: Idx env t -> Idx (env, s) t

data PairIdx p a where
  PairIdxLeft  :: PairIdx (a, b) a
  PairIdxRight :: PairIdx (a, b) b


idxToInt :: Idx env t -> Int
idxToInt ZeroIdx       = 0
idxToInt (SuccIdx idx) = 1 + idxToInt idx

rnfIdx :: Idx env t -> ()
rnfIdx ZeroIdx      = ()
rnfIdx (SuccIdx ix) = rnfIdx ix

liftIdx :: Idx env t -> Q (TExp (Idx env t))
liftIdx ZeroIdx      = [|| ZeroIdx ||]
liftIdx (SuccIdx ix) = [|| SuccIdx $$(liftIdx ix) ||]

