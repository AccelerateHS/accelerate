{-# LANGUAGE GADTs           #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.AST.LeftHandSide
-- Copyright   : [2008..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.AST.LeftHandSide
  where

import Data.Array.Accelerate.Representation.Type

import Language.Haskell.TH


data Exists f where
  Exists :: f a -> Exists f

data LeftHandSide s v env env' where
  LeftHandSideSingle
    :: s v
    -> LeftHandSide s v env (env, v)

  LeftHandSideWildcard
    :: TupR s v
    -> LeftHandSide s v env env

  LeftHandSidePair
    :: LeftHandSide s v1       env  env'
    -> LeftHandSide s v2       env' env''
    -> LeftHandSide s (v1, v2) env  env''

pattern LeftHandSideUnit
    :: ()                   -- required
    => (env' ~ env, v ~ ()) -- provided
    => LeftHandSide s v env env'
pattern LeftHandSideUnit = LeftHandSideWildcard TupRunit

lhsToTupR :: LeftHandSide s arrs aenv aenv' -> TupR s arrs
lhsToTupR (LeftHandSideSingle s)   = TupRsingle s
lhsToTupR (LeftHandSideWildcard r) = r
lhsToTupR (LeftHandSidePair as bs) = TupRpair (lhsToTupR as) (lhsToTupR bs)

rnfLeftHandSide :: (forall b. s b -> ()) -> LeftHandSide s arrs env env' -> ()
rnfLeftHandSide f (LeftHandSideWildcard r) = rnfTupR f r
rnfLeftHandSide f (LeftHandSideSingle s)   = f s
rnfLeftHandSide f (LeftHandSidePair as bs) = rnfLeftHandSide f as `seq` rnfLeftHandSide f bs

liftLeftHandSide :: (forall u. s u -> Q (TExp (s u))) -> LeftHandSide s v env env' -> Q (TExp (LeftHandSide s v env env'))
liftLeftHandSide f (LeftHandSideSingle s)   = [|| LeftHandSideSingle $$(f s) ||]
liftLeftHandSide f (LeftHandSideWildcard r) = [|| LeftHandSideWildcard $$(liftTupR f r) ||]
liftLeftHandSide f (LeftHandSidePair as bs) = [|| LeftHandSidePair $$(liftLeftHandSide f as) $$(liftLeftHandSide f bs) ||]

