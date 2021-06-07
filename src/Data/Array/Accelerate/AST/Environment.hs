{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.AST.Environment
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.AST.Environment
  where

import Data.Array.Accelerate.AST.Idx
import Data.Array.Accelerate.AST.LeftHandSide
import Data.Array.Accelerate.Error


-- Valuation for an environment
--
data Val env where
  Empty :: Val ()
  Push  :: Val env -> t -> Val (env, t)

-- Push a set of variables into an environment
--
push :: Val env -> (LeftHandSide s t env env', t) -> Val env'
push env (LeftHandSideWildcard _, _     ) = env
push env (LeftHandSideSingle _  , a     ) = env `Push` a
push env (LeftHandSidePair l1 l2, (a, b)) = push env (l1, a) `push` (l2, b)

-- Projection of a value from a valuation using a de Bruijn index
--
prj :: Idx env t -> Val env -> t
prj ZeroIdx       (Push _   v) = v
prj (SuccIdx idx) (Push val _) = prj idx val


-- The type of shifting terms from one context into another
--
-- This is defined as a newtype, as a type synonym containing a forall
-- quantifier may give issues with impredicative polymorphism, which GHC
-- does not support.
--
newtype env :> env' = Weaken { (>:>) :: forall t'. Idx env t' -> Idx env' t' } -- Weak or Weaken

weakenId :: env :> env
weakenId = Weaken id

weakenSucc' :: env :> env' -> env :> (env', t)
weakenSucc' (Weaken f) = Weaken (SuccIdx . f)

weakenSucc :: (env, t) :> env' -> env :> env'
weakenSucc (Weaken f) = Weaken (f . SuccIdx)

weakenEmpty :: () :> env'
weakenEmpty = Weaken $ \(VoidIdx x) -> x

sink :: forall env env' t. env :> env' -> (env, t) :> (env', t)
sink (Weaken f) = Weaken g
  where
    g :: Idx (env, t) t' -> Idx (env', t) t'
    g ZeroIdx      = ZeroIdx
    g (SuccIdx ix) = SuccIdx $ f ix

infixr 9 .>
(.>) :: env2 :> env3 -> env1 :> env2 -> env1 :> env3
(.>) (Weaken f) (Weaken g) = Weaken (f . g)

sinkWithLHS :: HasCallStack => LeftHandSide s t env1 env1' -> LeftHandSide s t env2 env2' -> env1 :> env2 -> env1' :> env2'
sinkWithLHS (LeftHandSideWildcard _) (LeftHandSideWildcard _) k = k
sinkWithLHS (LeftHandSideSingle _)   (LeftHandSideSingle _)   k = sink k
sinkWithLHS (LeftHandSidePair a1 b1) (LeftHandSidePair a2 b2) k = sinkWithLHS b1 b2 $ sinkWithLHS a1 a2 k
sinkWithLHS _ _ _ = internalError "left hand sides do not match"

weakenWithLHS :: forall s t env env'. LeftHandSide s t env env' -> env :> env'
weakenWithLHS = go weakenId
  where
    go :: env2 :> env' -> LeftHandSide s arrs env1 env2 -> env1 :> env'
    go k (LeftHandSideWildcard _) = k
    go k (LeftHandSideSingle _)   = weakenSucc k
    go k (LeftHandSidePair l1 l2) = go (go k l2) l1

