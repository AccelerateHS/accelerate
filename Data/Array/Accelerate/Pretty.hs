{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Pretty
-- Copyright   : [2008..2014] Manuel M T Chakravarty, Gabriele Keller
--               [2008..2009] Sean Lee
--               [2009..2014] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Pretty (

  -- * Pretty printing functions
  module Data.Array.Accelerate.Pretty.Print,
  module Data.Array.Accelerate.Pretty.Graphviz,

  -- * Instances of Show

) where

-- standard libraries
import Text.PrettyPrint

-- friends
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Trafo.Base
import Data.Array.Accelerate.Pretty.Print
import Data.Array.Accelerate.Pretty.Graphviz

-- |Show instances
-- ---------------

wide :: Style
wide = style { lineLength = 150 }

-- Explicitly enumerate Show instances for the Accelerate array AST types. If we
-- instead use a generic instance of the form:
--
--   instance Kit acc => Show (acc aenv a) where
--
-- This matches any type of kind (* -> * -> *), which can cause problems
-- interacting with other packages. See Issue #108.
--
instance PrettyEnv aenv => Show (OpenAcc aenv a) where
  show c = renderStyle wide $ prettyAcc noParens prettyEnv c

instance PrettyEnv aenv => Show (DelayedOpenAcc aenv a) where
  show c = renderStyle wide $ prettyAcc noParens prettyEnv c

-- These parameterised instances are fine because there is a concrete kind
--
-- TLM: Ugh, his new 'PrettyEnv' constraint really just enforces something
--      that we already know, which is that our environments are nested
--      tuples, but our type parameter 'env' doesn't capture that.
--
instance (Kit acc, PrettyEnv aenv) => Show (PreOpenAfun acc aenv f) where
  show f = renderStyle wide $ prettyPreOpenAfun prettyAcc prettyEnv f

instance (Kit acc, PrettyEnv env, PrettyEnv aenv) => Show (PreOpenFun acc env aenv f) where
  show f = renderStyle wide $ prettyPreOpenFun prettyAcc prettyEnv prettyEnv f

instance (Kit acc, PrettyEnv env, PrettyEnv aenv) => Show (PreOpenExp acc env aenv t) where
  show e = renderStyle wide $ prettyPreOpenExp prettyAcc noParens prettyEnv prettyEnv e

instance (Kit acc, PrettyEnv aenv) => Show (PreOpenSeq index acc aenv t) where
  show s = renderStyle wide $ sep $ punctuate (text ";") $ prettySeq prettyAcc noParens prettyEnv s

instance Show (DelayedSeq a) where
  show = renderStyle wide . prettyDelayedSeq noParens
