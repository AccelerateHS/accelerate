{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Pretty
-- Copyright   : [2008..2017] Manuel M T Chakravarty, Gabriele Keller
--               [2009..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
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
import Text.PrettyPrint.ANSI.Leijen

-- friends
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Trafo.Base
import Data.Array.Accelerate.Pretty.Print
import Data.Array.Accelerate.Pretty.Graphviz


-- Show
-- ----

-- Explicitly enumerate Show instances for the Accelerate array AST types. If we
-- instead use a generic instance of the form:
--
--   instance Kit acc => Show (acc aenv a) where
--
-- This matches any type of kind (* -> * -> *), which can cause problems
-- interacting with other packages. See Issue #108.
--
instance PrettyEnv aenv => Show (OpenAcc aenv a) where
  showsPrec _ = displayS . renderStyle . pretty

instance PrettyEnv aenv => Show (DelayedOpenAcc aenv a) where
  showsPrec _ = displayS . renderStyle . pretty

-- These parameterised instances are fine because there is a concrete kind
--
-- TLM: Ugh, his new 'PrettyEnv' constraint really just enforces something
--      that we already know, which is that our environments are nested
--      tuples, but our type parameter 'env' doesn't capture that.
--
instance (Kit acc, PrettyEnv aenv) => Show (PreOpenAfun acc aenv f) where
  showsPrec _ = displayS . renderStyle . pretty

instance (Kit acc, PrettyEnv env, PrettyEnv aenv) => Show (PreOpenFun acc env aenv f) where
  showsPrec _ = displayS . renderStyle . pretty

instance (Kit acc, PrettyEnv env, PrettyEnv aenv) => Show (PreOpenExp acc env aenv t) where
  showsPrec _ = displayS . renderStyle . pretty

-- instance Kit acc => Show (PreOpenSeq acc aenv senv t) where
--   show s = renderStyle wide $ sep $ punctuate (text ";") $ prettySeq prettyAcc 0 0 noParens s

renderStyle :: Doc -> SimpleDoc
renderStyle = renderSmart 0.7 120

-- Pretty
-- ------

instance PrettyEnv aenv => Pretty (OpenAcc aenv a) where
  pretty c = prettyAcc noParens prettyEnv c

instance PrettyEnv aenv => Pretty (DelayedOpenAcc aenv a) where
  pretty c = prettyAcc noParens prettyEnv c

instance (Kit acc, PrettyEnv aenv) => Pretty (PreOpenAfun acc aenv f) where
  pretty f = prettyPreOpenAfun prettyAcc prettyEnv f

instance (Kit acc, PrettyEnv env, PrettyEnv aenv) => Pretty (PreOpenFun acc env aenv f) where
  pretty f = prettyPreOpenFun prettyAcc prettyEnv prettyEnv f

instance (Kit acc, PrettyEnv env, PrettyEnv aenv) => Pretty (PreOpenExp acc env aenv t) where
  pretty e = prettyPreOpenExp prettyAcc noParens prettyEnv prettyEnv e

