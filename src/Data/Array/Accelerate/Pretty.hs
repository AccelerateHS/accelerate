{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Pretty
-- Copyright   : [2008..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Pretty (

  -- * Pretty printing functions
  module Data.Array.Accelerate.Pretty.Print,
  module Data.Array.Accelerate.Pretty.Graphviz,

  -- * Instances of Show

) where

-- libraries
import System.IO
import System.IO.Unsafe
import Text.PrettyPrint.ANSI.Leijen
import qualified System.Console.ANSI                                as Term
import qualified System.Console.Terminal.Size                       as Term

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
  showsPrec _ = renderForTerminal . pretty

instance PrettyEnv aenv => Show (DelayedOpenAcc aenv a) where
  showsPrec _ = renderForTerminal . pretty

-- These parameterised instances are fine because there is a concrete kind
--
-- TLM: Ugh, his new 'PrettyEnv' constraint really just enforces something
--      that we already know, which is that our environments are nested
--      tuples, but our type parameter 'env' doesn't capture that.
--
instance (Kit acc, PrettyEnv aenv) => Show (PreOpenAfun acc aenv f) where
  showsPrec _ = renderForTerminal . pretty

instance (Kit acc, PrettyEnv env, PrettyEnv aenv) => Show (PreOpenFun acc env aenv f) where
  showsPrec _ = renderForTerminal . pretty

instance (Kit acc, PrettyEnv env, PrettyEnv aenv) => Show (PreOpenExp acc env aenv t) where
  showsPrec _ = renderForTerminal . pretty

-- instance Kit acc => Show (PreOpenSeq acc aenv senv t) where
--   show s = renderForTerminal wide $ sep $ punctuate (text ";") $ prettySeq prettyAcc 0 0 noParens s

renderForTerminal :: Doc -> ShowS
renderForTerminal doc next =
  unsafePerformIO $ do
    term <- Term.size
    ansi <- Term.hSupportsANSI stdout
    let
        w             = maybe 120 Term.width term
        d | ansi      = doc
          | otherwise = plain doc
        f | w <= 100  = 0.7
          | w <= 120  = 0.6
          | otherwise = 0.5
    --
    return $ displayS (renderSmart f w d) next

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

