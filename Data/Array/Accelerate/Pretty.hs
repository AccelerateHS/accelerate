{-# LANGUAGE GADTs, FlexibleInstances, PatternGuards, TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |Embedded array processing language: pretty printing
--
--  Copyright (c) 2009 Manuel M T Chakravarty, Gabriele Keller, Sean Lee
--
--  License: BSD3
--
--- Description ---------------------------------------------------------------
--

module Data.Array.Accelerate.Pretty (

  -- * Instances of Show

  -- * Functions
  module Data.Array.Accelerate.Pretty.Graphviz

) where

-- standard libraries
import Text.PrettyPrint

-- friends
import Data.Array.Accelerate.Pretty.Print
import Data.Array.Accelerate.Pretty.Graphviz
import Data.Array.Accelerate.AST

-- |Show instances
-- ---------------

instance Show (OpenAcc aenv a) where
  show c = render $ prettyAcc 0 c

instance Show (OpenFun env aenv f) where
  show f = render $ prettyFun 0 f

instance Show (OpenExp env aenv t) where
  show e = render $ prettyExp 0 0 noParens e
