{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Pretty
-- Copyright   : [2008..2011] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
--               [2009..2012] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Pretty (

  -- * Pretty printing functions
  module Data.Array.Accelerate.Pretty.Print

  -- * Instances of Show

) where

-- standard libraries
import Text.PrettyPrint

-- friends
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Trafo.Base
import Data.Array.Accelerate.Pretty.Print

-- |Show instances
-- ---------------

wide :: Style
wide = style { lineLength = 150 }

instance Kit acc => Show (acc env aenv a) where
  show c = renderStyle wide $ prettyAcc 0 noParens c

instance Kit acc => Show (PreOpenAfun acc env aenv f) where
  show f = renderStyle wide $ prettyPreAfun prettyAcc 0 f

instance Kit acc => Show (PreOpenFun acc env aenv f) where
  show f = renderStyle wide $ prettyPreFun prettyAcc 0 f

instance Kit acc => Show (PreOpenExp acc env aenv t) where
  show e = renderStyle wide $ prettyPreExp prettyAcc 0 0 noParens e

