{-# LANGUAGE TemplateHaskell, TypeOperators #-}
-- |
-- Module      : Data.Array.Accelerate.CUDA.CodeGen.Scalar
-- Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--
-- A scalar function, to be applied by a kernel skeleton
--

module Data.Array.Accelerate.CUDA.CodeGen.Scalar
  where

import Data.Record.Label
import Data.Array.Accelerate.CUDA.Syntax

data Scalar = Scalar
  {
    _outputTy   :: TySpec,
    _argumentTy :: [TySpec],
    _body       :: [BlkItem],
    _identity   :: Maybe Const
  }
  deriving (Show)

$(mkLabels [''Scalar])

outputTy   :: Scalar :-> TySpec
argumentTy :: Scalar :-> [TySpec]
body       :: Scalar :-> [BlkItem]
identity   :: Scalar :-> Maybe Const

