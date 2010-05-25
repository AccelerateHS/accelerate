{-# LANGUAGE TemplateHaskell, TypeOperators #-}
-- |
-- Module      : Data.Array.Accelerate.CUDA.CodeGen.Expr
-- Copyright   : [2008..2010] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--
-- A scalar function, to be applied by a kernel skeleton
--

module Data.Array.Accelerate.CUDA.CodeGen.Expr
  (
    Expr(Expr), outputTy, argumentTy, body, identity
  )
  where

import Data.Record.Label
import Data.Array.Accelerate.CUDA.Syntax

data Expr = Expr
  {
    _outputTy   :: TySpec,
    _argumentTy :: [TySpec],
    _body       :: [BlkItem],
    _identity   :: Maybe Const
  }
  deriving (Show)

$(mkLabels [''Expr])

outputTy   :: Expr :-> TySpec
argumentTy :: Expr :-> [TySpec]
body       :: Expr :-> [BlkItem]
identity   :: Expr :-> Maybe Const

