{-# LANGUAGE GADTs, ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.CUDA.Analysis.Stencil
-- Copyright   : [2010..2011] Ben Lever, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--

module Data.Array.Accelerate.Analysis.Stencil (offsets, offsets2) where

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Array.Sugar


-- |Calculate the offset coordinates for each stencil element relative to the
-- focal point. The coordinates are returned as a flattened list from the
-- top-left element to the bottom-right.
--
offsets :: forall a b sh aenv stencil. Stencil sh a stencil
        => {- dummy -} Fun aenv (stencil -> b)
        -> {- dummy -} OpenAcc aenv (Array sh a)
        -> [sh]
offsets _ _ = positionsR (stencil :: StencilR sh a stencil)

offsets2 :: forall a b c sh aenv stencil1 stencil2. (Stencil sh a stencil1, Stencil sh b stencil2)
         => {- dummy -} Fun aenv (stencil1 -> stencil2 -> c)
         -> {- dummy -} OpenAcc aenv (Array sh a)
         -> {- dummy -} OpenAcc aenv (Array sh b)
         -> ([sh], [sh])
offsets2 _ _ _ =
  ( positionsR (stencil :: StencilR sh a stencil1)
  , positionsR (stencil :: StencilR sh b stencil2) )


-- |Position calculation on reified stencil values.
--
positionsR :: StencilR sh e pat -> [sh]
positionsR StencilRunit3 = map (Z:.) [         -1, 0, 1         ]
positionsR StencilRunit5 = map (Z:.) [      -2,-1, 0, 1, 2      ]
positionsR StencilRunit7 = map (Z:.) [   -3,-2,-1, 0, 1, 2, 3   ]
positionsR StencilRunit9 = map (Z:.) [-4,-3,-2,-1, 0, 1, 2, 3, 4]

positionsR (StencilRtup3 a b c) = concat
  [ map (:.(-1)) $ positionsR a
  , map (:.  0 ) $ positionsR b
  , map (:.  1 ) $ positionsR c ]

positionsR (StencilRtup5 a b c d e) = concat
  [ map (:.(-2)) $ positionsR a
  , map (:.(-1)) $ positionsR b
  , map (:.  0 ) $ positionsR c
  , map (:.  1 ) $ positionsR d
  , map (:.  2 ) $ positionsR e ]

positionsR (StencilRtup7 a b c d e f g) = concat
  [ map (:.(-3)) $ positionsR a
  , map (:.(-2)) $ positionsR b
  , map (:.(-1)) $ positionsR c
  , map (:.  0 ) $ positionsR d
  , map (:.  1 ) $ positionsR e
  , map (:.  2 ) $ positionsR f
  , map (:.  3 ) $ positionsR g ]

positionsR (StencilRtup9 a b c d e f g h i) = concat
  [ map (:.(-4)) $ positionsR a
  , map (:.(-3)) $ positionsR b
  , map (:.(-2)) $ positionsR c
  , map (:.(-1)) $ positionsR d
  , map (:.  0 ) $ positionsR e
  , map (:.  1 ) $ positionsR f
  , map (:.  2 ) $ positionsR g
  , map (:.  3 ) $ positionsR h
  , map (:.  4 ) $ positionsR i ]

