{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.CUDA.Analysis.Stencil
-- Copyright   : [2010..2011] Ben Lever, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Analysis.Stencil (offsets, offsets2) where

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Array.Sugar


-- |Calculate the offset coordinates for each stencil element relative to the
-- focal point. The coordinates are returned as a flattened list from the
-- bottom-left element to the top-right. This ordering matches the Var indexing
-- order.
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
positionsR StencilRunit3 = map (Z:.) [         -1, 0, 1          ]
positionsR StencilRunit5 = map (Z:.) [      -2,-1, 0, 1, 2       ]
positionsR StencilRunit7 = map (Z:.) [   -3,-2,-1, 0, 1, 2, 3    ]
positionsR StencilRunit9 = map (Z:.) [-4,-3,-2,-1, 0, 1, 2, 3, 4 ]

positionsR (StencilRtup3 c b a) = concat
  [ map (innermost (:. -1)) $ positionsR c
  , map (innermost (:.  0)) $ positionsR b
  , map (innermost (:.  1)) $ positionsR a ]

positionsR (StencilRtup5 e d c b a) = concat
  [ map (innermost (:. -2)) $ positionsR e
  , map (innermost (:. -1)) $ positionsR d
  , map (innermost (:.  0)) $ positionsR c
  , map (innermost (:.  1)) $ positionsR b
  , map (innermost (:.  2)) $ positionsR a ]

positionsR (StencilRtup7 g f e d c b a) = concat
  [ map (innermost (:. -3)) $ positionsR g
  , map (innermost (:. -2)) $ positionsR f
  , map (innermost (:. -1)) $ positionsR e
  , map (innermost (:.  0)) $ positionsR d
  , map (innermost (:.  1)) $ positionsR c
  , map (innermost (:.  2)) $ positionsR b
  , map (innermost (:.  3)) $ positionsR a ]

positionsR (StencilRtup9 i h g f e d c b a) = concat
  [ map (innermost (:. -4)) $ positionsR i
  , map (innermost (:. -3)) $ positionsR h
  , map (innermost (:. -2)) $ positionsR g
  , map (innermost (:. -1)) $ positionsR f
  , map (innermost (:.  0)) $ positionsR e
  , map (innermost (:.  1)) $ positionsR d
  , map (innermost (:.  2)) $ positionsR c
  , map (innermost (:.  3)) $ positionsR b
  , map (innermost (:.  4)) $ positionsR a ]


-- Inject a dimension component inner-most
--
innermost :: Shape sh => (sh -> sh :. Int) -> sh -> sh :. Int
innermost f = invertShape . f . invertShape

invertShape :: Shape sh => sh -> sh
invertShape =  listToShape . reverse . shapeToList

