{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Analysis.Stencil
-- Copyright   : [2010..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Analysis.Stencil ( positionsR )
  where

import Data.Array.Accelerate.Representation.Shape
import Data.Array.Accelerate.Representation.Stencil


-- | Calculate the offset coordinates for each stencil element relative to
-- the focal point. The coordinates are returned as a flattened list from
-- the bottom-left element to the top-right. This ordering matches the Var
-- indexing order.
--
positionsR :: StencilR sh e pat -> [sh]
positionsR StencilRunit3{} = map ((), ) [         -1, 0, 1          ]
positionsR StencilRunit5{} = map ((), ) [      -2,-1, 0, 1, 2       ]
positionsR StencilRunit7{} = map ((), ) [   -3,-2,-1, 0, 1, 2, 3    ]
positionsR StencilRunit9{} = map ((), ) [-4,-3,-2,-1, 0, 1, 2, 3, 4 ]

positionsR (StencilRtup3 c b a) = concat
  [ map (innermost shr (, -1)) $ positionsR c
  , map (innermost shr (,  0)) $ positionsR b
  , map (innermost shr (,  1)) $ positionsR a ]
  where
    shr =  stencilShapeR a

positionsR (StencilRtup5 e d c b a) = concat
  [ map (innermost shr (, -2)) $ positionsR e
  , map (innermost shr (, -1)) $ positionsR d
  , map (innermost shr (,  0)) $ positionsR c
  , map (innermost shr (,  1)) $ positionsR b
  , map (innermost shr (,  2)) $ positionsR a ]
  where
    shr =  stencilShapeR a

positionsR (StencilRtup7 g f e d c b a) = concat
  [ map (innermost shr (, -3)) $ positionsR g
  , map (innermost shr (, -2)) $ positionsR f
  , map (innermost shr (, -1)) $ positionsR e
  , map (innermost shr (,  0)) $ positionsR d
  , map (innermost shr (,  1)) $ positionsR c
  , map (innermost shr (,  2)) $ positionsR b
  , map (innermost shr (,  3)) $ positionsR a ]
  where
    shr =  stencilShapeR a

positionsR (StencilRtup9 i h g f e d c b a) = concat
  [ map (innermost shr (, -4)) $ positionsR i
  , map (innermost shr (, -3)) $ positionsR h
  , map (innermost shr (, -2)) $ positionsR g
  , map (innermost shr (, -1)) $ positionsR f
  , map (innermost shr (,  0)) $ positionsR e
  , map (innermost shr (,  1)) $ positionsR d
  , map (innermost shr (,  2)) $ positionsR c
  , map (innermost shr (,  3)) $ positionsR b
  , map (innermost shr (,  4)) $ positionsR a ]
  where
    shr =  stencilShapeR a


-- Inject a dimension component inner-most
--
innermost :: ShapeR sh -> (sh -> (sh, Int)) -> sh -> (sh, Int)
innermost shr f = invertShape (ShapeRsnoc shr) . f . invertShape shr

invertShape :: ShapeR sh -> sh -> sh
invertShape shr = listToShape shr . reverse . shapeToList shr

