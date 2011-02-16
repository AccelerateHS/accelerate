{-# LANGUAGE GADTs, ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.CUDA.Analysis.Stencil
-- Copyright   : [2010..2011] Ben Lever
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--

module Data.Array.Accelerate.CUDA.Analysis.Stencil (positions, positions2) where

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Array.Sugar as Sugar


-- | Reify type-level stencil to value-level/GADT stencil.
--
reify :: forall e e' aenv sh stencil . (Elt e', Stencil sh e stencil)
      => Fun aenv (stencil -> e')
      -> OpenAcc aenv (Array sh e)
      -> StencilR sh e stencil
reify _ _ = stencil::(StencilR sh e stencil)


-- | As above except for two stencils.
--
reify2 :: forall e e' e'' aenv sh stencil stencil'. (Elt e', Stencil sh e stencil, Stencil sh e' stencil')
       => Fun aenv (stencil -> stencil' -> e'')
       -> OpenAcc aenv (Array sh e)
       -> OpenAcc aenv (Array sh e')
       -> (StencilR sh e stencil, StencilR sh e' stencil')
reify2 _ _ _ = (stencil::(StencilR sh e stencil), stencil::(StencilR sh e' stencil'))


-- |Calculate the coordinate of each stencil element relative to the focal point. The
-- coordinates are returned in a flattened list ordered from top-left element to
-- bottom-right.
--
positions :: (Elt e', Stencil sh e stencil)
          => Fun aenv (stencil -> e')
          -> OpenAcc aenv (Array sh e)
          -> [[Int]]
positions f a = map shapeToList $ positionsR (reify f a)

-- |As above except for two stencils.
--
positions2 :: (Elt e', Stencil sh e stencil, Stencil sh e' stencil')
          => Fun aenv (stencil -> stencil' -> e'')
          -> OpenAcc aenv (Array sh e)
          -> OpenAcc aenv (Array sh e')
          -> ([[Int]], [[Int]])
positions2 f a0 a1 = (pos s0, pos s1)
  where
    pos :: (Elt e, Stencil sh e stencil) => StencilR sh e stencil -> [[Int]]
    pos     = map shapeToList . positionsR
    (s0,s1) = reify2 f a0 a1


-- |Position calculation on reified stencil values.
--
positionsR :: StencilR sh' e pat -> [sh']
positionsR StencilRunit3 = map (Z:.) [-1,0,1]
positionsR StencilRunit5 = map (Z:.) [-2,-1,0,1,2]
positionsR StencilRunit7 = map (Z:.) [-3,-2,-1,0,1,2,3]
positionsR StencilRunit9 = map (Z:.) [-4,-3,-2,-1,0,1,2,3]
positionsR (StencilRtup3 a b c) = concat
  [ map (:.(-1)) $ positionsR a,
    map (:.  0 ) $ positionsR b,
    map (:.  1 ) $ positionsR c ]
positionsR (StencilRtup5 a b c d e) = concat
  [ map (:.(-2)) $ positionsR a,
    map (:.(-1)) $ positionsR b,
    map (:.  0 ) $ positionsR c,
    map (:.  1 ) $ positionsR d,
    map (:.  2 ) $ positionsR e ]
positionsR (StencilRtup7 a b c d e f g) = concat
  [ map (:.(-3)) $ positionsR a,
    map (:.(-2)) $ positionsR b,
    map (:.(-1)) $ positionsR c,
    map (:.  0 ) $ positionsR d,
    map (:.  1 ) $ positionsR e,
    map (:.  2 ) $ positionsR f,
    map (:.  3 ) $ positionsR g ]
positionsR (StencilRtup9 a b c d e f g h i) = concat
  [ map (:.(-4)) $ positionsR a,
    map (:.(-3)) $ positionsR b,
    map (:.(-2)) $ positionsR c,
    map (:.(-1)) $ positionsR d,
    map (:.  0 ) $ positionsR e,
    map (:.  1 ) $ positionsR f,
    map (:.  2 ) $ positionsR g,
    map (:.  3 ) $ positionsR h,
    map (:.  4 ) $ positionsR i ]

