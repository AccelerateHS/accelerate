{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Analysis.Shape
-- Copyright   : [2008..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Analysis.Shape (

  -- * query AST dimensionality
  accDim,
  expDim,

) where

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.Representation.Shape
import Data.Array.Accelerate.Representation.Type


-- | Reify the dimensionality of the result type of an array computation
--
accDim :: forall acc aenv sh e. HasArraysR acc => acc aenv (Array sh e) -> Int
accDim = rank . arrayRshape . arrayR

-- | Reify dimensionality of a scalar expression yielding a shape
--
expDim :: forall env aenv sh. OpenExp env aenv sh -> Int
expDim = ndim . expType

-- | Count the number of components to a tuple type
--
ndim :: TupR s a -> Int
ndim TupRunit       = 0
ndim TupRsingle{}   = 1
ndim (TupRpair a b) = ndim a + ndim b

