module Data.Array.Accelerate.CUDA.Scalar (

  Scalar(..)

) where

import Data.Array.Accelerate.CUDA.Syntax

--
-- Scalar description
--
-- Each scalar function is supplied with the output type, the list of
-- parameters, identity value, if applicable, and the computation.
--
data Scalar = Scalar
  { params   :: [(TySpec, String)]
  , outTy    :: TySpec
  , comp     :: [BlkItem]
  , identity :: Maybe Const}
  deriving Show

