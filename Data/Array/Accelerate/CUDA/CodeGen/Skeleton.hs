-- |
-- Module      : Data.Array.Accelerate.CUDA.CodeGen.Skeleton
-- Copyright   : [2008..2009] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.CUDA.CodeGen.Skeleton
  (
    module Data.Array.Accelerate.CUDA.CodeGen.Fold,
    module Data.Array.Accelerate.CUDA.CodeGen.FoldSeg,
    module Data.Array.Accelerate.CUDA.CodeGen.Index,
    module Data.Array.Accelerate.CUDA.CodeGen.Map,
--    module Data.Array.Accelerate.CUDA.CodeGen.Permute,
    module Data.Array.Accelerate.CUDA.CodeGen.Replicate,
--    module Data.Array.Accelerate.CUDA.CodeGen.Scan,
--    module Data.Array.Accelerate.CUDA.CodeGen.Vector
  )
  where

-- TLM 2010-06-02:
--
-- Actually, we may not need to store the skeletons in abstract syntax. The only
-- aspects that change ore the types (via typedefs) and the identity() and
-- apply() functions, all of which are separate. How about just #include-ing the
-- the skeleton file below the definitions of these?
--

import Data.Array.Accelerate.CUDA.CodeGen.Fold
import Data.Array.Accelerate.CUDA.CodeGen.FoldSeg
import Data.Array.Accelerate.CUDA.CodeGen.Index
import Data.Array.Accelerate.CUDA.CodeGen.Map
--import Data.Array.Accelerate.CUDA.CodeGen.Permute
import Data.Array.Accelerate.CUDA.CodeGen.Replicate
--import Data.Array.Accelerate.CUDA.CodeGen.Scan
--import Data.Array.Accelerate.CUDA.CodeGen.Vector

