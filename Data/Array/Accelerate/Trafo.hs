{-# OPTIONS -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo
-- Copyright   : [2012] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Trafo (

  -- * HOAS -> de Bruijn conversion
  convertAcc, convertAccFun1

) where

import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Array.Sugar                ( Arrays )
import qualified Data.Array.Accelerate.AST              as AST
import qualified Data.Array.Accelerate.Trafo.Fusion     as Fusion
import qualified Data.Array.Accelerate.Trafo.Sharing    as Sharing


-- Configuration
-- -------------


-- HOAS -> de Bruijn conversion
-- ----------------------------

-- | Convert a closed array expression to de Bruijn form while also
-- incorporating sharing observation and array fusion.
--
convertAcc :: Arrays arrs => Acc arrs -> AST.Acc arrs
convertAcc = Fusion.fuseAcc . Sharing.convertAcc

-- | Convert a unary function over array computations
--
convertAccFun1 :: (Arrays a, Arrays b) => (Acc a -> Acc b) -> AST.Afun (a -> b)
convertAccFun1 = Fusion.fuseAccFun1 . Sharing.convertAccFun1


-- Pretty printing
-- ---------------

instance Arrays arrs => Show (Acc arrs) where
  show = show . convertAcc

-- show instance for scalar expressions inherited from Sharing module

