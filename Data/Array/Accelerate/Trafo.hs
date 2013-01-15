{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
  Phase(..), phases,

  convertAcc,     convertAccWith,
  convertAccFun1, convertAccFun1With,

  -- * Fusion
  Fusion.embedOpenAcc, Fusion.Embedded(..),

) where

import Prelude                                          hiding ( exp )

import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Array.Sugar                ( Arrays, Elt )
import Data.Array.Accelerate.Trafo.Common
import qualified Data.Array.Accelerate.AST              as AST
import qualified Data.Array.Accelerate.Trafo.Fusion     as Fusion
import qualified Data.Array.Accelerate.Trafo.Rewrite    as Rewrite
import qualified Data.Array.Accelerate.Trafo.Simplify   as Rewrite
import qualified Data.Array.Accelerate.Trafo.Sharing    as Sharing


-- Configuration
-- -------------

data Phase = Phase
  {
    -- | Recover sharing of array computations?
    recoverAccSharing           :: Bool

    -- | Recover sharing of scalar expressions?
  , recoverExpSharing           :: Bool

    -- | Are array computations floated out of expressions irrespective of
    --   whether they are shared or not? Requires 'recoverAccSharing'.
  , floatOutAccFromExp          :: Bool

    -- | Fuse array computations? This also implies simplifying scalar
    --   expressions.
  , enableAccFusion             :: Bool

    -- | Convert segment length arrays into segment offset arrays?
  , convertOffsetOfSegment      :: Bool
  }


-- | The default method of converting from HOAS to de Bruijn; incorporating
--   sharing recovery and fusion optimisation.
--
phases :: Phase
phases =  Phase
  { recoverAccSharing      = True
  , recoverExpSharing      = True
  , floatOutAccFromExp     = True
  , enableAccFusion        = True
  , convertOffsetOfSegment = False
  }


-- HOAS -> de Bruijn conversion
-- ----------------------------

-- | Convert a closed array expression to de Bruijn form while also
--   incorporating sharing observation and array fusion.
--
convertAcc :: Arrays arrs => Acc arrs -> AST.Acc arrs
convertAcc = convertAccWith phases

convertAccWith :: Arrays arrs => Phase -> Acc arrs -> AST.Acc arrs
convertAccWith ok acc
  = Fusion.fuseAcc          `when` enableAccFusion
  $ Rewrite.convertSegments `when` convertOffsetOfSegment
  $ Sharing.convertAcc (recoverAccSharing ok) (recoverExpSharing ok) (floatOutAccFromExp ok) acc
  where
    when f phase
      | phase ok        = f
      | otherwise       = id


-- | Convert a unary function over array computations, incorporating sharing
--   observation and array fusion
--
convertAccFun1 :: (Arrays a, Arrays b) => (Acc a -> Acc b) -> AST.Afun (a -> b)
convertAccFun1 = convertAccFun1With phases

convertAccFun1With :: (Arrays a, Arrays b) => Phase -> (Acc a -> Acc b) -> AST.Afun (a -> b)
convertAccFun1With ok acc
  = Fusion.fuseAfun             `when` enableAccFusion
  $ Rewrite.convertSegmentsAfun `when` convertOffsetOfSegment
  $ Sharing.convertAccFun1 (recoverAccSharing ok) (recoverExpSharing ok) (floatOutAccFromExp ok) acc
  where
    when f phase
      | phase ok        = f
      | otherwise       = id


-- | Convert a closed scalar expression, incorporating sharing observation and
--   optimisation.
--
convertExp :: Elt e => Exp e -> AST.Exp () e
convertExp exp
  = Rewrite.simplifyExp EmptyAcc
  $ Sharing.convertExp (recoverExpSharing phases) exp


-- | Convert closed scalar functions, incorporating sharing observation and
--   optimisation.
--
convertFun1 :: (Elt a, Elt b) => (Exp a -> Exp b) -> AST.Fun () (a -> b)
convertFun1 fun
  = Rewrite.simplifyFun EmptyAcc
  $ Sharing.convertFun1 (recoverExpSharing phases) fun

convertFun2 :: (Elt a, Elt b, Elt c) => (Exp a -> Exp b -> Exp c) -> AST.Fun () (a -> b -> c)
convertFun2 fun
  = Rewrite.simplifyFun EmptyAcc
  $ Sharing.convertFun2 (recoverExpSharing phases) fun


-- Pretty printing
-- ---------------

instance Arrays arrs => Show (Acc arrs) where
  show = show . convertAcc

instance (Arrays a, Arrays b) => Show (Acc a -> Acc b) where
  show = show . convertAccFun1

instance Elt e => Show (Exp e) where
  show = show . convertExp

instance (Elt a, Elt b) => Show (Exp a -> Exp b) where
  show = show . convertFun1

instance (Elt a, Elt b, Elt c) => Show (Exp a -> Exp b -> Exp c) where
  show = show . convertFun2

