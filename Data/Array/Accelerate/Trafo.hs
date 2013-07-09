{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
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

  convertAcc,  convertAccWith,
  convertAfun, convertAfunWith,

  -- * Fusion
  module Data.Array.Accelerate.Trafo.Fusion,

  -- * Substitution
  rebuildAcc,
  module Data.Array.Accelerate.Trafo.Substitution,

) where

import System.IO.Unsafe

import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Debug
import Data.Array.Accelerate.Pretty                     ( ) -- show instances
import Data.Array.Accelerate.Array.Sugar                ( Arrays, Elt )
import Data.Array.Accelerate.Trafo.Base
import Data.Array.Accelerate.Trafo.Fusion               hiding ( convertAcc, convertAfun ) -- to export types
import Data.Array.Accelerate.Trafo.Sharing              ( Function, FunctionR, Afunction, AfunctionR )
import Data.Array.Accelerate.Trafo.Substitution
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
    --   expressions. NOTE: currently always enabled.
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

when :: (a -> a) -> Bool -> a -> a
when f True  = f
when _ False = id


-- HOAS -> de Bruijn conversion
-- ----------------------------

-- | Convert a closed array expression to de Bruijn form while also
--   incorporating sharing observation and array fusion.
--
convertAcc :: Arrays arrs => Acc arrs -> DelayedAcc arrs
convertAcc = convertAccWith phases

convertAccWith :: Arrays arrs => Phase -> Acc arrs -> DelayedAcc arrs
convertAccWith Phase{..} acc
  = Fusion.convertAcc enableAccFusion
  $ Rewrite.convertSegments `when` convertOffsetOfSegment
  $ Sharing.convertAcc recoverAccSharing recoverExpSharing floatOutAccFromExp
  $ acc


-- | Convert a unary function over array computations, incorporating sharing
--   observation and array fusion
--
convertAfun :: Afunction f => f -> DelayedAfun (AfunctionR f)
convertAfun = convertAfunWith phases

convertAfunWith :: Afunction f => Phase -> f -> DelayedAfun (AfunctionR f)
convertAfunWith Phase{..} acc
  = Fusion.convertAfun enableAccFusion
  $ Rewrite.convertSegmentsAfun `when` convertOffsetOfSegment
  $ Sharing.convertAfun recoverAccSharing recoverExpSharing floatOutAccFromExp
  $ acc


-- | Convert a closed scalar expression, incorporating sharing observation and
--   optimisation.
--
convertExp :: Elt e => Exp e -> AST.Exp () e
convertExp
  = Rewrite.simplify
  . Sharing.convertExp (recoverExpSharing phases)


-- | Convert closed scalar functions, incorporating sharing observation and
--   optimisation.
--
convertFun :: Function f => f -> AST.Fun () (FunctionR f)
convertFun
  = Rewrite.simplify
  . Sharing.convertFun (recoverExpSharing phases)


-- Pretty printing
-- ---------------

instance Arrays arrs => Show (Acc arrs) where
  show = withSimplStats . show . convertAcc

instance Afunction (Acc a -> f) => Show (Acc a -> f) where
  show = withSimplStats . show . convertAfun

instance Elt e => Show (Exp e) where
  show = withSimplStats . show . convertExp

instance Function (Exp a -> f) => Show (Exp a -> f) where
  show = withSimplStats . show . convertFun


-- Debugging
-- ---------

-- Attach simplifier statistics to the tail of the given string. Since the
-- statistics rely on fully evaluating the expression this is difficult to do
-- generally (without an additional deepseq), but easy enough for our show
-- instances.
--
-- For now, we just reset the statistics at the beginning of a conversion, and
-- leave it to a backend to choose an appropriate moment to dump the summary.
--
withSimplStats :: String -> String
#ifdef ACCELERATE_DEBUG
withSimplStats x = unsafePerformIO $ do
  enabled <- queryFlag dump_simpl_stats
  if not enabled
     then return x
     else do resetSimplCount
             stats <- length x `seq` simplCount
             traceMessage dump_simpl_stats (show stats)
             return x
#else
withSimplStats x = x
#endif

