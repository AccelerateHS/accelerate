{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo
-- Copyright   : [2012..2014] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
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
  convertSeq,  convertSeqWith,

  -- * Fusion
  module Data.Array.Accelerate.Trafo.Fusion,
  DelayedSeq, StreamSeq(..), Extend(..),

  -- * Substitution
  module Data.Array.Accelerate.Trafo.Substitution,

  -- * Term equality
  Match(..), (:~:)(..),

) where

import Control.DeepSeq
import Data.Typeable

import Data.Array.Accelerate.Smart
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
import qualified Data.Array.Accelerate.Trafo.Vectorise  as Vectorise

#ifdef ACCELERATE_DEBUG
import Text.Printf
import System.IO.Unsafe
import Data.Array.Accelerate.Debug                      hiding ( when )
import qualified Data.Array.Accelerate.Debug            as Debug
#endif


-- Configuration
-- -------------

data Phase = Phase
  {
    -- | Recover sharing of array computations?
    recoverAccSharing           :: Bool

    -- | Recover sharing of scalar expressions?
  , recoverExpSharing           :: Bool

    -- | Recover sharing of sequence computations?
  , recoverSeqSharing           :: Bool

    -- | Are array computations floated out of expressions irrespective of
    --   whether they are shared or not? Requires 'recoverAccSharing'.
  , floatOutAccFromExp          :: Bool

    -- | Fuse array computations? This also implies simplifying scalar
    --   expressions. NOTE: currently always enabled.
  , enableAccFusion             :: Bool

    -- | Convert segment length arrays into segment offset arrays?
  , convertOffsetOfSegment      :: Bool

    -- | Convert subarray extraction into array indexing?
  , convertSubarrayToIndex      :: Bool

    -- | Vectorise maps and zipwiths in sequence computations to
    --   enable chunked execution?
  , vectoriseSequences          :: Bool
  }


-- | The default method of converting from HOAS to de Bruijn; incorporating
--   sharing recovery and fusion optimisation.
--
phases :: Phase
phases =  Phase
  { recoverAccSharing      = True
  , recoverExpSharing      = True
  , recoverSeqSharing      = True
  , floatOutAccFromExp     = True
  , enableAccFusion        = True
  , convertOffsetOfSegment = False
  , convertSubarrayToIndex = False
  , vectoriseSequences     = True
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
  = phase "array-fusion"           (Fusion.convertAcc enableAccFusion)
  $ phase "rewrite-segment-offset" Rewrite.convertSegments `when` convertOffsetOfSegment
  $ phase "rewrite-subarray-index" Rewrite.convertSubarray `when` convertSubarrayToIndex
  $ phase "vectorise-sequences"    Vectorise.vectoriseAcc  `when` vectoriseSequences
  $ phase "sharing-recovery"       (Sharing.convertAcc recoverAccSharing recoverExpSharing recoverSeqSharing floatOutAccFromExp)
  $ acc


-- | Convert a unary function over array computations, incorporating sharing
--   observation and array fusion
--
convertAfun :: Afunction f => f -> DelayedAfun (AfunctionR f)
convertAfun = convertAfunWith phases

convertAfunWith :: Afunction f => Phase -> f -> DelayedAfun (AfunctionR f)
convertAfunWith Phase{..} acc
  = phase "array-fusion"           (Fusion.convertAfun enableAccFusion)
  $ phase "rewrite-segment-offset" Rewrite.convertSegmentsAfun `when` convertOffsetOfSegment
  $ phase "rewrite-subarray-index" Rewrite.convertSubarrayAfun `when` convertSubarrayToIndex
  $ phase "vectorise-sequences"    Vectorise.vectoriseAfun     `when` vectoriseSequences
  $ phase "sharing-recovery"       (Sharing.convertAfun recoverAccSharing recoverExpSharing recoverSeqSharing floatOutAccFromExp)
  $ acc


-- | Convert a closed scalar expression, incorporating sharing observation and
--   optimisation.
--
convertExp :: Elt e => Exp e -> AST.Exp () e
convertExp
  = phase "exp-simplify"      Rewrite.simplify
  . phase "sharing-recovery" (Sharing.convertExp (recoverExpSharing phases))


-- | Convert closed scalar functions, incorporating sharing observation and
--   optimisation.
--
convertFun :: Function f => f -> AST.Fun () (FunctionR f)
convertFun
  = phase "exp-simplify"      Rewrite.simplify
  . phase "sharing-recovery" (Sharing.convertFun (recoverExpSharing phases))

-- | Convert a closed sequence computation, incorporating sharing observation and
--   optimisation.
--
convertSeq :: Typeable s => Seq s -> DelayedSeq s
convertSeq = convertSeqWith phases

convertSeqWith :: Typeable s => Phase -> Seq s -> DelayedSeq s
convertSeqWith Phase{..} s
  = phase "array-fusion"           (Fusion.convertStreamSeq enableAccFusion)
  $ phase "rewrite-segment-offset" Rewrite.convertSegmentsStreamSeq `when` convertOffsetOfSegment
  $ phase "rewrite-subarray-index" Rewrite.convertSubarrayStreamSeq `when` convertSubarrayToIndex
  $ phase "vectorise-sequences"    Vectorise.vectoriseStreamSeq
  $ phase "sharing-recovery"       (Sharing.convertSeq recoverAccSharing recoverExpSharing recoverSeqSharing floatOutAccFromExp)
  $ s


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

instance Typeable a => Show (Seq a) where
  show = withSimplStats . show . convertSeq


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
  Debug.when dump_simpl_stats $ x `deepseq` dumpSimplStats
  return x
#else
withSimplStats x = x
#endif

-- Execute a phase of the compiler and (possibly) print some timing/gc
-- statistics.
--
phase :: NFData b => String -> (a -> b) -> a -> b
#ifdef ACCELERATE_DEBUG
phase n f x = unsafePerformIO $ do
  enabled <- queryFlag dump_phases
  if enabled
    then timed dump_phases (\wall cpu -> printf "phase %s: %s" n (elapsed wall cpu)) (return $!! f x)
    else return (f x)
#else
phase _ f x = f x
#endif
