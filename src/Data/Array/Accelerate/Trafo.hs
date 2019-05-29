{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE RecordWildCards      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo
-- Copyright   : [2012..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Trafo (

  -- * HOAS -> de Bruijn conversion
  Config(..), defaultOptions, convert_segment_offset,

  -- ** Array computations
  convertAcc, convertAccWith,

  -- ** Array functions
  Afunction, AfunctionR,
  convertAfun, convertAfunWith,

  -- ** Sequence computations
  -- convertSeq, convertSeqWith,

  -- ** Scalar expressions
  Function, FunctionR,
  convertExp, convertFun,

  -- * Fusion
  DelayedAcc, DelayedOpenAcc(..),
  DelayedAfun, DelayedOpenAfun,
  DelayedExp, DelayedOpenExp,
  DelayedFun, DelayedOpenFun,

  -- * Substitution
  module Data.Array.Accelerate.Trafo.Substitution,

  -- * Term equality
  Match(..), (:~:)(..),

  -- ** Auxiliary
  matchDelayedOpenAcc,
  encodeDelayedOpenAcc,

) where

import Control.DeepSeq
import Data.Typeable

import Data.BitSet
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Array.Sugar                ( Arrays, Elt )
import Data.Array.Accelerate.Trafo.Base
import Data.Array.Accelerate.Trafo.Config
import Data.Array.Accelerate.Trafo.Fusion               ( DelayedAcc, DelayedOpenAcc(..), DelayedAfun, DelayedOpenAfun, DelayedExp, DelayedFun, DelayedOpenExp, DelayedOpenFun )
import Data.Array.Accelerate.Trafo.Sharing              ( Function, FunctionR, Afunction, AfunctionR )
import Data.Array.Accelerate.Trafo.Substitution
import qualified Data.Array.Accelerate.AST              as AST
import qualified Data.Array.Accelerate.Trafo.Fusion     as Fusion
import qualified Data.Array.Accelerate.Trafo.Rewrite    as Rewrite
import qualified Data.Array.Accelerate.Trafo.Simplify   as Rewrite
import qualified Data.Array.Accelerate.Trafo.Sharing    as Sharing
-- import qualified Data.Array.Accelerate.Trafo.Vectorise  as Vectorise

import Data.Array.Accelerate.Debug.Flags                hiding ( when )

#ifdef ACCELERATE_DEBUG
import Text.Printf
import System.IO.Unsafe
import Data.Array.Accelerate.Debug.Timed
#endif


-- HOAS -> de Bruijn conversion
-- ----------------------------

-- | Convert a closed array expression to de Bruijn form while also
--   incorporating sharing observation and array fusion.
--
convertAcc :: Arrays arrs => Acc arrs -> DelayedAcc arrs
convertAcc = convertAccWith defaultOptions

convertAccWith :: Arrays arrs => Config -> Acc arrs -> DelayedAcc arrs
convertAccWith config acc
  = phase "array-fusion"           (Fusion.convertAccWith config)
  -- phase "vectorise-sequences"    Vectorise.vectoriseSeqAcc `when` vectoriseSequences
  $ phase "rewrite-segment-offset" Rewrite.convertSegments   `when` (convert_segment_offset `member` options config)
  $ phase "sharing-recovery"       (Sharing.convertAccWith config)
  $ acc


-- | Convert a unary function over array computations, incorporating sharing
--   observation and array fusion
--
convertAfun :: Afunction f => f -> DelayedAfun (AfunctionR f)
convertAfun = convertAfunWith defaultOptions

convertAfunWith :: Afunction f => Config -> f -> DelayedAfun (AfunctionR f)
convertAfunWith config acc
  = phase "array-fusion"           (Fusion.convertAfunWith config)
  -- phase "vectorise-sequences"    Vectorise.vectoriseSeqAfun  `when` vectoriseSequences
  $ phase "rewrite-segment-offset" Rewrite.convertSegmentsAfun `when` (convert_segment_offset `member` options config)
  $ phase "sharing-recovery"       (Sharing.convertAfunWith config)
  $ acc


-- | Convert a closed scalar expression, incorporating sharing observation and
--   optimisation.
--
convertExp :: Elt e => Exp e -> AST.Exp () e
convertExp
  = phase "exp-simplify"     Rewrite.simplify     -- XXX: only if simplification is enabled
  . phase "sharing-recovery" Sharing.convertExp


-- | Convert closed scalar functions, incorporating sharing observation and
--   optimisation.
--
convertFun :: Function f => f -> AST.Fun () (FunctionR f)
convertFun
  = phase "exp-simplify"     Rewrite.simplify
  . phase "sharing-recovery" Sharing.convertFun

{--
-- | Convert a closed sequence computation, incorporating sharing observation and
--   optimisation.
--
convertSeq :: Typeable s => Seq s -> DelayedSeq s
convertSeq = convertSeqWith phases

convertSeqWith :: Typeable s => Phase -> Seq s -> DelayedSeq s
convertSeqWith Phase{..} s
  = phase "array-fusion"           (Fusion.convertSeq enableAccFusion)
  -- $ phase "vectorise-sequences"    Vectorise.vectoriseSeq     `when` vectoriseSequences
  $ phase "rewrite-segment-offset" Rewrite.convertSegmentsSeq `when` convertOffsetOfSegment
  $ phase "sharing-recovery"       (Sharing.convertSeq recoverAccSharing recoverExpSharing recoverSeqSharing floatOutAccFromExp)
  $ s
--}


when :: (a -> a) -> Bool -> a -> a
when f True  = f
when _ False = id

convert_segment_offset :: Flag
convert_segment_offset = Flag 31  -- TLM: let's remove the need for this

-- Debugging
-- ---------

-- Execute a phase of the compiler and (possibly) print some timing/gc
-- statistics.
--
phase :: NFData b => String -> (a -> b) -> a -> b
#ifdef ACCELERATE_DEBUG
phase n f x = unsafePerformIO $ do
  enabled <- getFlag dump_phases
  if enabled
    then timed dump_phases (\wall cpu -> printf "phase %s: %s" n (elapsed wall cpu)) (return $!! f x)
    else return (f x)
#else
phase _ f x = f x
#endif

