{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo.Delayed
-- Copyright   : [2012..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The type of delayed arrays. This representation is used to annotate the AST
-- in the recursive knot to distinguish standard AST terms from operand arrays
-- that should be embedded into their consumers.
--

module Data.Array.Accelerate.Trafo.Delayed
  where

import Data.Array.Accelerate.Annotations
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Analysis.Hash
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Trafo.Substitution

import Data.Array.Accelerate.Debug.Internal.Stats                   as Stats

import Control.DeepSeq
import Data.ByteString.Builder
import Data.ByteString.Builder.Extra


type DelayedAcc      = DelayedOpenAcc ()
type DelayedAfun     = PreOpenAfun DelayedOpenAcc ()
type DelayedOpenAfun = PreOpenAfun DelayedOpenAcc

-- type DelayedOpenSeq = PreOpenSeq DelayedOpenAcc
-- data DelayedSeq t where
--   DelayedSeq :: Extend DelayedOpenAcc () aenv
--              -> DelayedOpenSeq aenv () t
--              -> DelayedSeq t

data DelayedOpenAcc aenv a where
  Manifest              :: PreOpenAcc DelayedOpenAcc aenv a
                        -> DelayedOpenAcc aenv a

  -- TODO: The current idea is to merge annotations from any AST constructors we
  --       convert to the delayed representation. Does this make sense wrt
  --       things like loop unrolling?
  Delayed               ::
    { annD              :: Ann
    , reprD             :: ArrayR (Array sh e)
    , extentD           :: Exp aenv sh
    , indexD            :: Fun aenv (sh  -> e)
    , linearIndexD      :: Fun aenv (Int -> e)
    }                   -> DelayedOpenAcc aenv (Array sh e)

instance HasArraysR DelayedOpenAcc where
  arraysR (Manifest a) = arraysR a
  arraysR Delayed{..}  = TupRsingle reprD

instance Rebuildable DelayedOpenAcc where
  type AccClo DelayedOpenAcc = DelayedOpenAcc
  rebuildPartial v = \case
    Manifest pacc -> Manifest <$> rebuildPartial v pacc
    Delayed{..}   -> (\e i l -> Delayed annD reprD (unOpenAccExp e) (unOpenAccFun i) (unOpenAccFun l))
                              <$> rebuildPartial v (OpenAccExp extentD)
                              <*> rebuildPartial v (OpenAccFun indexD)
                              <*> rebuildPartial v (OpenAccFun linearIndexD)

instance Sink DelayedOpenAcc where
  weaken k = Stats.substitution "weaken" . rebuildA (rebuildWeakenVar k)

instance NFData (DelayedOpenAfun aenv t) where
  rnf = rnfPreOpenAfun rnfDelayedOpenAcc

instance NFData (DelayedOpenAcc aenv t) where
  rnf = rnfDelayedOpenAcc

-- TODO: Like elsewhere, we should probably consider optimization flags in the
--       hashes
encodeDelayedOpenAcc :: EncodeAcc DelayedOpenAcc
encodeDelayedOpenAcc options acc =
  let
      travE :: Exp aenv sh -> Builder
      travE = encodeOpenExp

      travF :: Fun aenv f -> Builder
      travF = encodeOpenFun

      travA :: PreOpenAcc DelayedOpenAcc aenv a -> Builder
      travA = encodePreOpenAcc options encodeDelayedOpenAcc

      deepA :: PreOpenAcc DelayedOpenAcc aenv a -> Builder
      deepA | perfect options = travA
            | otherwise       = encodeArraysType . arraysR
  in
  case acc of
    Manifest pacc      -> intHost $(hashQ ("Manifest" :: String)) <> deepA pacc
    Delayed _ _ sh f g -> intHost $(hashQ ("Delayed"  :: String)) <> travE sh <> travF f <> travF g

-- TODO: Like everywhere else, we should probably consider at least the
--       optimization flags when matching ASTs.
matchDelayedOpenAcc :: MatchAcc DelayedOpenAcc
matchDelayedOpenAcc (Manifest pacc1) (Manifest pacc2)
  = matchPreOpenAcc matchDelayedOpenAcc pacc1 pacc2
matchDelayedOpenAcc (Delayed _ _ sh1 ix1 lx1) (Delayed _ _ sh2 ix2 lx2)
  | Just Refl <- matchOpenExp sh1 sh2
  , Just Refl <- matchOpenFun ix1 ix2
  , Just Refl <- matchOpenFun lx1 lx2
  = Just Refl
matchDelayedOpenAcc _ _
  = Nothing

rnfDelayedOpenAcc :: NFDataAcc DelayedOpenAcc
rnfDelayedOpenAcc (Manifest pacc) =
  rnfPreOpenAcc rnfDelayedOpenAcc pacc
rnfDelayedOpenAcc (Delayed ann aR sh ix lx) =
  rnfAnn ann `seq` rnfArrayR aR `seq` rnfOpenExp sh `seq` rnfOpenFun ix `seq` rnfOpenFun lx

liftDelayedOpenAcc :: LiftAcc DelayedOpenAcc
liftDelayedOpenAcc (Manifest pacc) =
  [|| Manifest $$(liftPreOpenAcc liftDelayedOpenAcc pacc) ||]
liftDelayedOpenAcc (Delayed ann aR sh ix lx) =
  [|| Delayed $$(liftAnn ann) $$(liftArrayR aR) $$(liftOpenExp sh) $$(liftOpenFun ix) $$(liftOpenFun lx) ||]
