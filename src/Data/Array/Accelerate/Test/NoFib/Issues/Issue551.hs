{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Data.Array.Accelerate.Test.NoFib.Issues.Issue551 where

import Data.Array.Accelerate (Elt, Exp, Word16, match, mkPattern)
import Data.Function ((&))
import GHC.Generics (Generic)

import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Unsafe as A
import Data.Array.Accelerate.Test.NoFib.Base

import Test.Tasty
import Test.Tasty.HUnit


data Digit = One | Two | Three | Four | Five | Six | Seven | Eight | Nine
  deriving (Generic)

instance Elt Digit

mkPattern ''Digit

word16ToDigit :: (HasCallStack) => Exp Word16 -> Exp Digit
word16ToDigit i =
  A.cond (i A.== 0) One_ $
  A.cond (i A.== 1) Two_ $
  A.cond (i A.== 2) Three_ $
  A.cond (i A.== 3) Four_ $
  A.cond (i A.== 4) Five_ $
  A.cond (i A.== 5) Six_ $
  A.cond (i A.== 6) Seven_ $
  A.cond (i A.== 7) Eight_ $
  A.cond (i A.== 8) Nine_ $
    A.undef

digitToWord16 :: Exp Digit -> Exp Word16
digitToWord16 = match \case
  One_ -> 0
  Two_ -> 1
  Three_ -> 2
  Four_ -> 3
  Five_ -> 4
  Six_ -> 5
  Seven_ -> 6
  Eight_ -> 7
  Nine_ -> 8


instance A.Eq Digit where
  d == d' =
    A.T2 d d' & A.match \case
      A.T2 One_ One_ -> A.True_
      A.T2 Two_ Two_ -> A.True_
      A.T2 Three_ Three_ -> A.True_
      A.T2 Four_ Four_ -> A.True_
      A.T2 Five_ Five_ -> A.True_
      A.T2 Six_ Six_ -> A.True_
      A.T2 Seven_ Seven_ -> A.True_
      A.T2 Eight_ Eight_ -> A.True_
      A.T2 Nine_ Nine_ -> A.True_
      _ -> A.False_

digits :: A.Acc (A.Vector Digit)
digits = A.generate (A.Z_ A.::. 9) (\(A.I1 i) -> word16ToDigit (A.fromIntegral i))

test_issue551 :: RunN -> TestTree
test_issue551 runN = testCase "551" $
  (A.fromList (A.Z A.:. 9 A.:. 9) [i == j | i <- [0::Int ..8], j <- [0..8]]
  ,A.fromList (A.Z A.:. 9) [0..8])
    @=?
  runN (A.T2 (A.generate (A.Z_ A.::. 9 A.::. 9) (\(A.I2 i j) ->
                word16ToDigit (A.fromIntegral i) A.== word16ToDigit (A.fromIntegral j)))
             (A.map digitToWord16 digits))
