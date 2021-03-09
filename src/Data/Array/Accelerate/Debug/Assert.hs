{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
-- Module      : Data.Array.Accelerate.Debug.Assert
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Functions for checking properties or invariants
-- of a program.
--
-- @since 1.4.0.0
--

module Data.Array.Accelerate.Debug.Assert (

  -- * Assertions
  -- $assertions
  --
  assert, Assertion,
  expEqual, AssertEqual,
  arraysEqual, AssertArraysEqual,

) where

import qualified Data.Array.Accelerate                              as A
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Array                            as S
import Data.Array.Accelerate.Sugar.Elt
import qualified Data.Array.Accelerate.Representation.Array         as R
import qualified Data.Array.Accelerate.Representation.Shape         as R


-- $assertions
--
-- The 'assert' function verifies whether a predicate holds and will stop
-- the execution of the array computation if the assertion does not hold.
-- It will then also print the given error message to the console.
--
-- The predicate can be passed as a boolean expression ('Exp Bool'), but we
-- have specialized assertions for array equivalence ('arraysEqual') and
-- scalar equivalence ('expEqual').
--

-- Verifies whether the predicate holds, before the computation can continue
-- with the result of the last argument. If the assertion does not hold,
-- it will stop the array computation and print the error message.
--
assert :: forall a bs. (Assertion a, Arrays bs) => String -> a -> Acc bs -> Acc bs
assert text assertion result
  = A.acond (assertionCondition assertion) result
  $ Acc
  $ SmartAcc
  $ Aerror (S.arraysR @bs)
           (assertionMessage assertion $ "Assertion failed: " ++ text)
           arg
  where
    Acc arg = assertionArg assertion

class Arrays (AssertionArg a) => Assertion a where
  type AssertionArg a

  assertionArg :: a -> Acc (AssertionArg a)
  assertionMessage :: a -> String -> Message (ArraysR (AssertionArg a))
  assertionCondition :: a -> Exp Bool

instance Assertion (Exp Bool) where
  type AssertionArg (Exp Bool) = ()

  assertionArg _ = Acc (SmartAcc Anil)
  assertionMessage _ = Message (\_ -> "") (Just [|| \_ -> "" ||])
  assertionCondition = id

data AssertEqual e = AssertEqual (Exp e) (Exp e)

expEqual :: Exp e -> Exp e -> AssertEqual e
expEqual = AssertEqual

instance (Elt e, A.Eq e, Show e) => Assertion (AssertEqual e) where
  type AssertionArg (AssertEqual e) = Scalar (e, e)

  assertionArg (AssertEqual a b) = A.unit (A.T2 a b)
  assertionMessage _ = Message  (\e -> let (a, b) = toElt @(e, e) (R.indexArray (R.ArrayR R.dim0 (eltR @(e, e))) e ()) in show a ++ " does not equal " ++ show b)
                       (Just [||(\e -> let (a, b) = toElt @(e, e) (R.indexArray (R.ArrayR R.dim0 (eltR @(e, e))) e ()) in show a ++ " does not equal " ++ show b) ||])
  assertionCondition (AssertEqual a b) = a A.== b

data AssertArraysEqual as = AssertArraysEqual (Acc as) (Acc as)

arraysEqual :: Acc as -> Acc as -> AssertArraysEqual as
arraysEqual = AssertArraysEqual

instance (Show sh, Show e, A.Shape sh, Elt e, A.Eq sh, A.Eq e) => Assertion (AssertArraysEqual (S.Array sh e)) where
  type AssertionArg (AssertArraysEqual (S.Array sh e)) = (S.Array sh e, S.Array sh e)

  assertionArg (AssertArraysEqual xs ys) = A.T2 xs ys
  assertionMessage _ = Message  (\(((), xs), ys) -> "\n" ++ show (toArr @(S.Array sh e) xs) ++ "\ndoes not equal\n" ++ show (toArr @(S.Array sh e) ys))
                       (Just [||(\(((), xs), ys) -> "\n" ++ show (toArr @(S.Array sh e) xs) ++ "\ndoes not equal\n" ++ show (toArr @(S.Array sh e) ys)) ||])
  assertionCondition (AssertArraysEqual xs ys) = (A.shape xs A.== A.shape ys) A.&& A.the (A.all id $ A.reshape (A.I1 $ A.size xs) $ A.zipWith (A.==) xs ys)
