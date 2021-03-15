{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
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
assert :: forall a bs. (Assertion a bs, Arrays bs) => String -> a -> Acc bs -> Acc bs
assert text assertion result
  = A.acond (assertionCondition assertion result) result
  $ Acc
  $ SmartAcc
  $ Aerror (S.arraysR @bs)
           (assertionMessage @a @bs $ "Assertion failed: " ++ text)
           arg
  where
    Acc arg = assertionArg assertion result

class Arrays (AssertionArg a res) => Assertion a res where
  type AssertionArg a res

  assertionArg       :: a -> Acc res -> Acc (AssertionArg a res)
  assertionMessage   :: String -> Message (ArraysR (AssertionArg a res))
  assertionCondition :: a -> Acc res -> Exp Bool

instance Assertion (Exp Bool) res where
  type AssertionArg (Exp Bool) res = ()

  assertionArg _ _ = Acc (SmartAcc Anil)
  assertionMessage = Message (\_ -> "") (Just [|| \_ -> "" ||])
  assertionCondition = const

instance Assertion (Acc (Scalar Bool)) res where
  type AssertionArg (Acc (Scalar Bool)) res = ()

  assertionArg _ _ = Acc (SmartAcc Anil)
  assertionMessage = Message (\_ -> "") (Just [|| \_ -> "" ||])
  assertionCondition a _ = A.the a

instance (Assertion a (), Show res, Arrays res) => Assertion (Acc res -> a) res where
  type AssertionArg (Acc res -> a) res = res

  assertionArg _ res = res
  assertionMessage = Message  (\xs -> "\n" ++ show (toArr @res xs))
                     (Just [||(\xs -> "\n" ++ show (toArr @res xs)) ||])
  assertionCondition f res = assertionCondition (f res) (Acc (SmartAcc Anil) :: Acc ())

data AssertEqual e = AssertEqual (Exp e) (Exp e)

expEqual :: Exp e -> Exp e -> AssertEqual e
expEqual = AssertEqual

instance (Elt e, A.Eq e, Show e) => Assertion (AssertEqual e) res where
  type AssertionArg (AssertEqual e) res = Scalar (e, e)

  assertionArg (AssertEqual a b) _ = A.unit (A.T2 a b)
  assertionMessage = Message  (\e -> let (a, b) = toElt @(e, e) (R.indexArray (R.ArrayR R.dim0 (eltR @(e, e))) e ()) in show a ++ " does not equal " ++ show b)
                     (Just [||(\e -> let (a, b) = toElt @(e, e) (R.indexArray (R.ArrayR R.dim0 (eltR @(e, e))) e ()) in show a ++ " does not equal " ++ show b) ||])
  assertionCondition (AssertEqual a b) _ = a A.== b

data AssertArraysEqual as = AssertArraysEqual (Acc as) (Acc as)

arraysEqual :: Acc as -> Acc as -> AssertArraysEqual as
arraysEqual = AssertArraysEqual

instance (Show sh, Show e, A.Shape sh, Elt e, A.Eq sh, A.Eq e) => Assertion (AssertArraysEqual (S.Array sh e)) res where
  type AssertionArg (AssertArraysEqual (S.Array sh e)) res = (S.Array sh e, S.Array sh e)

  assertionArg (AssertArraysEqual xs ys) _ = A.T2 xs ys
  assertionMessage = Message  (\(((), xs), ys) -> "\n" ++ show (toArr @(S.Array sh e) xs) ++ "\ndoes not equal\n" ++ show (toArr @(S.Array sh e) ys))
                     (Just [||(\(((), xs), ys) -> "\n" ++ show (toArr @(S.Array sh e) xs) ++ "\ndoes not equal\n" ++ show (toArr @(S.Array sh e) ys)) ||])
  assertionCondition (AssertArraysEqual xs ys) _ = (A.shape xs A.== A.shape ys) A.&& A.the (A.all id $ A.reshape (A.I1 $ A.size xs) $ A.zipWith (A.==) xs ys)
