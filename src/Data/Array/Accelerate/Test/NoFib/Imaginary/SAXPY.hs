{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Imaginary.SAXPY
-- Copyright   : [2009..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Test.NoFib.Imaginary.SAXPY (

  test_saxpy

) where

import Data.Proxy
import Data.Typeable
import Prelude                                                  as P

import Data.Array.Accelerate                                    as A
import Data.Array.Accelerate.Array.Sugar                        as S
import Data.Array.Accelerate.Test.NoFib.Base
import Data.Array.Accelerate.Test.NoFib.Config
import Data.Array.Accelerate.Test.Similar

import Hedgehog
import qualified Hedgehog.Gen                                   as Gen
import qualified Hedgehog.Range                                 as Range

import Test.Tasty
import Test.Tasty.Hedgehog


test_saxpy :: RunN -> TestTree
test_saxpy runN =
  testGroup "saxpy"
    [ at (Proxy::Proxy TestInt8)   $ testElt i8
    , at (Proxy::Proxy TestInt16)  $ testElt i16
    , at (Proxy::Proxy TestInt32)  $ testElt i32
    , at (Proxy::Proxy TestInt64)  $ testElt i64
    , at (Proxy::Proxy TestWord8)  $ testElt w8
    , at (Proxy::Proxy TestWord16) $ testElt w16
    , at (Proxy::Proxy TestWord32) $ testElt w32
    , at (Proxy::Proxy TestWord64) $ testElt w64
    , at (Proxy::Proxy TestFloat)  $ testElt f32
    , at (Proxy::Proxy TestDouble) $ testElt f64
    ]
  where
    testElt :: forall a. (P.Num a, P.Ord a , A.Num a, A.Ord a , Similar a)
        => Gen a
        -> TestTree
    testElt e =
      testProperty (show (typeOf (undefined :: a))) $ test_saxpy' runN e


test_saxpy'
    :: (P.Num e, A.Num e, Similar e)
    => RunN
    -> Gen e
    -> Property
test_saxpy' runN e =
  property $ do
    sh    <- forAll ((Z:.) <$> Gen.int (Range.linear 0 16384))
    alpha <- forAll e
    xs    <- forAll (array sh e)
    ys    <- forAll (array sh e)
    let !go = runN saxpy in go (scalar alpha) xs ys ~~~ saxpyRef alpha xs ys

scalar :: Elt e => e -> Scalar e
scalar x = fromFunction Z (const x)

saxpy :: A.Num e => Acc (Scalar e) -> Acc (Vector e) -> Acc (Vector e) -> Acc (Vector e)
saxpy alpha xs ys = A.zipWith (\x y -> the alpha * x + y) xs ys

saxpyRef :: (P.Num e, Elt e) => e -> Vector e -> Vector e -> Vector e
saxpyRef alpha xs ys =
  fromFunction
    (S.shape xs `S.intersect` S.shape ys)
    (\ix -> alpha * (xs S.! ix) + (ys S.! ix))

