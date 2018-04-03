{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Imaginary.DotP
-- Copyright   : [2009..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Test.NoFib.Imaginary.DotP (

  test_dotp

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


test_dotp :: RunN -> TestTree
test_dotp runN =
  testGroup "dot product"
    [ at (Proxy::Proxy TestInt8)   $ testElt i8
    , at (Proxy::Proxy TestInt16)  $ testElt i16
    , at (Proxy::Proxy TestInt32)  $ testElt i32
    , at (Proxy::Proxy TestInt64)  $ testElt i64
    , at (Proxy::Proxy TestWord8)  $ testElt w8
    , at (Proxy::Proxy TestWord16) $ testElt w16
    , at (Proxy::Proxy TestWord32) $ testElt w32
    , at (Proxy::Proxy TestWord64) $ testElt w64
    , at (Proxy::Proxy TestHalf)   $ testElt f16
    , at (Proxy::Proxy TestFloat)  $ testElt f32
    , at (Proxy::Proxy TestDouble) $ testElt f64
    ]
  where
    testElt :: forall a. (P.Num a, P.Ord a , A.Num a, A.Ord a , Similar a)
        => Gen a
        -> TestTree
    testElt e =
      testProperty (show (typeOf (undefined :: a))) $ test_dotp' runN e


test_dotp'
    :: (P.Num e, A.Num e, Similar e)
    => RunN
    -> Gen e
    -> Property
test_dotp' runN e =
  property $ do
    sh <- forAll ((Z:.) <$> Gen.int (Range.linear 0 16384))
    xs <- forAll (array sh e)
    ys <- forAll (array sh e)
    let !go = runN dotp in go xs ys S.! Z ~~~ dotpRef xs ys

dotp :: A.Num e => Acc (Vector e) -> Acc (Vector e) -> Acc (Scalar e)
dotp xs ys
  = A.fold (+) 0
  $ A.zipWith (*) xs ys

dotpRef :: P.Num e => Vector e -> Vector e -> e
dotpRef xs ys
  = P.sum ( P.zipWith (*) (toList xs) (toList ys) )

