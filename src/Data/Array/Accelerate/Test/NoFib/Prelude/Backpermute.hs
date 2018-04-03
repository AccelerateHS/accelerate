{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Prelude.Backpermute
-- Copyright   : [2009..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Test.NoFib.Prelude.Backpermute (

  test_backpermute

) where

import Data.Proxy
import Data.Typeable
import Prelude                                                      as P

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Array.Sugar                            as S
import Data.Array.Accelerate.Test.NoFib.Base
import Data.Array.Accelerate.Test.NoFib.Config
import Data.Array.Accelerate.Test.Similar

import Hedgehog
import qualified Hedgehog.Gen                                       as Gen
import qualified Hedgehog.Range                                     as Range

import Test.Tasty
import Test.Tasty.Hedgehog


test_backpermute :: RunN -> TestTree
test_backpermute runN =
  testGroup "backpermute"
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
    testElt
        :: forall a. (Similar a, Elt a)
        => Gen a
        -> TestTree
    testElt e =
      testGroup (show (typeOf (undefined :: a)))
        [ testDim dim1
        , testDim dim2
        , testDim dim3
        ]
      where
        testDim
            :: forall sh. (Shape sh, Slice sh, P.Eq sh)
            => Gen (sh:.Int)
            -> TestTree
        testDim sh =
          testGroup ("DIM" P.++ show (rank (undefined::(sh:.Int))))
            [
              testProperty "take"         $ test_take runN sh e
            , testProperty "drop"         $ test_drop runN sh e
            , testProperty "gather->DIM1" $ test_gather runN sh dim1 e
            , testProperty "gather->DIM2" $ test_gather runN sh dim2 e
            , testProperty "gather->DIM3" $ test_gather runN sh dim3 e
            ]

test_take
    :: (Shape sh, Slice sh, Similar e, P.Eq sh, Elt e)
    => RunN
    -> Gen (sh:.Int)
    -> Gen e
    -> Property
test_take runN dim e =
  property $ do
    sh@(_:.n) <- forAll dim
    xs        <- forAll (array sh e)
    i         <- forAll (Gen.int (Range.linear 0 (n-1)))
    let !go = runN (\v -> A.take (the v)) in go (scalar i) xs ~~~ takeRef i xs

test_drop
    :: (Shape sh, Slice sh, Similar e, P.Eq sh, Elt e)
    => RunN
    -> Gen (sh:.Int)
    -> Gen e
    -> Property
test_drop runN dim e =
  property $ do
    sh@(_:.n) <- forAll dim
    xs        <- forAll (array sh e)
    i         <- forAll (Gen.int (Range.linear 0 (n-1)))
    let !go = runN (\v -> A.drop (the v)) in go (scalar i) xs ~~~ dropRef i xs

test_gather
    :: (Shape sh, Shape sh', P.Eq sh', Similar e, Elt e)
    => RunN
    -> Gen sh
    -> Gen sh'
    -> Gen e
    -> Property
test_gather runN dim dim' e =
  property $ do
    sh  <- forAll (dim `except` \v -> S.size v P.== 0)
    sh' <- forAll dim'
    let
        n       = S.size sh
        n'      = S.size sh'
        toIxArr = fromList sh' . P.map (S.fromIndex sh)
    --
    xs  <- forAll (array sh e)
    ix  <- forAll (toIxArr <$> Gen.list (Range.singleton n') (Gen.int (Range.linear 0 (n-1))))
    --
    let !go = runN $ \i -> A.backpermute (A.shape i) (i A.!)
    --
    go ix xs ~~~ backpermuteRef sh' (ix S.!) xs


scalar :: Elt e => e -> Scalar e
scalar x = fromFunction Z (const x)

backpermuteRef
    :: (Shape sh, Shape sh', Elt e)
    => sh'
    -> (sh' -> sh)
    -> Array sh  e
    -> Array sh' e
backpermuteRef sh' p arr =
  fromFunction sh' (\ix -> arr S.! p ix)

takeRef
    :: (Shape sh, Slice sh, Elt e)
    => Int
    -> Array (sh:.Int) e
    -> Array (sh:.Int) e
takeRef n arr =
  let sh :. m = S.shape arr
  in  fromFunction (sh :. P.min m n) (arr S.!)

dropRef
    :: (Shape sh, Slice sh, Elt e)
    => Int
    -> Array (sh:.Int) e
    -> Array (sh:.Int) e
dropRef n arr =
  let sh :. m = S.shape arr
  in  fromFunction (sh :. P.max 0 (m-n)) (\(sz:.i) -> arr S.! (sz :. i+n))

