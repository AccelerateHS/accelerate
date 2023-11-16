{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Prelude.Backpermute
-- Copyright   : [2009..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Test.NoFib.Prelude.Backpermute (

  test_backpermute

) where

import Prelude                                                      as P

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Test.NoFib.Base
import Data.Array.Accelerate.Test.NoFib.Config
import Data.Array.Accelerate.Test.Similar
import qualified Data.Array.Accelerate.Sugar.Shape                  as S

import Hedgehog
import qualified Hedgehog.Gen                                       as Gen
import qualified Hedgehog.Range                                     as Range

import Test.Tasty
import Test.Tasty.Hedgehog


test_backpermute :: RunN -> TestTree
test_backpermute runN =
  testGroup "backpermute"
    [ at @TestInt8   $ testElt i8
    , at @TestInt16  $ testElt i16
    , at @TestInt32  $ testElt i32
    , at @TestInt64  $ testElt i64
    , at @TestWord8  $ testElt w8
    , at @TestWord16 $ testElt w16
    , at @TestWord32 $ testElt w32
    , at @TestWord64 $ testElt w64
    , at @TestHalf   $ testElt f16
    , at @TestFloat  $ testElt f32
    , at @TestDouble $ testElt f64
    ]
  where
    testElt
        :: forall a. (Similar a, Elt a, Show a)
        => Gen a
        -> TestTree
    testElt e =
      testGroup (show (eltR @a))
        [ testDim dim1
        , testDim dim2
        , testDim dim3
        ]
      where
        testDim
            :: forall sh. (Shape sh, Slice sh, Show sh, P.Eq sh)
            => Gen (sh:.Int)
            -> TestTree
        testDim sh =
          testGroup ("DIM" P.++ show (S.rank @(sh:.Int)))
            [
              testProperty "take"         $ test_take runN sh e
            , testProperty "drop"         $ test_drop runN sh e
            , testProperty "gather->DIM1" $ test_gather runN sh dim1 e
            , testProperty "gather->DIM2" $ test_gather runN sh dim2 e
            , testProperty "gather->DIM3" $ test_gather runN sh dim3 e
            ]

test_take
    :: (Shape sh, Slice sh, Show sh, Similar e, Show e, P.Eq sh, Elt e)
    => RunN
    -> Gen (sh:.Int)
    -> Gen e
    -> Property
test_take runN dim e =
  property $ do
    sh@(_:.n) <- forAll dim
    xs        <- forAll (array sh e)
    i         <- forAll (Gen.int (Range.linear (-2) (n+1)))
    let !go = runN (\v -> A.take (the v)) in go (scalar i) xs ~~~ takeRef i xs

test_drop
    :: (Shape sh, Slice sh, Show sh, Similar e, Show e, P.Eq sh, Elt e)
    => RunN
    -> Gen (sh:.Int)
    -> Gen e
    -> Property
test_drop runN dim e =
  property $ do
    sh@(_:.n) <- forAll dim
    xs        <- forAll (array sh e)
    i         <- forAll (Gen.int (Range.linear (-2) (n+1)))
    let !go = runN (\v -> A.drop (the v)) in go (scalar i) xs ~~~ dropRef i xs

test_gather
    :: (Shape sh, Shape sh', Show sh, Show sh', P.Eq sh', Similar e, Show e, Elt e)
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
    ix  <- forAll (toIxArr P.<$> Gen.list (Range.singleton n') (Gen.int (Range.linear 0 (n-1))))
    --
    let !go = runN $ \i -> A.backpermute (A.shape i) (i A.!)
    --
    go ix xs ~~~ backpermuteRef sh' (ix `indexArray`) xs


scalar :: Elt e => e -> Scalar e
scalar x = fromFunction Z (const x)

backpermuteRef
    :: (Shape sh, Shape sh', Elt e)
    => sh'
    -> (sh' -> sh)
    -> Array sh  e
    -> Array sh' e
backpermuteRef sh' p arr =
  fromFunction sh' (\ix -> arr `indexArray` p ix)

takeRef
    :: (Shape sh, Slice sh, Elt e)
    => Int
    -> Array (sh:.Int) e
    -> Array (sh:.Int) e
takeRef n arr =
  let sh :. m = arrayShape arr
  in  fromFunction (sh :. P.min m n) (arr `indexArray`)

dropRef
    :: (Shape sh, Slice sh, Elt e)
    => Int
    -> Array (sh:.Int) e
    -> Array (sh:.Int) e
dropRef n arr =
  let sh :. m = arrayShape arr
      n' = P.max 0 n
  in  fromFunction (sh :. P.max 0 (m - n')) (\(sz:.i) -> arr `indexArray` (sz :. i+n'))

