{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Prelude.SIMD
-- Copyright   : [2009..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Test.NoFib.Prelude.SIMD (

  test_simd,

) where

import Lens.Micro                                                 ( _1, _2, _3, _4 )
import Lens.Micro.Extras                                          ( view )
import Prelude                                                      as P

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Sugar.Array                            as S
import Data.Array.Accelerate.Sugar.Elt                              as S
import Data.Array.Accelerate.Sugar.Shape                            as S
import Data.Array.Accelerate.Test.NoFib.Base
import Data.Array.Accelerate.Test.NoFib.Config
import Data.Primitive.Vec
import Data.Primitive.Types

import Hedgehog
import qualified Hedgehog.Gen                                       as Gen

import Test.Tasty
import Test.Tasty.Hedgehog


test_simd :: RunN -> TestTree
test_simd runN =
  testGroup "simd"
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
    testElt :: forall e. (VecElt e, P.Eq e, Show e)
            => Gen e
            -> TestTree
    testElt e =
      testGroup (show (eltR @e))
        [ testExtract e
        , testInject  e
        ]

    testExtract :: forall e. (VecElt e, P.Eq e, Show e)
                => Gen e
                -> TestTree
    testExtract e =
      testGroup "extract"
        [ testProperty "V2" $ test_extract_v2 runN dim1 e
        , testProperty "V3" $ test_extract_v3 runN dim1 e
        , testProperty "V4" $ test_extract_v4 runN dim1 e
        ]

    testInject :: forall e. (VecElt e, P.Eq e, Show e)
               => Gen e
               -> TestTree
    testInject e =
      testGroup "inject"
        [ testProperty "V2" $ test_inject_v2 runN dim1 e
        , testProperty "V3" $ test_inject_v3 runN dim1 e
        , testProperty "V4" $ test_inject_v4 runN dim1 e
        ]


{-# NOINLINE test_extract_v2 #-}
test_extract_v2
    :: (Shape sh, Show sh, Show e, VecElt e, P.Eq e, P.Eq sh)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_extract_v2 runN dim e =
  property $ do
    sh      <- forAll dim
    xs      <- forAll (array sh (v2 e))
    (_l,_m) <- P.snd P.<$> forAllWith P.fst (Gen.element [("_1",(_1,_1)), ("_2",(_2,_2))])
    let !go = runN (A.map (view _m . unpackVec2')) in go xs === mapRef (view _l . unpackVec2) xs

{-# NOINLINE test_extract_v3 #-}
test_extract_v3
    :: (Shape sh, Show sh, Show e, VecElt e, P.Eq e, P.Eq sh)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_extract_v3 runN dim e =
  property $ do
    sh      <- forAll dim
    xs      <- forAll (array sh (v3 e))
    (_l,_m) <- P.snd P.<$> forAllWith P.fst (Gen.element [("_1",(_1,_1)), ("_2",(_2,_2)), ("_3",(_3,_3))])
    let !go = runN (A.map (view _m . unpackVec3')) in go xs === mapRef (view _l . unpackVec3) xs

{-# NOINLINE test_extract_v4 #-}
test_extract_v4
    :: (Shape sh, Show sh, Show e, VecElt e, P.Eq e, P.Eq sh)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_extract_v4 runN dim e =
  property $ do
    sh      <- forAll dim
    xs      <- forAll (array sh (v4 e))
    (_l,_m) <- P.snd P.<$> forAllWith P.fst (Gen.element [("_1",(_1,_1)), ("_2",(_2,_2)), ("_3",(_3,_3)), ("_4",(_4,_4))])
    let !go = runN (A.map (view _m . unpackVec4')) in go xs === mapRef (view _l . unpackVec4) xs

{-# NOINLINE test_inject_v2 #-}
test_inject_v2
    :: (Shape sh, Show sh, Show e, VecElt e, P.Eq e, P.Eq sh)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_inject_v2 runN dim e =
  property $ do
    sh1 <- forAll dim
    sh2 <- forAll dim
    xs  <- forAll (array sh1 e)
    ys  <- forAll (array sh2 e)
    let !go = runN (A.zipWith A.V2) in go xs ys === zipWithRef Vec2 xs ys

{-# NOINLINE test_inject_v3 #-}
test_inject_v3
    :: (Shape sh, Show sh, Show e, VecElt e, P.Eq e, P.Eq sh)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_inject_v3 runN dim e =
  property $ do
    sh1 <- forAll dim
    sh2 <- forAll dim
    sh3 <- forAll dim
    xs  <- forAll (array sh1 e)
    ys  <- forAll (array sh2 e)
    zs  <- forAll (array sh3 e)
    let !go = runN (A.zipWith3 A.V3) in go xs ys zs === zipWith3Ref Vec3 xs ys zs

{-# NOINLINE test_inject_v4 #-}
test_inject_v4
    :: (Shape sh, Show sh, Show e, VecElt e, P.Eq e, P.Eq sh)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_inject_v4 runN dim e =
  property $ do
    sh1 <- forAll dim
    sh2 <- forAll dim
    sh3 <- forAll dim
    sh4 <- forAll dim
    xs  <- forAll (array sh1 e)
    ys  <- forAll (array sh2 e)
    zs  <- forAll (array sh3 e)
    ws  <- forAll (array sh4 e)
    let !go = runN (A.zipWith4 A.V4) in go xs ys zs ws === zipWith4Ref Vec4 xs ys zs ws


unpackVec2 :: Prim e => Vec2 e -> (e, e)
unpackVec2 (Vec2 a b) = (a, b)

unpackVec3 :: Prim e => Vec3 e -> (e, e, e)
unpackVec3 (Vec3 a b c) = (a, b, c)

unpackVec4 :: Prim e => Vec4 e -> (e, e, e, e)
unpackVec4 (Vec4 a b c d) = (a, b, c, d)

unpackVec2' :: VecElt e => Exp (Vec2 e) -> (Exp e, Exp e)
unpackVec2' (A.V2 a b) = (a, b)

unpackVec3' :: VecElt e => Exp (Vec3 e) -> (Exp e, Exp e, Exp e)
unpackVec3' (A.V3 a b c) = (a, b, c)

unpackVec4' :: VecElt e => Exp (Vec4 e) -> (Exp e, Exp e, Exp e, Exp e)
unpackVec4' (A.V4 a b c d) = (a, b, c, d)


-- Reference Implementation
-- ------------------------

mapRef :: (Shape sh, Elt a, Elt b) => (a -> b) -> Array sh a -> Array sh b
mapRef f xs = fromFunction (arrayShape xs) (\ix -> f (xs S.! ix))

zipWithRef
    :: (Shape sh, Elt a, Elt b, Elt c)
    => (a -> b -> c)
    -> Array sh a
    -> Array sh b
    -> Array sh c
zipWithRef f xs ys =
  fromFunction
    (S.shape xs `S.intersect` S.shape ys)
    (\ix -> f (xs S.! ix) (ys S.! ix))

zipWith3Ref
    :: (Shape sh, Elt a, Elt b, Elt c, Elt d)
    => (a -> b -> c -> d)
    -> Array sh a
    -> Array sh b
    -> Array sh c
    -> Array sh d
zipWith3Ref f xs ys zs =
  fromFunction
    (S.shape xs `S.intersect` S.shape ys `S.intersect` S.shape zs)
    (\ix -> f (xs S.! ix) (ys S.! ix) (zs S.! ix))

zipWith4Ref
    :: (Shape sh, Elt a, Elt b, Elt c, Elt d, Elt e)
    => (a -> b -> c -> d -> e)
    -> Array sh a
    -> Array sh b
    -> Array sh c
    -> Array sh d
    -> Array sh e
zipWith4Ref f xs ys zs ws =
  fromFunction
    (S.shape xs `S.intersect` S.shape ys `S.intersect` S.shape zs `S.intersect` S.shape ws)
    (\ix -> f (xs S.! ix) (ys S.! ix) (zs S.! ix) (ws S.! ix))

