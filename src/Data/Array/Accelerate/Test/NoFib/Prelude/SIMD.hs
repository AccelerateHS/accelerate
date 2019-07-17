{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Prelude.SIMD
-- Copyright   : [2009..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Test.NoFib.Prelude.SIMD (

  test_simd,

) where

import Data.Typeable
import Data.Primitive.Types
import Control.Lens                                                 ( view, _1, _2, _3, _4 )
import Prelude                                                      as P

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Array.Sugar                            as S
import Data.Array.Accelerate.Test.NoFib.Base
import Data.Array.Accelerate.Test.NoFib.Config
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Product

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
    testElt :: forall e. (Prim e, P.Eq e, Elt e, Elt (V2 e), Elt (V3 e), Elt (V4 e))
            => Gen e
            -> TestTree
    testElt e =
      testGroup (show (typeOf (undefined::e)))
        [ testExtract e
        , testInject  e
        ]

    testExtract :: forall e. (Prim e, P.Eq e, Elt e, Elt (V2 e), Elt (V3 e), Elt (V4 e))
                => Gen e
                -> TestTree
    testExtract e =
      testGroup "extract"
        [ testProperty "V2" $ test_extract_v2 runN dim1 e
        , testProperty "V3" $ test_extract_v3 runN dim1 e
        , testProperty "V4" $ test_extract_v4 runN dim1 e
        ]

    testInject :: forall e. (Prim e, P.Eq e, Elt e, Elt (V2 e), Elt (V3 e), Elt (V4 e))
               => Gen e
               -> TestTree
    testInject e =
      testGroup "inject"
        [ testProperty "V2" $ test_inject_v2 runN dim1 e
        , testProperty "V3" $ test_inject_v3 runN dim1 e
        , testProperty "V4" $ test_inject_v4 runN dim1 e
        ]


test_extract_v2
    :: (Shape sh, Prim e, P.Eq e, P.Eq sh, Elt e, Elt (V2 e))
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_extract_v2 runN dim e =
  property $ do
    sh      <- forAll dim
    xs      <- forAll (array sh (v2 e))
    (_l,_m) <- P.snd <$> forAllWith P.fst (Gen.element [("_1",(_1,_1)), ("_2",(_2,_2))])
    let !go = runN (A.map (view _m . unpackV2')) in go xs === mapRef (view _l . unpackV2) xs

test_extract_v3
    :: (Shape sh, Prim e, P.Eq e, P.Eq sh, Elt e, Elt (V3 e))
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_extract_v3 runN dim e =
  property $ do
    sh      <- forAll dim
    xs      <- forAll (array sh (v3 e))
    (_l,_m) <- P.snd <$> forAllWith P.fst (Gen.element [("_1",(_1,_1)), ("_2",(_2,_2)), ("_3",(_3,_3))])
    let !go = runN (A.map (view _m . unpackV3')) in go xs === mapRef (view _l . unpackV3) xs

test_extract_v4
    :: (Shape sh, Prim e, P.Eq e, P.Eq sh, Elt e, Elt (V4 e))
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_extract_v4 runN dim e =
  property $ do
    sh      <- forAll dim
    xs      <- forAll (array sh (v4 e))
    (_l,_m) <- P.snd <$> forAllWith P.fst (Gen.element [("_1",(_1,_1)), ("_2",(_2,_2)), ("_3",(_3,_3)), ("_4",(_4,_4))])
    let !go = runN (A.map (view _m . unpackV4')) in go xs === mapRef (view _l . unpackV4) xs

test_inject_v2
    :: (Shape sh, Prim e, P.Eq e, P.Eq sh, Elt e, Elt (V2 e))
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
    let !go = runN (A.zipWith packV2') in go xs ys === zipWithRef V2 xs ys

test_inject_v3
    :: (Shape sh, Prim e, P.Eq e, P.Eq sh, Elt e, Elt (V3 e))
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
    let !go = runN (A.zipWith3 packV3') in go xs ys zs === zipWith3Ref V3 xs ys zs

test_inject_v4
    :: (Shape sh, Prim e, P.Eq e, P.Eq sh, Elt e, Elt (V4 e))
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
    let !go = runN (A.zipWith4 packV4') in go xs ys zs ws === zipWith4Ref V4 xs ys zs ws


unpackV2' :: (Prim e, Elt e, Elt (V2 e)) => Exp (V2 e) -> (Exp e, Exp e)
unpackV2' e =
  ( Exp $ SuccTupIdx ZeroTupIdx `Prj` e
  , Exp $ ZeroTupIdx `Prj` e
  )

unpackV3' :: (Prim e, Elt e, Elt (V3 e)) => Exp (V3 e) -> (Exp e, Exp e, Exp e)
unpackV3' e =
  ( Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` e
  , Exp $ SuccTupIdx ZeroTupIdx `Prj` e
  , Exp $ ZeroTupIdx `Prj` e
  )

unpackV4' :: (Prim e, Elt e, Elt (V4 e)) => Exp (V4 e) -> (Exp e, Exp e, Exp e, Exp e)
unpackV4' e =
  ( Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` e
  , Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` e
  , Exp $ SuccTupIdx ZeroTupIdx `Prj` e
  , Exp $ ZeroTupIdx `Prj` e
  )

packV2' :: (Prim e, Elt e, Elt (V2 e)) => Exp e -> Exp e -> Exp (V2 e)
packV2' x y = Exp . Tuple $ NilTup `SnocTup` x `SnocTup` y

packV3' :: (Prim e, Elt e, Elt (V3 e)) => Exp e -> Exp e -> Exp e -> Exp (V3 e)
packV3' x y z = Exp . Tuple $ NilTup `SnocTup` x `SnocTup` y `SnocTup` z

packV4' :: (Prim e, Elt e, Elt (V4 e)) => Exp e -> Exp e -> Exp e -> Exp e -> Exp (V4 e)
packV4' x y z w = Exp . Tuple $ NilTup `SnocTup` x `SnocTup` y `SnocTup` z `SnocTup` w


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

