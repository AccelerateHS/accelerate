{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Prelude.Map
-- Copyright   : [2009..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Test.NoFib.Prelude.Map (

  test_map

) where

import Data.Bits                                                    as P
import Prelude                                                      as P

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Data.Bits                              as A
import Data.Array.Accelerate.Sugar.Array                            as S
import Data.Array.Accelerate.Sugar.Elt                              as S
import Data.Array.Accelerate.Sugar.Shape                            as S
import Data.Array.Accelerate.Test.NoFib.Base
import Data.Array.Accelerate.Test.NoFib.Config
import Data.Array.Accelerate.Test.Similar

import Hedgehog
import qualified Hedgehog.Gen                                       as Gen
import qualified Hedgehog.Range                                     as Range

import Test.Tasty
import Test.Tasty.Hedgehog


test_map :: RunN -> TestTree
test_map runN =
  testGroup "map"
    [ at @TestInt8   $ testIntegralElt i8
    , at @TestInt16  $ testIntegralElt i16
    , at @TestInt32  $ testIntegralElt i32
    , at @TestInt64  $ testIntegralElt i64
    , at @TestWord8  $ testIntegralElt w8
    , at @TestWord16 $ testIntegralElt w16
    , at @TestWord32 $ testIntegralElt w32
    , at @TestWord64 $ testIntegralElt w64
    , at @TestHalf   $ testFloatingElt (Gen.realFloat :: Range Half -> Gen Half)
    , at @TestFloat  $ testFloatingElt Gen.float
    , at @TestDouble $ testFloatingElt Gen.double
    ]
  where
    testIntegralElt
        :: forall a. ( P.Integral a, P.FiniteBits a
                     , A.Integral a, A.FiniteBits a
                     , A.FromIntegral a Double
                     , Similar a, Show a )
        => Gen a
        -> TestTree
    testIntegralElt e =
      testGroup (show (eltR @a))
        [ testDim dim0
        , testDim dim1
        , testDim dim2
        ]
      where
        testDim
            :: forall sh. (Shape sh, Show sh, P.Eq sh)
            => Gen sh
            -> TestTree
        testDim sh =
          testGroup ("DIM" P.++ show (rank @sh))
            [ -- operators on Num
              testProperty "neg"                $ test_negate runN sh e
            , testProperty "abs"                $ test_abs runN sh e
            , testProperty "signum"             $ test_signum runN sh e

              -- operators on Integral & Bits
            , testProperty "complement"         $ test_complement runN sh e
            , testProperty "popCount"           $ test_popCount runN sh e
            , testProperty "countLeadingZeros"  $ test_countLeadingZeros runN sh e
            , testProperty "countTrailingZeros" $ test_countTrailingZeros runN sh e

              -- conversions
            , testProperty "fromIntegral"       $ test_fromIntegral runN sh e
            ]

    testFloatingElt
        :: forall a. (P.RealFloat a, A.Floating a, A.RealFrac a, Similar a, Show a)
        => (Range a -> Gen a)
        -> TestTree
    testFloatingElt e =
      testGroup (show (eltR @a))
        [ testDim dim0
        , testDim dim1
        , testDim dim2
        ]
      where
        testDim
            :: forall sh. (Shape sh, Show sh, P.Eq sh)
            => Gen sh
            -> TestTree
        testDim sh =
          testGroup ("DIM" P.++ show (rank @sh))
            [ -- operators on Num
              testProperty "neg"        $ test_negate runN sh (fullrange e)
            , testProperty "abs"        $ test_abs runN sh (fullrange e)
            , testProperty "signum"     $ test_abs runN sh (fullrange e)

              -- operators on Fractional, Floating, RealFrac & RealFloat
            , testProperty "recip"      $ test_recip runN sh (fullrange e)
            , testProperty "sin"        $ test_sin runN sh (fullrange e)
            , testProperty "cos"        $ test_cos runN sh (fullrange e)
            , testProperty "tan"        $ test_tan runN sh (fullrange e `except` \v -> cos v ~= 0)
            , testProperty "asin"       $ test_asin runN sh (e (Range.linearFracFrom 0 (-1) 1))
            , testProperty "acos"       $ test_acos runN sh (e (Range.linearFracFrom 0 (-1) 1))
            , testProperty "atan"       $ test_atan runN sh (fullrange e)
            , testProperty "asinh"      $ test_asinh runN sh (e (Range.linearFracFrom 0 (-log_flt_max) (log_flt_max)))
            , testProperty "acosh"      $ test_acosh runN sh (e (Range.linearFrac 1 (sqrt flt_max)))
            , testProperty "atanh"      $ test_atanh runN sh (e (Range.linearFracFrom 0 (-1) 1))
            , testProperty "exp"        $ test_exp runN sh (fullrange e)
            , testProperty "sqrt"       $ test_sqrt runN sh (e (Range.linearFrac 0 flt_max))
            , testProperty "log"        $ test_log runN sh (e (Range.linearFrac 0 flt_max) `except` \v -> v P.== 0)
            , testProperty "truncate"   $ test_truncate runN sh (e (Range.linearFracFrom 0 (P.fromIntegral (minBound :: Int)) (P.fromIntegral (maxBound :: Int))))
            , testProperty "round"      $ test_round runN sh (e (Range.linearFracFrom 0 (P.fromIntegral (minBound :: Int)) (P.fromIntegral (maxBound :: Int))))
            , testProperty "floor"      $ test_floor runN sh (e (Range.linearFracFrom 0 (P.fromIntegral (minBound :: Int)) (P.fromIntegral (maxBound :: Int))))
            , testProperty "ceiling"    $ test_ceiling runN sh (e (Range.linearFracFrom 0 (P.fromIntegral (minBound :: Int)) (P.fromIntegral (maxBound :: Int))))
            ]

        -- NOTE: [asinh and acosh]
        --
        -- GHC uses an approximation for asinh and acosh which gives incorrect
        -- answers for extremely large values. I guess I should submit a bug
        -- report for that, but "for now" just test with a reduced range.

        fullrange :: P.RealFloat e => (Range e -> Gen e) -> Gen e
        fullrange gen = gen (Range.linearFracFrom 0 (-flt_max) flt_max)


{-# NOINLINE test_negate #-}
test_negate
    :: (Shape sh, Show sh, Similar e, Show e, A.Num e, P.Num e, P.Eq sh)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_negate runN dim e =
  property $ do
    sh <- forAll dim
    xs <- forAll (array sh e)
    let !go = runN (A.map negate) in go xs ~~~ mapRef negate xs

{-# NOINLINE test_abs #-}
test_abs
    :: (Shape sh, Show sh, Similar e, Show e, A.Num e, P.Num e, P.Eq sh)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_abs runN dim e =
  property $ do
    sh <- forAll dim
    xs <- forAll (array sh e)
    let !go = runN (A.map abs) in go xs ~~~ mapRef abs xs

{-# NOINLINE test_signum #-}
test_signum
    :: (Shape sh, Show sh, Similar e, Show e, A.Num e, P.Num e, P.Eq sh)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_signum runN dim e =
  property $ do
    sh <- forAll dim
    xs <- forAll (array sh e)
    let !go = runN (A.map signum) in go xs ~~~ mapRef signum xs

{-# NOINLINE test_complement #-}
test_complement
    :: (Shape sh, Show sh, Similar e, Show e, A.Bits e, P.Bits e, P.Eq sh)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_complement runN dim e =
  property $ do
    sh <- forAll dim
    xs <- forAll (array sh e)
    let !go = runN (A.map A.complement) in go xs ~~~ mapRef P.complement xs

{-# NOINLINE test_popCount #-}
test_popCount
    :: (Shape sh, Show sh, Show e, A.Bits e, P.Bits e, P.Eq sh)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_popCount runN dim e =
  property $ do
    sh <- forAll dim
    xs <- forAll (array sh e)
    let !go = runN (A.map A.popCount) in go xs ~~~ mapRef P.popCount xs

{-# NOINLINE test_countLeadingZeros #-}
test_countLeadingZeros
    :: (Shape sh, Show sh, Show e, A.FiniteBits e, P.FiniteBits e, P.Eq sh)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_countLeadingZeros runN dim e =
  property $ do
    sh <- forAll dim
    xs <- forAll (array sh e)
    let !go = runN (A.map A.countLeadingZeros) in go xs ~~~ mapRef countLeadingZerosRef xs

{-# NOINLINE test_countTrailingZeros #-}
test_countTrailingZeros
    :: (Shape sh, Show sh, Show e, A.FiniteBits e, P.FiniteBits e, P.Eq sh)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_countTrailingZeros runN dim e =
  property $ do
    sh <- forAll dim
    xs <- forAll (array sh e)
    let !go = runN (A.map A.countTrailingZeros) in go xs ~~~ mapRef countTrailingZerosRef xs

{-# NOINLINE test_fromIntegral #-}
test_fromIntegral
    :: forall sh e. (Shape sh, Show sh, Show e, P.Eq sh, P.Integral e, A.Integral e, A.FromIntegral e Double)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_fromIntegral runN dim e =
  property $ do
    sh <- forAll dim
    xs <- forAll (array sh e)
    let !go = runN (A.map A.fromIntegral) in go xs ~~~ mapRef (P.fromIntegral :: e -> Double) xs

{-# NOINLINE test_recip #-}
test_recip
    :: (Shape sh, Show sh, Similar e, Show e, P.Eq sh, P.Fractional e, A.Fractional e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_recip runN dim e =
  property $ do
    sh <- forAll dim
    xs <- forAll (array sh e)
    let !go = runN (A.map recip) in go xs ~~~ mapRef recip xs

{-# NOINLINE test_sin #-}
test_sin
    :: (Shape sh, Show sh, Similar e, Show e, P.Eq sh, P.Floating e, A.Floating e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_sin runN dim e =
  property $ do
    sh <- forAll dim
    xs <- forAll (array sh e)
    let !go = runN (A.map sin) in go xs ~~~ mapRef sin xs

{-# NOINLINE test_cos #-}
test_cos
    :: (Shape sh, Show sh, Similar e, Show e, P.Eq sh, P.Floating e, A.Floating e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_cos runN dim e =
  property $ do
    sh <- forAll dim
    xs <- forAll (array sh e)
    let !go = runN (A.map cos) in go xs ~~~ mapRef cos xs

{-# NOINLINE test_tan #-}
test_tan
    :: (Shape sh, Show sh, Similar e, Show e, P.Eq sh, P.Floating e, A.Floating e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_tan runN dim e =
  property $ do
    sh <- forAll dim
    xs <- forAll (array sh e)
    let !go = runN (A.map tan) in go xs ~~~ mapRef tan xs

{-# NOINLINE test_asin #-}
test_asin
    :: (Shape sh, Show sh, Similar e, Show e, P.Eq sh, P.Floating e, A.Floating e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_asin runN dim e =
  property $ do
    sh <- forAll dim
    xs <- forAll (array sh e)
    let !go = runN (A.map asin) in go xs ~~~ mapRef asin xs

{-# NOINLINE test_acos #-}
test_acos
    :: (Shape sh, Show sh, Similar e, Show e, P.Eq sh, P.Floating e, A.Floating e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_acos runN dim e =
  property $ do
    sh <- forAll dim
    xs <- forAll (array sh e)
    let !go = runN (A.map acos) in go xs ~~~ mapRef acos xs

{-# NOINLINE test_atan #-}
test_atan
    :: (Shape sh, Show sh, Similar e, Show e, P.Eq sh, P.Floating e, A.Floating e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_atan runN dim e =
  property $ do
    sh <- forAll dim
    xs <- forAll (array sh e)
    let !go = runN (A.map atan) in go xs ~~~ mapRef atan xs

{-# NOINLINE test_asinh #-}
test_asinh
    :: (Shape sh, Show sh, Similar e, Show e, P.Eq sh, P.Floating e, A.Floating e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_asinh runN dim e =
  property $ do
    sh <- forAll dim
    xs <- forAll (array sh e)
    let !go = runN (A.map asinh) in go xs ~~~ mapRef asinh xs

{-# NOINLINE test_acosh #-}
test_acosh
    :: (Shape sh, Show sh, Similar e, Show e, P.Eq sh, P.Floating e, A.Floating e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_acosh runN dim e =
  property $ do
    sh <- forAll dim
    xs <- forAll (array sh e)
    let !go = runN (A.map acosh) in go xs ~~~ mapRef acosh xs

{-# NOINLINE test_atanh #-}
test_atanh
    :: (Shape sh, Show sh, Similar e, Show e, P.Eq sh, P.Floating e, A.Floating e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_atanh runN dim e =
  property $ do
    sh <- forAll dim
    xs <- forAll (array sh e)
    let !go = runN (A.map atanh) in go xs ~~~ mapRef atanh xs

{-# NOINLINE test_exp #-}
test_exp
    :: (Shape sh, Show sh, Similar e, Show e, P.Eq sh, P.Floating e, A.Floating e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_exp runN dim e =
  property $ do
    sh <- forAll dim
    xs <- forAll (array sh e)
    let !go = runN (A.map exp) in go xs ~~~ mapRef exp xs

{-# NOINLINE test_sqrt #-}
test_sqrt
    :: (Shape sh, Show sh, Similar e, Show e, P.Eq sh, P.Floating e, A.Floating e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_sqrt runN dim e =
  property $ do
    sh <- forAll dim
    xs <- forAll (array sh e)
    let !go = runN (A.map sqrt) in go xs ~~~ mapRef sqrt xs

{-# NOINLINE test_log #-}
test_log
    :: (Shape sh, Show sh, Similar e, Show e, P.Eq sh, P.Floating e, A.Floating e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_log runN dim e =
  property $ do
    sh <- forAll dim
    xs <- forAll (array sh e)
    let !go = runN (A.map log) in go xs ~~~ mapRef log xs

{-# NOINLINE test_truncate #-}
test_truncate
    :: forall sh e. (Shape sh, Show sh, Show e, P.Eq sh, P.RealFrac e, A.RealFrac e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_truncate runN dim e =
  property $ do
    sh <- forAll dim
    xs <- forAll (array sh e)
    let !go = runN (A.map A.truncate) in go xs ~~~ mapRef (P.truncate :: e -> Int) xs

{-# NOINLINE test_round #-}
test_round
    :: forall sh e. (Shape sh, Show sh, Show e, P.Eq sh, P.RealFrac e, A.RealFrac e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_round runN dim e =
  property $ do
    sh <- forAll dim
    xs <- forAll (array sh e)
    let !go = runN (A.map A.round) in go xs ~~~ mapRef (P.round :: e -> Int) xs

{-# NOINLINE test_floor #-}
test_floor
    :: forall sh e. (Shape sh, Show sh, Show e, P.Eq sh, P.RealFrac e, A.RealFrac e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_floor runN dim e =
  property $ do
    sh <- forAll dim
    xs <- forAll (array sh e)
    let !go = runN (A.map A.floor) in go xs ~~~ mapRef (P.floor :: e -> Int) xs

{-# NOINLINE test_ceiling #-}
test_ceiling
    :: forall sh e. (Shape sh, Show sh, Show e, P.Eq sh, P.RealFrac e, A.RealFrac e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_ceiling runN dim e =
  property $ do
    sh <- forAll dim
    xs <- forAll (array sh e)
    let !go = runN (A.map A.ceiling) in go xs ~~~ mapRef (P.ceiling :: e -> Int) xs


-- Reference Implementation
-- ------------------------

mapRef :: (Shape sh, Elt a, Elt b) => (a -> b) -> Array sh a -> Array sh b
mapRef f xs = fromFunction (arrayShape xs) (\ix -> f (xs S.! ix))

countLeadingZerosRef :: P.FiniteBits a => a -> Int
countLeadingZerosRef = P.countLeadingZeros

countTrailingZerosRef :: P.FiniteBits a => a -> Int
countTrailingZerosRef = P.countTrailingZeros

