{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Prelude.ZipWith
-- Copyright   : [2009..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Test.NoFib.Prelude.ZipWith (

  test_zipWith

) where

import Data.Bits                                                    as P
import Prelude                                                      as P

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Data.Bits                              as A
import Data.Array.Accelerate.Smart                                  ( ($$) )
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Test.NoFib.Base
import Data.Array.Accelerate.Test.NoFib.Config
import Data.Array.Accelerate.Test.Similar
import qualified Data.Array.Accelerate.Sugar.Array                  as S
import qualified Data.Array.Accelerate.Sugar.Shape                  as S

import Hedgehog
import qualified Hedgehog.Gen                                       as Gen
import qualified Hedgehog.Range                                     as Range

import Test.Tasty
import Test.Tasty.Hedgehog


test_zipWith :: RunN -> TestTree
test_zipWith runN =
  testGroup "zipWith"
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
            :: forall sh. (Shape sh, Show sh, P.Eq sh, Show sh)
            => Gen sh
            -> TestTree
        testDim sh =
          testGroup ("DIM" P.++ show (S.rank @sh))
            [ -- operators on Num
              testProperty "(+)"          $ test_plus runN sh e
            , testProperty "(-)"          $ test_minus runN sh e
            , testProperty "(*)"          $ test_mult runN sh e

              -- operators on Integral & Bits
            , testProperty "quot"         $ test_quot runN sh e
            , testProperty "rem"          $ test_rem runN sh e
            , testProperty "quotRem"      $ test_quotRem runN sh e
            , testProperty "div"          $ test_idiv runN sh e
            , testProperty "mod"          $ test_mod runN sh e
            , testProperty "divMod"       $ test_divMod runN sh e
            , testProperty "(.&.)"        $ test_band runN sh e
            , testProperty "(.|.)"        $ test_bor runN sh e
            , testProperty "xor"          $ test_xor runN sh e
            , testProperty "shift"        $ test_shift runN sh e
            , testProperty "shiftL"       $ test_shiftL runN sh e
            , testProperty "shiftR"       $ test_shiftR runN sh e
            , testProperty "rotate"       $ test_rotate runN sh e
            , testProperty "rotateL"      $ test_rotateL runN sh e
            , testProperty "rotateR"      $ test_rotateR runN sh e

              -- relational and equality operators
            , testProperty "(<)"          $ test_lt runN sh e
            , testProperty "(>)"          $ test_gt runN sh e
            , testProperty "(<=)"         $ test_lte runN sh e
            , testProperty "(>=)"         $ test_gte runN sh e
            , testProperty "(==)"         $ test_eq runN sh e
            , testProperty "(/=)"         $ test_neq runN sh e
            , testProperty "min"          $ test_min runN sh e
            , testProperty "max"          $ test_max runN sh e
            ]

    testFloatingElt
        :: forall a. (P.RealFloat a, A.RealFloat a, Similar a, Show a)
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
          testGroup ("DIM" P.++ show (S.rank @sh))
            [ -- operators on Num
              testProperty "(+)"          $ test_plus runN sh (full e)
            , testProperty "(-)"          $ test_minus runN sh (full e)
            , testProperty "(*)"          $ test_mult runN sh (full e)

              -- operators on Fractional, Floating, RealFrac & RealFloat
            , testProperty "(/)"          $ test_fdiv runN sh (full e)
            , testProperty "(**)"         $ test_pow runN sh (full e)
            , testProperty "atan2"        $ test_atan2 runN sh (full e)
            , testProperty "logBase"      $ test_logBase runN sh (e (Range.linearFrac 0 flt_max) `except` zero)

              -- relational and equality operators
            , testProperty "(<)"          $ test_lt runN sh (full e)
            , testProperty "(>)"          $ test_gt runN sh (full e)
            , testProperty "(<=)"         $ test_lte runN sh (full e)
            , testProperty "(>=)"         $ test_gte runN sh (full e)
            , testProperty "(==)"         $ test_eq runN sh (full e)
            , testProperty "(/=)"         $ test_neq runN sh (full e)
            , testProperty "min"          $ test_min runN sh (full e)
            , testProperty "max"          $ test_max runN sh (full e)
            ]

        full :: P.RealFloat e => (Range e -> Gen e) -> Gen e
        full gen = gen (Range.linearFracFrom 0 (-flt_max) flt_max)


zero :: (P.Num a, P.Eq a) => a -> Bool
zero x = x P.== 0

{-# NOINLINE test_plus #-}
test_plus
    :: (Shape sh, Show sh, Similar e, Show e, P.Eq sh, P.Num e, A.Num e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_plus runN dim e =
  property $ do
    sh1 <- forAll dim
    sh2 <- forAll dim
    xs  <- forAll (array sh1 e)
    ys  <- forAll (array sh2 e)
    let !go = runN (A.zipWith (+)) in go xs ys ~~~ zipWithRef (+) xs ys

{-# NOINLINE test_minus #-}
test_minus
    :: (Shape sh, Show sh, Similar e, Show e, P.Eq sh, P.Num e, A.Num e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_minus runN dim e =
  property $ do
    sh1 <- forAll dim
    sh2 <- forAll dim
    xs  <- forAll (array sh1 e)
    ys  <- forAll (array sh2 e)
    let !go = runN (A.zipWith (-)) in go xs ys ~~~ zipWithRef (-) xs ys

{-# NOINLINE test_mult #-}
test_mult
    :: (Shape sh, Show sh, Similar e, Show e, P.Eq sh, P.Num e, A.Num e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_mult runN dim e =
  property $ do
    sh1 <- forAll dim
    sh2 <- forAll dim
    xs  <- forAll (array sh1 e)
    ys  <- forAll (array sh2 e)
    let !go = runN (A.zipWith (*)) in go xs ys ~~~ zipWithRef (*) xs ys

{-# NOINLINE test_quot #-}
test_quot
    :: (Shape sh, Show sh, Similar e, Show e, P.Eq sh, P.Integral e, A.Integral e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_quot runN dim e =
  property $ do
    sh1 <- forAll dim
    sh2 <- forAll dim
    xs  <- forAll (array sh1 e)
    ys  <- forAll (array sh2 (e `except` zero))
    let !go = runN (A.zipWith quot) in go xs ys ~~~ zipWithRef quot xs ys

{-# NOINLINE test_rem #-}
test_rem
    :: (Shape sh, Show sh, Similar e, Show e, P.Eq sh, P.Integral e, A.Integral e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_rem runN dim e =
  property $ do
    sh1 <- forAll dim
    sh2 <- forAll dim
    xs  <- forAll (array sh1 e)
    ys  <- forAll (array sh2 (e `except` zero))
    let !go = runN (A.zipWith rem) in go xs ys ~~~ zipWithRef rem xs ys

{-# NOINLINE test_quotRem #-}
test_quotRem
    :: (Shape sh, Show sh, Similar e, Show e, P.Eq sh, P.Integral e, A.Integral e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_quotRem runN dim e =
  property $ do
    sh1 <- forAll dim
    sh2 <- forAll dim
    xs  <- forAll (array sh1 e)
    ys  <- forAll (array sh2 (e `except` zero))
    let !go = runN (A.zipWith (lift $$ quotRem)) in go xs ys ~~~ zipWithRef quotRem xs ys

{-# NOINLINE test_idiv #-}
test_idiv
    :: (Shape sh, Show sh, Similar e, Show e, P.Eq sh, P.Integral e, A.Integral e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_idiv runN dim e =
  property $ do
    sh1 <- forAll dim
    sh2 <- forAll dim
    xs  <- forAll (array sh1 e)
    ys  <- forAll (array sh2 (e `except` zero))
    let !go = runN (A.zipWith div) in go xs ys ~~~ zipWithRef div xs ys

{-# NOINLINE test_fdiv #-}
test_fdiv
    :: (Shape sh, Show sh, Similar e, Show e, P.Eq sh, P.Eq e, P.Fractional e, A.Fractional e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_fdiv runN dim e =
  property $ do
    sh1 <- forAll dim
    sh2 <- forAll dim
    xs  <- forAll (array sh1 e)
    ys  <- forAll (array sh2 (e `except` zero))
    let !go = runN (A.zipWith (/)) in go xs ys ~~~ zipWithRef (/) xs ys

{-# NOINLINE test_pow #-}
test_pow
    :: (Shape sh, Show sh, Similar e, Show e, P.Eq sh, P.Floating e, A.Floating e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_pow runN dim e =
  property $ do
    sh1 <- forAll dim
    sh2 <- forAll dim
    xs  <- forAll (array sh1 e)
    ys  <- forAll (array sh2 e)
    let !go = runN (A.zipWith (**)) in go xs ys ~~~ zipWithRef (**) xs ys

{-# NOINLINE test_logBase #-}
test_logBase
    :: (Shape sh, Show sh, Similar e, Show e, P.Eq sh, P.Floating e, A.Floating e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_logBase runN dim e =
  property $ do
    sh1 <- forAll dim
    sh2 <- forAll dim
    xs  <- forAll (array sh1 e)
    ys  <- forAll (array sh2 e)
    let !go = runN (A.zipWith logBase) in go xs ys ~~~ zipWithRef logBase xs ys

{-# NOINLINE test_atan2 #-}
test_atan2
    :: (Shape sh, Show sh, Similar e, Show e, P.Eq sh, P.RealFloat e, A.RealFloat e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_atan2 runN dim e =
  property $ do
    sh1 <- forAll dim
    sh2 <- forAll dim
    xs  <- forAll (array sh1 e)
    ys  <- forAll (array sh2 e)
    let !go = runN (A.zipWith A.atan2) in go xs ys ~~~ zipWithRef P.atan2 xs ys

{-# NOINLINE test_mod #-}
test_mod
    :: (Shape sh, Show sh, Similar e, Show e, P.Eq sh, P.Integral e, A.Integral e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_mod runN dim e =
  property $ do
    sh1 <- forAll dim
    sh2 <- forAll dim
    xs  <- forAll (array sh1 e)
    ys  <- forAll (array sh2 (e `except` zero))
    let !go = runN (A.zipWith mod) in go xs ys ~~~ zipWithRef mod xs ys

{-# NOINLINE test_divMod #-}
test_divMod
    :: (Shape sh, Show sh, Similar e, Show e, P.Eq sh, P.Integral e, A.Integral e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_divMod runN dim e =
  property $ do
    sh1 <- forAll dim
    sh2 <- forAll dim
    xs  <- forAll (array sh1 e)
    ys  <- forAll (array sh2 (e `except` zero))
    let !go = runN (A.zipWith (lift $$ divMod)) in go xs ys ~~~ zipWithRef divMod xs ys

{-# NOINLINE test_band #-}
test_band
    :: (Shape sh, Show sh, Similar e, Show e, P.Eq sh, P.Bits e, A.Bits e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_band runN dim e =
  property $ do
    sh1 <- forAll dim
    sh2 <- forAll dim
    xs  <- forAll (array sh1 e)
    ys  <- forAll (array sh2 e)
    let !go = runN (A.zipWith (A..&.)) in go xs ys ~~~ zipWithRef (P..&.) xs ys

{-# NOINLINE test_bor #-}
test_bor
    :: (Shape sh, Show sh, Similar e, Show e, P.Eq sh, P.Bits e, A.Bits e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_bor runN dim e =
  property $ do
    sh1 <- forAll dim
    sh2 <- forAll dim
    xs  <- forAll (array sh1 e)
    ys  <- forAll (array sh2 e)
    let !go = runN (A.zipWith (A..|.)) in go xs ys ~~~ zipWithRef (P..|.) xs ys

{-# NOINLINE test_xor #-}
test_xor
    :: (Shape sh, Show sh, Similar e, Show e, P.Eq sh, P.Bits e, A.Bits e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_xor runN dim e =
  property $ do
    sh1 <- forAll dim
    sh2 <- forAll dim
    xs  <- forAll (array sh1 e)
    ys  <- forAll (array sh2 e)
    let !go = runN (A.zipWith A.xor) in go xs ys ~~~ zipWithRef P.xor xs ys

{-# NOINLINE test_shift #-}
test_shift
    :: forall sh e. (Shape sh, Show sh, Similar e, Show e, P.Eq sh, P.FiniteBits e, A.FiniteBits e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_shift runN dim e =
  property $ do
    let s = P.finiteBitSize (undefined::e)
    sh1 <- forAll dim
    sh2 <- forAll dim
    xs  <- forAll (array sh1 e)
    ys  <- forAll (array sh2 (Gen.int (Range.linearFrom 0 (-s) s)))
    let !go = runN (A.zipWith A.shift) in go xs ys ~~~ zipWithRef P.shift xs ys

{-# NOINLINE test_shiftL #-}
test_shiftL
    :: forall sh e. (Shape sh, Show sh, Similar e, Show e, P.Eq sh, P.FiniteBits e, A.FiniteBits e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_shiftL runN dim e =
  property $ do
    let s = P.finiteBitSize (undefined::e)
    sh1 <- forAll dim
    sh2 <- forAll dim
    xs  <- forAll (array sh1 e)
    ys  <- forAll (array sh2 (Gen.int (Range.linear 0 s)))
    let !go = runN (A.zipWith A.shiftL) in go xs ys ~~~ zipWithRef P.shiftL xs ys

{-# NOINLINE test_shiftR #-}
test_shiftR
    :: forall sh e. (Shape sh, Show sh, Similar e, Show e, P.Eq sh, P.FiniteBits e, A.FiniteBits e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_shiftR runN dim e =
  property $ do
    let s = P.finiteBitSize (undefined::e)
    sh1 <- forAll dim
    sh2 <- forAll dim
    xs  <- forAll (array sh1 e)
    ys  <- forAll (array sh2 (Gen.int (Range.linear 0 s)))
    let !go = runN (A.zipWith A.shiftR) in go xs ys ~~~ zipWithRef P.shiftR xs ys

{-# NOINLINE test_rotate #-}
test_rotate
    :: forall sh e. (Shape sh, Show sh, Similar e, Show e, P.Eq sh, P.FiniteBits e, A.FiniteBits e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_rotate runN dim e =
  property $ do
    let s = P.finiteBitSize (undefined::e)
    sh1 <- forAll dim
    sh2 <- forAll dim
    xs  <- forAll (array sh1 e)
    ys  <- forAll (array sh2 (Gen.int (Range.linearFrom 0 (-s) s)))
    let !go = runN (A.zipWith A.rotate) in go xs ys ~~~ zipWithRef P.rotate xs ys

{-# NOINLINE test_rotateL #-}
test_rotateL
    :: forall sh e. (Shape sh, Show sh, Similar e, Show e, P.Eq sh, P.FiniteBits e, A.FiniteBits e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_rotateL runN dim e =
  property $ do
    let s = P.finiteBitSize (undefined::e)
    sh1 <- forAll dim
    sh2 <- forAll dim
    xs  <- forAll (array sh1 e)
    ys  <- forAll (array sh2 (Gen.int (Range.linear 0 s)))
    let !go = runN (A.zipWith A.rotateL) in go xs ys ~~~ zipWithRef P.rotateL xs ys

{-# NOINLINE test_rotateR #-}
test_rotateR
    :: forall sh e. (Shape sh, Show sh, Similar e, Show e, P.Eq sh, P.FiniteBits e, A.FiniteBits e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_rotateR runN dim e =
  property $ do
    let s = P.finiteBitSize (undefined::e)
    sh1 <- forAll dim
    sh2 <- forAll dim
    xs  <- forAll (array sh1 e)
    ys  <- forAll (array sh2 (Gen.int (Range.linear 0 s)))
    let !go = runN (A.zipWith A.rotateR) in go xs ys ~~~ zipWithRef P.rotateR xs ys

{-# NOINLINE test_lt #-}
test_lt
    :: (Shape sh, Show sh, Show e, P.Eq sh, P.Ord e, A.Ord e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_lt runN dim e =
  property $ do
    sh1 <- forAll dim
    sh2 <- forAll dim
    xs  <- forAll (array sh1 e)
    ys  <- forAll (array sh2 e)
    let !go = runN (A.zipWith (A.<)) in go xs ys ~~~ zipWithRef (P.<) xs ys

{-# NOINLINE test_gt #-}
test_gt
    :: (Shape sh, Show sh, Show e, P.Eq sh, P.Ord e, A.Ord e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_gt runN dim e =
  property $ do
    sh1 <- forAll dim
    sh2 <- forAll dim
    xs  <- forAll (array sh1 e)
    ys  <- forAll (array sh2 e)
    let !go = runN (A.zipWith (A.>)) in go xs ys ~~~ zipWithRef (P.>) xs ys

{-# NOINLINE test_lte #-}
test_lte
    :: (Shape sh, Show sh, Show e, P.Eq sh, P.Ord e, A.Ord e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_lte runN dim e =
  property $ do
    sh1 <- forAll dim
    sh2 <- forAll dim
    xs  <- forAll (array sh1 e)
    ys  <- forAll (array sh2 e)
    let !go = runN (A.zipWith (A.<=)) in go xs ys ~~~ zipWithRef (P.<=) xs ys

{-# NOINLINE test_gte #-}
test_gte
    :: (Shape sh, Show sh, Show e, P.Eq sh, P.Ord e, A.Ord e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_gte runN dim e =
  property $ do
    sh1 <- forAll dim
    sh2 <- forAll dim
    xs  <- forAll (array sh1 e)
    ys  <- forAll (array sh2 e)
    let !go = runN (A.zipWith (A.>=)) in go xs ys ~~~ zipWithRef (P.>=) xs ys

{-# NOINLINE test_eq #-}
test_eq
    :: (Shape sh, Show sh, Show e, P.Eq sh, P.Ord e, A.Ord e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_eq runN dim e =
  property $ do
    sh1 <- forAll dim
    sh2 <- forAll dim
    xs  <- forAll (array sh1 e)
    ys  <- forAll (array sh2 e)
    let !go = runN (A.zipWith (A.==)) in go xs ys ~~~ zipWithRef (P.==) xs ys

{-# NOINLINE test_neq #-}
test_neq
    :: (Shape sh, Show sh, Show e, P.Eq sh, P.Ord e, A.Ord e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_neq runN dim e =
  property $ do
    sh1 <- forAll dim
    sh2 <- forAll dim
    xs  <- forAll (array sh1 e)
    ys  <- forAll (array sh2 e)
    let !go = runN (A.zipWith (A./=)) in go xs ys ~~~ zipWithRef (P./=) xs ys

{-# NOINLINE test_min #-}
test_min
    :: (Shape sh, Show sh, Similar e, Show e, P.Eq sh, P.Ord e, A.Ord e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_min runN dim e =
  property $ do
    sh1 <- forAll dim
    sh2 <- forAll dim
    xs  <- forAll (array sh1 e)
    ys  <- forAll (array sh2 e)
    let !go = runN (A.zipWith (A.min)) in go xs ys ~~~ zipWithRef (P.min) xs ys

{-# NOINLINE test_max #-}
test_max
    :: (Shape sh, Show sh, Similar e, Show e, P.Eq sh, P.Ord e, A.Ord e)
    => RunN
    -> Gen sh
    -> Gen e
    -> Property
test_max runN dim e =
  property $ do
    sh1 <- forAll dim
    sh2 <- forAll dim
    xs  <- forAll (array sh1 e)
    ys  <- forAll (array sh2 e)
    let !go = runN (A.zipWith (A.max)) in go xs ys ~~~ zipWithRef (P.max) xs ys


-- Reference Implementation
-- ------------------------

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

