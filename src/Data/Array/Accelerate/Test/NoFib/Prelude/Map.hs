{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Prelude.Map
-- Copyright   : [2009..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Test.NoFib.Prelude.Map (

  test_map

) where

import Control.Monad
import Data.Bits                                                as P
import Data.Maybe
import Data.Typeable
import Lens.Micro
import Prelude                                                  as P

import Data.Array.Accelerate                                    as A
import Data.Array.Accelerate.Data.Bits                          as A
import Data.Array.Accelerate.Array.Sugar                        as Sugar
import Data.Array.Accelerate.Test.NoFib.Base
import Data.Array.Accelerate.Test.NoFib.Config

import Data.Array.Accelerate.Hedgehog.Similar
import qualified Data.Array.Accelerate.Hedgehog.Gen.Array       as Gen

import Hedgehog
import qualified Hedgehog.Gen                                   as Gen
import qualified Hedgehog.Range                                 as Range

import Test.Tasty
import Test.Tasty.Hedgehog

--
-- Map -------------------------------------------------------------------------
--

test_map :: RunN -> Config -> TestTree
test_map runN opt =
  testGroup "map" $ catMaybes
    [ testIntegralElt configInt8   i8
    , testIntegralElt configInt16  i16
    , testIntegralElt configInt32  i32
    , testIntegralElt configInt64  i64
    , testIntegralElt configWord8  w8
    , testIntegralElt configWord16 w16
    , testIntegralElt configWord32 w32
    , testIntegralElt configWord64 w64
    , testFloatingElt configFloat  Gen.float
    , testFloatingElt configDouble Gen.double
    ]
  where
    testIntegralElt
        :: forall a. ( P.Integral a, P.FiniteBits a
                     , A.Integral a, A.FiniteBits a
                     , A.FromIntegral a Double, Similar a)
        => (Config :-> Bool)
        -> Gen a
        -> Maybe TestTree
    testIntegralElt ok e
      | P.not (opt ^. ok) = Nothing
      | otherwise         = Just $ testGroup (show (typeOf (undefined :: a)))
          [ testDim dim0
          , testDim dim1
          , testDim dim2
          ]
      where
        testDim
            :: forall sh. (A.Shape sh, P.Eq sh)
            => Gen sh
            -> TestTree
        testDim sh =
          testGroup ("DIM" P.++ show (rank (undefined::sh)))
            [ -- operators on Num
              testProperty "neg"                $ test_negate sh e
            , testProperty "abs"                $ test_abs sh e
            , testProperty "signum"             $ test_signum sh e

              -- operators on Integral & Bits
            , testProperty "complement"         $ test_complement sh e
            , testProperty "popCount"           $ test_popCount sh e
            , testProperty "countLeadingZeros"  $ test_countLeadingZeros sh e
            , testProperty "countTrailingZeros" $ test_countTrailingZeros sh e

              -- conversions
            , testProperty "fromIntegral"       $ test_fromIntegral sh e
            ]

    testFloatingElt
        :: forall a. (P.RealFloat a, A.Floating a, A.RealFrac a, Similar a)
        => (Config :-> Bool)
        -> (Range a -> Gen a)
        -> Maybe TestTree
    testFloatingElt ok e
      | P.not (opt ^. ok) = Nothing
      | otherwise         = Just $ testGroup (show (typeOf (undefined :: a)))
          [ testDim dim0
          , testDim dim1
          , testDim dim2
          ]
      where
        testDim
            :: forall sh. (A.Shape sh, P.Eq sh)
            => Gen sh
            -> TestTree
        testDim sh =
          testGroup ("DIM" P.++ show (rank (undefined::sh)))
            [ -- operators on Num
              testProperty "neg"        $ test_negate sh (fullrange e)
            , testProperty "abs"        $ test_abs sh (fullrange e)
            , testProperty "signum"     $ test_abs sh (fullrange e)

              -- operators on Fractional, Floating, RealFrac & RealFloat
            , testProperty "recip"      $ test_recip sh (fullrange e)
            , testProperty "sin"        $ test_sin sh (fullrange e)
            , testProperty "cos"        $ test_cos sh (fullrange e)
            , testProperty "tan"        $ test_tan sh (fullrange e `except` \v -> sin v ~= 1)
            , testProperty "asin"       $ test_asin sh (e (Range.linearFracFrom 0 (-1) 1))
            , testProperty "acos"       $ test_acos sh (e (Range.linearFracFrom 0 (-1) 1))
            , testProperty "atan"       $ test_atan sh (fullrange e)
            , testProperty "asinh"      $ test_asinh sh (fullrange e)
            , testProperty "acosh"      $ test_acosh sh (e (Range.linearFrac 1 flt_max))
            , testProperty "atanh"      $ test_atanh sh (e (Range.linearFracFrom 0 (-1) 1))
            , testProperty "exp"        $ test_exp sh (fullrange e)
            , testProperty "sqrt"       $ test_sqrt sh (e (Range.linearFrac 0 flt_max))
            , testProperty "log"        $ test_log sh (e (Range.linearFrac 0 flt_max) `except` \v -> v P.== 0)
            , testProperty "truncate"   $ test_truncate sh (fullrange e)
            , testProperty "round"      $ test_round sh (fullrange e)
            , testProperty "floor"      $ test_floor sh (fullrange e)
            , testProperty "ceiling"    $ test_ceiling sh (fullrange e)
            ]

    testMap :: (A.Shape sh, P.Eq sh, Elt e, Similar e) => (Array sh e -> PropertyT IO ()) -> Gen sh -> Gen e -> Property
    testMap doit dim e =
      property $ do
        sh  <- forAll dim
        arr <- forAll (Gen.array sh e)
        doit arr

    test_negate :: (A.Shape sh, A.Num e, P.Num e, P.Eq sh, Similar e) => Gen sh -> Gen e -> Property
    test_negate = testMap $ \xs -> runN (A.map negate) xs ~~~ mapRef negate xs

    test_abs :: (A.Shape sh, A.Num e, P.Num e, P.Eq sh, Similar e) => Gen sh -> Gen e -> Property
    test_abs = testMap $ \xs -> runN (A.map abs) xs ~~~ mapRef abs xs

    test_signum :: (A.Shape sh, A.Num e, P.Num e, P.Eq sh, Similar e) => Gen sh -> Gen e -> Property
    test_signum = testMap $ \xs -> runN (A.map signum) xs ~~~ mapRef signum xs

    test_complement :: (A.Shape sh, A.Bits e, P.Bits e, P.Eq sh, Similar e) => Gen sh -> Gen e -> Property
    test_complement = testMap $ \xs -> runN (A.map A.complement) xs ~~~ mapRef P.complement xs

    test_popCount :: (A.Shape sh, A.Bits e, P.Bits e, P.Eq sh, Similar e) => Gen sh -> Gen e -> Property
    test_popCount = testMap $ \xs -> runN (A.map A.popCount) xs ~~~ mapRef P.popCount xs

    test_countLeadingZeros :: (A.Shape sh, A.FiniteBits e, P.FiniteBits e, P.Eq sh, Similar e) => Gen sh -> Gen e -> Property
    test_countLeadingZeros = testMap $ \xs -> runN (A.map A.countLeadingZeros) xs ~~~ mapRef countLeadingZerosRef xs

    test_countTrailingZeros :: (A.Shape sh, A.FiniteBits e, P.FiniteBits e, P.Eq sh, Similar e) => Gen sh -> Gen e -> Property
    test_countTrailingZeros = testMap $ \xs -> runN (A.map A.countTrailingZeros) xs ~~~ mapRef countTrailingZerosRef xs

    test_fromIntegral :: forall sh e. (A.Shape sh, P.Eq sh, P.Integral e, A.Integral e, A.FromIntegral e Double, Similar e) => Gen sh -> Gen e -> Property
    test_fromIntegral = testMap $ \xs -> runN (A.map A.fromIntegral) xs ~~~ mapRef (P.fromIntegral :: e -> Double) xs

    test_recip :: (A.Shape sh, P.Eq sh, P.Fractional e, A.Fractional e, Similar e) => Gen sh -> Gen e -> Property
    test_recip = testMap $ \xs -> runN (A.map recip) xs ~~~ mapRef recip xs

    test_sin :: (A.Shape sh, P.Eq sh, P.Floating e, A.Floating e, Similar e) => Gen sh -> Gen e -> Property
    test_sin = testMap $ \xs -> runN (A.map sin) xs ~~~ mapRef sin xs

    test_cos :: (A.Shape sh, P.Eq sh, P.Floating e, A.Floating e, Similar e) => Gen sh -> Gen e -> Property
    test_cos = testMap $ \xs -> runN (A.map cos) xs ~~~ mapRef cos xs

    test_tan :: (A.Shape sh, P.Eq sh, P.Floating e, A.Floating e, Similar e) => Gen sh -> Gen e -> Property
    test_tan = testMap $ \xs -> runN (A.map tan) xs ~~~ mapRef tan xs

    test_asin :: (A.Shape sh, P.Eq sh, P.Floating e, A.Floating e, Similar e) => Gen sh -> Gen e -> Property
    test_asin = testMap $ \xs -> runN (A.map asin) xs ~~~ mapRef asin xs

    test_acos :: (A.Shape sh, P.Eq sh, P.Floating e, A.Floating e, Similar e) => Gen sh -> Gen e -> Property
    test_acos = testMap $ \xs -> runN (A.map acos) xs ~~~ mapRef acos xs

    test_atan :: (A.Shape sh, P.Eq sh, P.Floating e, A.Floating e, Similar e) => Gen sh -> Gen e -> Property
    test_atan = testMap $ \xs -> runN (A.map atan) xs ~~~ mapRef atan xs

    test_asinh :: (A.Shape sh, P.Eq sh, P.Floating e, A.Floating e, Similar e) => Gen sh -> Gen e -> Property
    test_asinh = testMap $ \xs -> runN (A.map asinh) xs ~~~ mapRef asinh xs

    test_acosh :: (A.Shape sh, P.Eq sh, P.Floating e, A.Floating e, Similar e) => Gen sh -> Gen e -> Property
    test_acosh = testMap $ \xs -> runN (A.map acosh) xs ~~~ mapRef acosh xs

    test_atanh :: (A.Shape sh, P.Eq sh, P.Floating e, A.Floating e, Similar e) => Gen sh -> Gen e -> Property
    test_atanh = testMap $ \xs -> runN (A.map atanh) xs ~~~ mapRef atanh xs

    test_exp :: (A.Shape sh, P.Eq sh, P.Floating e, A.Floating e, Similar e) => Gen sh -> Gen e -> Property
    test_exp = testMap $ \xs -> runN (A.map exp) xs ~~~ mapRef exp xs

    test_sqrt :: (A.Shape sh, P.Eq sh, P.Floating e, A.Floating e, Similar e) => Gen sh -> Gen e -> Property
    test_sqrt = testMap $ \xs -> runN (A.map sqrt) xs ~~~ mapRef sqrt xs

    test_log :: (A.Shape sh, P.Eq sh, P.Floating e, A.Floating e, Similar e) => Gen sh -> Gen e -> Property
    test_log = testMap $ \xs -> runN (A.map log) xs ~~~ mapRef log xs

    test_truncate :: forall sh e. (A.Shape sh, P.Eq sh, P.RealFrac e, A.RealFrac e, Similar e) => Gen sh -> Gen e -> Property
    test_truncate = testMap $ \xs -> runN (A.map A.truncate) xs ~~~ mapRef (P.truncate :: e -> Int) xs

    test_round :: forall sh e. (A.Shape sh, P.Eq sh, P.RealFrac e, A.RealFrac e, Similar e) => Gen sh -> Gen e -> Property
    test_round = testMap $ \xs -> runN (A.map A.round) xs ~~~ mapRef (P.round :: e -> Int) xs

    test_floor :: forall sh e. (A.Shape sh, P.Eq sh, P.RealFrac e, A.RealFrac e, Similar e) => Gen sh -> Gen e -> Property
    test_floor = testMap $ \xs -> runN (A.map A.floor) xs ~~~ mapRef (P.floor :: e -> Int) xs

    test_ceiling :: forall sh e. (A.Shape sh, P.Eq sh, P.RealFrac e, A.RealFrac e, Similar e) => Gen sh -> Gen e -> Property
    test_ceiling = testMap $ \xs -> runN (A.map A.ceiling) xs ~~~ mapRef (P.ceiling :: e -> Int) xs

    fullrange :: P.RealFloat e => (Range e -> Gen e) -> Gen e
    fullrange gen = gen (Range.linearFracFrom 0 flt_min flt_max)

    except :: Gen e -> (e -> Bool) -> Gen e
    except gen f  = do
      v <- gen
      when (f v) Gen.discard
      return v


-- Reference Implementation
-- ------------------------

mapRef :: (Shape sh, Elt b) => (a -> b) -> Array sh a -> Array sh b
mapRef f xs
  = fromList (arrayShape xs)
  $ P.map f
  $ toList xs

countLeadingZerosRef :: P.FiniteBits a => a -> Int
#if __GLASGOW_HASKELL__ >= 710
countLeadingZerosRef = P.countLeadingZeros
#else
countLeadingZerosRef = clz
  where
    clz x = (w-1) - go (w-1)
      where
        go i | i < 0         = i  -- no bit set
             | P.testBit x i = i
             | otherwise     = go (i-1)
        w = P.finiteBitSize x
#endif

countTrailingZerosRef :: P.FiniteBits a => a -> Int
#if __GLASGOW_HASKELL__ >= 710
countTrailingZerosRef = P.countTrailingZeros
#else
countTrailingZerosRef = ctz
  where
    ctz x = go 0
      where
        go i | i >= w        = i
             | P.testBit x i = i
             | otherwise     = go (i+1)
        w = P.finiteBitSize x
#endif

