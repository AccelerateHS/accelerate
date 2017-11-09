{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Test.Prelude.Map (

  test_map

) where

import Prelude                                                  as P
import Data.Bits                                                as P
import Data.Label
import Data.Maybe
import Data.Typeable
import System.Random
import Test.QuickCheck                                          hiding ( (.&.), suchThat )
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Config
import Test.Base
import QuickCheck.Arbitrary.Array
import QuickCheck.Arbitrary.Shape
import Data.Array.Accelerate                                    as A hiding ( Ord(..) )
import Data.Array.Accelerate.Data.Bits                          as A
import Data.Array.Accelerate.Examples.Internal                  as A
import Data.Array.Accelerate.Array.Sugar                        as Sugar

--
-- Map -------------------------------------------------------------------------
--

test_map :: Backend -> Config -> Test
test_map backend opt = testGroup "map" $ catMaybes
  [ testIntegralElt configInt8   (undefined :: Int8)
  , testIntegralElt configInt16  (undefined :: Int16)
  , testIntegralElt configInt32  (undefined :: Int32)
  , testIntegralElt configInt64  (undefined :: Int64)
  , testIntegralElt configWord8  (undefined :: Word8)
  , testIntegralElt configWord16 (undefined :: Word16)
  , testIntegralElt configWord32 (undefined :: Word32)
  , testIntegralElt configWord64 (undefined :: Word64)
  , testFloatingElt configFloat  (undefined :: Float)
  , testFloatingElt configDouble (undefined :: Double)
  ]
  where
    testIntegralElt :: forall a. (P.Integral a, P.FiniteBits a, A.Integral a, A.FiniteBits a, Arbitrary a, Similar a, A.FromIntegral a Float) => (Config :-> Bool) -> a -> Maybe Test
    testIntegralElt ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testGroup (show (typeOf (undefined :: a)))
          [ testDim dim0
          , testDim dim1
          , testDim dim2
          ]
      where
        testDim :: forall sh. (Shape sh, P.Eq sh, Arbitrary (Array sh a)) => sh -> Test
        testDim sh = testGroup ("DIM" P.++ show (rank sh))
          [ -- operators on Num
            testProperty "neg"                (test_negate :: Array sh a -> Property)
          , testProperty "abs"                (test_abs    :: Array sh a -> Property)
          , testProperty "signum"             (test_signum :: Array sh a -> Property)

            -- operators on Integral & Bits
          , testProperty "complement"         (test_complement :: Array sh a -> Property)
          , testProperty "popCount"           (test_popCount   :: Array sh a -> Property)
          , testProperty "countLeadingZeros"  (test_countLeadingZeros  :: Array sh a -> Property)
          , testProperty "countTrailingZeros" (test_countTrailingZeros :: Array sh a -> Property)

            -- conversions
          , testProperty "fromIntegral"       (test_fromIntegral :: Array sh a -> Property)
          ]
          where
            test_fromIntegral xs = run1 backend (A.map A.fromIntegral) xs ~?= mapRef (P.fromIntegral :: a -> Float) xs

    testFloatingElt :: forall a. (P.Floating a, P.RealFloat a, A.Floating a, A.RealFloat a, Random a, Arbitrary a, Similar a) => (Config :-> Bool) -> a -> Maybe Test
    testFloatingElt ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testGroup (show (typeOf (undefined :: a)))
          [ testDim dim0
          , testDim dim1
          , testDim dim2
          ]
      where
        testDim :: forall sh. (Shape sh, P.Eq sh, Random a, Arbitrary sh, Arbitrary (Array sh a)) => sh -> Test
        testDim _ = testGroup ("DIM" P.++ show (rank (undefined::sh)))
          [ -- operators on Num
            testProperty "neg"          (test_negate :: Array sh a -> Property)
          , testProperty "abs"          (test_abs    :: Array sh a -> Property)
          , testProperty "signum"       (test_signum :: Array sh a -> Property)

            -- operators on Fractional, Floating, RealFrac & RealFloat
          , testProperty "recip"        (test_recip  :: Array sh a -> Property)
          , testProperty "sin"          (test_sin    :: Array sh a -> Property)
          , testProperty "cos"          (test_cos    :: Array sh a -> Property)
          , testProperty "tan"          (requiring (\x -> P.not (sin x ~= 1)) (test_tan  :: Array sh a -> Property))
          , testProperty "asin"         (forAll (sized arbitraryShape)                $ \sh ->
                                         forAll (arbitraryArrayOf sh (choose (-1,1))) $ \(xs :: Array sh a) -> test_asin xs)
          , testProperty "acos"         (forAll (sized arbitraryShape)                $ \sh ->
                                         forAll (arbitraryArrayOf sh (choose (-1,1))) $ \(xs :: Array sh a) -> test_acos xs)
          , testProperty "atan"         (test_atan  :: Array sh a -> Property)
          , testProperty "asinh"        (test_asinh :: Array sh a -> Property)
          , testProperty "acosh"        (requiring (>= 1) (test_acosh :: Array sh a -> Property))
          , testProperty "atanh"        (forAll (sized arbitraryShape)                $ \sh ->
                                         forAll (arbitraryArrayOf sh (choose (-1,1))) $ \(xs :: Array sh a) -> test_atanh xs)
          , testProperty "exp"          (test_exp :: Array sh a -> Property)
          , testProperty "sqrt"         (requiring (>= 0) (test_sqrt :: Array sh a -> Property))
          , testProperty "log"          (requiring (> 0)  (test_log  :: Array sh a -> Property))
          , testProperty "truncate"     (test_truncate :: Array sh a -> Property)
          , testProperty "round"        (test_round :: Array sh a -> Property)
          , testProperty "floor"        (test_floor :: Array sh a -> Property)
          , testProperty "ceiling"      (test_ceiling :: Array sh a -> Property)
          ]
          where
            test_truncate xs = run1 backend (A.map A.truncate) xs ~?= mapRef (P.truncate :: a -> Int) xs
            test_round xs    = run1 backend (A.map A.round) xs ~?= mapRef (P.round :: a -> Int) xs
            test_floor xs    = run1 backend (A.map A.floor) xs ~?= mapRef (P.floor :: a -> Int) xs
            test_ceiling xs  = run1 backend (A.map A.ceiling) xs ~?= mapRef (P.ceiling :: a -> Int) xs

    test_negate xs     = run1 backend (A.map negate) xs ~?= mapRef negate xs
    test_abs    xs     = run1 backend (A.map abs) xs    ~?= mapRef abs xs
    test_signum xs     = run1 backend (A.map signum) xs ~?= mapRef signum xs

    test_complement xs          = run1 backend (A.map A.complement) xs ~?= mapRef P.complement xs
    test_popCount   xs          = run1 backend (A.map A.popCount)   xs ~?= mapRef P.popCount xs
    test_countLeadingZeros xs   = run1 backend (A.map A.countLeadingZeros) xs  ~?= mapRef countLeadingZerosRef xs
    test_countTrailingZeros xs  = run1 backend (A.map A.countTrailingZeros) xs ~?= mapRef countTrailingZerosRef xs

    test_recip xs      = run1 backend (A.map recip) xs ~?= mapRef recip xs
    test_sin xs        = run1 backend (A.map sin) xs ~?= mapRef sin xs
    test_cos xs        = run1 backend (A.map cos) xs ~?= mapRef cos xs
    test_tan xs        = run1 backend (A.map tan) xs ~?= mapRef tan xs
    test_asin xs       = run1 backend (A.map asin) xs ~?= mapRef asin xs
    test_acos xs       = run1 backend (A.map acos) xs ~?= mapRef acos xs
    test_atan xs       = run1 backend (A.map atan) xs ~?= mapRef atan xs
    test_asinh xs      = run1 backend (A.map asinh) xs ~?= mapRef asinh xs
    test_acosh xs      = run1 backend (A.map acosh) xs ~?= mapRef acosh xs
    test_atanh xs      = run1 backend (A.map atanh) xs ~?= mapRef atanh xs
    test_exp xs        = run1 backend (A.map exp) xs ~?= mapRef exp xs
    test_sqrt xs       = run1 backend (A.map sqrt) xs ~?= mapRef sqrt xs
    test_log xs        = run1 backend (A.map log) xs ~?= mapRef log xs


suchThat :: Gen a -> (a -> Bool) -> Gen a
suchThat gen p = do
  x <- gen
  case p x of
    True  -> return x
    False -> sized $ \n -> resize (n+1) (suchThat gen p)


{-# INLINE requiring #-}
requiring
    :: (Elt e, Shape sh, Arbitrary e, Arbitrary sh, Testable prop)
    => (e -> Bool)
    -> (Array sh e -> prop)
    -> Property
requiring f go =
  let
      shrinkRequiring arr       = [ fromList (Sugar.shape arr) sl | sl <- shrinkOneRequiring (toList arr) ]
      shrinkOneRequiring []     = []
      shrinkOneRequiring (x:xs) = [ x':xs | x'  <- shrink x, f x' ]
                             P.++ [ x:xs' | xs' <- shrinkOneRequiring xs ]
  in
  forAllShrink arbitrary                                      shrink          $ \sh ->
  forAllShrink (arbitraryArrayOf sh (arbitrary `suchThat` f)) shrinkRequiring $ \arr ->
    go arr


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

