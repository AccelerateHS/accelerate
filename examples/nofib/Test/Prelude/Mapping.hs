{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Test.Prelude.Mapping (

  test_map,
  test_zipWith,
  mapRef,
  zipWithRef,

) where

import Prelude                                                  as P
import Data.Bits                                                as P
import Data.Label
import Data.Maybe
import Data.Typeable
import Test.QuickCheck                                          hiding ( (.&.) )
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Config
import Test.Base
import QuickCheck.Arbitrary.Array
import QuickCheck.Arbitrary.Shape
import Data.Array.Accelerate                                    as A
import Data.Array.Accelerate.Data.Bits                          as A
import Data.Array.Accelerate.Examples.Internal                  as A
import Data.Array.Accelerate.Array.Sugar                        as Sugar
import qualified Data.Array.Accelerate.Array.Representation     as R

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
    testIntegralElt :: forall a. (P.Integral a, P.Bits a, A.Integral a, A.Bits a, Arbitrary a, Similar a, A.FromIntegral a Float) => (Config :-> Bool) -> a -> Maybe Test
    testIntegralElt ok a
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testGroup (show (typeOf (undefined :: a)))
          [ testDim dim0
          , testDim dim1
          , testDim dim2
          ]
      where
        testDim :: forall sh. (Shape sh, P.Eq sh, Arbitrary sh, Arbitrary (Array sh a)) => sh -> Test
        testDim sh = testGroup ("DIM" P.++ show (rank sh))
          [ -- operators on Num
            testProperty "neg"          (test negate negate)
          , testProperty "abs"          (test abs abs)
          , testProperty "signum"       (test signum signum)

            -- operators on Integral & Bits
          , testProperty "complement"   (test A.complement P.complement)

            -- conversions
          , testProperty "fromIntegral" (testF A.fromIntegral P.fromIntegral)
          ]
          where
            test  = mkTest a a sh
            testF = mkTest a (undefined::Float) sh

    testFloatingElt :: forall a. (P.Floating a, P.RealFloat a, A.Floating a, A.RealFloat a, Arbitrary a, Similar a) => (Config :-> Bool) -> a -> Maybe Test
    testFloatingElt ok a
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testGroup (show (typeOf (undefined :: a)))
          [ testDim dim0
          , testDim dim1
          , testDim dim2
          ]
      where
        testDim :: forall sh. (Shape sh, P.Eq sh, P.Ord a, Arbitrary sh, Arbitrary (Array sh a)) => sh -> Test
        testDim sh = testGroup ("DIM" P.++ show (rank sh))
          [ -- operators on Num
            testProperty "neg"          (test negate negate)
          , testProperty "abs"          (test abs abs)
          , testProperty "signum"       (test signum signum)

            -- operators on Fractional, Floating, RealFrac & RealFloat
          , testProperty "recip"        (test recip recip)
          , testProperty "sin"          (test sin sin)
          , testProperty "cos"          (test cos cos)
          , testProperty "tan"          (requiring (\x -> P.not (sin x ~= 1)) $ test tan tan)
          , testProperty "asin"         (requiring (\x -> -1 <= x && x <= 1) $ test asin asin)
          , testProperty "acos"         (requiring (\x -> -1 <= x && x <= 1) $ test acos acos)
          , testProperty "atan"         (test atan atan)
          , testProperty "asinh"        (test asinh asinh)
          , testProperty "acosh"        (requiring (>= 1) $ test acosh acosh)
          , testProperty "atanh"        (requiring (\x -> -1 < x && x < 1) $ test atanh atanh)
          , testProperty "exp"          (test exp exp)
          , testProperty "sqrt"         (requiring (>= 0) $ test sqrt sqrt)
          , testProperty "log"          (requiring (> 0)  $ test log log)
          , testProperty "truncate"     (testI A.truncate P.truncate)
          , testProperty "round"        (testI A.round P.round)
          , testProperty "floor"        (testI A.floor P.floor)
          , testProperty "ceiling"      (testI A.ceiling P.ceiling)
          ]
          where
            test  = mkTest a a sh
            testI = mkTest a (undefined::Int) sh

    -- The test generator. The first three arguments are dummies that are used
    -- to fix the types. The next two are the Accelerate and Prelude functions
    -- respectively that are arguments to the Map operation, and the final is
    -- the (randomly generated) input data.
    --
    mkTest :: (Elt a, Elt b, Shape sh, P.Eq sh, Similar b)
           => a -> b -> sh -> (Exp a -> Exp b) -> (a -> b) -> Array sh a -> Property
    mkTest _ _ _ f g xs = run1 backend (A.map f) xs ~?= mapRef g xs


test_zipWith :: Backend -> Config -> Test
test_zipWith backend opt = testGroup "zipWith" $ catMaybes
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
    testIntegralElt :: forall a. (P.Integral a, P.Bits a, A.Integral a, A.Bits a, Arbitrary a, Similar a) => (Config :-> Bool) -> a -> Maybe Test
    testIntegralElt ok a
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testGroup (show (typeOf (undefined :: a)))
          [ testDim dim0
          , testDim dim1
          , testDim dim2
          ]
      where
        testDim :: forall sh. (Shape sh, P.Eq sh, Arbitrary sh, Arbitrary (Array sh a), Arbitrary (Array sh Int)) => sh -> Test
        testDim sh = testGroup ("DIM" P.++ show (rank sh))
          [ -- operators on Num
            testProperty "(+)"          (test (+) (+))
          , testProperty "(-)"          (test (-) (-))
          , testProperty "(*)"          (test (*) (*))

            -- operators on Integral & Bits
          , testProperty "quot"         (denom $ test quot quot)
          , testProperty "rem"          (denom $ test rem rem)
          , testProperty "quotRem"      (denom $ test' (\x y -> lift (quotRem x y)) quotRem)
          , testProperty "div"          (denom $ test div div)
          , testProperty "mod"          (denom $ test mod mod)
          , testProperty "divMod"       (denom $ test' (\x y -> lift (divMod x y)) divMod)
          , testProperty "(.&.)"        (test (A..&.) (P..&.))
          , testProperty "(.|.)"        (test (A..|.) (P..|.))
          , testProperty "xor"          (test A.xor P.xor)
          , testProperty "shift"        (testSR  A.shift P.shift)
          , testProperty "shiftL"       (testSR' A.shiftL P.shiftL)
          , testProperty "shiftR"       (testSR' A.shiftR P.shiftR)
          , testProperty "rotate"       (testSR  A.rotate P.rotate)
          , testProperty "rotateL"      (testSR' A.rotateL P.rotateL)
          , testProperty "rotateR"      (testSR' A.rotateR P.rotateR)

            -- relational and equality operators
          , testProperty "(<)"          (testAB (A.<*) (<))
          , testProperty "(>)"          (testAB (A.>*) (>))
          , testProperty "(<=)"         (testAB (<=*) (<=))
          , testProperty "(>=)"         (testAB (>=*) (>=))
          , testProperty "(==)"         (testAB (==*) (==))
          , testProperty "(/=)"         (testAB (/=*) (/=))
          , testProperty "min"          (test A.min P.min)
          , testProperty "max"          (test A.max P.max)
          ]
          where
            test        = mkTest a a a sh
            test'       = mkTest a a (undefined::(a,a)) sh
            testAB      = mkTest a a (undefined::Bool) sh

            testSR      = mkTest a (undefined::Int) a sh
            testSR' f g = forAll arbitrary $ \xs ->
                          requiring (>= 0) $ \ys ->
                            mkTest a (undefined::Int) a sh f g xs ys

    testFloatingElt :: forall a. (P.RealFrac a, P.RealFloat a, A.RealFloat a, A.RealFrac a, Arbitrary a, Similar a) => (Config :-> Bool) -> a -> Maybe Test
    testFloatingElt ok a
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testGroup (show (typeOf (undefined :: a)))
          [ testDim dim0
          , testDim dim1
          , testDim dim2
          ]
      where
        testDim :: forall sh. (Shape sh, P.Eq sh, Arbitrary sh, Arbitrary (Array sh a)) => sh -> Test
        testDim sh = testGroup ("DIM" P.++ show (rank sh))
          [ -- operators on Num
            testProperty "(+)"          (test (+) (+))
          , testProperty "(-)"          (test (-) (-))
          , testProperty "(*)"          (test (*) (*))

            -- operators on Fractional, Floating, RealFrac & RealFloat
          , testProperty "(/)"          (denom $ test (/) (/))
          , testProperty "(**)"         (test (**) (**))
          , testProperty "atan2"        (test A.atan2 P.atan2)
          , testProperty "logBase"      (requiring (> 0) $ \xs ->
                                         requiring (> 0) $ \ys -> test logBase logBase xs ys)

            -- relational and equality operators
          , testProperty "(<)"          (testAB (A.<*) (<))
          , testProperty "(>)"          (testAB (A.>*) (>))
          , testProperty "(<=)"         (testAB (<=*) (<=))
          , testProperty "(>=)"         (testAB (>=*) (>=))
          , testProperty "(==)"         (testAB (==*) (==))
          , testProperty "(/=)"         (testAB (/=*) (/=))
          , testProperty "min"          (test A.min P.min)
          , testProperty "max"          (test A.max P.max)
          ]
          where
            test        = mkTest a a a sh
            testAB      = mkTest a a (undefined::Bool) sh

    -- The test generator. See comments in test_map above.
    --
    mkTest :: (Elt a, Elt b, Elt c, Shape sh, P.Eq sh, Similar c)
           => a -> b -> c -> sh -> (Exp a -> Exp b -> Exp c) -> (a -> b -> c) -> Array sh a -> Array sh b -> Property
    mkTest _ _ _ _ f g xs ys = run2 backend (A.zipWith f) xs ys ~?= zipWithRef g xs ys

    denom f = forAll arbitrary $ \xs ->
              requiring (/= 0) $ \ys -> f xs ys


requiring
    :: (Elt e, Shape sh, Arbitrary e, Arbitrary sh, Testable prop)
    => (e -> Bool)
    -> (Array sh e -> prop)
    -> Property
requiring f go =
  forAll (do sh <- sized arbitraryShape
             arbitraryArrayOf sh (arbitrary `suchThat` f)) go


-- Reference Implementation
-- ------------------------

mapRef :: (Shape sh, Elt b) => (a -> b) -> Array sh a -> Array sh b
mapRef f xs
  = fromList (arrayShape xs)
  $ P.map f
  $ toList xs

zipWithRef :: (Shape sh, Elt c) => (a -> b -> c) -> Array sh a -> Array sh b -> Array sh c
zipWithRef f xs ys =
  let shx       = fromElt (arrayShape xs)
      shy       = fromElt (arrayShape ys)
      sh        = toElt (R.intersect shx shy)
  in
  newArray sh (\ix -> f (xs Sugar.! ix) (ys Sugar.! ix))

