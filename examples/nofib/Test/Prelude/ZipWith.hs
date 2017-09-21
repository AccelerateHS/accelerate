{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Test.Prelude.ZipWith (

  test_zipWith

) where

import Prelude                                                  as P
import Data.Bits                                                as P
import Data.Label
import Data.Maybe
import Data.Typeable
import Test.QuickCheck                                          hiding ( (.&.), suchThat )
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Config
import Test.Base
import QuickCheck.Arbitrary.Array
import Data.Array.Accelerate                                    as A
import Data.Array.Accelerate.Data.Bits                          as A
import Data.Array.Accelerate.Examples.Internal                  as A
import Data.Array.Accelerate.Array.Sugar                        as Sugar
import qualified Data.Array.Accelerate.Array.Representation     as R

--
-- ZipWith ---------------------------------------------------------------------
--

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
    testIntegralElt ok _
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
            testProperty "(+)"          (test_plus  :: Array sh a -> Array sh a -> Property)
          , testProperty "(-)"          (test_minus :: Array sh a -> Array sh a -> Property)
          , testProperty "(*)"          (test_mult  :: Array sh a -> Array sh a -> Property)

            -- operators on Integral & Bits
          , testProperty "quot"         (denom (test_quot    :: Array sh a -> Array sh a -> Property))
          , testProperty "rem"          (denom (test_rem     :: Array sh a -> Array sh a -> Property))
          , testProperty "quotRem"      (denom (test_quotRem :: Array sh a -> Array sh a -> Property))
          , testProperty "div"          (denom (test_div     :: Array sh a -> Array sh a -> Property))
          , testProperty "mod"          (denom (test_mod     :: Array sh a -> Array sh a -> Property))
          , testProperty "divMod"       (denom (test_divMod  :: Array sh a -> Array sh a -> Property))
          , testProperty "(.&.)"        (test_band :: Array sh a -> Array sh a -> Property)
          , testProperty "(.|.)"        (test_bor  :: Array sh a -> Array sh a -> Property)
          , testProperty "xor"          (test_xor  :: Array sh a -> Array sh a -> Property)
          , testProperty "shift"        (test_shift :: Array sh a -> Array sh Int -> Property)
          , testProperty "shiftL"       (requiring (P.>= 0) (flip test_shiftL :: Array sh Int -> Array sh a -> Property))
          , testProperty "shiftR"       (requiring (P.>= 0) (flip test_shiftR :: Array sh Int -> Array sh a -> Property))
          , testProperty "rotate"       (test_rotate :: Array sh a -> Array sh Int -> Property)
          , testProperty "rotateL"      (requiring (P.>= 0) (flip test_rotateL :: Array sh Int -> Array sh a -> Property))
          , testProperty "rotateR"      (requiring (P.>= 0) (flip test_rotateR :: Array sh Int -> Array sh a -> Property))

            -- relational and equality operators
          , testProperty "(<)"          (test_lt  :: Array sh a -> Array sh a -> Property)
          , testProperty "(>)"          (test_gt  :: Array sh a -> Array sh a -> Property)
          , testProperty "(<=)"         (test_lte :: Array sh a -> Array sh a -> Property)
          , testProperty "(>=)"         (test_gte :: Array sh a -> Array sh a -> Property)
          , testProperty "(==)"         (test_eq  :: Array sh a -> Array sh a -> Property)
          , testProperty "(/=)"         (test_neq :: Array sh a -> Array sh a -> Property)
          , testProperty "min"          (test_min :: Array sh a -> Array sh a -> Property)
          , testProperty "max"          (test_max :: Array sh a -> Array sh a -> Property)
          ]
          where
            test_quot xs ys     = runN backend (A.zipWith quot) xs ys ~?= zipWithRef quot xs ys
            test_rem xs ys      = runN backend (A.zipWith rem) xs ys ~?= zipWithRef rem xs ys
            test_quotRem xs ys  = runN backend (A.zipWith (lift $$ quotRem)) xs ys ~?= zipWithRef quotRem xs ys
            test_div xs ys      = runN backend (A.zipWith div) xs ys ~?= zipWithRef div xs ys
            test_mod xs ys      = runN backend (A.zipWith mod) xs ys ~?= zipWithRef mod xs ys
            test_divMod xs ys   = runN backend (A.zipWith (lift $$ divMod)) xs ys ~?= zipWithRef divMod xs ys

            test_band xs ys     = runN backend (A.zipWith (A..&.)) xs ys ~?= zipWithRef (P..&.) xs ys
            test_bor xs ys      = runN backend (A.zipWith (A..|.)) xs ys ~?= zipWithRef (P..|.) xs ys
            test_xor xs ys      = runN backend (A.zipWith A.xor) xs ys ~?= zipWithRef P.xor xs ys

            test_shift xs ys    = runN backend (A.zipWith A.shift) xs ys ~?= zipWithRef P.shift xs ys
            test_shiftL xs ys   = runN backend (A.zipWith A.shiftL) xs ys ~?= zipWithRef P.shiftL xs ys
            test_shiftR xs ys   = runN backend (A.zipWith A.shiftR) xs ys ~?= zipWithRef P.shiftR xs ys
            test_rotate xs ys   = runN backend (A.zipWith A.rotate) xs ys ~?= zipWithRef P.rotate xs ys
            test_rotateL xs ys  = runN backend (A.zipWith A.rotateL) xs ys ~?= zipWithRef P.rotateL xs ys
            test_rotateR xs ys  = runN backend (A.zipWith A.rotateR) xs ys ~?= zipWithRef P.rotateR xs ys

    testFloatingElt :: forall a. (P.RealFrac a, P.RealFloat a, A.RealFloat a, A.RealFrac a, Arbitrary a, Similar a) => (Config :-> Bool) -> a -> Maybe Test
    testFloatingElt ok _
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
            testProperty "(+)"          (test_plus  :: Array sh a -> Array sh a -> Property)
          , testProperty "(-)"          (test_minus :: Array sh a -> Array sh a -> Property)
          , testProperty "(*)"          (test_mult  :: Array sh a -> Array sh a -> Property)

            -- operators on Fractional, Floating, RealFrac & RealFloat
          , testProperty "(/)"          (denom (test_div :: Array sh a -> Array sh a -> Property))
          , testProperty "(**)"         (test_pow :: Array sh a -> Array sh a -> Property)
          , testProperty "atan2"        (test_atan2 :: Array sh a -> Array sh a -> Property)
          , testProperty "logBase"      (requiring (P.> 0) $ \(xs :: Array sh a) ->
                                         requiring (P.> 0) $ \(ys :: Array sh a) -> test_logBase xs ys)

            -- relational and equality operators
          , testProperty "(<)"          (test_lt  :: Array sh a -> Array sh a -> Property)
          , testProperty "(>)"          (test_gt  :: Array sh a -> Array sh a -> Property)
          , testProperty "(<=)"         (test_lte :: Array sh a -> Array sh a -> Property)
          , testProperty "(>=)"         (test_gte :: Array sh a -> Array sh a -> Property)
          , testProperty "(==)"         (test_eq  :: Array sh a -> Array sh a -> Property)
          , testProperty "(/=)"         (test_neq :: Array sh a -> Array sh a -> Property)
          , testProperty "min"          (test_min :: Array sh a -> Array sh a -> Property)
          , testProperty "max"          (test_max :: Array sh a -> Array sh a -> Property)
          ]
          where
            test_div xs ys      = runN backend (A.zipWith (/)) xs ys ~?= zipWithRef (/) xs ys
            test_pow xs ys      = runN backend (A.zipWith (**)) xs ys ~?= zipWithRef (**) xs ys
            test_atan2 xs ys    = runN backend (A.zipWith A.atan2) xs ys ~?= zipWithRef P.atan2 xs ys
            test_logBase xs ys  = runN backend (A.zipWith logBase) xs ys ~?= zipWithRef logBase xs ys

    test_plus xs ys     = runN backend (A.zipWith (+)) xs ys ~?= zipWithRef (+) xs ys
    test_minus xs ys    = runN backend (A.zipWith (-)) xs ys ~?= zipWithRef (-) xs ys
    test_mult xs ys     = runN backend (A.zipWith (*)) xs ys ~?= zipWithRef (*) xs ys

    test_lt xs ys       = runN backend (A.zipWith (A.<))  xs ys ~?= zipWithRef (P.<) xs ys
    test_gt xs ys       = runN backend (A.zipWith (A.>))  xs ys ~?= zipWithRef (P.>) xs ys
    test_lte xs ys      = runN backend (A.zipWith (A.<=)) xs ys ~?= zipWithRef (P.<=) xs ys
    test_gte xs ys      = runN backend (A.zipWith (A.>=)) xs ys ~?= zipWithRef (P.>=) xs ys
    test_eq xs ys       = runN backend (A.zipWith (A.==)) xs ys ~?= zipWithRef (P.==) xs ys
    test_neq xs ys      = runN backend (A.zipWith (A./=)) xs ys ~?= zipWithRef (P./=) xs ys
    test_min xs ys      = runN backend (A.zipWith (A.min)) xs ys ~?= zipWithRef (P.min) xs ys
    test_max xs ys      = runN backend (A.zipWith (A.max)) xs ys ~?= zipWithRef (P.max) xs ys

    {-# INLINE denom #-}
    denom f = forAllShrink arbitrary shrink $ \xs ->
              requiring (P./= 0)            $ \ys -> f xs ys


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

zipWithRef :: (Shape sh, Elt c) => (a -> b -> c) -> Array sh a -> Array sh b -> Array sh c
zipWithRef f xs ys =
  let shx       = fromElt (arrayShape xs)
      shy       = fromElt (arrayShape ys)
      sh        = toElt (R.intersect shx shy)
  in
  fromFunction sh (\ix -> f (xs Sugar.! ix) (ys Sugar.! ix))

