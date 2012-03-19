{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.PrefixSum where

import Prelude                                          as P hiding ( sum )
import Test.QuickCheck
import Data.Label
import Data.Maybe
import Data.Typeable
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Config
import Test.Base
import Arbitrary.Array                                  ()
import Data.Array.Accelerate                            as Acc


-- scan
-- ----

test_scan :: Options -> Test
test_scan opt = testGroup "scan" $ catMaybes
  [ testElt int32  (undefined :: Int32)
  , testElt int32  (undefined :: Word32)
  , testElt int64  (undefined :: Int64)
  , testElt int64  (undefined :: Word64)
  , testElt float  (undefined :: Float)
  , testElt double (undefined :: Double)
  ]
  where
    testElt :: forall e. (Elt e, IsNum e, Ord e, Similar e, Arbitrary e) => (Options :-> Bool) -> e -> Maybe Test
    testElt ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testGroup (show (typeOf (undefined :: e)))
          [
          -- simple tests
          --
            testProperty "scanl"
          $ \(xs :: Vector e)
          -> run opt (Acc.scanl (+) 0 (use xs)) .==. scanlRef (+) 0 xs

          , testProperty "scanl'"
          $ \(xs :: Vector e)
          -> let (vec,sum) = Acc.scanl' (+) 0 (use xs)
             in  (run opt vec, run opt sum) .==. scanl'Ref (+) 0 xs

          , testProperty "scanl1"
          $ \(xs :: Vector e)
          -> arraySize (arrayShape xs) > 0
            ==> run opt (Acc.scanl1 Acc.min (use xs)) .==. scanl1Ref P.min xs

          , testProperty "scanr"
          $ \(xs :: Vector e)
          -> run opt (Acc.scanr (+) 0 (use xs)) .==. scanrRef (+) 0 xs

          , testProperty "scanr'"
          $ \(xs :: Vector e)
          -> let (vec,sum) = Acc.scanr' (+) 0 (use xs)
             in  (run opt vec, run opt sum) .==. scanr'Ref (+) 0 xs

          , testProperty "scanr1"
          $ \(xs :: Vector e)
          -> arraySize (arrayShape xs) > 0
            ==> run opt (Acc.scanr1 Acc.max (use xs)) .==. scanr1Ref P.max xs
          ]


-- Reference implementation
-- ------------------------

scanlRef :: Elt e => (e -> e -> e) -> e -> Vector e -> Vector e
scanlRef f z vec =
  let (Z :. n)  = arrayShape vec
  in  Acc.fromList (Z :. n+1) . P.scanl f z . Acc.toList $ vec

scanl'Ref :: Elt e => (e -> e -> e) -> e -> Vector e -> (Vector e, Scalar e)
scanl'Ref f z vec =
  let (Z :. n)  = arrayShape vec
      result    = P.scanl f z (Acc.toList vec)
  in  (Acc.fromList (Z :. n) result, Acc.fromList Z (drop n result))

scanl1Ref :: Elt e => (e -> e -> e) -> Vector e -> Vector e
scanl1Ref f vec
  = Acc.fromList (arrayShape vec)
  . P.scanl1 f
  . Acc.toList $ vec

scanrRef :: Elt e => (e -> e -> e) -> e -> Vector e -> Vector e
scanrRef f z vec =
  let (Z :. n)  = arrayShape vec
  in  Acc.fromList (Z :. n+1) . P.scanr f z . Acc.toList $ vec

scanr'Ref :: Elt e => (e -> e -> e) -> e -> Vector e -> (Vector e, Scalar e)
scanr'Ref f z vec =
  let (Z :. n)  = arrayShape vec
      result    = P.scanr f z (Acc.toList vec)
  in  (Acc.fromList (Z :. n) (tail result), Acc.fromList Z result)

scanr1Ref :: Elt e => (e -> e -> e) -> Vector e -> Vector e
scanr1Ref f vec
  = Acc.fromList (arrayShape vec)
  . P.scanr1 f
  . Acc.toList $ vec

