{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Issues.Issue427
-- Copyright   : [2009..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- https://github.com/AccelerateHS/accelerate/issues/427
--

module Data.Array.Accelerate.Test.NoFib.Issues.Issue427 (

  test_issue427

) where

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Test.NoFib.Base
import Data.Array.Accelerate.Test.Similar

import Test.Tasty
import Test.Tasty.Hedgehog

import Hedgehog
import qualified Hedgehog.Gen                                       as Gen
import qualified Hedgehog.Range                                     as Range

import Prelude                                                      as P


test_issue427 :: RunN -> TestTree
test_issue427 runN
  = testGroup "427"
  [ testProperty "n-by-64" $ test_indicesOfTruth runN (by 64) -- should be okay
  , testProperty "n-by-8"  $ test_indicesOfTruth runN (by 8)  -- usually fails
  , testProperty "n-by-m"  $ test_indicesOfTruth runN dim2
  ]
  where
    by x = do
      y <- Gen.int (Range.linear 0 1024)
      pure (Z :. y :. x)


test_indicesOfTruth
    :: RunN
    -> Gen DIM2
    -> Property
test_indicesOfTruth runN dim =
  property $ do
    sh <- forAll dim
    xs <- forAll (array sh Gen.bool)
    let !go = runN indicesOfTruth
    go xs ~~~ indicesOfTruthRef xs


indicesOfTruth :: Acc (Array DIM2 Bool) -> Acc (Vector (Int, Int))
indicesOfTruth xs =
  let
      withInds :: Acc (Array DIM2 (Int, Int, Bool))
      withInds = imap addInds xs

      addInds :: Exp DIM2 -> Exp Bool -> Exp (Int, Int, Bool)
      addInds (I2 x y) b = T3 x y b

      accept :: Exp (Int, Int, Bool) -> Exp Bool
      accept (T3 _ _ b) = b

      dropBool :: Exp (Int, Int, Bool) -> Exp (Int, Int)
      dropBool (T3 x y _) = T2 x y
  in
  A.map dropBool (afst (A.filter accept withInds))

indicesOfTruthRef :: Array DIM2 Bool -> Vector (Int, Int)
indicesOfTruthRef xs =
  let
      withInds         = fromFunction (arrayShape xs) (\ix@(Z :. y :. x) -> (y, x, indexArray xs ix))
      accept (_,_,b)   = b
      dropBool (x,y,_) = (x,y)

      ys = P.map dropBool $ P.filter accept $ toList withInds
      m  = P.length ys
  in
  fromList (Z :. m) ys

